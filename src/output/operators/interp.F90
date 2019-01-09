module output_operators_interp

   use output_manager_core
   use field_manager
   use yaml_types
   use output_operators_base

   implicit none

   private

   public type_interp_operator

   type, extends(type_base_operator) :: type_interp_operator
      integer                      :: idim = -1
      integer                      :: idatadim = -1
      character(len=string_length) :: dimension
      character(len=string_length) :: target_dimension_name
      character(len=string_length) :: target_long_name
      character(len=string_length) :: target_standard_name
      real(rk), allocatable        :: target_coordinates(:)
      type (type_field), pointer   :: source_field => null()
      type (type_field), pointer   :: offset_field => null()
      class (type_base_output_field), pointer :: source_coordinate => null()
      class (type_base_output_field), pointer :: offset => null()
      integer                      :: out_of_bounds_treatment = 1
      real(rk)                     :: out_of_bounds_value
      real(rk)                     :: offset_scale = 1._rk
      type (type_dimension), pointer :: target_dimension => null()
   contains
      procedure :: configure
      procedure :: initialize
      procedure :: flag_as_required
      procedure :: before_save
   end type

   type type_interp_dimension
      class (type_interp_operator), pointer :: settings => null()
      type (type_interp_dimension), pointer :: next     => null()
   end type
   type (type_interp_dimension), pointer, save :: first_interp_dimension => null()

contains

   logical function compare_interp_settings(settings1, settings2)
      class (type_interp_operator), intent(in) :: settings1, settings2

      compare_interp_settings = .false.
      if (settings1%dimension /= settings2%dimension) return
      if ((associated(settings1%source_coordinate) .or. associated(settings2%source_coordinate)) .and. .not. associated(settings1%source_coordinate, settings2%source_coordinate)) return
      if ((associated(settings1%offset) .or. associated(settings2%offset)) .and. .not. associated(settings1%offset, settings2%offset)) return
      if (size(settings1%target_coordinates) /= size(settings2%target_coordinates)) return
      if (any(settings1%target_coordinates(:) /= settings2%target_coordinates(:))) return
      compare_interp_settings = .true.
   end function

   subroutine configure(self, mapping, field_manager)
      class (type_interp_operator), intent(inout) :: self
      class (type_dictionary),      intent(in)    :: mapping
      type (type_field_manager),    intent(inout) :: field_manager

      type (type_error),          pointer :: config_error
      type (type_dimension),      pointer :: dim
      character(len=string_length)        :: variable_name
      class (type_list),          pointer :: list
      type (type_list_item),      pointer :: list_item
      integer                             :: i, n
      logical                             :: success

      self%dimension = mapping%get_string('dimension', error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)

      ! Verify target dimension has been registered with field manager
      dim => field_manager%first_dimension
      do while (associated(dim))
         if (dim%name==self%dimension) exit
         dim => dim%next
      end do
      if (.not. associated(dim)) call host%fatal_error('type_interp_operator%initialize', &
         'Dimension "'//trim(self%dimension)//'" has not been registered with the field manager.')

      self%out_of_bounds_treatment = mapping%get_integer('out_of_bounds_treatment', default=1, error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)
      variable_name = mapping%get_string('offset', default='', error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)
      if (variable_name /= '') then
         if (variable_name(1:1) == '-') then
            self%offset_scale = -1._rk
            variable_name = variable_name(2:)
         end if
         self%offset_field => field_manager%select_for_output(trim(variable_name))
      end if
      variable_name = mapping%get_string('source_coordinate', default='', error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)
      if (variable_name /= '') self%source_field => field_manager%select_for_output(trim(variable_name))
      list => mapping%get_list('coordinates', required=.true., error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)
      n = 0
      list_item => list%first
      do while (associated(list_item))
         n = n + 1
         list_item => list_item%next
      end do
      allocate(self%target_coordinates(n))
      list_item => list%first
      do i=1,n
         select type (node => list_item%node)
         class is (type_scalar)
            self%target_coordinates(i) = node%to_real(0._rk, success)
            if (.not. success) call host%fatal_error('type_interp_operator%configure', trim(node%path)//': unable to convert '//trim(node%string)//' to real.')
            if (i > 1) then
               if (self%target_coordinates(i) < self%target_coordinates(i - 1)) call host%fatal_error('type_interp_operator%configure', trim(list%path)//' should be monotonically increasing.')
            end if
         class default
            call host%fatal_error('type_interp_operator%configure', trim(node%path)//' should be a real number.')
         end select
         list_item => list_item%next
      end do
      self%target_dimension_name = mapping%get_string('target_dimension', default='', error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)
      self%target_long_name = mapping%get_string('target_long_name', default='', error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)
      self%target_standard_name = mapping%get_string('target_standard_name', default='', error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)
   end subroutine

   recursive subroutine initialize(self, field_manager)
      class (type_interp_operator), intent(inout), target :: self
      type (type_field_manager),    intent(in)            :: field_manager

      type (type_dimension), pointer :: dim
      type (type_dimension_pointer), allocatable :: dimensions(:)
      type (type_interp_dimension), pointer :: interp_dimension
      character(len=:), allocatable :: long_name, units, standard_name, long_name2
      integer, allocatable :: extents(:)
      integer :: i

      call self%type_base_operator%initialize(field_manager)
      if (self%source%data%is_empty()) return

      call self%source%get_metadata(dimensions=dimensions, fill_value=self%out_of_bounds_value)
      do i = 1, size(dimensions)
         if (dimensions(i)%p%name == self%dimension) self%idim = i
      end do
      if (self%idim == -1) then
         self%data = self%source%data
         return
      end if
      if (dimensions(self%idim)%p%length == 1) call host%fatal_error('type_interp_operator%initialize', &
         'Cannot use interp on dimension ' // trim(self%dimension) // ' because it has length 1.')
      if (self%out_of_bounds_treatment == 1 .and. self%out_of_bounds_value == default_fill_value) &
         call host%fatal_error('type_interp_operator%initialize', 'Cannot use out_of_bounds_value=1 because ' // trim(self%source%output_name) // ' does not have fill_value set.')

      ! Data dim is idim with singletons removed
      self%idatadim = 0
      do i = 1, self%idim
         if (dimensions(i)%p%length > 1) self%idatadim = self%idatadim + 1
      end do

      if (.not. associated(self%source_field)) then
         if (.not. associated(dimensions(self%idim)%p%coordinate)) call host%fatal_error('type_interp_operator%initialize', &
            'Dimension ' // trim(self%dimension) // ' does not have a default coordinate. &
            &You need to explicitly specify the source coordinate with the source_coordinate attribute to the interp operator.')
         self%source_field => dimensions(self%idim)%p%coordinate
      end if
      self%source_coordinate => self%source%get_field(self%source_field)
      call self%source_coordinate%initialize(field_manager)
      if (associated(self%offset_field)) then
         self%offset => self%source%get_field(self%offset_field)
         call self%offset%initialize(field_manager)
      end if

      interp_dimension => first_interp_dimension
      do while (associated(interp_dimension))
         if (compare_interp_settings(self, interp_dimension%settings)) exit
         interp_dimension => interp_dimension%next
      end do
      if (associated(interp_dimension)) then
         self%target_dimension => interp_dimension%settings%target_dimension
      else
         if (self%target_dimension_name == '') then
            i = 0
            do
               i = i + 1
               write (self%target_dimension_name, '(a,i0)') trim(self%dimension), i
               dim => field_manager%first_dimension
               do while (associated(dim))
                  if (dim%name == self%target_dimension_name) exit
                  dim => dim%next
               end do
               if (associated(dim)) cycle
               interp_dimension => first_interp_dimension
               do while (associated(interp_dimension))
                  if (interp_dimension%settings%target_dimension%name == self%target_dimension_name) exit
                  interp_dimension => interp_dimension%next
               end do
               if (.not. associated(interp_dimension)) exit
            end do
         end if
         allocate(self%target_dimension)
         self%target_dimension%name = self%target_dimension_name
         self%target_dimension%length = size(self%target_coordinates)
         allocate(self%target_dimension%coordinate)
         call self%target_dimension%coordinate%data%set(self%target_coordinates)
         self%target_dimension%coordinate%status = status_registered_with_data
         self%target_dimension%coordinate%name = self%target_dimension_name
         call self%source_coordinate%get_metadata(long_name=long_name, units=units, standard_name=standard_name)
         if (self%target_long_name /= '') then
            self%target_dimension%coordinate%long_name = trim(self%target_long_name)
         elseif (.not. associated(self%offset)) then
            self%target_dimension%coordinate%long_name = long_name
         else
            call self%offset%get_metadata(long_name=long_name2)
            self%target_dimension%coordinate%long_name = long_name // ' relative to ' // long_name2
         end if
         if (self%target_standard_name /= '') then
            self%target_dimension%coordinate%standard_name = trim(self%target_standard_name)
         elseif (.not. associated(self%offset)) then
            self%target_dimension%coordinate%standard_name = standard_name
         end if
         self%target_dimension%coordinate%units = units
         allocate(self%target_dimension%coordinate%dimensions(1))
         self%target_dimension%coordinate%dimensions(1)%p => self%target_dimension
         allocate(interp_dimension)
         interp_dimension%settings => self
         interp_dimension%next => first_interp_dimension
         first_interp_dimension => interp_dimension
      end if

      allocate(self%dimensions(size(dimensions)))
      self%dimensions(:) = dimensions
      self%dimensions(self%idim)%p => self%target_dimension

      call self%source%data%get_extents(extents)
      extents(self%idatadim) = size(self%target_coordinates)
      select case (size(extents))
      case (3)
         if (self%idatadim /= 3) call host%fatal_error('type_interp_operator%initialize', 'interp can currently only operate along 3rd dimension of 3D arrays.')
         allocate(self%result_3d(extents(1), extents(2), extents(3)))
         call self%data%set(self%result_3d)
      case (2)
         if (self%idatadim /= 2) call host%fatal_error('type_interp_operator%initialize', 'interp can currently only operate along 2nd dimension of 2D arrays.')
         allocate(self%result_2d(extents(1), extents(2)))
         call self%data%set(self%result_2d)
      case (1)
         allocate(self%result_1d(size(self%target_coordinates)))
         call self%data%set(self%result_1d)
      end select
      call self%fill(self%out_of_bounds_value)
   end subroutine

   recursive subroutine flag_as_required(self, required)
      class (type_interp_operator), intent(inout) :: self
      logical,                      intent(in)    :: required

      call self%type_base_operator%flag_as_required(required)
      if (associated(self%source_coordinate)) call self%source_coordinate%flag_as_required(required)
      if (associated(self%offset)) call self%offset%flag_as_required(required)
   end subroutine

   recursive subroutine before_save(self)
      class (type_interp_operator), intent(inout) :: self

      integer :: i, j
      real(rk) :: offset
      real(rk), allocatable :: source_coordinate(:)

      call self%type_base_operator%before_save()
      if (self%idim == -1) return
      if (associated(self%source%data%p3d)) then
         allocate(source_coordinate(size(self%source%data%p3d, self%idatadim)))
         if (associated(self%source_coordinate%data%p1d)) source_coordinate(:) = self%source_coordinate%data%p1d
         do j=1,size(self%source%data%p3d, 2)
            do i=1, size(self%source%data%p3d, 1)
               offset = 0._rk
               if (associated(self%offset)) offset = self%offset_scale * self%offset%data%p2d(i,j)
               if (associated(self%source_coordinate%data%p3d)) source_coordinate(:) = self%source_coordinate%data%p3d(i,j,:)
               call interp(self%target_coordinates(:) + offset, source_coordinate, self%source%data%p3d(i,j,:), self%result_3d(i,j,:), self%out_of_bounds_treatment, self%out_of_bounds_value)
            end do
         end do
      elseif (associated(self%source%data%p2d)) then
         allocate(source_coordinate(size(self%source%data%p2d, self%idatadim)))
         if (associated(self%source_coordinate%data%p1d)) source_coordinate(:) = self%source_coordinate%data%p1d
         do i=1, size(self%source%data%p2d, 1)
            offset = 0._rk
            if (associated(self%offset)) offset = self%offset_scale * self%offset%data%p1d(i)
            if (associated(self%source_coordinate%data%p2d)) source_coordinate(:) = self%source_coordinate%data%p2d(i,:)
            call interp(self%target_coordinates(:) + offset, source_coordinate, self%source%data%p2d(i,:), self%result_2d(i,:), self%out_of_bounds_treatment, self%out_of_bounds_value)
         end do
      elseif (associated(self%source%data%p1d)) then
         offset = 0._rk
         if (associated(self%offset)) offset = self%offset_scale * self%offset%data%p0d
         call interp(self%target_coordinates(:) + offset, self%source_coordinate%data%p1d, self%source%data%p1d, self%result_1d, self%out_of_bounds_treatment, self%out_of_bounds_value)
      end if
   end subroutine

   subroutine interp(x, xp, fp, f, out_of_bounds_treatment, out_of_bounds_value)
      real(rk), intent(in) :: x(:), xp(:), fp(:)
      real(rk), intent(out) :: f(:)
      integer,  intent(in) :: out_of_bounds_treatment
      real(rk), intent(in) :: out_of_bounds_value

      integer :: i, j, istart, istop
      real(rk) :: slope

      ! Find first non-masked source value
      istart = 0
      do i = 1, size(xp)
         if (fp(i) /= out_of_bounds_value) then
            istart = i
            exit
         end if
      end do

      ! If all source values are masked, return missing value
      if (istart == 0) then
         f(:) = out_of_bounds_value
         return
      end if

      ! Find last non-masked source value
      do istop = size(xp), istart, -1
         if (fp(istop) /= out_of_bounds_value) exit
      end do

      i = istart
      do j = 1, size(x)
         if (out_of_bounds_treatment /= 3 .and. (x(j) < xp(istart) .or. x(j) > xp(istop))) then
            if (out_of_bounds_treatment == 1) then
               ! Use missing value
               f(j) = out_of_bounds_value
            else
               ! Use nearest valid value
               if (x(j) < xp(istart)) then
                  f(j) = fp(istart)
               else
                  f(j) = fp(istop)
               end if
            end if
         else
            do while (i + 1 < istop)
               if (xp(i + 1) >= x(j)) exit
               i = i + 1
            end do
            slope = (fp(i + 1) - fp(i)) / (xp(i + 1) - xp(i))
            f(j) = fp(i) + (x(j) - xp(i)) * slope
         end if
      end do
   end subroutine

end module
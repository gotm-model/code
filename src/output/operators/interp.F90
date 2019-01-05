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
      character(len=string_length) :: dimension
      character(len=string_length) :: target_dimension_name
      real(rk), allocatable        :: target_coordinates(:)
      type (type_field), pointer   :: source_coordinate => null()
      type (type_field), pointer   :: offset => null()
      integer                      :: out_of_bounds_treatment = 1
      real(rk)                     :: out_of_bounds_value
      real(rk)                     :: offset_scale = 1._rk
      type (type_dimension), pointer :: target_dimension => null()
   contains
      procedure :: configure
      procedure :: initialize
      procedure :: get_metadata
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
         self%offset => field_manager%select_for_output(trim(variable_name))
      end if
      variable_name = mapping%get_string('source_coordinate', default='', error=config_error)
      if (associated(config_error)) call host%fatal_error('type_interp_operator%configure', config_error%message)
      if (variable_name /= '') self%source_coordinate => field_manager%select_for_output(trim(variable_name))
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
   end subroutine

   recursive subroutine initialize(self, field_manager)
      class (type_interp_operator), intent(inout), target :: self
      type (type_field_manager),    intent(in)            :: field_manager

      type (type_dimension), pointer :: dim
      type (type_dimension_pointer), allocatable :: dimensions(:)
      type (type_interp_dimension), pointer :: interp_dimension
      integer :: extents(3)
      integer :: i

      call self%type_base_operator%initialize(field_manager)

      call self%source%get_metadata(dimensions=dimensions, fill_value=self%out_of_bounds_value)
      do i = 1, size(dimensions)
         if (dimensions(i)%p%name == self%dimension) self%idim = i
      end do
      if (self%idim == -1) then
         self%data_3d => self%source%data_3d
         self%data_2d => self%source%data_2d
         self%data_1d => self%source%data_1d
         self%data_0d => self%source%data_0d
         return
      end if

      if (.not. associated(self%source_coordinate)) then
         if (.not. associated(dimensions(self%idim)%p%coordinate)) call host%fatal_error('type_interp_operator%initialize', &
            'Dimension ' // trim(self%dimension) // ' does not have a default coordinate. &
            &You need to explicitly specify the source coordinate with the source_coordinate attribute to the interp operator.')
         self%source_coordinate => dimensions(self%idim)%p%coordinate
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
         if (self%target_dimension%length > 1) then
            self%target_dimension%coordinate%data_1d => self%target_coordinates
         else
            self%target_dimension%coordinate%data_0d => self%target_coordinates(1)
         end if
         self%target_dimension%coordinate%status = status_registered_with_data
         self%target_dimension%coordinate%name = self%target_dimension_name
         self%target_dimension%coordinate%long_name = trim(self%source_coordinate%long_name) // ' created by interp'
         self%target_dimension%coordinate%units = self%source_coordinate%units
         allocate(self%target_dimension%coordinate%dimensions(1))
         self%target_dimension%coordinate%dimensions(1)%p => self%target_dimension
         allocate(interp_dimension)
         interp_dimension%settings => self
         interp_dimension%next => first_interp_dimension
         first_interp_dimension => interp_dimension
      end if

      select case (self%source_coordinate%status)
      case (status_not_registered)
         call host%fatal_error('type_interp_operator%initialize', 'Source coordinate "'//trim(self%source_coordinate%name)//'" has not been registered with field manager.')
      case (status_registered_no_data)
         call host%fatal_error('type_interp_operator%initialize', 'Data for source coordinate "'//trim(self%source_coordinate%name)//'" have not been sent to field manager.')
      end select
      if (associated(self%offset)) then
         select case (self%offset%status)
         case (status_not_registered)
            call host%fatal_error('type_interp_operator%initialize', 'Offset "'//trim(self%offset%name)//'" has not been registered with field manager.')
         case (status_registered_no_data)
            call host%fatal_error('type_interp_operator%initialize', 'Data for offset "'//trim(self%offset%name)//'" have not been sent to field manager.')
         end select
      end if

      if (associated(self%source%data_3d)) then
         extents(1:3) = shape(self%source%data_3d)
         extents(self%idim) = size(self%target_coordinates)
         allocate(self%result_3d(extents(1), extents(2), extents(3)))
         if (size(self%target_coordinates) == 1) then
            select case (self%idim)
            case (1)
               self%data_2d => self%result_3d(1,:,:)
            case (2)
               self%data_2d => self%result_3d(:,1,:)
            case (3)
               self%data_2d => self%result_3d(:,:,1)
            end select
         else
            self%data_3d => self%result_3d
         end if
      elseif (associated(self%source%data_2d)) then
         extents(1:2) = shape(self%source%data_2d)
         extents(self%idim) = size(self%target_coordinates)
         allocate(self%result_2d(extents(1), extents(2)))
         if (size(self%target_coordinates) == 1) then
            select case (self%idim)
            case (1)
               self%data_1d => self%result_2d(1,:)
            case (2)
               self%data_1d => self%result_2d(:,1)
            end select
         else
            self%data_2d => self%result_2d
         end if
      elseif (associated(self%source%data_1d)) then
         allocate(self%result_1d(size(self%target_coordinates)))
         if (size(self%target_coordinates) == 1) then
            self%data_0d => self%result_1d(1)
         else
            self%data_1d => self%result_1d
         end if
      end if
      call self%fill(self%out_of_bounds_value)
   end subroutine

   recursive subroutine get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_interp_operator),               intent(in) :: self
      character(len=:),              allocatable, optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), optional :: attributes

      integer :: idim

      call self%type_base_operator%get_metadata(long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      if (present(dimensions)) then
         do idim = 1, size(dimensions)
            if (dimensions(idim)%p%name == self%dimension) dimensions(idim)%p => self%target_dimension
         end do
      end if
   end subroutine

   recursive subroutine flag_as_required(self, required)
      class (type_interp_operator), intent(inout) :: self
      logical,                      intent(in)    :: required

      call self%type_base_operator%flag_as_required(required)
      if (associated(self%source_coordinate)) then
         if (associated(self%source_coordinate%used_now) .and. required) self%source_coordinate%used_now = .true.
      end if
      if (associated(self%offset)) then
         if (associated(self%offset%used_now) .and. required) self%offset%used_now = .true.
      end if
   end subroutine

   recursive subroutine before_save(self)
      class (type_interp_operator), intent(inout) :: self

      integer :: i, j
      real(rk) :: offset
      real(rk), allocatable :: source_coordinate(:)

      call self%type_base_operator%before_save()
      if (self%idim == -1) return
      if (associated(self%source%data_3d)) then
         allocate(source_coordinate(size(self%source%data_3d, self%idim)))
         if (associated(self%source_coordinate%data_1d)) source_coordinate(:) = self%source_coordinate%data_1d
         do j=1,size(self%source_coordinate%data_3d, 2)
            do i=1, size(self%source_coordinate%data_3d, 1)
               offset = 0._rk
               if (associated(self%offset)) offset = self%offset_scale * self%offset%data_2d(i,j)
               if (associated(self%source_coordinate%data_3d)) source_coordinate(:) = self%source_coordinate%data_3d(i,j,:)
               call interp(self%target_coordinates(:) + offset, source_coordinate, self%source%data_3d(i,j,:), self%result_3d(i,j,:), self%out_of_bounds_treatment, self%out_of_bounds_value)
            end do
         end do
      elseif (associated(self%source%data_2d)) then
      elseif (associated(self%source%data_1d)) then
         offset = 0._rk
         if (associated(self%offset)) offset = self%offset_scale * self%offset%data_0d
         call interp(self%target_coordinates(:) + offset, self%source_coordinate%data_1d, self%source%data_1d, self%result_1d, self%out_of_bounds_treatment, self%out_of_bounds_value)
      end if
   end subroutine

   subroutine interp(x, xp, fp, f, out_of_bounds_treatment, out_of_bounds_value)
      real(rk), intent(in) :: x(:), xp(:), fp(:)
      real(rk), intent(out) :: f(:)
      integer,  intent(in) :: out_of_bounds_treatment
      real(rk), intent(in) :: out_of_bounds_value

      integer :: i, j, n, jstart, jstop
      real(rk) :: extreme_value, slope

      n = size(xp)
      if (out_of_bounds_treatment == 3) then
         ! Extrapolate
         jstart = 1
         jstop = size(x)
      else
         ! Use specified out-of-bounds value (out_of_bounds_treatment = 1) or nearest (out_of_bounds_treatment = 2)
         extreme_value = out_of_bounds_value
         if (out_of_bounds_treatment == 2) extreme_value = fp(1)
         do jstart = 1, size(x)
            if (x(jstart) >= xp(1)) exit
            f(jstart) = extreme_value
         end do
         if (out_of_bounds_treatment == 2) extreme_value = fp(n)
         do jstop = size(x), jstart, -1
            if (x(jstop) <= xp(n)) exit
            f(jstop) = extreme_value
         end do
      end if

      i = 1
      do j = jstart, jstop
         do while (i + 1 < n)
            if (xp(i + 1) >= x(j)) exit
            i = i + 1
         end do
         slope = (fp(i + 1) - fp(i)) / (xp(i + 1) - xp(i))
         f(j) = fp(i) + (x(j) - xp(i)) * slope
      end do
   end subroutine

end module
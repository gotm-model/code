module output_filters

   use output_manager_core
   use field_manager
   use yaml_types

   implicit none

   private

   public apply_operators, type_filter, type_time_filter

   type, extends(type_base_output_field) :: type_filter
      class (type_base_output_field), pointer :: source => null()
      real(rk)              :: result_0d
      real(rk), allocatable :: result_1d(:)
      real(rk), allocatable :: result_2d(:,:)
      real(rk), allocatable :: result_3d(:,:,:)
   contains
      procedure :: initialize   => filter_initialize
      procedure :: new_data     => filter_new_data
      procedure :: before_save  => filter_before_save
      procedure :: get_metadata => filter_get_metadata
      procedure :: fill         => filter_fill
   end type

   type, extends(type_filter) :: type_time_filter
      integer :: method = time_method_mean
      integer :: n = 0
   contains
      procedure :: initialize       => time_filter_initialize
      procedure :: flag_as_required => time_filter_flag_as_required
      procedure :: new_data         => time_filter_new_data
      procedure :: before_save      => time_filter_before_save
   end type

   type, extends(type_filter) :: type_interp_filter
      integer :: idim = -1
      character(len=string_length) :: dimension
      real(rk), allocatable        :: target_coordinates(:)
      type (type_field), pointer   :: source_coordinate => null()
      type (type_field), pointer   :: offset => null()
      integer                      :: out_of_bounds_treatment = 1
      real(rk)                     :: out_of_bounds_value
      real(rk)                     :: offset_scale = 1._rk
      type (type_dimension), pointer :: target_dimension => null()
   contains
      procedure :: configure        => interp_configure
      procedure :: initialize       => interp_initialize
      procedure :: get_metadata     => interp_get_metadata
      procedure :: flag_as_required => interp_flag_as_required
      procedure :: before_save      => interp_before_save
   end type

   type type_interp_dimension
      class (type_interp_filter),   pointer :: settings => null()
      type (type_interp_dimension), pointer :: next => null()
   end type
   type (type_interp_dimension), pointer, save :: first_interp_dimension => null()

contains

   subroutine apply_operators(field, list, field_manager)
      class (type_base_output_field), pointer  :: field
      class (type_list),         intent(in)    :: list
      type (type_field_manager), intent(inout) :: field_manager

      type (type_list_item),      pointer :: item
      character(len=string_length)        :: operator_type
      type (type_error),          pointer :: config_error
      class (type_interp_filter), pointer :: interp

      item => list%first
      do while (associated(item))
         select type (mapping=>item%node)
         class is (type_dictionary)
            operator_type = mapping%get_string('type', error=config_error)
            if (associated(config_error)) call host%fatal_error('process_operator', config_error%message)
            select case (operator_type)
            case ('interp')
               allocate(interp)
               call interp%configure(mapping, field_manager)
               interp%source => field
               field => interp
            case default
               call host%fatal_error('apply_operators', trim(mapping%path)//': operator type '//trim(operator_type)//' not recognized.')
            end select
         class default
            call host%fatal_error('apply_operators','Elements below '//trim(list%path)//' must be dictionaries.')
         end select
         item => item%next
      end do
   end subroutine

   recursive subroutine filter_initialize(self, field_manager)
      class (type_filter),       intent(inout), target :: self
      type (type_field_manager), intent(in)            :: field_manager

      if (.not. associated(self%source)) call host%fatal_error('filter_initialize', 'BUG: source has not been set.')
      call self%source%initialize(field_manager)
   end subroutine

   recursive subroutine filter_new_data(self)
      class (type_filter), intent(inout) :: self
      call self%source%new_data()
   end subroutine

   recursive subroutine filter_before_save(self)
      class (type_filter), intent(inout) :: self
      call self%source%before_save()
   end subroutine

   recursive subroutine filter_get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_filter), intent(in) :: self
      character(len=:), allocatable, optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), optional :: attributes
      call self%source%get_metadata(long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
   end subroutine

   subroutine filter_fill(self, value)
      class (type_filter), intent(inout) :: self
      real(rk),            intent(in)    :: value

      if (allocated(self%result_3d)) then
         self%result_3d(:,:,:) = value
      elseif (allocated(self%result_2d)) then
         self%result_2d(:,:) = value
      elseif (allocated(self%result_1d)) then
         self%result_1d(:) = value
      else
         self%result_0d = value
      end if
   end subroutine

   recursive subroutine time_filter_initialize(self, field_manager)
      class (type_time_filter), intent(inout), target :: self
      type (type_field_manager), intent(in)           :: field_manager

      real(rk) :: fill_value

      call self%type_filter%initialize(field_manager)
      if (associated(self%source%data_3d)) then
         allocate(self%result_3d(size(self%source%data_3d,1), size(self%source%data_3d,2), size(self%source%data_3d,3)))
         self%data_3d => self%result_3d
      elseif (associated(self%source%data_2d)) then
         allocate(self%result_2d(size(self%source%data_2d,1), size(self%source%data_2d,2)))
         self%data_2d => self%result_2d
      elseif (associated(self%source%data_1d)) then
         allocate(self%result_1d(size(self%source%data_1d)))
         self%data_1d => self%result_1d
      elseif (associated(self%source%data_0d)) then
         self%data_0d => self%result_0d
      end if
      call self%get_metadata(fill_value=fill_value)
      if (self%method == time_method_mean) call self%fill(fill_value)
   end subroutine

   recursive subroutine time_filter_flag_as_required(self, required)
      class (type_time_filter), intent(inout) :: self
      logical, intent(in) :: required
      call self%source%flag_as_required(.true.)
   end subroutine

   recursive subroutine time_filter_new_data(self)
      class (type_time_filter), intent(inout) :: self

      call self%source%before_save()
      if (self%n == 0) call self%fill(0.0_rk)
      if (allocated(self%result_3d)) then
         self%result_3d(:,:,:) = self%result_3d + self%source%data_3d
      elseif (allocated(self%result_2d)) then
         self%result_2d(:,:) = self%result_2d + self%source%data_2d
      elseif (allocated(self%result_1d)) then
         self%result_1d(:) = self%result_1d + self%source%data_1d
      else
         self%result_0d = self%result_0d + self%source%data_0d
      end if
      self%n = self%n + 1
   end subroutine

   recursive subroutine time_filter_before_save(self)
      class (type_time_filter), intent(inout) :: self

      if (self%method == time_method_mean) then
         if (allocated(self%result_3d)) then
            self%result_3d(:,:,:) = self%result_3d/self%n
         elseif (allocated(self%result_2d)) then
            self%result_2d(:,:) = self%result_2d/self%n
         elseif (allocated(self%result_1d)) then
            self%result_1d(:) = self%result_1d/self%n
         else
            self%result_0d = self%result_0d/self%n
         end if
      end if
      self%n = 0
   end subroutine

   logical function compare_interp_settings(settings1, settings2)
      class (type_interp_filter), intent(in) :: settings1, settings2

      compare_interp_settings = .false.
      if (settings1%dimension /= settings2%dimension) return
      if ((associated(settings1%source_coordinate) .or. associated(settings2%source_coordinate)) .and. .not. associated(settings1%source_coordinate, settings2%source_coordinate)) return
      if ((associated(settings1%offset) .or. associated(settings2%offset)) .and. .not. associated(settings1%offset, settings2%offset)) return
      if (size(settings1%target_coordinates) /= size(settings2%target_coordinates)) return
      if (any(settings1%target_coordinates(:) /= settings2%target_coordinates(:))) return
      compare_interp_settings = .true.
   end function

   recursive subroutine interp_configure(self, mapping, field_manager)
      class (type_interp_filter), intent(inout) :: self
      class (type_dictionary),    intent(in)    :: mapping
      type (type_field_manager),  intent(inout) :: field_manager

      type (type_error),          pointer :: config_error
      character(len=string_length)        :: variable_name
      class (type_list),          pointer :: list
      type (type_list_item),      pointer :: list_item
      integer                             :: i, n
      logical                             :: success

      self%dimension = mapping%get_string('dimension', error=config_error)
      if (associated(config_error)) call host%fatal_error('interp_configure', config_error%message)
      self%out_of_bounds_treatment = mapping%get_integer('out_of_bounds_treatment', default=1, error=config_error)
      if (associated(config_error)) call host%fatal_error('interp_configure', config_error%message)
      variable_name = mapping%get_string('offset', default='', error=config_error)
      if (associated(config_error)) call host%fatal_error('interp_configure', config_error%message)
      if (variable_name /= '') then
         if (variable_name(1:1) == '-') then
            self%offset_scale = -1._rk
            variable_name = variable_name(2:)
         end if
         self%offset => field_manager%select_for_output(trim(variable_name))
      end if
      variable_name = mapping%get_string('source_coordinate', default='', error=config_error)
      if (associated(config_error)) call host%fatal_error('interp_configure', config_error%message)
      if (variable_name /= '') self%source_coordinate => field_manager%select_for_output(trim(variable_name))
      list => mapping%get_list('coordinates', required=.true., error=config_error)
      if (associated(config_error)) call host%fatal_error('interp_configure', config_error%message)
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
            if (.not. success) call host%fatal_error('interp_configure', trim(node%path)//': unable to convert '//trim(node%string)//' to real.')
            if (i > 1) then
               if (self%target_coordinates(i) < self%target_coordinates(i - 1)) call host%fatal_error('interp_configure', trim(list%path)//' should be monotonically increasing.')
            end if
         class default
            call host%fatal_error('interp_configure', trim(node%path)//' should be a real number.')
         end select
         list_item => list_item%next
      end do
   end subroutine

   recursive subroutine interp_initialize(self, field_manager)
      class (type_interp_filter), intent(inout), target :: self
      type (type_field_manager),  intent(in)            :: field_manager

      type (type_dimension), pointer :: dim
      type (type_dimension_pointer), allocatable :: dimensions(:)
      type (type_interp_dimension), pointer :: interp_dimension
      integer :: extents(3)
      integer :: i
      character(len=string_length) :: name

      call self%type_filter%initialize(field_manager)

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
         if (.not. associated(dimensions(self%idim)%p%coordinate)) call host%fatal_error('interp_initialize', &
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
         i = 0
         do
            i = i + 1
            write (name, '(a,i0)') trim(self%dimension), i
            dim => field_manager%first_dimension
            do while (associated(dim))
               if (dim%name==name) exit
               dim => dim%next
            end do
            if (associated(dim)) cycle
            interp_dimension => first_interp_dimension
            do while (associated(interp_dimension))
               if (interp_dimension%settings%target_dimension%name==name) exit
               interp_dimension => interp_dimension%next
            end do
            if (.not. associated(interp_dimension)) exit
         end do
         allocate(self%target_dimension)
         self%target_dimension%name = name
         self%target_dimension%length = size(self%target_coordinates)
         allocate(interp_dimension)
         interp_dimension%settings => self
         interp_dimension%next => first_interp_dimension
         first_interp_dimension => interp_dimension
      end if

      select case (self%source_coordinate%status)
      case (status_not_registered)
         call host%fatal_error('interp_initialize', 'Source coordinate "'//trim(self%source_coordinate%name)//'" has not been registered with field manager.')
      case (status_registered_no_data)
         call host%fatal_error('interp_initialize', 'Data for source coordinate "'//trim(self%source_coordinate%name)//'" have not been sent to field manager.')
      end select
      if (associated(self%offset)) then
         select case (self%offset%status)
         case (status_not_registered)
            call host%fatal_error('interp_initialize', 'Offset "'//trim(self%offset%name)//'" has not been registered with field manager.')
         case (status_registered_no_data)
            call host%fatal_error('interp_initialize', 'Data for offset "'//trim(self%offset%name)//'" have not been sent to field manager.')
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

   recursive subroutine interp_get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_interp_filter), intent(in) :: self
      character(len=:), allocatable, optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), optional :: attributes

      integer :: idim

      call self%type_filter%get_metadata(long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      if (present(dimensions)) then
         do idim = 1, size(dimensions)
            if (dimensions(idim)%p%name == self%dimension) dimensions(idim)%p => self%target_dimension
         end do
      end if
   end subroutine

   recursive subroutine interp_flag_as_required(self, required)
      class (type_interp_filter), intent(inout) :: self
      logical, intent(in) :: required
      call self%source%flag_as_required(required)
      if (associated(self%source_coordinate)) then
         if (associated(self%source_coordinate%used_now) .and. required) self%source_coordinate%used_now = .true.
      end if
      if (associated(self%offset)) then
         if (associated(self%offset%used_now) .and. required) self%offset%used_now = .true.
      end if
   end subroutine

   recursive subroutine interp_before_save(self)
      class (type_interp_filter), intent(inout) :: self
      integer :: i, j
      real(rk) :: offset
      real(rk), allocatable :: source_coordinate(:)

      call self%type_filter%before_save()
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
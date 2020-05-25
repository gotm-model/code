module settings
   use yaml_settings
   use yaml_types, only: rk => real_kind, type_yaml_node => type_node, type_yaml_scalar => type_scalar, type_yaml_null => type_null, type_yaml_dictionary => type_dictionary, type_yaml_error => type_error, type_yaml_list => type_list
   use input, only: type_input, type_scalar_input, type_profile_input, method_unsupported

   implicit none

   private

   public type_gotm_settings, settings_store, type_input_create

   ! From yaml_settings module:
   public type_settings, option, display_normal, display_advanced, display_hidden, rk

   type,extends(type_settings) :: type_gotm_settings
   contains
      procedure :: create_child
      procedure :: get_typed_child
      procedure :: get_input2
      generic :: get => get_input2
   end type

   type,extends(type_gotm_settings) :: type_input_setting
   end type

   type (type_gotm_settings) :: settings_store

contains

   function create_child(self) result(child)
      class (type_gotm_settings), intent(in) :: self
      class (type_settings),  pointer   :: child
      allocate(type_gotm_settings::child)
   end function create_child

   function get_typed_child(self, name, long_name, display) result(child)
      class (type_gotm_settings), intent(inout) :: self
      character(len=*),           intent(in)    :: name
      character(len=*), optional, intent(in)    :: long_name
      integer,          optional, intent(in)    :: display
      class (type_gotm_settings),  pointer      :: child

      class (type_settings),  pointer :: generic_child

      child => null()
      generic_child => self%get_child(name, long_name, display=display)
      select type (generic_child)
      class is (type_gotm_settings)
         child => generic_child
      end select
   end function get_typed_child

   subroutine get_input2(self, target, name, long_name, units, default, minimum, maximum, description, extra_options, method_off, method_constant, method_file, pchild, treat_as_path, display, default_method, order)
      class (type_gotm_settings), intent(inout) :: self
      class (type_input), target                :: target
      character(len=*),           intent(in)    :: name
      character(len=*),           intent(in)    :: long_name
      character(len=*),           intent(in)    :: units
      real(rk),          optional,intent(in)    :: default
      real(rk),          optional,intent(in)    :: minimum
      real(rk),          optional,intent(in)    :: maximum
      character(len=*),  optional,intent(in)    :: description
      type (type_option),optional,intent(in)    :: extra_options(:)
      integer,           optional,intent(in)    :: method_off, method_constant, method_file
      class (type_gotm_settings), optional, pointer :: pchild
      logical,           optional,intent(in)    :: treat_as_path
      integer,           optional,intent(in)    :: display
      integer,           optional,intent(in)    :: default_method
      integer,           optional,intent(in)    :: order

      class (type_settings_node), pointer :: node
      integer                             :: istart

      node => self%get_node(name, treat_as_path=treat_as_path, istart=istart, order=order)
      call type_input_create(node, target, long_name, units, default, minimum, maximum, description, extra_options, method_off, method_constant, method_file, pchild, display, default_method)
   end subroutine

   subroutine type_input_create(node, target, long_name, units, default, minimum, maximum, description, extra_options, method_off, method_constant, method_file, pchild, display, default_method)
      class (type_settings_node), intent(inout) :: node
      class (type_input), target                :: target
      character(len=*),           intent(in)    :: long_name
      character(len=*),           intent(in)    :: units
      real(rk),          optional,intent(in)    :: default
      real(rk),          optional,intent(in)    :: minimum
      real(rk),          optional,intent(in)    :: maximum
      character(len=*),  optional,intent(in)    :: description
      type (type_option),optional,intent(in)    :: extra_options(:)
      integer,           optional,intent(in)    :: method_off, method_constant, method_file
      class (type_gotm_settings), optional, pointer :: pchild
      integer,           optional,intent(in)    :: display
      integer,           optional,intent(in)    :: default_method

      integer :: default_method_
      integer :: noptions
      type (type_option),allocatable :: options(:)
      real(rk)                            :: constant_value
      logical                             :: has_constant_value
      class (type_input_setting), pointer :: setting
      integer                             :: istart
      class(type_yaml_node),      pointer :: node2
      class(type_yaml_scalar),    pointer :: scalar
      type (type_yaml_error),     pointer :: yaml_error
      logical                             :: success

      select type (value => node%value)
      class is (type_input_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select

      istart = index(setting%path, '/', .true.) + 1
      setting%long_name = long_name
      if (present(description)) setting%description = description
      if (present(display)) setting%display = display

      if (present(method_off)) target%method_off = method_off
      if (present(method_constant)) target%method_constant = method_constant
      if (present(method_file)) target%method_file = method_file

      default_method_ = method_unsupported
      has_constant_value = .false.
      if (present(default_method)) default_method_ = default_method
      if (associated(setting%backing_store_node)) then
         select type (yaml_node => setting%backing_store_node)
         class is (type_yaml_dictionary)
            setting%backing_store => yaml_node
         class is (type_yaml_scalar)
            if (target%method_constant == method_unsupported) then
               call report_error(setting%path//' must be a dictionary (constant values are not supported).')
               return
            end if
            constant_value = yaml_node%to_real(default, success)
            if (.not. success) then
               call report_error(setting%path//' is set to a single value "'//trim(yaml_node%string)//'" that cannot be interpreted as a real number.')
               return
            end if
            has_constant_value = .true.
         class is (type_yaml_null)
            call report_error(setting%path//' must be a constant or a dictionary with further information. It cannot be null.')
         class is (type_yaml_list)
            call report_error(setting%path//' must be a constant or a dictionary with further information. It cannot be a list.')
         end select
      end if

      ! Count allowed options
      noptions = 0
      if (target%method_off /= method_unsupported) noptions = noptions + 1
      if (target%method_constant /= method_unsupported) noptions = noptions + 1
      if (target%method_file /= method_unsupported) noptions = noptions + 1
      if (present(extra_options)) noptions = noptions + size(extra_options)

      ! Construct list of options
      allocate(options(noptions))
      noptions = 0
      if (target%method_off /= method_unsupported) then
         noptions = noptions + 1
         options(noptions) = option(target%method_off, 'off')
      end if
      if (target%method_constant /= method_unsupported) then
         noptions = noptions + 1
         options(noptions) = option(target%method_constant, 'constant')
      end if
      if (target%method_file /= method_unsupported) then
         noptions = noptions + 1
         options(noptions) = option(target%method_file, 'from file')
      end if
      if (present(extra_options)) options(noptions + 1:) = extra_options
      if (default_method_ == method_unsupported) default_method_ = options(1)%value

      call setting%get(target%method, 'method', 'method', options=options, default=default_method_)
      if (target%method_constant /= method_unsupported) then
         call setting%get(target%constant_value, 'constant_value', 'value to use throughout the simulation', units=units, default=default, minimum=minimum, maximum=maximum)
         if (has_constant_value) then
            target%method = target%method_constant
            target%constant_value = constant_value
         end if
      end if
      if (target%method_file /= method_unsupported) then
         select type (target)
         class is (type_profile_input)
            call setting%get(target%path, 'file', 'path to file with series of profiles', default='')
         class default
            call setting%get(target%path, 'file', 'path to file with time series', default='')
         end select
         call setting%get(target%index, 'column', 'index of column to read from', default=1)
         call setting%get(target%scale_factor, 'scale_factor', 'scale factor to be applied to values read from file', '', default=1._rk, display=display_advanced)
         call setting%get(target%add_offset, 'offset', 'offset to be added to values read from file', units=units, default=0._rk, display=display_advanced)
      end if
      target%name = setting%path
      if (present(pchild)) pchild => setting

   end subroutine type_input_create

end module
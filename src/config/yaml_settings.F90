module yaml_settings
   
   use yaml_types, only: yaml_real_kind => real_kind, type_yaml_node => type_node, type_yaml_null => type_null, type_yaml_scalar => type_scalar, type_yaml_dictionary => type_dictionary, type_yaml_list => type_list, type_yaml_list_item => type_list_item, type_yaml_error => type_error, type_yaml_key_value_pair => type_key_value_pair
   use yaml, only: yaml_parse => parse, yaml_error_length => error_length

   implicit none

   private

   public type_settings, type_option, type_scalar_setting, report_error, type_real_setting, type_settings_node

   integer, parameter :: rk = yaml_real_kind

   real(rk), parameter :: default_minimum_real = -huge(1._rk)
   real(rk), parameter :: default_maximum_real = huge(1._rk)
   integer, parameter :: default_minimum_integer = -huge(1)
   integer, parameter :: default_maximum_integer = huge(1)

   type type_setting
      character(len=:), allocatable :: long_name
      character(len=:), allocatable :: description
      class (type_yaml_node), pointer :: backing_store_node => null()
      character(len=:), allocatable  :: path
      class (type_setting), pointer :: parent => null()
   contains
      procedure :: write_schema => node_write_schema
      procedure :: write_yaml => node_write_yaml
      procedure :: get_maximum_depth => node_get_maximum_depth
      procedure :: create_child
   end type type_setting

   type type_settings_node
      class (type_setting),   pointer :: value => null()
   contains
      procedure :: as_dictionary => node_as_dictionary
      procedure :: set_value => node_set_value
   end type

   type, extends(type_settings_node) :: type_key_value_pair
      character(len=:), allocatable       :: key
      character(len=:), allocatable       :: name
      type (type_key_value_pair), pointer :: next => null()
   end type

   type, extends(type_settings_node) :: type_list_item
      type (type_list_item), pointer :: next      => null()
   end type

   type,extends(type_setting) :: type_settings
      class (type_yaml_dictionary), pointer :: backing_store => null()
      type (type_key_value_pair),pointer :: first => null()
      type (type_key_value_pair),pointer :: last  => null()
   contains
      procedure :: write_schema => settings_write_schema
      procedure :: write_yaml => settings_write_yaml
      procedure :: get_maximum_depth => settings_get_maximum_depth
      procedure :: load
      procedure :: save
      procedure :: write_schema_file
      procedure :: get_real
      procedure :: get_integer
      procedure :: get_logical
      procedure :: get_string
      procedure :: get_child
      procedure :: get_list
      procedure :: get_node
      procedure :: split_path
      procedure :: populate
      procedure :: check_all_used
      generic :: get => get_real, get_integer, get_logical, get_string
      procedure :: finalize
   end type type_settings

   type,abstract,extends(type_setting) :: type_scalar_setting
      character(:),allocatable :: units
      logical                  :: has_default = .false.
   contains
      procedure (setting_as_string), deferred :: as_string
      procedure :: write_yaml => setting_write_yaml
      procedure :: get_comment => setting_get_comment
      procedure :: get_maximum_depth => setting_get_maximum_depth
   end type type_scalar_setting

   abstract interface
      subroutine setting_as_string(self, string, use_default)
         import type_scalar_setting
         class (type_scalar_setting),intent(in) :: self
         character(len=:), allocatable :: string
         logical,             intent(in) :: use_default
      end subroutine
   end interface

   type type_option
      integer                   :: value
      character(:), allocatable :: long_name
   end type

   type,extends(type_setting) :: type_list
      class (type_yaml_list), pointer :: backing_store => null()
      type (type_list_item), pointer :: first => null()
   contains
      procedure :: write_schema => list_write_schema
      procedure :: write_yaml => list_write_yaml
      procedure :: get_maximum_depth => list_get_maximum_depth
   end type
  
   type,extends(type_scalar_setting) :: type_integer_setting
      integer, pointer :: value => null()
      integer :: default = 0
      integer :: minimum = default_minimum_integer
      integer :: maximum = default_maximum_integer
      type (type_option), allocatable :: options(:)
   contains
      procedure :: as_string => integer_as_string
      procedure :: write_schema => integer_write_schema
      procedure :: get_comment => integer_get_comment
   end type

   type,extends(type_scalar_setting) :: type_real_setting
      real(rk), pointer :: value => null()
      real(rk) :: default = 0.0_rk
      real(rk) :: minimum = default_minimum_real
      real(rk) :: maximum = default_maximum_real
   contains
      procedure :: as_string => real_as_string
      procedure :: write_schema => real_write_schema
      procedure :: get_comment => real_get_comment
   end type

   type,extends(type_scalar_setting) :: type_logical_setting
      logical, pointer :: value => null()
      logical :: default = .true.
   contains
      procedure :: as_string => logical_as_string
      procedure :: write_schema => logical_write_schema
   end type

   type,extends(type_scalar_setting) :: type_string_setting
      character(:), pointer :: value => null()
      character(:), allocatable :: default
   contains
      procedure :: as_string => string_as_string
      procedure :: write_schema => string_write_schema
   end type

contains


   recursive subroutine node_write_schema(self, unit, name, indent)
      class (type_setting), intent(in) :: self
      integer,              intent(in) :: unit, indent
      character(len=*),     intent(in) :: name
   end subroutine

   recursive subroutine node_write_yaml(self, unit, name, indent, comment_depth, header)
      class (type_setting),  intent(in) :: self
      integer,               intent(in) :: unit
      character(len=*),      intent(in) :: name
      integer,               intent(in) :: indent
      integer,               intent(in) :: comment_depth
      logical,               intent(in) :: header
   end subroutine

   recursive function node_get_maximum_depth(self, name) result(maxdepth)
      class (type_setting), intent(in) :: self
      character(len=*),     intent(in) :: name
      integer                          :: maxdepth
   end function

   subroutine load(self, path, unit)
      class (type_settings), intent(inout) :: self
      character(len=*),      intent(in)    :: path
      integer,               intent(in)    :: unit

      class (type_yaml_node),pointer   :: root
      character(len=yaml_error_length) :: error

      if (associated(self%first)) then
         write (*,*) 'Cannot load settings from '//path//' because settings object already contains data.'
         stop 1
      end if
      root => yaml_parse(path, unit, error)
      if (error /= '') then
         write (*,*) trim(error)
         stop 1
      end if
      self%path  =''
      self%backing_store => null()
      if (associated(root)) then
         select type (root)
         class is (type_yaml_dictionary)
            self%backing_store => root
         class is (type_yaml_null)
         class default
            write (*,*) 'YAML file '//path//' should contain a mapping at root level.'
            stop 1
         end select
      end if
   end subroutine load

   logical function check_all_used(self)
      class (type_settings), intent(in) :: self

      integer :: n

      n = 0
      if (associated(self%backing_store)) call node_check(self%backing_store, n)
      check_all_used = n == 0

   contains

      recursive subroutine node_check(self, n)
         class (type_yaml_node), intent(in)    :: self
         integer,                intent(inout) :: n

         type (type_yaml_list_item),      pointer :: item
         type (type_yaml_key_value_pair), pointer :: pair

         select type (self)
         class is (type_yaml_dictionary)
            pair => self%first
            do while (associated(pair))
               if (.not. pair%accessed) then
                  n = n + 1
                  if (n == 1) write (*,*) 'ERROR: the following setting(s) were not recognized:'
                  write (*,*) '- ' // trim(pair%value%path)
               else
                  call node_check(pair%value, n)
               end if
               pair => pair%next
            end do
         class is (type_yaml_list)
            item => self%first
            do while (associated(item))
               call node_check(item%node, n)
               item => item%next
            end do
         end select
      end subroutine

   end function check_all_used

   subroutine save(self, path, unit)
      class (type_settings), intent(in) :: self
      character(len=*),      intent(in) :: path
      integer,               intent(in) :: unit

      integer :: ios
      integer :: comment_depth
      type (type_key_value_pair),pointer :: pair

      open(unit=unit, file=path, action='write', status='replace', iostat=ios)
      if (ios /= 0) then
         write (*,*) 'Failed to open '//path//' for writing.'
         stop 1
      end if
      comment_depth = self%get_maximum_depth('') + 1
      pair => self%first
      do while (associated(pair))
         call pair%value%write_yaml(unit, pair%name, 0, comment_depth, header=.false.)
         pair => pair%next
      end do
   end subroutine save

   subroutine write_schema_file(self, path, unit, version)
      class (type_settings), intent(in) :: self
      character(len=*),      intent(in) :: path
      integer,               intent(in) :: unit
      character(len=*),      intent(in) :: version

      integer :: ios
      type (type_key_value_pair),pointer :: pair

      open(unit=unit, file=path, action='write', status='replace', iostat=ios)
      if (ios /= 0) then
         write (*,*) 'Failed to open '//path//' for writing.'
         stop 1
      end if
      write (unit,'(a)') '<?xml version="1.0" ?>'
      write (unit,'(a,a,a)') '<element name="scenario" label="scenario" version="', version, '" namelistextension=".nml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../core/scenario-1.0.xsd">'
      pair => self%first
      do while (associated(pair))
         call pair%value%write_schema(unit, pair%name, 2)
         pair => pair%next
      end do
      write (unit,'(a)') '</element>'
   end subroutine write_schema_file

   function get_node(self, name) result(node)
      class (type_settings), intent(inout), target :: self
      character(len=*),      intent(in)            :: name
      class (type_settings_node), pointer          :: node

      type (type_key_value_pair), pointer  :: pair

      character(len=len(name))           :: key
      type (type_key_value_pair),pointer :: previous

      key = string_lower(name)

      ! First determine if a setting with this name already exists
      ! If so, move it to the end of the list as we preserve the order is which keys are inserted.
      previous => null()
      pair => self%first
      do while (associated(pair))
         if (pair%key==key) then
            ! Key exists. Move key/value pair to end of the list
            if (associated(self%last,pair)) then
               ! Already last in the list
               exit
            elseif (associated(previous)) then
               ! Second or further down the list (but not last)
               previous%next => pair%next
            elseif (associated(pair%next)) then
               ! First in the list.
               self%first => pair%next
            end if
            self%last%next => pair
            self%last => pair
            pair%next => null()
            exit
         end if
         previous => pair
         pair => pair%next
      end do

      if (.not.associated(pair)) then
         ! Key not found - create a new key-setting pair and append to end of list
         if (.not.associated(self%first)) then
            ! First setting in list
            allocate(self%first)
            self%last => self%first
         else
            ! Look for last element in list.
            allocate(self%last%next)
            self%last => self%last%next
         end if
         pair => self%last
         pair%key = key
         allocate(type_setting::pair%value)
         pair%value%parent => self
         pair%value%path = self%path//'/'//name
         if (associated(self%backing_store)) pair%value%backing_store_node  => self%backing_store%get(name)
      end if

      pair%name = name
      call check(self,'get_node')
      node => pair
   end function get_node
   
   subroutine get_real(self, target, name, long_name, units, default, minimum, maximum, description)
      class (type_settings),intent(inout) :: self
      real(rk), target                               :: target
      character(len=*),                intent(in)    :: name
      character(len=*),                intent(in)    :: long_name
      character(len=*),                intent(in)    :: units
      real(rk),        optional,       intent(in)    :: default
      real(rk),        optional,       intent(in)    :: minimum
      real(rk),        optional,       intent(in)    :: maximum
      character(len=*),optional,       intent(in)    :: description

      class (type_settings),      pointer :: settings
      integer                             :: istart
      class (type_settings_node), pointer :: node
      class (type_real_setting),  pointer :: setting
      logical                             :: success

      call split_path(self, name, settings, istart)
      node => settings%get_node(name(istart:))

      select type (value => node%value)
      class is (type_real_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select
      setting%value => target
      setting%long_name = long_name
      if (units /= '') setting%units = units
      if (present(minimum)) setting%minimum = minimum
      if (present(maximum)) setting%maximum = maximum
      if (present(default)) then
         if (default < setting%minimum) call report_error('Default value of setting '//setting%path//' lies below prescribed minimum.')
         if (default > setting%maximum) call report_error('Default value of setting '//setting%path//' exceeds prescribed maximum.')
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(setting%backing_store_node)) then
         select type (yaml_node => setting%backing_store_node)
         class is (type_yaml_scalar)
            setting%value = yaml_node%to_real(setting%value, success)
            if (.not. success) call report_error(setting%path//' is set to "'//trim(yaml_node%string)//'", which cannot be interpreted as a real number.')
         class default
            call report_error('Setting '//setting%path//' must be a real number.')
         end select
         if (setting%value < setting%minimum) call report_error('Value specified for parameter '//setting%path//' lies below prescribed minimum.')
         if (setting%value > setting%maximum) call report_error('Value specified for parameter '//setting%path//' exceeds prescribed maximum.')
      else
         if (setting%has_default) then
            setting%value = setting%default
         else
            call report_error('No value specified for setting '//setting%path//'; cannot continue because this parameter does not have a default value either.')
         end if
      end if
   end subroutine get_real

   subroutine get_integer(self, target, name, long_name, units, default, minimum, maximum, options, description)
      class (type_settings),       intent(inout) :: self
      integer, target                            :: target
      character(len=*),            intent(in)    :: name
      character(len=*),            intent(in)    :: long_name
      character(len=*),  optional, intent(in)    :: units
      integer,           optional, intent(in)    :: default
      integer,           optional, intent(in)    :: minimum
      integer,           optional, intent(in)    :: maximum
      type (type_option),optional, intent(in)    :: options(:)
      character(len=*),  optional, intent(in)    :: description

      class (type_settings),        pointer :: settings
      integer                               :: istart
      class (type_settings_node),   pointer :: node
      class (type_integer_setting), pointer :: setting
      logical                               :: success
      logical                               :: found
      integer                               :: ioption, ioption2

      call split_path(self, name, settings, istart)
      node => settings%get_node(name(istart:))

      select type (value => node%value)
      class is (type_integer_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select
      setting%value => target
      setting%long_name = long_name
      if (present(units)) setting%units = units
      if (present(minimum)) setting%minimum = minimum
      if (present(maximum)) setting%maximum = maximum
      if (present(options)) then
         do ioption = 1, size(options)
            do ioption2 = ioption + 1, size(options)
               if (options(ioption)%value == options(ioption2)%value) call report_error( &
                  'Setting '//setting%path//' has multiple options with the same integer value.')
            end do
         end do
         if (allocated(setting%options)) deallocate(setting%options)
         allocate(setting%options(size(options)))
         setting%options(:) = options
      end if
      if (present(default)) then
         if (default < setting%minimum) call report_error('Default value of setting '//setting%path//' lies below prescribed minimum.')
         if (default > setting%maximum) call report_error('Default value of setting '//setting%path//' exceeds prescribed maximum.')
         if (allocated(setting%options)) then
            found = .false.
            do ioption = 1, size(setting%options)
               if (default == setting%options(ioption)%value) found = .true.
            end do
            if (.not.found) call report_error('Default value of setting '//setting%path//' does not correspond to any known option.')
         end if
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(setting%backing_store_node)) then
         select type (yaml_node => setting%backing_store_node)
         class is (type_yaml_scalar)
            setting%value = yaml_node%to_integer(setting%value, success)
            if (.not. success) then
               do ioption = 1, size(setting%options)
                  if (yaml_node%string == setting%options(ioption)%long_name) then
                     setting%value = setting%options(ioption)%value
                     success = .true.
                     exit
                  end if
               end do
            end if
            if (.not. success) call report_error(setting%path//' is set to "'//trim(yaml_node%string)//'", which cannot be interpreted as an integer number.')
         class default
            call report_error('Setting '//setting%path//' must be an integer number.')
         end select
         if (setting%value < setting%minimum) call report_error('Value specified for setting '//setting%path//' lies below prescribed minimum.')
         if (setting%value > setting%maximum) call report_error('Value specified for setting '//setting%path//' exceeds prescribed maximum.')
         if (allocated(setting%options)) then
            found = .false.
            do ioption = 1, size(setting%options)
               if (setting%value == setting%options(ioption)%value) found = .true.
            end do
            if (.not.found) call report_error('Value specified for setting '//setting%path//' does not correspond to any known option.')
         end if
      else
         if (setting%has_default) then
            setting%value = setting%default
         else
            call report_error('No value specified for setting '//trim(settings%path)//'/'//name(istart:)//'; cannot continue because it does not have a default value either.')
         end if
      end if
   end subroutine get_integer

   subroutine get_logical(self, target, name, long_name, default, description)
      class (type_settings),    intent(inout) :: self
      logical, target                         :: target
      character(len=*),         intent(in)    :: name
      character(len=*),         intent(in)    :: long_name
      logical, optional,        intent(in)    :: default
      character(len=*),optional,intent(in)    :: description

      class (type_settings),        pointer :: settings
      integer                               :: istart
      class (type_settings_node),   pointer :: node
      class (type_logical_setting), pointer :: setting
      logical                               :: success

      call split_path(self, name, settings, istart)
      node => settings%get_node(name(istart:))

      select type (value => node%value)
      class is (type_logical_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select
      setting%value => target
      setting%long_name = long_name
      if (present(default)) then
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(setting%backing_store_node)) then
         select type (yaml_node => setting%backing_store_node)
         class is (type_yaml_scalar)
            setting%value = yaml_node%to_logical(setting%value, success)
            if (.not. success) call report_error(setting%path//' is set to "'//trim(yaml_node%string)//'", which cannot be interpreted as logical value (true or false).')
         class default
            call report_error('Setting '//setting%path//' must be set to a logical value (true or false).')
         end select
      else
         if (setting%has_default) then
            setting%value = setting%default
         else
            call report_error('No value specified for parameter '//setting%path//'; cannot continue because this parameter does not have a default value either.')
         end if
      end if
   end subroutine get_logical

   subroutine get_string(self, target, name, long_name, units, default, description)
      class (type_settings),           intent(inout) :: self
      character(len=*), target                       :: target
      character(len=*),                intent(in)    :: name
      character(len=*),                intent(in)    :: long_name
      character(len=*),optional,       intent(in)    :: units
      character(len=*),optional,       intent(in)    :: default
      character(len=*),optional,       intent(in)    :: description

      class (type_settings),       pointer :: settings
      integer                              :: istart
      class (type_settings_node),  pointer :: node
      class (type_string_setting), pointer :: setting

      call split_path(self, name, settings, istart)
      node => settings%get_node(name(istart:))

      select type (value => node%value)
      class is (type_string_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select
      setting%value => target
      setting%long_name = long_name
      if (present(units)) setting%units = units
      if (present(default)) then
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(setting%backing_store_node)) then
         select type (yaml_node => setting%backing_store_node)
         class is (type_yaml_null)
            setting%value = ''
         class is (type_yaml_scalar)
            setting%value = trim(yaml_node%string)
         class default
            call report_error(setting%path//' must be be a string or null.')
         end select
      else
         if (setting%has_default) then
            setting%value = setting%default
         else
            call report_error('No value specified for parameter '//trim(settings%path)//'/'//name(istart:)//'; cannot continue because this parameter does not have a default value either.')
         end if
      end if
   end subroutine get_string

   recursive function create_child(self) result(child)
      class (type_setting), intent(in) :: self
      class (type_settings),  pointer   :: child
      if (associated(self%parent)) then
         child => self%parent%create_child()
      else
         allocate(child)
      end if
   end function create_child

   recursive function get_child(self, name, long_name, treat_as_path) result(child)
      class (type_settings), target, intent(inout) :: self
      character(len=*),              intent(in)    :: name
      character(len=*),optional,     intent(in)    :: long_name
      logical, optional,             intent(in)    :: treat_as_path
      class (type_settings),  pointer :: child

      class (type_settings),      pointer :: parent
      integer                             :: istart
      class (type_settings_node), pointer :: node

      call split_path(self, name, parent, istart, treat_as_path)
      node => get_node(parent, name(istart:))
      child => node%as_dictionary(long_name)
   end function get_child

   subroutine node_set_value(self, value)
      class (type_settings_node), intent(inout) :: self
      class (type_setting), target :: value
      value%parent => self%value%parent
      value%path = self%value%path
      value%backing_store_node => self%value%backing_store_node
      deallocate(self%value)
      self%value => value
   end subroutine

   function node_as_dictionary(self, long_name) result(child)
      class (type_settings_node), intent(inout) :: self
      character(len=*), optional, intent(in)    :: long_name
      class (type_settings),  pointer :: child

      select type (value => self%value)
      class is (type_settings)
         child => value
      class default
         child => self%value%parent%create_child()
         call self%set_value(child)
      end select

      if (present(long_name)) child%long_name = long_name
      if (associated(self%value%backing_store_node)) then
         select type (yaml_node => self%value%backing_store_node)
         class is (type_yaml_dictionary)
            child%backing_store => yaml_node
         class default
            call report_error(trim(self%value%path)//' should be a dictionary')
         end select 
      end if
   end function
   
   recursive subroutine get_list(self, name, long_name, treat_as_path, populator)
      class (type_settings), target, intent(inout) :: self
      character(len=*),              intent(in)    :: name
      character(len=*),optional,     intent(in)    :: long_name
      logical, optional,             intent(in)    :: treat_as_path
      interface
         subroutine populator(node)
            import type_settings_node
            class (type_settings_node), intent(inout) :: node
         end subroutine
      end interface

      class (type_settings),      pointer :: parent
      integer                             :: istart
      type (type_settings_node),  pointer :: node
      class (type_list),          pointer :: list
      type (type_yaml_list_item), pointer :: yaml_item
      type (type_list_item),      pointer :: item, last_item
      integer                             :: i
      character(len=8)                    :: strindex

      call split_path(self, name, parent, istart, treat_as_path)
      node => get_node(parent, name(istart:))

      select type (value => node%value)
      class is (type_list)
         list => value
      class default
         allocate(list)
         call node%set_value(list)
      end select

      if (present(long_name)) list%long_name = long_name
      if (associated(list%backing_store_node)) then
         select type (yaml_node => list%backing_store_node)
         class is (type_yaml_list)
            list%backing_store => yaml_node
            last_item => list%first
            yaml_item => yaml_node%first
            i = 1
            do while (associated(yaml_item))
               write (strindex,'(i0)') i
               allocate(item)
               allocate(type_setting::item%value)
               item%value%path = list%path//'['//strindex//']'
               item%value%backing_store_node => yaml_item%node
               item%value%parent => list
               if (.not. associated(last_item)) then
                  list%first => item
               else
                  last_item%next => item
               end if
               last_item => item
               call populator(item)
               yaml_item => yaml_item%next
               i = i + 1
            end do
         class default
            call report_error(trim(node%value%path)//' should be a list')
         end select 
      end if
   end subroutine get_list

   subroutine populate(self, callback)
      class (type_settings), intent(inout) :: self
      interface
         subroutine callback(settings, name)
            import type_settings
            class (type_settings), intent(inout) :: settings
            character(len=*),      intent(in)    :: name
         end subroutine
      end interface

      type (type_yaml_key_value_pair), pointer :: yaml_pair

      if (associated(self%backing_store)) then
         yaml_pair => self%backing_store%first
         do while (associated(yaml_pair))
            call callback(self, trim(yaml_pair%key))
            yaml_pair => yaml_pair%next
         end do           
      end if
   end subroutine populate

   subroutine finalize(self)
      class (type_settings),intent(inout) :: self

      type (type_key_value_pair),pointer :: current, next

      current => self%first
      do while (associated(current))
         next => current%next
         select type (value => current%value)
         class is (type_settings)
            call value%finalize()
         end select
         deallocate(current%value)
         deallocate(current)
         current => next
      end do
      self%first => null()
      self%last  => null()
      call check(self,'finalize')
   end subroutine finalize

   function string_lower(string) result (lowerstring)
       character(len=*),intent(in) :: string
       character(len=len(string))  :: lowerstring

       integer                     :: i,k

       lowerstring = string
       do i = 1,len(string)
           k = iachar(string(i:i))
           if (k>=iachar('A').and.k<=iachar('Z')) then
               k = k + iachar('a') - iachar('A')
               lowerstring(i:i) = achar(k)
           end if
       end do
   end function string_lower

   subroutine check(self, caller)
      class (type_settings),intent(in) :: self
      character(len=*),     intent(in) :: caller
#ifndef NDEBUG
      type (type_key_value_pair),pointer :: current,previous
      previous => null()
      current => self%first
      do while (associated(current))
         !if (.not.associated(current%node)) then         
         !   write (*,*) 'setting not associated after '//caller
         !   stop 1
         !end if
         previous => current
         current => previous%next
      end do
      if ((associated(self%last).or.associated(previous)).and..not.associated(self%last,previous)) then
         write (*,*) 'last does not match actual last node after '//caller
         stop 1
      end if
#endif
   end subroutine check

   subroutine report_error(message)
      character(len=*), intent(in) :: message
      write (*,*) trim(message)
      stop 1
   end subroutine report_error

   subroutine split_path(self, path, settings, istart, treat_as_path)
      class (type_settings), target      :: self
      character(len=*),      intent(in)  :: path
      class (type_settings), pointer     :: settings
      integer,               intent(out) :: istart
      logical, optional,     intent(in)  :: treat_as_path

      logical :: treat_as_path_
      integer :: islash

      istart = 1
      settings => self

      treat_as_path_ = .true.
      if (present(treat_as_path)) treat_as_path_ = treat_as_path
      if (.not. treat_as_path_) return

      do
         islash = index(path(istart:), '/')
         if (islash == 0) exit
         settings => get_child(settings, path(istart:istart+islash-2), treat_as_path=.false.)
         istart = istart + islash
      end do
   end subroutine split_path

   recursive subroutine settings_write_yaml(self, unit, name, indent, comment_depth, header)
      class (type_settings), intent(in) :: self
      integer,               intent(in) :: unit
      character(len=*),      intent(in) :: name
      integer,               intent(in) :: indent
      integer,               intent(in) :: comment_depth
      logical,               intent(in) :: header

      type (type_key_value_pair), pointer  :: pair

      if (header) then
         write (unit, '()')
         write (unit, '("# ",a,a)') repeat(' ', indent), repeat('-', 80)
         call write_header(self, name, indent)
         write (unit, '("# ",a,a)') repeat(' ', indent), repeat('-', 80)
      end if

      write (unit, '(a,a,":",a)', advance='no') repeat(' ', indent), name
      if (allocated(self%long_name)) write (unit,'(a,"# ",a)', advance='no') repeat(' ', comment_depth - indent - len(name) - 1), self%long_name
      write (unit,*)

      pair => self%first
      do while (associated(pair))
         call pair%value%write_yaml(unit, pair%name, indent + 2, comment_depth, header=.false.)
         pair => pair%next
      end do

   contains

      recursive subroutine write_header(self, name, indent)
         class (type_setting), intent(in) :: self
         character(len=*),           intent(in) :: name
         integer,                    intent(in) :: indent

         type (type_key_value_pair), pointer  :: pair
         integer :: ioption
         character(:), allocatable :: strmin, strmax
         logical :: written

         write (unit, '("# ",a,a,": ")', advance='no') repeat(' ', indent), name
         if (allocated(self%long_name)) write (unit, '(a)', advance='no') self%long_name
         write (unit,*)

         select type (self)
         class is (type_settings)
            pair => self%first
            do while (associated(pair))
               call write_header(pair%value, pair%name, indent + 2)
               pair => pair%next
            end do
         class is (type_scalar_setting)
            if (allocated(self%description)) write (unit,'("# ",a,a)') repeat(' ', indent + 2), self%description
            select type (self)
            class is (type_real_setting)
               !write (unit,'(" (",a,")")', advance='no') node%units
               written = .false.
               if (self%maximum /= default_maximum_real) call format_real(self%maximum, strmax)
               if (self%minimum /= default_minimum_real) then
                  call format_real(self%minimum, strmin)
                  write (unit,'("# ",a,a,a)', advance='no') repeat(' ', indent + 2), 'minimum: ', strmin
                  written = .true.
               end if
               if (self%maximum /= default_maximum_real) then
                  call format_real(self%maximum, strmin)
                  if (written) then
                     write (unit,'(", ")', advance='no')
                  else
                     write (unit,'("# ",a)', advance='no') repeat(' ', indent + 2)
                  end if
                  write (unit,'(a,a)', advance='no') 'maximum: ', strmin
                  written = .true.
               end if
               if (self%has_default) then
                  call format_real(self%default, strmin)
                  if (written) then
                     write (unit,'(", ")', advance='no')
                  else
                     write (unit,'("# ",a)', advance='no') repeat(' ', indent + 2)
                  end if
                  write (unit,'(a,a)', advance='no') 'default: ', strmin
                  written = .true.
               end if
               if (written) write (unit,*)
            class is (type_integer_setting)
               !if (allocated(node%units)) write (unit,'(" (",a,")")', advance='no') node%units
               if (allocated(self%options)) then
                  do ioption=1,size(self%options)
                     !if (ioption > 1) write (unit,'(", ")', advance='no')
                     write (unit,'("# ",a,i0,": ",a)') repeat(' ', indent + 2), self%options(ioption)%value, self%options(ioption)%long_name
                  end do
               end if
            end select
         end select
      end subroutine write_header

   end subroutine


   recursive subroutine list_write_yaml(self, unit, name, indent, comment_depth, header)
      class (type_list), intent(in) :: self
      integer,           intent(in) :: unit
      character(len=*),  intent(in) :: name
      integer,           intent(in) :: indent
      integer,           intent(in) :: comment_depth
      logical,           intent(in) :: header

      type (type_list_item), pointer :: item

      write (unit, '(a,a,":",a)', advance='no') repeat(' ', indent), name
      if (allocated(self%long_name)) write (unit,'(a,"# ",a)', advance='no') repeat(' ', comment_depth - indent - len(name) - 1), self%long_name
      write (unit,*)

      item => self%first
      do while (associated(item))
         call item%value%write_yaml(unit, '', indent + 2, comment_depth, header=.false.)
         item => item%next
      end do
   end subroutine

   recursive subroutine setting_write_yaml(self, unit, name, indent, comment_depth, header)
      class (type_scalar_setting), intent(in) :: self
      integer,              intent(in) :: unit
      character(len=*),     intent(in) :: name
      integer,              intent(in) :: indent
      integer,              intent(in) :: comment_depth
      logical,              intent(in) :: header

      character(len=:), allocatable :: string, comment
      integer :: nspaces

      call self%as_string(string, .false.)
      nspaces = comment_depth - indent - len(name) - 2 - len(string)
      write (unit, '(a,a,": ",a,a,"# ")', advance='no') repeat(' ', indent), name, string, repeat(' ', nspaces)
      write (unit,'(a)', advance='no') self%long_name
      if (allocated(self%units)) then
         if (self%units == '-') then
            call append_string(comment, '; ', 'dimensionless')
         elseif (self%units == '1') then
            call append_string(comment, '; ', 'fraction')
         else
            call append_string(comment, '; ', self%units)
         end if
      end if
      call self%get_comment(comment)
      if (self%has_default) then
         call self%as_string(string, .true.)
         call append_string(comment, '; ', 'default=' // string)
      end if
      if (allocated(comment)) write (unit,'(" [",a,"]")', advance='no') comment
      write (unit,*)
   end subroutine setting_write_yaml

   recursive subroutine setting_get_comment(self, comment)
      class (type_scalar_setting), intent(in) :: self
      character(len=:),allocatable, intent(inout) :: comment
   end subroutine
   
   recursive function setting_get_maximum_depth(self, name) result(maxdepth)
      class (type_scalar_setting), intent(in) :: self
      character(len=*),     intent(in) :: name
      integer                          :: maxdepth

      character(len=:), allocatable :: string

      call self%as_string(string, .false.)
      maxdepth = len(name) + 2 + len(string)
   end function

   recursive function settings_get_maximum_depth(self, name) result(maxdepth)
      class (type_settings), intent(in) :: self
      character(len=*),      intent(in) :: name
      integer                           :: maxdepth

      type (type_key_value_pair), pointer :: pair

      maxdepth = len(name) + 1
      pair => self%first
      do while (associated(pair))
         maxdepth = max(maxdepth, pair%value%get_maximum_depth(pair%name) + 2)
         pair => pair%next
      end do
   end function settings_get_maximum_depth

   recursive function list_get_maximum_depth(self, name) result(maxdepth)
      class (type_list), intent(in) :: self
      character(len=*),  intent(in) :: name
      integer                       :: maxdepth

      type (type_list_item), pointer :: item

      maxdepth = len(name) + 1
      item => self%first
      do while (associated(item))
         maxdepth = max(maxdepth, item%value%get_maximum_depth('') + 2)
         item => item%next
      end do
   end function list_get_maximum_depth

   subroutine real_as_string(self, string, use_default)
      class (type_real_setting), intent(in) :: self
      character(len=:), allocatable :: string
      logical, intent(in)                   :: use_default
      if (use_default) then
         call format_real(self%default, string)
      else
         call format_real(self%value, string)
      end if
   end subroutine real_as_string

   subroutine format_real(value, string)
      real(rk), intent(in) :: value
      character(:), allocatable :: string

      integer :: idecimals
      real(rk) :: test
      character(len=15) :: tmp

      idecimals = -1
      if (value < 1.e7_rk) then
         do idecimals = 0, 3
            test = value * 10._rk**idecimals
            if (test == int(test)) exit
         end do
      end if
      select case (idecimals)
      case (0,1)
         write (tmp, '(f15.1)') value
      case (2)
         write (tmp, '(f15.2)') value
      case (3)
         write (tmp, '(f15.3)') value
      case default
         write (tmp, '(e15.8)') value
      end select
      string = trim(adjustl(tmp))
   end subroutine

   recursive subroutine real_get_comment(self, comment)
      class (type_real_setting), intent(in) :: self
      character(len=:),allocatable, intent(inout) :: comment

      character(:), allocatable :: string

      if (self%minimum /= default_minimum_real) then
         call format_real(self%minimum, string)
         call append_string(comment, '; ', 'min=' // string)
      end if
      if (self%maximum /= default_maximum_real) then
         call format_real(self%maximum, string)
         call append_string(comment, '; ', 'max=' // string)
      end if
   end subroutine

   subroutine append_string(target, infix, string)
      character(len=:),allocatable, intent(inout) :: target
      character(len=*), intent(in) :: infix, string

      if (allocated(target)) then
         target = target // infix // string
      else
         target = string
      end if
   end subroutine
   
   recursive subroutine integer_get_comment(self, comment)
      class (type_integer_setting), intent(in) :: self
      character(len=:),allocatable, intent(inout) :: comment

      integer :: ioption
      character(len=8) :: string

      if (allocated(self%options)) then
         do ioption = 1, size(self%options)
            write (string, '(i0)') self%options(ioption)%value
            call append_string(comment, ', ', trim(string) // '=' // self%options(ioption)%long_name)
         end do
      end if
   end subroutine

   recursive subroutine integer_as_string(self, string, use_default)
      class (type_integer_setting), intent(in) :: self
      character(len=:), allocatable            :: string
      logical, intent(in)                      :: use_default

      integer :: value
      character(len=8) :: tmp

      value = self%value
      if (use_default) value = self%default
      write (tmp, '(i0)') value
      string = trim(tmp)
   end subroutine integer_as_string

   recursive subroutine logical_as_string(self, string, use_default)
      class (type_logical_setting), intent(in) :: self
      character(len=:), allocatable :: string
      logical, intent(in)                      :: use_default
      logical :: value
      value = self%value
      if (use_default) value = self%default
      if (value) then
         string = 'true'
      else
         string = 'false'
      end if
   end subroutine logical_as_string

   recursive subroutine string_as_string(self, string, use_default)
      class (type_string_setting), intent(in) :: self
      character(len=:), allocatable :: string
      logical, intent(in)                      :: use_default
      if (use_default) then
         string = self%default
      else
         string = trim(self%value)
      end if
   end subroutine string_as_string

   recursive subroutine settings_write_schema(self, unit, name, indent)
      class (type_settings), intent(in) :: self
      character(len=*),      intent(in) :: name
      integer,               intent(in) :: unit, indent

      type (type_key_value_pair), pointer  :: pair

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      write (unit, '(a)') '>'
      pair => self%first
      do while (associated(pair))
         call pair%value%write_schema(unit, pair%name, indent + 2)
         pair => pair%next
      end do
      write (unit, '(a,a)') repeat(' ', indent), '</element>'
   end subroutine settings_write_schema

   recursive subroutine list_write_schema(self, unit, name, indent)
      class (type_list), intent(in) :: self
      character(len=*),  intent(in) :: name
      integer,           intent(in) :: unit, indent
   end subroutine list_write_schema

   recursive subroutine integer_write_schema(self, unit, name, indent)
      class (type_integer_setting), intent(in) :: self
      character(len=*),             intent(in) :: name
      integer,                      intent(in) :: unit, indent

      integer :: ioption

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '" type="integer"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      if (self%minimum /= default_minimum_integer) write (unit, '(a,i0,a)', advance='no') ' minInclusive="', self%minimum, '"'
      if (self%maximum /= default_maximum_integer) write (unit, '(a,i0,a)', advance='no') ' maxInclusive="', self%maximum, '"'
      if (allocated(self%options)) then
         write (unit, '(a)') '>'
         write (unit, '(a,a)') repeat(' ', indent + 2), '<options>'
         do ioption=1, size(self%options)
            write (unit,'(a,a,i0,a,a,a)') repeat(' ', indent + 4), '<option value="', self%options(ioption)%value, '" label="', self%options(ioption)%long_name, '"/>'
         end do
         write (unit, '(a,a)') repeat(' ', indent + 2), '</options>'
         write (unit, '(a,a)') repeat(' ', indent), '</element>'
      else
         write (unit, '("/>")')
      end if
   end subroutine

   recursive subroutine real_write_schema(self, unit, name, indent)
      class (type_real_setting), intent(in) :: self
      character(len=*),          intent(in) :: name
      integer,                   intent(in) :: unit, indent

      character(:), allocatable :: strvalue

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '" type="float"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      if (self%minimum /= default_minimum_real) then
         call format_real(self%minimum, strvalue)
         write (unit, '(a,a,a)', advance='no') ' minInclusive="', strvalue, '"'
      end if
      if (self%maximum /= default_maximum_real) then
         call format_real(self%maximum, strvalue)
         write (unit, '(a,a,a)', advance='no') ' maxInclusive="', strvalue, '"'
      end if
      write (unit, '("/>")')
   end subroutine

   recursive subroutine logical_write_schema(self, unit, name, indent)
      class (type_logical_setting), intent(in) :: self
      character(len=*),             intent(in) :: name
      integer,                      intent(in) :: unit, indent

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '" type="bool"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      write (unit, '("/>")')
   end subroutine

   recursive subroutine string_write_schema(self, unit, name, indent)
      class (type_string_setting), intent(in) :: self
      character(len=*),            intent(in) :: name
      integer,                     intent(in) :: unit, indent

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '" type="string"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      write (unit, '("/>")')
   end subroutine

end module yaml_settings

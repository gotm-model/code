module yaml_settings
   
   use yaml_types, only: yaml_real_kind => real_kind, type_yaml_node => type_node, type_yaml_null => type_null, type_yaml_dictionary => type_dictionary, type_yaml_error => type_error, type_yaml_key_value_pair => type_key_value_pair
   use yaml, only: yaml_parse => parse, yaml_error_length => error_length

   implicit none

   private

   public type_settings, type_option, type_key_value_pair, type_setting, report_error, type_real_setting

   integer, parameter :: rk = yaml_real_kind

   real(rk), parameter :: default_minimum_real = -huge(1._rk)
   real(rk), parameter :: default_maximum_real = huge(1._rk)
   integer, parameter :: default_minimum_integer = -huge(1)
   integer, parameter :: default_maximum_integer = huge(1)

   type,abstract :: type_settings_node
      character(len=:), allocatable :: long_name
      character(len=:), allocatable :: description
   contains
      procedure (node_write_schema),      deferred :: write_schema
      procedure (node_write_yaml),        deferred :: write_yaml
      procedure (node_get_maximum_depth), deferred :: get_maximum_depth
   end type type_settings_node
   
   type type_key_value_pair
      character(len=:), allocatable          :: key
      character(len=:), allocatable          :: name
      class (type_settings_node),    pointer :: node      => null()
      type (type_key_value_pair),    pointer :: next      => null()
   end type

   type,extends(type_settings_node) :: type_settings
      character(len=:), allocatable :: path
      class (type_yaml_dictionary), pointer :: backing_store => null()
      type (type_key_value_pair),pointer :: first => null()
      type (type_key_value_pair),pointer :: last  => null()
   contains
      procedure :: load
      procedure :: save
      procedure :: write_schema => settings_write_schema
      procedure :: write_yaml => settings_write_yaml
      procedure :: write_schema_file
      procedure :: get_real
      procedure :: get_integer
      procedure :: get_logical
      procedure :: get_string
      procedure :: get_child
      procedure :: get_node
      procedure :: split_path
      procedure :: create_child
      procedure :: get_maximum_depth => settings_get_maximum_depth
      generic :: get => get_real, get_integer, get_logical, get_string
      procedure :: finalize
   end type type_settings

   type,abstract,extends(type_settings_node) :: type_setting
      character(:),allocatable :: units
      logical                  :: has_default = .false.
   contains
      procedure (setting_as_string), deferred :: as_string
      procedure :: write_yaml => setting_write_yaml
      procedure :: get_comment => setting_get_comment
      procedure :: get_maximum_depth => setting_get_maximum_depth
   end type type_setting

   abstract interface
      subroutine setting_as_string(self, string, use_default)
         import type_setting
         class (type_setting),intent(in) :: self
         character(len=:), allocatable :: string
         logical,             intent(in) :: use_default
      end subroutine
   end interface

   abstract interface
      recursive subroutine node_write_schema(self, unit, name, indent)
         import type_settings_node
         class (type_settings_node), intent(in) :: self
         integer,                    intent(in) :: unit, indent
         character(len=*),           intent(in) :: name
      end subroutine
   end interface

   abstract interface
      recursive subroutine node_write_yaml(self, unit, name, indent, comment_depth, header)
         import type_settings_node
         class (type_settings_node), intent(in) :: self
         integer,               intent(in) :: unit
         character(len=*),      intent(in) :: name
         integer,               intent(in) :: indent
         integer,               intent(in) :: comment_depth
         logical,               intent(in) :: header
      end subroutine
   end interface

   abstract interface
      recursive function node_get_maximum_depth(self, name) result(maxdepth)
         import type_settings_node
         class (type_settings_node), intent(in) :: self
         character(len=*),           intent(in) :: name
         integer                                :: maxdepth
      end function
   end interface

   type type_option
      integer                   :: value
      character(:), allocatable :: long_name
   end type

   type,extends(type_setting) :: type_integer_setting
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

   type,extends(type_setting) :: type_real_setting
      real(rk), pointer :: value => null()
      real(rk) :: default = 0.0_rk
      real(rk) :: minimum = default_minimum_real
      real(rk) :: maximum = default_maximum_real
   contains
      procedure :: as_string => real_as_string
      procedure :: write_schema => real_write_schema
      procedure :: get_comment => real_get_comment
   end type

   type,extends(type_setting) :: type_logical_setting
      logical, pointer :: value => null()
      logical :: default = .true.
   contains
      procedure :: as_string => logical_as_string
      procedure :: write_schema => logical_write_schema
   end type

   type,extends(type_setting) :: type_string_setting
      character(:), pointer :: value => null()
      character(:), allocatable :: default
   contains
      procedure :: as_string => string_as_string
      procedure :: write_schema => string_write_schema
   end type

contains

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
      nullify(self%backing_store)
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
         call pair%node%write_yaml(unit, pair%name, 0, comment_depth, header=.false.)
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
         call pair%node%write_schema(unit, pair%name, 2)
         pair => pair%next
      end do
      write (unit,'(a)') '</element>'
   end subroutine write_schema_file

   function get_node(self, name) result(pair)
      class (type_settings), intent(inout) :: self
      character(len=*),      intent(in)    :: name
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
      end if

      pair%name = name
      call check(self,'get_node')
   end function get_node

   function get_real_setting(self,name) result(setting)
      class (type_settings),intent(inout) :: self
      character(len=*),                intent(in)    :: name
      class (type_real_setting), pointer :: setting

      type (type_key_value_pair),pointer :: pair
      pair => self%get_node(name)

      if (associated(pair%node)) then
         select type (node => pair%node)
         class is (type_real_setting)
            setting => node
            return
         end select
         deallocate(pair%node)
      end if
      allocate(setting)
      pair%node => setting
   end function get_real_setting

   function get_integer_setting(self,name) result(setting)
      class (type_settings),intent(inout) :: self
      character(len=*),                intent(in)    :: name
      class (type_integer_setting), pointer :: setting

      type (type_key_value_pair),pointer :: pair
      pair => self%get_node(name)

      if (associated(pair%node)) then
         select type (node => pair%node)
         class is (type_integer_setting)
            setting => node
            return
         end select
         deallocate(pair%node)
      end if
      allocate(setting)
      pair%node => setting
   end function get_integer_setting

   function get_logical_setting(self,name) result(setting)
      class (type_settings),intent(inout) :: self
      character(len=*),                intent(in)    :: name
      class (type_logical_setting), pointer :: setting

      type (type_key_value_pair),pointer :: pair
      pair => self%get_node(name)

      if (associated(pair%node)) then
         select type (node => pair%node)
         class is (type_logical_setting)
            setting => node
            return
         end select
         deallocate(pair%node)
      end if
      allocate(setting)
      pair%node => setting
   end function get_logical_setting

   function get_string_setting(self,name) result(setting)
      class (type_settings),intent(inout) :: self
      character(len=*),                intent(in)    :: name
      class (type_string_setting), pointer :: setting

      type (type_key_value_pair),pointer :: pair
      pair => self%get_node(name)

      if (associated(pair%node)) then
         select type (node => pair%node)
         class is (type_string_setting)
            setting => node
            return
         end select
         deallocate(pair%node)
      end if
      allocate(setting)
      pair%node => setting
   end function get_string_setting
   
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

      class (type_settings),     pointer :: settings
      integer                            :: istart
      class (type_real_setting), pointer :: setting
      type (type_yaml_error),    pointer :: yaml_error

      call split_path(self, name, settings, istart)

      setting => get_real_setting(settings, name(istart:))
      setting%value => target
      setting%long_name = long_name
      if (units /= '') setting%units = units
      if (present(minimum)) setting%minimum = minimum
      if (present(maximum)) setting%maximum = maximum
      if (present(default)) then
         if (default < setting%minimum) call report_error('Default value of parameter '//trim(settings%path)//'/'//name(istart:)//' lies below prescribed minimum.')
         if (default > setting%maximum) call report_error('Default value of parameter '//trim(settings%path)//'/'//name(istart:)//' exceeds prescribed maximum.')
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(settings%backing_store)) then
         setting%value = settings%backing_store%get_real(name(istart:), default, yaml_error)
         if (associated(yaml_error)) then
            call report_error(trim(yaml_error%message))
         else
            if (setting%value < setting%minimum) call report_error('Value specified for parameter '//trim(settings%path)//'/'//name(istart:)//' lies below prescribed minimum.')
            if (setting%value > setting%maximum) call report_error('Value specified for parameter '//trim(settings%path)//'/'//name(istart:)//' exceeds prescribed maximum.')
         end if
      else
         if (setting%has_default) then
            setting%value = setting%default
         else
            call report_error('No value specified for parameter '//trim(settings%path)//'/'//name(istart:)//'; cannot continue because this parameter does not have a default value either.')
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
      class (type_integer_setting), pointer :: setting
      type (type_yaml_error),       pointer :: yaml_error
      logical                               :: found
      integer                               :: ioption, ioption2

      call split_path(self, name, settings, istart)

      setting => get_integer_setting(settings, name(istart:))
      setting%value => target
      setting%long_name = long_name
      if (present(units)) setting%units = units
      if (present(minimum)) setting%minimum = minimum
      if (present(maximum)) setting%maximum = maximum
      if (present(options)) then
         do ioption = 1, size(options)
            do ioption2 = ioption + 1, size(options)
               if (options(ioption)%value == options(ioption2)%value) call report_error( &
                  'Setting '//trim(settings%path)//'/'//name(istart:)//' has multiple options with the same integer value.')
            end do
         end do
         allocate(setting%options(size(options)))
         setting%options(:) = options
      end if
      if (present(default)) then
         if (default < setting%minimum) call report_error('Default value of setting '//trim(settings%path)//'/'//name(istart:)//' lies below prescribed minimum.')
         if (default > setting%maximum) call report_error('Default value of setting '//trim(settings%path)//'/'//name(istart:)//' exceeds prescribed maximum.')
         if (allocated(setting%options)) then
            found = .false.
            do ioption = 1, size(setting%options)
               if (default == setting%options(ioption)%value) found = .true.
            end do
            if (.not.found) call report_error('Default value of setting '//trim(settings%path)//'/'//name(istart:)//' does not correspond to any known option.')
         end if
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(settings%backing_store)) then
         setting%value = settings%backing_store%get_integer(name(istart:), default, yaml_error)
         if (associated(yaml_error)) then
            call report_error(trim(yaml_error%message))
         else
            if (setting%value < setting%minimum) call report_error('Value specified for setting '//trim(settings%path)//'/'//name(istart:)//' lies below prescribed minimum.')
            if (setting%value > setting%maximum) call report_error('Value specified for setting '//trim(settings%path)//'/'//name(istart:)//' exceeds prescribed maximum.')
            if (allocated(setting%options)) then
               found = .false.
               do ioption = 1, size(setting%options)
                  if (setting%value == setting%options(ioption)%value) found = .true.
               end do
               if (.not.found) call report_error('Value specified for setting '//trim(settings%path)//'/'//name(istart:)//' does not correspond to any known option.')
            end if
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
      class (type_logical_setting), pointer :: setting
      type (type_yaml_error),       pointer :: yaml_error

      call split_path(self, name, settings, istart)

      setting => get_logical_setting(settings, name(istart:))
      setting%value => target
      setting%long_name = long_name
      if (present(default)) then
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(settings%backing_store)) then
         setting%value = settings%backing_store%get_logical(name(istart:), default, yaml_error)
         if (associated(yaml_error)) call report_error(trim(yaml_error%message))
      else
         if (setting%has_default) then
            setting%value = setting%default
         else
            call report_error('No value specified for parameter '//trim(settings%path)//'/'//name(istart:)//'; cannot continue because this parameter does not have a default value either.')
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
      class (type_string_setting), pointer :: setting
      class (type_yaml_node),      pointer :: node
      type (type_yaml_error),      pointer :: yaml_error

      call split_path(self, name, settings, istart)

      setting => get_string_setting(settings, name(istart:))
      setting%value => target
      setting%long_name = long_name
      if (present(units)) setting%units = units
      if (present(default)) then
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(settings%backing_store)) then
         node => settings%backing_store%get(name(istart:))
         if (associated(node)) then
            select type (node)
            class is (type_yaml_null)
               setting%value = ''
               return
            end select
         end if
         setting%value = settings%backing_store%get_string(name(istart:), default, yaml_error)
         if (associated(yaml_error)) call report_error(trim(yaml_error%message))
      else
         if (setting%has_default) then
            setting%value = setting%default
         else
            call report_error('No value specified for parameter '//trim(settings%path)//'/'//name(istart:)//'; cannot continue because this parameter does not have a default value either.')
         end if
      end if
   end subroutine get_string

   function create_child(self) result(child)
      class (type_settings), intent(in) :: self
      class (type_settings),  pointer   :: child
      allocate(child)
   end function create_child

   function get_child(self, name, long_name) result(child)
      class (type_settings),     intent(inout) :: self
      character(len=*),          intent(in)    :: name
      character(len=*),optional, intent(in)    :: long_name
      class (type_settings),  pointer :: child

      type (type_key_value_pair),     pointer :: pair
      type (type_yaml_error),         pointer :: yaml_error

      pair => get_node(self,name)

      child => null()
      if (associated(pair%node)) then
         select type (node => pair%node)
         class is (type_settings)
            child => node
         class default
            deallocate(pair%node)
         end select
      end if
      if (.not.associated(child)) then
         child => self%create_child()
         pair%node => child
      end if
      child%path = trim(self%path)//'/'//name
      if (present(long_name)) child%long_name = long_name
      if (associated(self%backing_store)) then
         child%backing_store => self%backing_store%get_dictionary(pair%key, required=.false., error=yaml_error)
         if (associated(yaml_error)) call report_error(trim(yaml_error%message))
      end if
   end function get_child

   subroutine populate(self, callback)
      class (type_settings), intent(inout) :: self
      interface
         subroutine callback(settings, name)
            import type_settings
            class (type_settings), intent(inout) :: settings
            character(len=*),      intent(in)    :: name
         end subroutine
      end interface

      type (type_yaml_key_value_pair),pointer :: yaml_pair

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
         select type (node => current%node)
         class is (type_settings)
            call node%finalize()
         end select
         deallocate(current%node)
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

   subroutine split_path(self, path, settings, istart)
      class (type_settings), target      :: self
      character(len=*),      intent(in)  :: path
      class (type_settings), pointer     :: settings
      integer,               intent(out) :: istart

      integer :: islash

      istart = 1
      settings => self
      do
         islash = index(path(istart:), '/')
         if (islash == 0) exit
         settings => settings%get_child(path(istart:istart+islash-2))
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
         call pair%node%write_yaml(unit, pair%name, indent + 2, comment_depth, header=.false.)
         pair => pair%next
      end do

   contains

      recursive subroutine write_header(self, name, indent)
         class (type_settings_node), intent(in) :: self
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
               call write_header(pair%node, pair%name, indent + 2)
               pair => pair%next
            end do
         class is (type_setting)
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
   
   recursive subroutine setting_write_yaml(self, unit, name, indent, comment_depth, header)
      class (type_setting), intent(in) :: self
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
      class (type_setting), intent(in) :: self
      character(len=:),allocatable, intent(inout) :: comment
   end subroutine
   
   recursive function setting_get_maximum_depth(self, name) result(maxdepth)
      class (type_setting), intent(in) :: self
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
      character(len=:), allocatable :: string

      maxdepth = len(name) + 1
      pair => self%first
      do while (associated(pair))
         maxdepth = max(maxdepth, pair%node%get_maximum_depth(pair%name) + 2)
         pair => pair%next
      end do
   end function settings_get_maximum_depth

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
         call pair%node%write_schema(unit, pair%name, indent + 2)
         pair => pair%next
      end do
      write (unit, '(a,a)') repeat(' ', indent), '</element>'
   end subroutine settings_write_schema

   recursive subroutine integer_write_schema(self, unit, name, indent)
      class (type_integer_setting), intent(in) :: self
      character(len=*),             intent(in) :: name
      integer,                      intent(in) :: unit, indent

      integer :: ioption
      character(:), allocatable :: strvalue

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

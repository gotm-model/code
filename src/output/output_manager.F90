module output_manager

   use field_manager
   use output_manager_core
   use netcdf_output
   use text_output
   use output_operators_library
   use output_operators_time_average
   use output_operators_slice

   use yaml_types
   use yaml,yaml_parse=>parse,yaml_error_length=>error_length

   implicit none

   public output_manager_init, output_manager_prepare_save, output_manager_save, output_manager_clean, output_manager_add_file

   private

   class (type_file),pointer :: first_file
   logical                   :: files_initialized

   interface output_manager_save
      module procedure output_manager_save1
      module procedure output_manager_save2
   end interface

contains

   subroutine output_manager_init(field_manager,title,postfix)
      type (type_field_manager), target :: field_manager
      character(len=*),           intent(in) :: title
      character(len=*), optional, intent(in) :: postfix

      if (.not.associated(host)) then
         write (*,*) 'output_manager_init: the host of an output manager must set the host pointer before calling output_manager_init'
         stop 1
      end if
      nullify(first_file)
      files_initialized = .false.
      call configure_from_yaml(field_manager,title,postfix)
   end subroutine

   subroutine output_manager_clean()
      class (type_file),pointer :: file
      file => first_file
      do while (associated(file))
         call file%finalize()
         file => file%next
      end do
   end subroutine

   subroutine populate(file)
      class (type_file), intent(inout) :: file

      type (type_output_item),               pointer :: item
      type (type_field_set)                          :: set
      class (type_field_set_member),         pointer :: member, next_member
      class (type_output_variable_settings), pointer :: settings
      type (type_dictionary)                         :: mapping
      class (type_base_output_field),        pointer :: output_field, coordinate_field
      type (type_dimension_pointer), allocatable     :: dimensions(:)
      integer                                        :: i

      ! First add fields selected by name
      ! (they take priority over fields included with wildcard expressions)
      item => file%first_item
      do while (associated(item))
         if (associated(item%field)) call create_field(item%settings, item%field, trim(item%name), .false.)
         item => item%next
      end do

      item => file%first_item
      do while (associated(item))
         if (associated(item%category)) then
            call host%log_message('Processing output category /'//trim(item%name)//':')
            if (.not.item%category%has_fields()) call host%fatal_error('collect_from_categories','No variables have been registered under output category "'//trim(item%name)//'".')
            call item%category%get_all_fields(set, item%output_level)
            member => set%first
            if (.not.associated(member)) call host%log_message('WARNING: output category "'//trim(item%name)//'" does not contain any variables with requested output level.')
            do while (associated(member))
               call host%log_message('  - '//trim(member%field%name))
               settings => file%create_settings()
               call settings%initialize(mapping, item%settings)
               call create_field(settings, member%field, trim(item%prefix) // trim(member%field%name) // trim(item%postfix), .true.)
               next_member => member%next
               deallocate(member)
               member => next_member
            end do
            set%first => null()
         end if
         item => item%next
      end do

      output_field => file%first_field
      do while (associated(output_field))
         call output_field%get_metadata(dimensions=dimensions)
         allocate(output_field%coordinates(size(dimensions)))
         do i=1,size(dimensions)
            if (.not.associated(dimensions(i)%p)) cycle
            if (.not.associated(dimensions(i)%p%coordinate)) cycle
            coordinate_field => find_field(dimensions(i)%p%coordinate%name)
            if (.not. associated(coordinate_field)) then
               coordinate_field => output_field%get_field(dimensions(i)%p%coordinate)
               if (associated(coordinate_field)) call append_field(trim(dimensions(i)%p%coordinate%name), coordinate_field, file%create_settings())
            end if
            output_field%coordinates(i)%p => coordinate_field
         end do
         output_field => output_field%next
      end do

   contains

      function find_field(output_name) result(field)
         character(len=*), intent(in)            :: output_name
         class (type_base_output_field), pointer :: field
         field => file%first_field
         do while (associated(field))
            if (field%output_name == output_name) return
            field => field%next
         end do
      end function

      subroutine create_field(settings, field, output_name, ignore_if_exists)
         class (type_output_variable_settings), target :: settings
         type (type_field),                     target :: field
         character(len=*), intent(in)                  :: output_name
         logical,          intent(in)                  :: ignore_if_exists

         class (type_base_operator),         pointer :: final_operator
         class (type_base_output_field),     pointer :: output_field
         class (type_time_average_operator), pointer :: time_filter
         
         output_field => find_field(output_name)
         if (associated(output_field)) then
            if (.not. ignore_if_exists) call host%fatal_error('create_field', 'A different output field with name "'//output_name//'" already exists.')
            return
         end if

         final_operator => settings%final_operator
         if (settings%time_method /= time_method_instantaneous .and. settings%time_method /= time_method_none) then
            ! Apply time averaging/integration operator
            allocate(time_filter)
            time_filter%method = settings%time_method
            time_filter%previous => final_operator
            final_operator => time_filter
         end if

         output_field => wrap_field(field)

         if (associated(final_operator)) output_field => final_operator%apply_all(output_field)
         if (associated(output_field)) call append_field(output_name, output_field, settings)
      end subroutine

      subroutine append_field(output_name, output_field, settings)
         character(len=*),               intent(in)            :: output_name
         class (type_base_output_field), intent(inout), target :: output_field
         class (type_output_variable_settings), target         :: settings

         class (type_base_output_field), pointer :: last_field

         output_field%settings => settings
         output_field%output_name = trim(output_name)

         if (associated(file%first_field)) then
            last_field => file%first_field
            do while (associated(last_field%next))
               last_field => last_field%next
            end do
            last_field%next => output_field
         else
            file%first_field => output_field
         end if
      end subroutine

   end subroutine populate

   subroutine initialize_files(julianday,secondsofday,microseconds,n)
      integer, intent(in) :: julianday,secondsofday,microseconds,n

      class (type_file), pointer :: file

      file => first_file
      do while (associated(file))
         call populate(file)

         ! If we do not have a start time yet, use current.
         if (file%first_julian <= 0) then
            file%first_julian = julianday
            file%first_seconds = secondsofday
         end if

         ! Create output file
         call file%initialize()

         file => file%next
      end do
      files_initialized = .true.
   end subroutine

   subroutine output_manager_save1(julianday,secondsofday,n)
      integer,intent(in) :: julianday,secondsofday,n
      call output_manager_save2(julianday,secondsofday,0,n)
   end subroutine

   subroutine output_manager_prepare_save(julianday, secondsofday, microseconds, n)
      integer,intent(in) :: julianday, secondsofday, microseconds, n

      class (type_file),              pointer :: file
      class (type_base_output_field), pointer :: output_field
      logical                                 :: required

      if (.not. files_initialized) call initialize_files(julianday, secondsofday, microseconds, n)

      ! Start by marking all fields as not needing computation
      if (associated(first_file)) call first_file%field_manager%reset_used()

      file => first_file
      do while (associated(file))
         if (in_window(file, julianday, secondsofday, microseconds, n)) then
            output_field => file%first_field
            do while (associated(output_field))
               select case (file%time_unit)
               case (time_unit_dt)
                  required = file%first_index == -1 .or. mod(n - file%first_index, file%time_step) == 0
               case default
                  required = file%next_julian == -1 .or. (julianday == file%next_julian .and. secondsofday >= file%next_seconds) .or. julianday > file%next_julian
               end select
               call output_field%flag_as_required(required)
               output_field => output_field%next
            end do
         end if
         file => file%next
      end do
   end subroutine

   logical function in_window(self, julianday, secondsofday, microseconds, n)
      class (type_file), intent(in) :: self
      integer,           intent(in) :: julianday, secondsofday, microseconds, n

      in_window = ((julianday == self%first_julian .and. secondsofday >= self%first_seconds) .or. julianday > self%first_julian) &
            .and. ((julianday == self%last_julian .and. secondsofday <= self%last_seconds)  .or. julianday < self%last_julian)
   end function

   subroutine output_manager_save2(julianday,secondsofday,microseconds,n)
      integer,intent(in) :: julianday,secondsofday,microseconds,n

      class (type_file),              pointer :: file
      class (type_base_output_field), pointer :: output_field
      integer                                 :: yyyy,mm,dd
      logical                                 :: output_required

      if (.not.files_initialized) call initialize_files(julianday,secondsofday,microseconds,n)

      file => first_file
      do while (associated(file))
         if (in_window(file, julianday, secondsofday, microseconds, n)) then

         ! Increment time-integrated fields
         output_field => file%first_field
         do while (associated(output_field))
            call output_field%new_data()
            output_field => output_field%next
         end do

         if (file%next_julian==-1) then
            ! Store current time step so next time step can be computed correctly.
            file%next_julian = file%first_julian
            file%next_seconds = file%first_seconds
         end if

         ! Determine whether output is required
         if (file%time_unit /= time_unit_dt) then
            output_required  = (julianday == file%next_julian .and. secondsofday >= file%next_seconds) .or. julianday > file%next_julian
         else
            if (file%first_index == -1) file%first_index = n
            output_required = mod(n - file%first_index, file%time_step) == 0
         end if

         if (output_required) then
            output_field => file%first_field
            do while (associated(output_field))
               call output_field%before_save()
               output_field => output_field%next
            end do

            ! Do output
            call file%save(julianday,secondsofday,microseconds)

            ! Determine time (julian day, seconds of day) for next output.
            select case (file%time_unit)
               case (time_unit_second)
                  file%next_seconds = file%next_seconds + file%time_step
                  file%next_julian = file%next_julian + file%next_seconds/86400
                  file%next_seconds = mod(file%next_seconds,86400)
               case (time_unit_hour)
                  file%next_seconds = file%next_seconds + file%time_step*3600
                  file%next_julian = file%next_julian + file%next_seconds/86400
                  file%next_seconds = mod(file%next_seconds,86400)
               case (time_unit_day)
                  file%next_julian = file%next_julian + file%time_step
               case (time_unit_month)
                  call host%calendar_date(julianday,yyyy,mm,dd)
                  mm = mm + file%time_step
                  yyyy = yyyy + (mm-1)/12
                  mm = mod(mm-1,12)+1
                  call host%julian_day(yyyy,mm,dd,file%next_julian)
               case (time_unit_year)
                  call host%calendar_date(julianday,yyyy,mm,dd)
                  yyyy = yyyy + file%time_step
                  call host%julian_day(yyyy,mm,dd,file%next_julian)
            end select
         end if

         end if ! in output time window

         file => file%next
      end do
   end subroutine output_manager_save2

   subroutine configure_from_yaml(field_manager,title,postfix)
      type (type_field_manager), target      :: field_manager
      character(len=*),           intent(in) :: title
      character(len=*), optional, intent(in) :: postfix

      character(len=yaml_error_length)   :: yaml_error
      class (type_node),         pointer :: node
      type (type_key_value_pair),pointer :: pair
      integer,parameter                  :: yaml_unit = 100
      logical                            :: file_exists

      inquire(file='output.yaml',exist=file_exists)
      if (.not.file_exists) then
         call host%log_message('WARNING: no output files will be written because output.yaml is not present.')
         return
      end if

      ! Parse YAML.
      node => yaml_parse('output.yaml',yaml_unit,yaml_error)
      if (yaml_error/='') call host%fatal_error('configure_from_yaml',trim(yaml_error))
      if (.not.associated(node)) then
         call host%log_message('WARNING: no output files will be written because output.yaml is empty.')
         return
      end if

      ! Process root-level dictionary.
      select type (node)
         class is (type_dictionary)
            pair => node%first
            do while (associated(pair))
               if (pair%key=='') call host%fatal_error('configure_from_yaml','Error parsing output.yaml: empty file path specified.')
               select type (dict=>pair%value)
                  class is (type_dictionary)
                     call process_file(field_manager,trim(pair%key),dict,title,postfix=postfix)
                  class default
                     call host%fatal_error('configure_from_yaml','Error parsing output.yaml: contents of '//trim(dict%path)//' must be a dictionary, not a single value.')
               end select
               pair => pair%next
            end do
         class default
            call host%fatal_error('configure_from_yaml','output.yaml must contain a dictionary with (variable name : information) pairs.')
      end select
   end subroutine configure_from_yaml

   subroutine output_manager_add_file(field_manager,file)
      type (type_field_manager), target :: field_manager
      class (type_file),target :: file

      file%field_manager => field_manager
      file%next => first_file
      first_file => file
   end subroutine output_manager_add_file

   subroutine process_file(field_manager,path,mapping,title,postfix,default_settings)
      type (type_field_manager), target                           :: field_manager
      character(len=*),                                intent(in) :: path
      class (type_dictionary),                         intent(in) :: mapping
      character(len=*),                                intent(in) :: title
      character(len=*),                      optional, intent(in) :: postfix
      class (type_output_variable_settings), optional, intent(in) :: default_settings

      type (type_error),   pointer :: config_error
      class (type_scalar), pointer :: scalar
      class (type_file),   pointer :: file
      character(len=string_length) :: string
      logical                      :: success
      class (type_output_variable_settings), pointer :: file_settings

      type (type_dimension),       pointer :: dim
      integer                              :: global_start, global_stop, stride
      character(len=8)                     :: strmax
#ifdef NETCDF_FMT
      character(len=*), parameter :: default_format = 'netcdf'
#else
      character(len=*), parameter :: default_format = 'text'
#endif
      logical                              :: is_active
      class (type_slice_operator), pointer :: slice_operator

      is_active = mapping%get_logical('is_active',default=.true.,error=config_error)
      if (is_active) then
         string = mapping%get_string('format',default=default_format,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
         select case (string)
         case ('netcdf')
#ifdef NETCDF_FMT
            allocate(type_netcdf_file::file)
#else
            call host%fatal_error('process_file','Error parsing output.yaml: "netcdf" specified for format of output file "'//trim(path)//'", but GOTM has been compiled without NetCDF support. Valid options are: text.')
#endif
         case ('text')
            allocate(type_text_file::file)
         case default
            call host%fatal_error('process_file','Error parsing output.yaml: invalid value "'//trim(string)//'" specified for format of output file "'//trim(path)//'". Valid options are: netcdf, text.')
         end select

         ! Create file object and prepend to list.
         file%path = path
         if (present(postfix)) file%postfix = postfix
         call output_manager_add_file(field_manager,file)

         ! Can be used for CF global attributes
         file%title = mapping%get_string('title',default=title,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_file',config_error%message)

         ! Determine time unit
         scalar => mapping%get_scalar('time_unit',required=.true.,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
         select case (scalar%string)
            case ('second')
               file%time_unit = time_unit_second
            case ('hour')
               file%time_unit = time_unit_hour
            case ('day')
               file%time_unit = time_unit_day
            case ('month')
               file%time_unit = time_unit_month
            case ('year')
               file%time_unit = time_unit_year
            case ('dt')
               file%time_unit = time_unit_dt
            case default
               call host%fatal_error('process_file','Error parsing output.yaml: invalid value "'//trim(scalar%string)//'" specified for time_unit of output file "'//trim(path)//'". Valid options are second, day, month, year.')
         end select

         ! Determine time step
         file%time_step = mapping%get_integer('time_step',error=config_error)
         if (associated(config_error)) call host%fatal_error('process_file',config_error%message)

         ! Determine time of first output (default to start of simulation)
         scalar => mapping%get_scalar('time_start',required=.false.,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
         if (associated(scalar)) then
            call read_time_string(trim(scalar%string),file%first_julian,file%first_seconds,success)
            if (.not.success) call host%fatal_error('process_file','Error parsing output.yaml: invalid value "'//trim(scalar%string)//'" specified for '//trim(scalar%path)//'. Required format: yyyy-mm-dd HH:MM:SS.')
         end if

         ! Determine time of last output (default: save until simulation ends)
         scalar => mapping%get_scalar('time_stop',required=.false.,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
         if (associated(scalar)) then
            call read_time_string(trim(scalar%string),file%last_julian,file%last_seconds,success)
            if (.not.success) call host%fatal_error('process_file','Error parsing output.yaml: invalid value "'//trim(scalar%string)//'" specified for '//trim(scalar%path)//'. Required format: yyyy-mm-dd HH:MM:SS.')
         end if

         ! Determine dimension ranges
         allocate(slice_operator)
         dim => field_manager%first_dimension
         do while (associated(dim))
            if (dim%iterator/='') then
               write (strmax,'(i0)') dim%global_length
               global_start = mapping%get_integer(trim(dim%iterator)//'_start',default=1,error=config_error)
               if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
               if (global_start < 0 .or. global_start > dim%global_length) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_start must lie between 0 and '//trim(strmax))
               global_stop = mapping%get_integer(trim(dim%iterator)//'_stop',default=dim%global_length,error=config_error)
               if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
               if (global_stop < 1 .or. global_stop > dim%global_length) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_stop must lie between 1 and '//trim(strmax))
               if (global_start > global_stop) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_stop must equal or exceed '//trim(dim%iterator)//'_start')
               stride = mapping%get_integer(trim(dim%iterator)//'_stride',default=1,error=config_error)
               if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
               if (stride < 1) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_stride must be larger than 0.')
               call slice_operator%add(trim(dim%name), global_start, global_stop, stride)
            end if
            dim => dim%next
         end do
         file_settings => file%create_settings()
         call file_settings%initialize(mapping, default_settings)
         file_settings%final_operator => slice_operator

         ! Allow specific file implementation to parse additional settings from yaml file.
         call file%configure(mapping)

         call process_group(file, mapping, file_settings)
      end if
   end subroutine process_file

   recursive subroutine process_group(file,mapping,default_settings)
      class (type_file),                               intent(inout) :: file
      class (type_dictionary),                         intent(in)    :: mapping
      class (type_output_variable_settings), optional, intent(in)    :: default_settings

      class (type_list),                     pointer :: list
      type (type_list_item),                 pointer :: item
      type (type_error),                     pointer :: config_error
      class (type_output_variable_settings), pointer :: settings
      type (type_key_value_pair),            pointer :: pair

      settings => file%create_settings()
      call settings%initialize(mapping, default_settings)

      ! Get list with groups [if any]
      list => mapping%get_list('groups',required=.false.,error=config_error)
      if (associated(config_error)) call host%fatal_error('process_group',config_error%message)
      if (associated(list)) then
         item => list%first
         do while (associated(item))
            select type (node=>item%node)
               class is (type_dictionary)
                  call process_group(file, node, settings)
               class default
                  call host%fatal_error('process_group','Elements below '//trim(list%path)//' must be dictionaries.')
            end select
            item => item%next
         end do
      end if

      ! Get operators
      list => mapping%get_list('operators',required=.false.,error=config_error)
      if (associated(config_error)) call host%fatal_error('process_group', config_error%message)
      if (associated(list)) call apply_operators(settings%final_operator, list, file%field_manager)

      ! Get list with variables
      list => mapping%get_list('variables',required=.true.,error=config_error)
      if (associated(config_error)) call host%fatal_error('process_group',config_error%message)
      item => list%first
      do while (associated(item))
         select type (node=>item%node)
            class is (type_dictionary)
               call process_variable(file, node, settings)
            class default
               call host%fatal_error('process_group','Elements below '//trim(list%path)//' must be dictionaries.')
         end select
         item => item%next
      end do

      ! Raise error if any keys are left unused.
      pair => mapping%first
      do while (associated(pair))
         if (.not.pair%accessed) call host%fatal_error('process_group','key '//trim(pair%key)//' below '//trim(mapping%path)//' not recognized.')
         pair => pair%next
      end do

      deallocate(settings)
   end subroutine process_group

   subroutine process_variable(file,mapping,default_settings)
      class (type_file),                    intent(inout) :: file
      class (type_dictionary),              intent(in)    :: mapping
      class (type_output_variable_settings),intent(in)    :: default_settings

      character(len=string_length)        :: source_name
      type (type_error),          pointer :: config_error
      type (type_output_item),    pointer :: output_item
      integer                             :: n
      type (type_key_value_pair), pointer :: pair

      ! Name of source variable
      source_name = mapping%get_string('source',error=config_error)
      if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)

      allocate(output_item)
      output_item%settings => file%create_settings()
      call output_item%settings%initialize(mapping, default_settings)

      ! Determine whether to create an output field or an output category
      n = len_trim(source_name)
      if (source_name(n:n)=='*') then
         if (n==1) then
            output_item%name = ''
         else
            output_item%name = source_name(:n-2)
         end if

         ! Prefix for output name
         output_item%prefix = mapping%get_string('prefix',default='',error=config_error)
         if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)

         ! Postfix for output name
         output_item%postfix = mapping%get_string('postfix',default='',error=config_error)
         if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)

         ! Output level
         output_item%output_level = mapping%get_integer('output_level',default=output_level_default,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)
      else
         output_item%field => file%field_manager%select_for_output(source_name)

         ! Name of output variable (may differ from source name)
         output_item%name = mapping%get_string('name',default=source_name,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)
      end if
      call file%append_item(output_item)

      ! Raise error if any keys are left unused.
      pair => mapping%first
      do while (associated(pair))
         if (.not.pair%accessed) call host%fatal_error('process_group','key '//trim(pair%key)//' below '//trim(mapping%path)//' not recognized.')
         pair => pair%next
      end do

   end subroutine process_variable

end module

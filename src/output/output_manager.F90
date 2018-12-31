module output_manager

   use field_manager
   use output_manager_core
   use netcdf_output
   use text_output
   use output_filters

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

   function create_field(file, settings, field) result(generic_field)
      class (type_file), intent(inout) :: file
      class (type_output_variable_settings), target :: settings
      type (type_field), target :: field
      class (type_base_output_field), pointer :: generic_field

      class (type_output_field),pointer :: output_field
      class (type_time_filter), pointer :: time_filter

      generic_field => file%find(field)
      if (associated(generic_field)) return

      allocate(output_field)
      output_field%settings => settings
      output_field%source => field
      output_field%output_name = trim(field%name)
      generic_field => output_field

      if (associated(settings%first_operator)) then
         select type (op=>settings%first_operator)
         class is (type_filter)
            call apply_operator(op)
         end select
      end if

      if (settings%time_method /= time_method_instantaneous .and. settings%time_method /= time_method_none) then
         ! We are not storing the instantaneous value. Create a work array that will be stored instead.
         allocate(time_filter)
         time_filter%source => generic_field
         time_filter%method = settings%time_method
         time_filter%output_name = generic_field%output_name
         time_filter%settings => settings
         generic_field => time_filter
      end if
   contains
      recursive subroutine apply_operator(op)
         class (type_filter), intent(in) :: op
         class (type_filter), pointer :: new_op
         if (associated(op%source)) then
            select type (nested_op=>op%source)
            class is (type_filter)
               call apply_operator(nested_op)
            end select
         end if
         allocate(new_op, source=op)
         new_op%source => generic_field
         new_op%output_name = generic_field%output_name
         new_op%settings => settings
         generic_field => new_op
      end subroutine
   end function

   subroutine collect_from_categories(file)
      class (type_file), intent(inout) :: file
      class (type_output_category), pointer :: output_category
      class (type_base_output_field),pointer :: output_field
      type (type_field_set) :: set
      class (type_field_set_member), pointer :: member, next_member
      class (type_output_variable_settings), pointer :: settings
      type (type_dictionary) :: mapping

      output_category => file%first_category
      do while (associated(output_category))
         call host%log_message('Processing output category /'//trim(output_category%name)//':')
         if (.not.output_category%source%has_fields()) call host%fatal_error('collect_from_categories','No variables have been registered under output category "'//trim(output_category%name)//'".')
         call output_category%source%get_all_fields(set,output_category%output_level)
         member => set%first
         if (.not.associated(member)) call host%log_message('WARNING: output category "'//trim(output_category%name)//'" does not contain any variables with requested output level.')
         do while (associated(member))
            call host%log_message('  - '//trim(member%field%name))
            settings => file%create_settings()
            call settings%initialize(mapping, output_category%settings)
            output_field => create_field(file, settings, member%field)
            output_field%output_name = trim(output_category%prefix) // trim(output_field%output_name) // trim(output_category%postfix)
            call file%append(output_field)
            next_member => member%next
            deallocate(member)
            member => next_member
         end do
         nullify(set%first)
         output_category => output_category%next
      end do
   end subroutine collect_from_categories

   subroutine filter_variables(file)
      class (type_file), intent(inout) :: file

      integer :: i
      type (type_dimension_pointer), allocatable :: dimensions(:)
      class (type_base_output_field), pointer :: output_field, next_field, previous_field
      type (type_output_dimension), pointer :: output_dim
      logical :: empty

      previous_field => null()
      output_field => file%first_field
      do while (associated(output_field))
         next_field => output_field%next

         ! Determine whether the field is empty (one or more zero-length dimensions)
         empty = .false.
         call output_field%get_metadata(dimensions=dimensions)
         do i=1,size(dimensions)
            if (dimensions(i)%p%id/=id_dim_time) then
               output_dim => file%get_dimension(dimensions(i)%p)
               if (output_dim%stop<output_dim%start) empty = .true.
            end if
         end do

         if (empty) then
            ! Empty field - deallocate and remove from list.
            deallocate(output_field)
            if (.not.associated(previous_field)) then
               file%first_field => next_field
            else
               previous_field%next => next_field
            end if
         else
            ! Non-empty field - keep it.
            previous_field => output_field
         end if

         output_field => next_field
      end do
   end subroutine filter_variables

   subroutine add_coordinate_variables(file)
      class (type_file), intent(inout) :: file

      class (type_base_output_field),  pointer :: output_field, existing_coordinate_field
      class (type_base_output_field),  pointer :: coordinate_field
      class (type_output_variable_settings), pointer :: settings
      integer :: i
      type (type_dimension_pointer), allocatable :: dimensions(:)

      output_field => file%first_field
      do while (associated(output_field))
         call output_field%get_metadata(dimensions=dimensions)
         allocate(output_field%coordinates(size(dimensions)))
         do i=1,size(dimensions)
            if (.not.associated(dimensions(i)%p%coordinate)) cycle
            settings => file%create_settings()
            settings%time_method = time_method_instantaneous
            output_field%coordinates(i)%p => create_field(file, settings, dimensions(i)%p%coordinate)
            call file%append(output_field%coordinates(i)%p)
         end do
         output_field => output_field%next
      end do
   end subroutine add_coordinate_variables

   subroutine initialize_files(julianday,secondsofday,microseconds,n)
      integer, intent(in) :: julianday,secondsofday,microseconds,n

      class (type_file),              pointer    :: file
      class (type_base_output_field), pointer    :: output_field
      type (type_output_dimension),   pointer    :: output_dim
      integer, allocatable, dimension(:)         :: starts, stops, strides
      integer                                    :: i,j
      type (type_dimension_pointer), allocatable :: dimensions(:)

      file => first_file
      do while (associated(file))
         ! Add variables below selected categories to output
         call collect_from_categories(file)

         ! Remove empty variables (with one or more zero-length dimensions)
         call filter_variables(file)

         ! Add any missing coordinate variables
         call add_coordinate_variables(file)

         ! First check whether all fields included in this file have been registered.
         output_field => file%first_field
         do while (associated(output_field))
            call output_field%initialize(file%field_manager)
            output_field => output_field%next
         end do

         ! If we do not have a start time yet, use current.
         if (file%first_julian <= 0) then
            file%first_julian = julianday
            file%first_seconds = secondsofday
         end if

         ! Initialize fields based on time integrals
         output_field => file%first_field
         do while (associated(output_field))
            ! Determine effective dimension range
            call output_field%get_metadata(dimensions=dimensions)
            allocate(starts(1:size(dimensions)))
            allocate(stops(1:size(dimensions)))
            allocate(strides(1:size(dimensions)))
            j = 0
            do i=1,size(dimensions)
               if (dimensions(i)%p%length>1) then
                  ! Not a singleton dimension - create the slice spec
                  j = j + 1
                  output_dim => file%get_dimension(dimensions(i)%p)
                  starts(j) = output_dim%start
                  stops(j) = output_dim%stop
                  strides(j) = output_dim%stride
               end if
            end do

            if (all(stops(1:j)>=starts(1:j))) then
               ! Select appropriate data slice
               if (associated(output_field%data_3d)) then
                  if (j/=3) call host%fatal_error('output_manager_save','BUG: data of '//trim(output_field%output_name)//' contains one or more singleton dimensions.')
                  output_field%data_3d => output_field%data_3d(starts(1):stops(1):strides(1),starts(2):stops(2):strides(2),starts(3):stops(3):strides(3))
               elseif (associated(output_field%data_2d)) then
                  if (j/=2) call host%fatal_error('output_manager_save','BUG: data of '//trim(output_field%output_name)//' contains one or more singleton dimensions.')
                  output_field%data_2d => output_field%data_2d(starts(1):stops(1):strides(1),starts(2):stops(2):strides(2))
               elseif (associated(output_field%data_1d)) then
                  if (j/=1) call host%fatal_error('output_manager_save','BUG: data of '//trim(output_field%output_name)//' contains one or more singleton dimensions.')
                  output_field%data_1d => output_field%data_1d(starts(1):stops(1):strides(1))
               else
                  if (j/=0) call host%fatal_error('output_manager_save','BUG: data of '//trim(output_field%output_name)//' contains one or more singleton dimensions.')
               end if
            end if

            ! Deallocate dimension range specifyers.
            deallocate(starts,stops,strides)

            output_field => output_field%next
         end do

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
      class (type_filter),            pointer :: filter

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

      type (type_dimension),       pointer :: dim
      type (type_output_dimension),pointer :: output_dim
      character(len=8)                     :: strmax
      integer                              :: distance
#ifdef NETCDF_FMT
      character(len=*), parameter :: default_format = 'netcdf'
#else
      character(len=*), parameter :: default_format = 'text'
#endif
      logical                              :: is_active

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
         dim => field_manager%first_dimension
         do while (associated(dim))
            if (dim%iterator/='') then
               write (strmax,'(i0)') dim%global_length
               output_dim => file%get_dimension(dim)
               output_dim%global_start = mapping%get_integer(trim(dim%iterator)//'_start',default=1,error=config_error)
               if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
               if (output_dim%global_start<0.or.output_dim%global_start>dim%global_length) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_start must lie between 0 and '//trim(strmax))
               output_dim%global_stop = mapping%get_integer(trim(dim%iterator)//'_stop',default=dim%global_length,error=config_error)
               if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
               if (output_dim%global_stop<1.or.output_dim%global_stop>dim%global_length) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_stop must lie between 1 and '//trim(strmax))
               if (output_dim%global_start>output_dim%global_stop) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_stop must equal or exceed '//trim(dim%iterator)//'_start')
               output_dim%stride = mapping%get_integer(trim(dim%iterator)//'_stride',default=1,error=config_error)
               if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
               if (output_dim%stride<1) call host%fatal_error('process_file','Error parsing output.yaml: '//trim(dim%iterator)//'_stride must be larger than 0.')

               ! Reduce stop to last point that is actually included (due to stride>1)
               output_dim%global_stop = output_dim%global_stop - mod(output_dim%global_stop-output_dim%global_start,output_dim%stride)

               ! Compute local [i.e., within-subdomain] start and stop positons from global positions and local offset.
               if (output_dim%global_start>dim%offset+dim%length) then
                  ! Start point lies beyond our subdomain
                  output_dim%start = 1
                  output_dim%stop = output_dim%start - output_dim%stride
               else
                  if (output_dim%global_start>dim%offset) then
                     ! Starting point lies within our subdomain
                     output_dim%start = output_dim%global_start - dim%offset
                  else
                     ! Starting point lies before our subdomain: we start immediately but have to account for stride

                     ! Determine distance between subdomain start and nearest included point outside the domain.
                     distance = mod(dim%offset + 1 - output_dim%global_start, output_dim%stride)

                     ! Convert to distance to next point within the domain
                     if (distance>0) distance = output_dim%stride - distance
                     output_dim%start = 1 + distance
                  end if

                  ! Determine local stop by subtracting subdomain offset [maximum is subdomain length)
                  output_dim%stop = min(output_dim%global_stop - dim%offset, dim%length)

                  if (output_dim%stop<output_dim%start) then
                     ! stop precedes start, so we have 0 length, i.e.,
                     ! length = (output_dimension%stop-output_dimension%start)/output_dimension%stride + 1 = 0
                     output_dim%stop = output_dim%start - output_dim%stride
                  else
                     ! Reduce stop to last point that is actually included (due to stride>1)
                     output_dim%stop = output_dim%stop - mod(output_dim%stop-output_dim%start,output_dim%stride)
                  end if
               end if
            end if
            dim => dim%next
         end do

         ! Allow specific file implementation to parse additional settings from yaml file.
         call file%configure(mapping)

         call process_group(file,mapping,default_settings)
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

      ! Get operators
      list => mapping%get_list('operators',required=.false.,error=config_error)
      if (associated(config_error)) call host%fatal_error('process_group', config_error%message)
      if (associated(list)) then
         item => list%first
         do while (associated(item))
            select type (node=>item%node)
               class is (type_dictionary)
                  call process_operator(node, settings, file%field_manager)
               class default
                  call host%fatal_error('process_group','Elements below '//trim(list%path)//' must be dictionaries.')
            end select
            item => item%next
         end do
      end if

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

      character(len=string_length)            :: source_name
      type (type_error),              pointer :: config_error
      class (type_output_category),   pointer :: output_category
      class (type_base_output_field), pointer :: output_field
      type (type_field),              pointer :: field
      integer                                 :: n
      type (type_key_value_pair),     pointer :: pair

      ! Name of source variable
      source_name = mapping%get_string('source',error=config_error)
      if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)

      ! Determine whether to create an output field or an output category
      n = len_trim(source_name)
      if (source_name(n:n)=='*') then
         allocate(output_category)
         output_category%settings => file%create_settings()
         call output_category%settings%initialize(mapping, default_settings)

         if (n==1) then
            output_category%name = ''
         else
            output_category%name = source_name(:n-2)
         end if

         ! Prefix for output name
         output_category%prefix = mapping%get_string('prefix',default='',error=config_error)
         if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)

         ! Postfix for output name
         output_category%postfix = mapping%get_string('postfix',default='',error=config_error)
         if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)

         ! Output level
         output_category%output_level = mapping%get_integer('output_level',default=output_level_default,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)

         call file%append_category(output_category)
      else
         allocate(output_field)
         output_field%settings => file%create_settings()
         call output_field%settings%initialize(mapping, default_settings)
         field => file%field_manager%select_for_output(source_name)
         output_field => create_field(file, output_field%settings, field)
         call file%append(output_field)

         ! Name of output variable (may differ from source name)
         output_field%output_name = mapping%get_string('name',default=source_name,error=config_error)
         if (associated(config_error)) call host%fatal_error('process_variable',config_error%message)
      end if

      ! Raise error if any keys are left unused.
      pair => mapping%first
      do while (associated(pair))
         if (.not.pair%accessed) call host%fatal_error('process_group','key '//trim(pair%key)//' below '//trim(mapping%path)//' not recognized.')
         pair => pair%next
      end do

   end subroutine process_variable

   subroutine process_operator(mapping, settings, field_manager)
      class (type_dictionary), intent(in) :: mapping
      class (type_output_variable_settings),intent(inout) :: settings
      type (type_field_manager), intent(inout) :: field_manager

      type (type_error),          pointer :: config_error
      character(len=string_length)        :: operator_type, variable_name
      class (type_interp_filter), pointer :: interp
      class (type_list),          pointer :: list
      type (type_list_item),      pointer :: list_item
      integer :: i, n
      logical :: success

      operator_type = mapping%get_string('type', error=config_error)
      if (associated(config_error)) call host%fatal_error('process_operator', config_error%message)
      select case (operator_type)
      case ('interp')
         allocate(interp)
         interp%dimension = mapping%get_string('dimension', error=config_error)
         if (associated(config_error)) call host%fatal_error('process_operator', config_error%message)
         variable_name = mapping%get_string('offset', default='', error=config_error)
         if (associated(config_error)) call host%fatal_error('process_operator', config_error%message)
         if (variable_name /= '') interp%offset => field_manager%select_for_output(trim(variable_name))
         variable_name = mapping%get_string('source_coordinate', default='', error=config_error)
         if (associated(config_error)) call host%fatal_error('process_operator', config_error%message)
         if (variable_name /= '') interp%source_coordinate => field_manager%select_for_output(trim(variable_name))
         list => mapping%get_list('coordinates', required=.true., error=config_error)
         if (associated(config_error)) call host%fatal_error('process_operator', config_error%message)
         n = 0
         list_item => list%first
         do while (associated(list_item))
            n = n + 1
            list_item => list_item%next
         end do
         allocate(interp%target_coordinates(n))
         list_item => list%first
         do i=1,n
            select type (node => list_item%node)
            class is (type_scalar)
               interp%target_coordinates(i) = node%to_real(0._rk, success)
               if (.not. success) call host%fatal_error('process_operator', trim(node%path)//': unable to convert '//trim(node%string)//' to real.')
               if (i > 1) then
                  if (interp%target_coordinates(i) < interp%target_coordinates(i - 1)) call host%fatal_error('process_operator', trim(list%path)//' should be monotonically increasing.')
               end if
            class default
               call host%fatal_error('process_operator', trim(node%path)//' should be a real number.')
            end select
            list_item => list_item%next
         end do
         interp%source => settings%first_operator
         settings%first_operator => interp
      case default
         call host%fatal_error('process_operator', trim(mapping%path)//': operator type '//trim(operator_type)//' not recognized.')
      end select
   end subroutine

end module

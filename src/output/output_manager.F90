module output_manager

   use field_manager
   use output_manager_core
   use netcdf_output
   use text_output

   use yaml_settings

   implicit none

   public output_manager_init, output_manager_save, output_manager_clean, output_manager_add_file

   private

   class (type_file), pointer :: first_file
   logical                    :: files_initialized

   interface output_manager_save
      module procedure output_manager_save1
      module procedure output_manager_save2
   end interface

   type,extends(type_dictionary_populator) :: type_file_populator
      type (type_field_manager), pointer :: fm => null()
      character(len=:), allocatable :: title
      character(len=:), allocatable :: postfix
   contains
      procedure :: create => process_file
   end type

   type,extends(type_list_populator) :: type_group_populator
      class (type_file), pointer :: file
      class (type_output_variable_settings), pointer :: variable_settings => null()
   contains
      procedure :: create => create_group_settings
   end type

   type,extends(type_list_populator) :: type_variable_populator
      class (type_file), pointer :: file => null()
      class (type_output_variable_settings), pointer :: variable_settings => null()
   contains
      procedure :: create => create_variable_settings
   end type

contains

   subroutine output_manager_init(field_manager, title, postfix, settings)
      type (type_field_manager), target :: field_manager
      character(len=*),           intent(in) :: title
      character(len=*), optional, intent(in) :: postfix
      class (type_settings), pointer, optional :: settings

      if (.not.associated(host)) then
         write (*,*) 'output_manager_init: the host of an output manager must set the host pointer before calling output_manager_init'
         stop 1
      end if
      nullify(first_file)
      files_initialized = .false.
      call configure_from_yaml(field_manager,title,postfix, settings)
   end subroutine

   subroutine output_manager_clean()
      class (type_file),pointer :: file
      file => first_file
      do while (associated(file))
         call file%finalize()
         file => file%next
      end do
   end subroutine

   subroutine collect_from_categories(file)
      class (type_file), intent(inout) :: file
      class (type_output_category), pointer :: output_category
      class (type_output_field),pointer  :: output_field
      type (type_field_set) :: set
      class (type_field_set_member), pointer :: member, next_member

      output_category => file%first_category
      do while (associated(output_category))
         call host%log_message('Processing output category /'//trim(output_category%name)//':')
         if (.not.output_category%source%has_fields()) call host%fatal_error('collect_from_categories','No variables have been registered under output category "'//trim(output_category%name)//'".')
         call output_category%source%get_all_fields(set,output_category%output_level)
         member => set%first
         if (.not.associated(member)) call host%log_message('WARNING: output category "'//trim(output_category%name)//'" does not contain any variables with requested output level.')
         do while (associated(member))
            call host%log_message('  - '//trim(member%field%name))
            output_field => file%create_field()
            output_field%settings => output_category%settings
            output_field%source => member%field
            output_field%output_name = trim(output_category%prefix)//trim(member%field%name)//trim(output_category%postfix)
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
      class (type_output_field),    pointer :: output_field, next_field, previous_field
      type (type_output_dimension), pointer :: output_dim
      logical :: empty

      previous_field => null()
      output_field => file%first_field
      do while (associated(output_field))
         next_field => output_field%next

         ! Determine whether the field is empty (one or more zero-length dimensions)
         empty = .false.
         do i=1,size(output_field%source%dimensions)
            if (output_field%source%dimensions(i)%p%id/=id_dim_time) then
               output_dim => file%get_dimension(output_field%source%dimensions(i)%p)
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

      class (type_output_field),  pointer :: output_field
      class (type_output_field),  pointer :: coordinate_field
      integer :: i

      output_field => file%first_field
      do while (associated(output_field))
         allocate(output_field%coordinates(size(output_field%source%dimensions)))
         do i=1,size(output_field%source%dimensions)
            if (.not.associated(output_field%source%dimensions(i)%p%coordinate)) cycle
            coordinate_field => file%find(output_field%source%dimensions(i)%p%coordinate)
            if (.not.associated(coordinate_field)) then
               coordinate_field => file%create_field()
               coordinate_field%settings => file%create_settings()
               coordinate_field%settings%time_method = time_method_instantaneous
               coordinate_field%source => output_field%source%dimensions(i)%p%coordinate
               coordinate_field%output_name = trim(coordinate_field%source%name)
               call file%append(coordinate_field)
            end if
            output_field%coordinates(i)%p => coordinate_field
         end do
         output_field => output_field%next
      end do
   end subroutine add_coordinate_variables

   subroutine initialize_files(julianday,secondsofday,microseconds,n)
      integer, intent(in) :: julianday,secondsofday,microseconds,n

      class (type_file),            pointer :: file
      class (type_output_field),    pointer :: output_field
      type (type_output_dimension), pointer :: output_dim
      integer, allocatable, dimension(:)    :: starts, stops, strides
      integer                               :: i,j

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
            select case (output_field%source%status)
               case (status_not_registered)
                  call host%fatal_error('output_manager_save', 'File '//trim(file%path)//': &
                     requested field "'//trim(output_field%source%name)//'" has not been registered with field manager.')
               case (status_registered_no_data)
                  call host%fatal_error('output_manager_save', 'File '//trim(file%path)//': &
                     data for requested field "'//trim(output_field%source%name)//'" have not been provided.')
            end select
            output_field => output_field%next
         end do

         ! If we do not have a start time yet, use current.
         if (file%first_julian<=0) then
            file%first_julian = julianday
            file%first_seconds = secondsofday
         end if
         file%first_index = n

         ! Initialize fields based on time integrals
         output_field => file%first_field
         do while (associated(output_field))
            ! Determine effective dimension range
            allocate(starts(1:size(output_field%source%dimensions)))
            allocate(stops(1:size(output_field%source%dimensions)))
            allocate(strides(1:size(output_field%source%dimensions)))
            j = 0
            do i=1,size(output_field%source%dimensions)
               if (output_field%source%dimensions(i)%p%length>1) then
                  ! Not a singleton dimension - create the slice spec
                  j = j + 1
                  output_dim => file%get_dimension(output_field%source%dimensions(i)%p)
                  starts(j) = output_dim%start
                  stops(j) = output_dim%stop
                  strides(j) = output_dim%stride
               end if
            end do

            if (all(stops(1:j)>=starts(1:j))) then
               ! Select appropriate data slice
               if (associated(output_field%source%data_3d)) then
                  if (j/=3) call host%fatal_error('output_manager_save','BUG: data of '//trim(output_field%source%name)//' contains one or more singleton dimensions.')
                  output_field%source_3d => output_field%source%data_3d(starts(1):stops(1):strides(1),starts(2):stops(2):strides(2),starts(3):stops(3):strides(3))
               elseif (associated(output_field%source%data_2d)) then
                  if (j/=2) call host%fatal_error('output_manager_save','BUG: data of '//trim(output_field%source%name)//' contains one or more singleton dimensions.')
                  output_field%source_2d => output_field%source%data_2d(starts(1):stops(1):strides(1),starts(2):stops(2):strides(2))
               elseif (associated(output_field%source%data_1d)) then
                  if (j/=1) call host%fatal_error('output_manager_save','BUG: data of '//trim(output_field%source%name)//' contains one or more singleton dimensions.')
                  output_field%source_1d => output_field%source%data_1d(starts(1):stops(1):strides(1))
               else
                  if (j/=0) call host%fatal_error('output_manager_save','BUG: data of '//trim(output_field%source%name)//' contains one or more singleton dimensions.')
                  output_field%source_0d => output_field%source%data_0d
               end if
            end if

            ! Deallocate dimension range specifyers.
            deallocate(starts,stops,strides)

            ! Store instantaneous data by default.
            output_field%data_0d => output_field%source_0d
            output_field%data_1d => output_field%source_1d
            output_field%data_2d => output_field%source_2d
            output_field%data_3d => output_field%source_3d

            if (output_field%settings%time_method/=time_method_instantaneous.and.output_field%settings%time_method/=time_method_none) then
               ! We are not storing the instantaneous value. Create a work array that will be stored instead.
               if (associated(output_field%source_3d)) then
                  allocate(output_field%work_3d(size(output_field%source_3d,1),size(output_field%source_3d,2),size(output_field%source_3d,3)))
                  output_field%work_3d(:,:,:) = 0.0_rk
                  output_field%data_3d => output_field%work_3d
               elseif (associated(output_field%source_2d)) then
                  allocate(output_field%work_2d(size(output_field%source_2d,1),size(output_field%source_2d,2)))
                  output_field%work_2d(:,:) = 0.0_rk
                  output_field%data_2d => output_field%work_2d
               elseif (associated(output_field%source_1d)) then
                  allocate(output_field%work_1d(size(output_field%source_1d)))
                  output_field%work_1d(:) = 0.0_rk
                  output_field%data_1d => output_field%work_1d
               elseif (associated(output_field%source_0d)) then
                  output_field%work_0d = 0.0_rk
                  output_field%data_0d => output_field%work_0d
               end if
            end if

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

   subroutine output_manager_save2(julianday,secondsofday,microseconds,n)
      integer,intent(in) :: julianday,secondsofday,microseconds,n

      class (type_file),            pointer :: file
      class (type_output_field),    pointer :: output_field
      integer                               :: yyyy,mm,dd
      logical                               :: in_window
      logical                               :: output_based_on_time, output_based_on_index

      if (.not.files_initialized) call initialize_files(julianday,secondsofday,microseconds,n)

      file => first_file
      do while (associated(file))

         ! Continue only if in output time window.
         in_window = ((julianday==file%first_julian.and.secondsofday>=file%first_seconds) .or. julianday>file%first_julian) &
               .and. ((julianday==file%last_julian .and.secondsofday<=file%last_seconds)  .or. julianday<file%last_julian)
         if (in_window) then

         ! Increment time-integrated fields
         output_field => file%first_field
         do while (associated(output_field))
            if (output_field%settings%time_method==time_method_mean .or. (output_field%settings%time_method==time_method_integrated.and.file%next_julian/=-1)) then
               ! This is a time-integrated field that needs to be incremented.
               if (associated(output_field%source_3d)) then
                  output_field%work_3d(:,:,:) = output_field%work_3d + output_field%source_3d
               elseif (associated(output_field%source_2d)) then
                  output_field%work_2d(:,:) = output_field%work_2d + output_field%source_2d
               elseif (associated(output_field%source_1d)) then
                  output_field%work_1d(:) = output_field%work_1d + output_field%source_1d
               elseif (associated(output_field%source_0d)) then
                  output_field%work_0d = output_field%work_0d + output_field%source_0d
               end if
            end if
            output_field => output_field%next
         end do
         file%n = file%n + 1

         if (file%next_julian==-1) then
            ! Store current time step so next time step can be computed correctly.
            file%next_julian = file%first_julian
            file%next_seconds = file%first_seconds
            file%next_index = file%first_index
         end if

         ! Determine whether output is required
         if (file%time_unit .ne. time_unit_dt) then
            output_based_on_time  = (julianday==file%next_julian.and.secondsofday>=file%next_seconds) .or. julianday>file%next_julian
            output_based_on_index = .false.
         else
            output_based_on_time  = .false.
            output_based_on_index = file%next_index >= file%first_index .and. mod(n,file%time_step) .eq. 0
         end if
         if (output_based_on_time .or. output_based_on_index) then
            ! Output required

            ! Perform temporal averaging where required.
            output_field => file%first_field
            do while (associated(output_field))
               if (output_field%settings%time_method==time_method_mean) then
                  ! This is a time-integrated field that needs to be incremented.
                  if (associated(output_field%source_3d)) then
                     output_field%work_3d(:,:,:) = output_field%work_3d/file%n
                  elseif (associated(output_field%source_2d)) then
                     output_field%work_2d(:,:) = output_field%work_2d/file%n
                  elseif (associated(output_field%source_1d)) then
                     output_field%work_1d(:) = output_field%work_1d/file%n
                  elseif (associated(output_field%source_0d)) then
                     output_field%work_0d = output_field%work_0d/file%n
                  end if
               end if
               output_field => output_field%next
            end do

            ! Do NetCDF output
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
               case (time_unit_dt)
                  file%next_index = file%next_index + file%time_step
            end select

            ! Reset time step counter
            file%n = 0

            ! Zero out time-step averaged fields (start of new time step)
            output_field => file%first_field
            do while (associated(output_field))
               if (output_field%settings%time_method==time_method_mean) then
                  if (associated(output_field%source_3d)) then
                     output_field%work_3d(:,:,:) = 0.0_rk
                  elseif (associated(output_field%source_2d)) then
                     output_field%work_2d(:,:) = 0.0_rk
                  elseif (associated(output_field%source_1d)) then
                     output_field%work_1d(:) = 0.0_rk
                  elseif (associated(output_field%source_0d)) then
                     output_field%work_0d = 0.0_rk
                  end if
               end if
               output_field => output_field%next
            end do
         end if

         end if ! in output time window

         file => file%next
      end do
   end subroutine output_manager_save2

   subroutine configure_from_yaml(field_manager, title, postfix, settings)
      type (type_field_manager), target      :: field_manager
      character(len=*),           intent(in) :: title
      character(len=*), optional, intent(in) :: postfix
      class (type_settings), pointer, optional :: settings

      logical                        :: file_exists
      integer,parameter              :: yaml_unit = 100
      class (type_settings), pointer :: settings_
      class (type_file_populator), pointer :: populator

      allocate(populator)
      populator%fm => field_manager
      populator%title = trim(title)
      if (present(postfix)) populator%postfix = postfix
      if (present(settings)) then
         settings_ => settings
         call settings_%populate(populator)
      else
         settings_ => type_settings_create(populator=populator)
      end if

      inquire(file='output.yaml',exist=file_exists)
      if (file_exists) then
         call settings_%load('output.yaml', yaml_unit)
      elseif (.not. present(settings)) then
         call host%log_message('WARNING: no output files will be written because output.yaml is not present.')
         return
      end if
   end subroutine

   subroutine output_manager_add_file(field_manager, file)
      type (type_field_manager), target :: field_manager
      class (type_file),         target :: file

      file%field_manager => field_manager
      file%next => first_file
      first_file => file
   end subroutine output_manager_add_file

   subroutine process_file(self, pair)
      class (type_file_populator), intent(inout) :: self
      type (type_key_value_pair),  intent(inout) :: pair

      logical                              :: is_active
      integer                              :: fmt
      class (type_file), pointer           :: file
      character(len=:), allocatable        :: string
      class (type_settings),       pointer :: file_settings
      logical :: success
      type (type_dimension),       pointer :: dim
      type (type_output_dimension),pointer :: output_dim
      character(len=8)                     :: strmax
      integer                              :: distance

      file_settings => type_settings_create(pair, 'path of output file')

      is_active = file_settings%get_logical('is_active', 'write output to this file', default=.true.)
#ifdef NETCDF_FMT
      fmt = file_settings%get_integer('format', 'format', options=(/type_option(1, 'text'), type_option(2, 'NetCDF')/), default=2)
#else
      fmt = file_settings%get_integer('format', 'format', options=(/type_option(1, 'text')/), default=1)
#endif

      select case (fmt)
      case (1)
         allocate(type_text_file::file)
      case (2)
#ifdef NETCDF_FMT
         allocate(type_netcdf_file::file)
#endif
      end select

      ! Create file object and prepend to list.
      file%path = pair%name
      if (allocated(self%postfix)) file%postfix = self%postfix
      call output_manager_add_file(self%fm, file)

      ! Can be used for CF global attributes
      call file_settings%get(file%title, 'title', 'title', default=self%title)
      call file_settings%get(file%time_unit, 'time_unit', 'time unit', default=time_unit_day, options=(/ &
         type_option(time_unit_second, 'second'), type_option(time_unit_hour, 'hour'), type_option(time_unit_day, 'day'), &
         type_option(time_unit_month, 'month'), type_option(time_unit_year, 'year'), type_option(time_unit_dt, 'dt')/))

      ! Determine time step
      call file_settings%get(file%time_step, 'time_step', 'number of time units between output', minimum=1, default=1)
      string = file_settings%get_string('time_start', 'start', 'yyyy-mm-dd HH:MM:SS', default='')
      if (string /= '') then
         call read_time_string(string, file%first_julian, file%first_seconds, success)
         if (.not. success) call host%fatal_error('process_file','Error in output configuration: invalid time_start "'//string//'" specified for file "'//pair%name//'". Required format: yyyy-mm-dd HH:MM:SS.')
      end if
      string = file_settings%get_string('time_stop', 'stop', 'yyyy-mm-dd HH:MM:SS', default='')
      if (string /= '') then
         call read_time_string(string, file%last_julian, file%last_seconds, success)
         if (.not. success) call host%fatal_error('process_file','Error in output configuration: invalid time_stop "'//string//'" specified for file "'//pair%name//'". Required format: yyyy-mm-dd HH:MM:SS.')
      end if

      ! Determine dimension ranges
      dim => self%fm%first_dimension
      do while (associated(dim))
         if (dim%iterator /= '') then
            write (strmax,'(i0)') dim%global_length
            output_dim => file%get_dimension(dim)
            if (dim%global_length > 1) then
               call file_settings%get(output_dim%global_start, trim(dim%iterator)//'_start', 'start index for '//trim(dim%iterator)//' dimension', default=1, minimum=1, maximum=dim%global_length)
               call file_settings%get(output_dim%global_stop, trim(dim%iterator)//'_stop', 'stop index for '//trim(dim%iterator)//' dimension', default=dim%global_length, minimum=1, maximum=dim%global_length)
               if (output_dim%global_start > output_dim%global_stop) call host%fatal_error('process_file', 'Error in output configuration: '//trim(dim%iterator)//'_stop must equal or exceed '//trim(dim%iterator)//'_start')
               call file_settings%get(output_dim%stride, trim(dim%iterator)//'_stride', 'stride for '//trim(dim%iterator)//' dimension', default=1)
            end if

            ! Reduce stop to last point that is actually included (due to stride > 1)
            output_dim%global_stop = output_dim%global_stop - mod(output_dim%global_stop - output_dim%global_start, output_dim%stride)

            ! Compute local [i.e., within-subdomain] start and stop positons from global positions and local offset.
            if (output_dim%global_start > dim%offset + dim%length) then
               ! Start point lies beyond our subdomain
               output_dim%start = 1
               output_dim%stop = output_dim%start - output_dim%stride
            else
               if (output_dim%global_start > dim%offset) then
                  ! Starting point lies within our subdomain
                  output_dim%start = output_dim%global_start - dim%offset
               else
                  ! Starting point lies before our subdomain: we start immediately but have to account for stride

                  ! Determine distance between subdomain start and nearest included point outside the domain.
                  distance = mod(dim%offset + 1 - output_dim%global_start, output_dim%stride)

                  ! Convert to distance to next point within the domain
                  if (distance > 0) distance = output_dim%stride - distance
                  output_dim%start = 1 + distance
               end if

               ! Determine local stop by subtracting subdomain offset [maximum is subdomain length)
               output_dim%stop = min(output_dim%global_stop - dim%offset, dim%length)

               if (output_dim%stop < output_dim%start) then
                  ! stop precedes start, so we have 0 length, i.e.,
                  ! length = (output_dimension%stop-output_dimension%start)/output_dimension%stride + 1 = 0
                  output_dim%stop = output_dim%start - output_dim%stride
               else
                  ! Reduce stop to last point that is actually included (due to stride>1)
                  output_dim%stop = output_dim%stop - mod(output_dim%stop - output_dim%start, output_dim%stride)
               end if
            end if
         end if
         dim => dim%next
      end do

      ! Allow specific file implementation to parse additional settings from yaml file.
      call file%configure(file_settings)

      call configure_group(file, file_settings) !, default_variable_settings)

   end subroutine process_file

   recursive subroutine configure_group(file, settings, default_variable_settings)
      class (type_file), target, intent(inout) :: file
      class (type_settings),     intent(inout) :: settings
      class (type_output_variable_settings), optional, intent(in) :: default_variable_settings

      class (type_group_populator),    pointer :: group_populator
      class (type_variable_populator), pointer :: variable_populator

      ! Get list with groups [if any]
      allocate(group_populator)
      group_populator%variable_settings => file%create_settings()
      call group_populator%variable_settings%initialize(settings, default_variable_settings)
      group_populator%file => file
      call settings%get_list('groups', group_populator)

      ! Get list with variables
      allocate(variable_populator)
      variable_populator%file => file
      variable_populator%variable_settings => group_populator%variable_settings
      call settings%get_list('variables', variable_populator)

   end subroutine

   recursive subroutine create_group_settings(self, item)
      class (type_group_populator), intent(inout) :: self
      type (type_list_item),        intent(inout) :: item

      class (type_settings), pointer :: group_settings

      group_settings => type_settings_create(item)
      call configure_group(self%file, group_settings, self%variable_settings)
   end subroutine

   recursive subroutine create_variable_settings(self, item)
      class (type_variable_populator), intent(inout) :: self
      type (type_list_item),           intent(inout) :: item

      class (type_settings),        pointer :: variable_settings
      character(len=:), allocatable         :: source_name
      integer                               :: n
      class (type_output_category), pointer :: output_category
      class (type_output_field),    pointer :: output_field

      variable_settings => type_settings_create(item)

      ! Name of source variable
      source_name = variable_settings%get_string('source', 'name in model')

      ! Determine whether to create an output field or an output category
      n = len(source_name)
      if (source_name(n:n)=='*') then
         allocate(output_category)

         if (n==1) then
            output_category%name = ''
         else
            output_category%name = source_name(:n-2)
         end if

         ! Prefix for output name
         call variable_settings%get(output_category%prefix, 'prefix', 'name prefix used in output', default='')

         ! Postfix for output name
         call variable_settings%get(output_category%postfix, 'postfix', 'name postfix used in output', default='')

         ! Output level
         call variable_settings%get(output_category%output_level, 'output_level', 'output level', default=output_level_default)

         output_category%settings => self%file%create_settings()
         call output_category%settings%initialize(variable_settings, self%variable_settings)

         call self%file%append_category(output_category)
      else
         output_field => self%file%create_field()

         ! Name of output variable (may differ from source name)
         call variable_settings%get(output_field%output_name, 'name', 'name used in output', default=source_name)

         output_field%settings => self%file%create_settings()
         call output_field%settings%initialize(variable_settings, self%variable_settings)

         ! Select this variable for output in the field manager.
         output_field%source => self%file%field_manager%select_for_output(source_name)

         call self%file%append(output_field)
      end if
   end subroutine

end module

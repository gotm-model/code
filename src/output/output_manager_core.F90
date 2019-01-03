#include "cppdefs.h"

module output_manager_core

   use field_manager
   use yaml_types,only: type_dictionary

   implicit none

   public type_output_variable_settings,type_output_category,type_output_field, type_file, write_time_string, read_time_string, host, type_host, type_output_dimension
   public type_base_output_field

   private

   integer,parameter,public :: max_path = 256

   integer,parameter,public :: time_method_none          = 0  ! time-independent variable
   integer,parameter,public :: time_method_instantaneous = 1
   integer,parameter,public :: time_method_mean          = 2
   integer,parameter,public :: time_method_integrated    = 3

   integer,parameter,public :: time_unit_none   = 0
   integer,parameter,public :: time_unit_second = 1
   integer,parameter,public :: time_unit_hour   = 2
   integer,parameter,public :: time_unit_day    = 3
   integer,parameter,public :: time_unit_month  = 4
   integer,parameter,public :: time_unit_year   = 5
   integer,parameter,public :: time_unit_dt     = 6
   integer,parameter,public :: time_from_list   = 7

   integer,parameter,public :: rk = kind(_ONE_)

   type,abstract :: type_host
   contains
      procedure (host_julian_day),deferred :: julian_day
      procedure (host_calendar_date),deferred :: calendar_date
      procedure :: fatal_error => host_fatal_error
      procedure :: log_message => host_log_message
   end type

   abstract interface
      subroutine host_julian_day(self,yyyy,mm,dd,julian)
         import type_host
         class (type_host), intent(in) :: self
         integer, intent(in)  :: yyyy,mm,dd
         integer, intent(out) :: julian
      end subroutine
   end interface

   abstract interface
      subroutine host_calendar_date(self,julian,yyyy,mm,dd)
         import type_host
         class (type_host), intent(in) :: self
         integer, intent(in)  :: julian
         integer, intent(out) :: yyyy,mm,dd
      end subroutine
   end interface

   type type_output_variable_settings
      integer :: time_method = time_method_instantaneous
      class (type_base_output_field), pointer :: first_operator => null()
   contains
      procedure :: initialize => output_variable_settings_initialize
   end type

   type type_output_category
      class (type_output_variable_settings), pointer :: settings => null()
      character(len=string_length)         :: name = ''
      character(len=string_length)         :: prefix = ''
      character(len=string_length)         :: postfix = ''
      integer                              :: output_level = output_level_default
      class (type_category_node),  pointer :: source => null()
      class (type_output_category),pointer :: next => null()
   end type

   type type_output_field_pointer
      class (type_base_output_field), pointer :: p => null()
   end type

   type type_base_output_field
      class (type_output_variable_settings), pointer :: settings => null()
      character(len=string_length) :: output_name = ''

      real(rk), pointer :: data_0d        => null()
      real(rk), pointer :: data_1d(:)     => null()
      real(rk), pointer :: data_2d(:,:)   => null()
      real(rk), pointer :: data_3d(:,:,:) => null()

      type (type_output_field_pointer), allocatable :: coordinates(:)

      class (type_base_output_field), pointer :: next => null()
   contains
      procedure :: initialize       => base_field_initialize
      procedure :: new_data         => field_new_data
      procedure :: before_save      => field_before_save
      procedure :: flag_as_required => base_field_flag_as_required
      procedure :: get_metadata     => base_field_get_metadata
   end type type_base_output_field

   type, extends(type_base_output_field) :: type_output_field
      type (type_field), pointer :: source => null()

      ! Pointers to source data
      real(rk), pointer :: source_0d        => null()
      real(rk), pointer :: source_1d(:)     => null()
      real(rk), pointer :: source_2d(:,:)   => null()
      real(rk), pointer :: source_3d(:,:,:) => null()
   contains
      procedure :: initialize       => field_initialize
      procedure :: flag_as_required => field_flag_as_required
      procedure :: get_metadata     => field_get_metadata
   end type type_output_field

   type type_output_dimension
      type (type_dimension), pointer :: source => null()
      integer :: start        = 1
      integer :: stop         = -1
      integer :: stride       = 1
      integer :: global_start = 1
      integer :: global_stop  = -1
      type (type_output_dimension), pointer :: next => null()
   end type type_output_dimension

   type type_file
      type (type_field_manager),    pointer :: field_manager   => null()
      character(len=max_path)               :: path            = ''
      character(len=max_path)               :: postfix         = ''
      character(len=string_length)          :: title           = ''
      integer                               :: time_unit       = time_unit_none
      integer                               :: time_step       = 0
      integer                               :: first_index     = 0
      integer                               :: next_julian     = -1
      integer                               :: next_seconds    = -1
      integer                               :: first_julian    = -1
      integer                               :: first_seconds   = -1
      integer                               :: last_julian     = huge(1)
      integer                               :: last_seconds    = 0
      type (type_output_dimension), pointer :: first_dimension => null()
      class (type_output_category), pointer :: first_category  => null()
      class (type_base_output_field),    pointer :: first_field     => null()
      class (type_file),            pointer :: next            => null()
   contains
      procedure :: configure
      procedure :: initialize
      procedure :: save
      procedure :: finalize
      procedure :: create_settings
      procedure :: is_dimension_used
      procedure :: find
      procedure :: append
      procedure :: get_dimension
      procedure :: append_category
   end type type_file

   class (type_host),pointer,save :: host => null()

contains

   recursive subroutine base_field_initialize(self, field_manager)
      class (type_base_output_field), intent(inout), target :: self
      type (type_field_manager),      intent(in)            :: field_manager
   end subroutine

   recursive subroutine base_field_flag_as_required(self, required)
      class (type_base_output_field), intent(inout) :: self
      logical, intent(in) :: required
   end subroutine

   recursive subroutine base_field_get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_base_output_field), intent(in) :: self
      character(len=:), allocatable, optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), optional :: attributes
      if (present(dimensions)) allocate(dimensions(0))
   end subroutine

   recursive subroutine field_flag_as_required(self, required)
      class (type_output_field), intent(inout) :: self
      logical, intent(in) :: required
      if (associated(self%source%used_now) .and. required) self%source%used_now = .true.
   end subroutine

   recursive subroutine field_initialize(self, field_manager)
      class (type_output_field), intent(inout), target :: self
      type (type_field_manager), intent(in)            :: field_manager

      select case (self%source%status)
      case (status_not_registered)
         call host%fatal_error('field_initialize', 'Requested field "'//trim(self%source%name)//'" has not been registered with field manager.')
      case (status_registered_no_data)
         call host%fatal_error('field_initialize', 'Data for requested field "'//trim(self%source%name)//'" have not been provided.')
      end select
      self%data_0d => self%source%data_0d
      self%data_1d => self%source%data_1d
      self%data_2d => self%source%data_2d
      self%data_3d => self%source%data_3d
   end subroutine

   recursive subroutine field_get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_output_field), intent(in) :: self
      character(len=:), allocatable, optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      type (type_attributes), optional :: attributes
      real(rk), intent(out), optional :: minimum, maximum, fill_value

      if (self%source%status == status_not_registered) then
         call self%type_base_output_field%get_metadata(long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
         return
      end if
      if (present(long_name)) long_name = trim(self%source%long_name)
      if (present(units)) units = trim(self%source%units)
      if (present(dimensions)) then
         allocate(dimensions(size(self%source%dimensions)))
         dimensions(:) = self%source%dimensions(:)
      end if
      if (present(minimum)) minimum = self%source%minimum
      if (present(maximum)) maximum = self%source%maximum
      if (present(fill_value)) fill_value = self%source%fill_value
      if (present(standard_name) .and. self%source%standard_name /= '') standard_name = trim(self%source%standard_name)
      if (present(path) .and. associated(self%source%category)) path = trim(self%source%category%get_path())
   end subroutine

   recursive subroutine field_new_data(self)
      class (type_base_output_field), intent(inout) :: self
   end subroutine

   recursive subroutine field_before_save(self)
      class (type_base_output_field), intent(inout) :: self
   end subroutine

   recursive subroutine field_after_save(self)
      class (type_base_output_field), intent(inout) :: self
   end subroutine

   subroutine configure(self,mapping)
      class (type_file),      intent(inout) :: self
      class (type_dictionary),intent(in)    :: mapping
   end subroutine

   subroutine initialize(self)
      class (type_file),intent(inout) :: self
      stop 'output_manager_core:initialize not implemented'
   end subroutine

   function create_settings(self) result(settings)
      class (type_file),intent(inout) :: self
      class (type_output_variable_settings), pointer :: settings
      allocate(settings)
   end function create_settings

   subroutine save(self,julianday,secondsofday,microseconds)
      class (type_file),intent(inout) :: self
      integer,          intent(in)    :: julianday,secondsofday,microseconds
      stop 'output_manager_core:save not implemented'
   end subroutine

   subroutine finalize(self)
      class (type_file),intent(inout) :: self
   end subroutine

   subroutine write_time_string(jul,secs,timestr)
      integer,         intent(in)  :: jul,secs
      character(len=*),intent(out) :: timestr

      integer :: ss,min,hh,dd,mm,yy

      hh   = secs/3600
      min  = (secs-hh*3600)/60
      ss   = secs - 3600*hh - 60*min

      call host%calendar_date(jul,yy,mm,dd)

      write(timestr,'(i4.4,a1,i2.2,a1,i2.2,1x,i2.2,a1,i2.2,a1,i2.2)')  &
                           yy,'-',mm,'-',dd,hh,':',min,':',ss
   end subroutine write_time_string

   subroutine read_time_string(timestr,jul,secs,success)
      character(len=19)    :: timestr
      integer, intent(out) :: jul,secs
      logical, intent(out) :: success

      integer   :: ios
      character :: c1,c2,c3,c4
      integer   :: yy,mm,dd,hh,min,ss

      read(timestr,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)',iostat=ios)  &
                          yy,c1,mm,c2,dd,hh,c3,min,c4,ss
      success = ios == 0
      if (ios==0) then
         call host%julian_day(yy,mm,dd,jul)
         secs = 3600*hh + 60*min + ss
      end if
   end subroutine read_time_string

   subroutine host_fatal_error(self,location,error)
      class (type_host), intent(in) :: self
      character(len=*),  intent(in) :: location,error

      FATAL trim(location)//': '//trim(error)
      stop 1
   end subroutine

   subroutine host_log_message(self,message)
      class (type_host), intent(in) :: self
      character(len=*),  intent(in) :: message

      LEVEL2 trim(message)
   end subroutine

   logical function is_dimension_used(self,dim)
      class (type_file),intent(inout) :: self
      type (type_dimension), target   :: dim

      class (type_base_output_field),pointer :: output_field
      type (type_dimension_pointer), allocatable :: dimensions(:)
      integer :: i

      is_dimension_used = .true.
      output_field => self%first_field
      do while (associated(output_field))
         call output_field%get_metadata(dimensions=dimensions)
         do i=1,size(dimensions)
            if (associated(dimensions(i)%p,dim)) return
         end do
         output_field => output_field%next
      end do
      is_dimension_used = .false.
   end function is_dimension_used

   function find(self,field) result(output_field)
      class (type_file),intent(inout) :: self
      type (type_field), target       :: field
      class (type_base_output_field),pointer :: output_field

      output_field => self%first_field
      do while (associated(output_field))
         !if (associated(output_field%source,field)) return
         output_field => output_field%next
      end do
   end function find

   subroutine append(self,output_field)
      class (type_file),intent(inout)    :: self
      class (type_base_output_field), target :: output_field
      class (type_base_output_field),pointer  :: current

      current => self%first_field
      do while (associated(current))
         if (current%output_name==output_field%output_name) then
            !if (current%settings%time_method==output_field%settings%time_method .and. associated(current%source,output_field%source)) then
               ! The exact same output field already exists. Deallocate the new field and return a pointer to the old.
               !deallocate(output_field)
               !output_field => current
               return
            !end if
            call host%fatal_error('append','A different output field with name "'//trim(output_field%output_name)//'" already exists.')
         end if
         current => current%next
      end do

      if (associated(self%first_field)) then
         current => self%first_field
         do while (associated(current%next))
            current => current%next
         end do
         current%next => output_field
      else
         self%first_field => output_field
      end if
      output_field%next => null()
   end subroutine append

   subroutine append_category(self,output_category)
      class (type_file),intent(inout)      :: self
      class (type_output_category), target :: output_category

      ! Select this category for output in the field manager.
      if (.not.associated(output_category%settings)) output_category%settings => self%create_settings()
      output_category%source => self%field_manager%select_category_for_output(output_category%name,output_category%output_level)

      ! Prepend to list of output categories.
      output_category%next => self%first_category
      self%first_category => output_category
   end subroutine append_category

   function get_dimension(self,dim) result(output_dimension)
      class (type_file),intent(inout) :: self
      type (type_dimension),pointer   :: dim
      type (type_output_dimension), pointer :: output_dimension

      ! First try to find existing dimension entry.
      output_dimension => self%first_dimension
      do while (associated(output_dimension))
         if (associated(output_dimension%source,dim)) return
         output_dimension => output_dimension%next
      end do

      ! Create new dimension entry.
      allocate(output_dimension)
      output_dimension%next => self%first_dimension
      self%first_dimension => output_dimension
      output_dimension%source => dim
      output_dimension%stop = dim%length
      output_dimension%global_stop = dim%global_length
   end function get_dimension

   subroutine output_variable_settings_initialize(self,mapping,parent)
      use yaml_types

      class (type_output_variable_settings), intent(inout)        :: self
      class (type_dictionary),               intent(in)           :: mapping
      class (type_output_variable_settings), intent(in), optional :: parent

      type (type_error),  pointer :: config_error
      class (type_scalar),pointer :: scalar
      logical                     :: success

      if (present(parent)) then
         self%time_method = parent%time_method
         self%first_operator => parent%first_operator
      end if

      scalar => mapping%get_scalar('time_method',required=.false.,error=config_error)
      if (associated(config_error)) call host%fatal_error('output_item_initialize',config_error%message)
      if (.not.associated(scalar)) return
      select case (scalar%string)
      case ('mean')
         self%time_method = time_method_mean
      case ('point','instantaneous')
         self%time_method = time_method_instantaneous
      case ('integrated')
         self%time_method = time_method_integrated
      case default
         self%time_method = scalar%to_integer(self%time_method,success)
         if (.not.success.or.self%time_method<0.or.self%time_method>3) call host%fatal_error('output_item_initialize', trim(scalar%path)//' is set to "' &
            //trim(scalar%string)//'", which is not a supported value. Supported: point (1), mean (2), integrated (3).')
      end select
   end subroutine output_variable_settings_initialize

end module output_manager_core

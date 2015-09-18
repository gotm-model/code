#include "cppdefs.h"

module output_manager_core

   use field_manager
   use fabm_config_types,only: type_dictionary

   implicit none

   public type_output_item,type_output_category,type_output_field, type_file, write_time_string, read_time_string, host, type_host, type_output_dimension

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

   type type_output_item
      integer :: time_method = time_method_instantaneous
   end type

   type,extends(type_output_item) ::  type_output_category
      character(len=string_length)         :: name = ''
      character(len=string_length)         :: prefix = ''
      character(len=string_length)         :: postfix = ''
      integer                              :: output_level = output_level_default
      class (type_category_node),  pointer :: source => null()
      class (type_output_category),pointer :: next => null()
   end type

   type type_output_field_pointer
      class (type_output_field), pointer :: p => null()
   end type

   type,extends(type_output_item) :: type_output_field
      character(len=string_length) :: output_name = ''
      type (type_field),pointer    :: source => null()

      ! Pointers to source data
      real(rk),pointer                  :: source_0d        => null()
      real(rk),pointer                  :: source_1d(:)     => null()
      real(rk),pointer                  :: source_2d(:,:)   => null()
      real(rk),pointer                  :: source_3d(:,:,:) => null()

      ! Work arrays (only allocated/used if storing non-instantaneous data)
      real(rk)                          :: work_0d
      real(rk),allocatable              :: work_1d(:)
      real(rk),allocatable              :: work_2d(:,:)
      real(rk),allocatable              :: work_3d(:,:,:)

      ! Pointers to data to store (either pointing to instantaneous data, or to the above work arrays)
      real(rk),pointer                  :: data_0d        => null()
      real(rk),pointer                  :: data_1d(:)     => null()
      real(rk),pointer                  :: data_2d(:,:)   => null()
      real(rk),pointer                  :: data_3d(:,:,:) => null()

      type (type_output_field_pointer), allocatable :: coordinates(:)

      class (type_output_field),pointer :: next => null()
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
      integer                               :: time_unit       = time_unit_none
      integer                               :: time_step       = 0
      integer                               :: n               = 0  ! Number of model time steps processed so far for next output
      integer                               :: next_julian     = -1
      integer                               :: next_seconds    = -1
      integer                               :: first_julian    = -1
      integer                               :: first_seconds   = -1
      integer                               :: last_julian     = huge(1)
      integer                               :: last_seconds    = 0
      type (type_output_dimension), pointer :: first_dimension => null()
      class (type_output_category), pointer :: first_category  => null()
      class (type_output_field),    pointer :: first_field     => null()
      class (type_file),            pointer :: next            => null()
   contains
      procedure :: configure
      procedure :: initialize
      procedure :: save
      procedure :: finalize
      procedure :: create_field
      procedure :: is_dimension_used
      procedure :: find
      procedure :: append
      procedure :: get_dimension
   end type type_file

   class (type_host),pointer,save :: host => null()

contains

   subroutine configure(self,mapping)
      class (type_file),      intent(inout) :: self
      class (type_dictionary),intent(in)    :: mapping
   end subroutine

   subroutine initialize(self)
      class (type_file),intent(inout) :: self
      stop 'output_manager_core:initialize not implemented'
   end subroutine

   function create_field(self) result(field)
      class (type_file),intent(inout) :: self
      class (type_output_field), pointer :: field
      allocate(field)
   end function create_field

   subroutine save(self,julianday,secondsofday)
      class (type_file),intent(inout) :: self
      integer,          intent(in)    :: julianday,secondsofday
      stop 'output_manager_core:save not implemented'
   end subroutine

   subroutine finalize(file)
      class (type_file),intent(inout) :: file
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

   subroutine read_time_string(timestr,jul,secs)
      character(len=19)    :: timestr
      integer, intent(out) :: jul,secs

      character :: c1,c2,c3,c4
      integer   :: yy,mm,dd,hh,min,ss

      read(timestr,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)')  &
                          yy,c1,mm,c2,dd,hh,c3,min,c4,ss
      call host%julian_day(yy,mm,dd,jul)
      secs = 3600*hh + 60*min + ss
   end subroutine read_time_string

   subroutine host_fatal_error(self,location,error)
      class (type_host), intent(in) :: self
      character(len=*),  intent(in) :: location,error

      FATAL trim(location)//': '//trim(error)
      stop 'output_manager::host_fatal_error'
   end subroutine

   subroutine host_log_message(self,message)
      class (type_host), intent(in) :: self
      character(len=*),  intent(in) :: message

      LEVEL2 trim(message)
   end subroutine

   logical function is_dimension_used(self,dim)
      class (type_file),intent(inout) :: self
      type (type_dimension), target   :: dim

      class (type_output_field),pointer :: output_field
      integer :: i

      is_dimension_used = .true.
      output_field => self%first_field
      do while (associated(output_field))
         do i=1,size(output_field%source%dimensions)
            if (associated(output_field%source%dimensions(i)%p,dim)) return
         end do
         output_field => output_field%next
      end do
      is_dimension_used = .false.
   end function is_dimension_used

   function find(self,field) result(output_field)
      class (type_file),intent(inout) :: self
      type (type_field), target       :: field
      class (type_output_field),pointer :: output_field

      output_field => self%first_field
      do while (associated(output_field))
         if (associated(output_field%source,field)) return
         output_field => output_field%next
      end do
   end function find

   subroutine append(self,output_field)
      class (type_file),intent(inout)    :: self
      class (type_output_field), pointer :: output_field
      class (type_output_field),pointer  :: current

      current => self%first_field
      do while (associated(current))
         if (current%output_name==output_field%output_name) then
            if (current%time_method==output_field%time_method .and. associated(current%source,output_field%source)) then
               ! The exact same output field already exists. Deallocate the new field and return a pointer to the old.
               deallocate(output_field)
               output_field => current
               return
            end if
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

end module output_manager_core

#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: input
!
! !INTERFACE:
   module input
!
! !DESCRIPTION:
!
! !USES:
   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_input, do_input, close_input, register_input_0d, register_input_1d
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!

!  PRIVATE TYPES
   integer,parameter,public :: maxpathlen=256

!  Information on an observed variable
   type type_1d_variable
      integer                            :: index = -1     ! Column index of variable in input file
      REALTYPE, allocatable,dimension(:) :: data           ! Pointer to profile data (depth-dependent variable)
      type (type_1d_variable),pointer    :: next => null() ! Next variable in current input file
   end type

   type type_0d_variable
      integer                           :: index = -1     ! Column index of variable in input file
      REALTYPE                          :: data           ! Pointer to scalar data (depth-independent variable)
      type (type_0d_variable),pointer   :: next => null() ! Next variable in current input file
   end type

!  Information on file with observed profiles
   type type_profile_file
      character(len=maxpathlen)             :: path = ''
      REALTYPE, dimension(:,:), allocatable :: prof1,prof2,alpha
      integer                               :: jul1  = 0
      integer                               :: secs1 = 0
      integer                               :: jul2  = 0
      integer                               :: secs2 = 0
      integer                               :: unit  = -1
      integer                               :: lines = 0
      integer                               :: nprofiles = 0
      logical                               :: one_profile = .false.
      type (type_1d_variable), pointer      :: first_variable => null()
      type (type_profile_file),pointer      :: next => null()
   end type

!  Information on file with observed scalars (time series)
   type type_timeseries_file
      character(len=maxpathlen)           :: path = ''
      REALTYPE, dimension(:), allocatable :: obs1,obs2,alpha
      integer                             :: jul1  = 0
      integer                             :: secs1 = 0
      integer                             :: jul2  = 0
      integer                             :: secs2 = 0
      integer                             :: unit = -1
      type (type_0d_variable),    pointer :: first_variable => null()
      type (type_timeseries_file),pointer :: next => null()
   end type

!  PRIVATE DATA MEMBERS
!  Pointers to first files with observed profiles and observed scalars.
   type (type_profile_file),    pointer :: first_profile_file
   type (type_timeseries_file), pointer :: first_timeseries_file

!  Unit to use for next data file.
   integer :: next_unit_no
!
!  PRIVATE PARAMETERS
   integer,parameter :: max_variable_count_per_file = 256
   integer,parameter :: first_unit_no = 555
   
   integer :: nlev

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize input
!
! !INTERFACE:
   subroutine init_input(n)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   integer,intent(in) :: n
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_input'
   
   nlev = n
   next_unit_no = first_unit_no
   nullify(first_profile_file)
   nullify(first_timeseries_file)

   LEVEL1 'done'

   end subroutine init_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Register a 1d input variable.
!
! !INTERFACE:
   function register_input_1d(path,icolumn) result(pdata)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   character(len=*),intent(in) :: path
   integer,         intent(in) :: icolumn
   REALTYPE,pointer            :: pdata(:)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_profile_file),pointer :: file
   type (type_1d_variable), pointer :: variable
!
!-----------------------------------------------------------------------
!BOC
!  Find a file object for the specified file path; create one if it does exist yet.
   if (.not.associated(first_profile_file)) then
      allocate(first_profile_file)
      file => first_profile_file
   else
      file => first_profile_file
      do while (associated(file))
         if (file%path==path) exit
         file => file%next
      end do
      if (.not.associated(file)) then
         file => first_profile_file
         do while (associated(file%next))
            file => file%next
         end do
         allocate(file%next)
         file => file%next
      end if
   end if
   file%path = path

!  Create a variable object for the specified column index
   if (associated(file%first_variable)) then
      ! First determine whether a variable object for the specified column has already been created.
      variable => file%first_variable
      do while (associated(variable))
         if (variable%index==icolumn) then
            pdata => variable%data
            return
         end if
         variable => variable%next
      end do

!     Append a new variable object to the linked list.
      variable => file%first_variable
      do while (associated(variable%next))
         variable => variable%next
      end do
      allocate(variable%next)
      variable => variable%next
   else
!     This file does not have any associated variables; create the first.
      allocate(file%first_variable)
      variable => file%first_variable
   end if
   
   variable%index = icolumn
   allocate(variable%data(0:nlev))
   variable%data = _ZERO_
   pdata => variable%data

   end function register_input_1d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Register a 0d input variable.
!
! !INTERFACE:
   function register_input_0d(path,icolumn) result(pdata)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   character(len=*),intent(in) :: path
   integer,         intent(in) :: icolumn
   REALTYPE,pointer            :: pdata
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_timeseries_file),pointer :: file
   type (type_0d_variable),    pointer :: variable
!
!-----------------------------------------------------------------------
!BOC
!  Find a file object for the specified file path; create one if it does exist yet.
   if (.not.associated(first_timeseries_file)) then
      allocate(first_timeseries_file)
      file => first_timeseries_file
   else
      file => first_timeseries_file
      do while (associated(file))
         if (file%path==path) exit
         file => file%next
      end do
      if (.not.associated(file)) then
         file => first_timeseries_file
         do while (associated(file%next))
            file => file%next
         end do
         allocate(file%next)
         file => file%next
      end if
   end if
   file%path = path

!  Create a variable object for the specified column index
   if (associated(file%first_variable)) then
!     First determine whether a variable object for the specified column has already been created.
      variable => file%first_variable
      do while (associated(variable))
         if (variable%index==icolumn) then
            pdata => variable%data
            return
         end if
         variable => variable%next
      end do

!     Append a new variable object to the linked list.
      variable => file%first_variable
      do while (associated(variable%next))
         variable => variable%next
      end do
      allocate(variable%next)
      variable => variable%next
   else
!     This file does not have any associated variables; create the first.
      allocate(file%first_variable)
      variable => file%first_variable
   end if
   
   variable%index = icolumn
   variable%data = _ZERO_
   pdata => variable%data

   end function register_input_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read input observations
!
! !INTERFACE:
   subroutine do_input(jul,secs,nlev,z)
!
! !DESCRIPTION:
!  Read observations for all FABM variables for the current time.
!
! !USES:
!
! !INPUT PARAMETERS:
   integer,  intent(in) :: jul,secs,nlev
   REALTYPE, intent(in) :: z(:)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_profile_file),   pointer :: profile_file
   type (type_timeseries_file),pointer :: timeseries_file
!-----------------------------------------------------------------------
!BOC
!  Loop over files with observed profiles.
   profile_file => first_profile_file
   do while (associated(profile_file))
      call get_observed_profiles(profile_file,jul,secs,nlev,z)
      profile_file => profile_file%next
   end do

!  Loop over files with observed scalars.
   timeseries_file => first_timeseries_file
   do while (associated(timeseries_file))
      call get_observed_scalars(timeseries_file,jul,secs)
      timeseries_file => timeseries_file%next
   end do

   end subroutine do_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize a single input file with depth-explicit (1D) variables
!
! !INTERFACE:
   subroutine initialize_profile_file(info,nlev)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !USES:
!
! !INPUT PARAMETERS:
   type (type_profile_file),intent(inout) :: info
   integer,                 intent(in)    :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_1d_variable),pointer :: curvar
   integer :: nvar
   integer :: rc
!
!-----------------------------------------------------------------------
!BOC
!  Open the input file.
   open(next_unit_no,file=info%path,status='old',err=80)

!  Opening was successful - store the file unit, and increment the next unit with 1.
   info%unit = next_unit_no
   next_unit_no = next_unit_no + 1
   
!  Determine the maximum number of column that we need to read.
   nvar = 0
   curvar => info%first_variable
   do while (associated(curvar))
      nvar = max(nvar,curvar%index)
      curvar => curvar%next
   end do

   allocate(info%prof1(0:nlev,nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_profile_file: Error allocating memory (prof1)'
   info%prof1 = _ZERO_

   allocate(info%prof2(0:nlev,nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_profile_file: Error allocating memory (prof2)'
   info%prof2 = _ZERO_

   allocate(info%alpha(0:nlev,nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_profile_file: Error allocating memory (alpha)'
   info%alpha = _ZERO_

   return

80 FATAL 'Unable to open "',trim(info%path),'" for reading'
   stop 'input::initialize_profile_file'

   end subroutine initialize_profile_file
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read 1D data from a single input file
!
! !INTERFACE:
   subroutine get_observed_profiles(info,jul,secs,nlev,z)
!
! !DESCRIPTION:
!  Get observations for the current time from a single input file.
!  This reads in new observations if necessary (and available),
!  and performs linear interpolation in time and vertical space.
!
! !USES:
   use time, only: time_diff,julian_day
   use observations, only: read_profiles
!
! !INPUT PARAMETERS:
   integer,                 intent(in)   :: jul,secs
   integer,                 intent(in)   :: nlev
   REALTYPE,                intent(in)   :: z(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
   type(type_profile_file), intent(inout):: info
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                      :: rc
   integer                      :: yy,mm,dd,hh,min,ss
   REALTYPE                     :: t,dt
   type (type_1d_variable),pointer :: curvar
!
!-----------------------------------------------------------------------
!BOC
   if (info%unit==-1) call initialize_profile_file(info,nlev)

!  This part reads in new values if necessary.
   if(.not. info%one_profile .and. time_diff(info%jul2,info%secs2,jul,secs)<0) then
      do
         info%jul1 = info%jul2
         info%secs1 = info%secs2
         info%prof1 = info%prof2
         call read_profiles(info%unit,nlev,ubound(info%prof2,2),yy,mm,dd,hh,min,ss,z,info%prof2,info%lines,rc)
         if(rc/=0) then
            if(info%nprofiles==1) then
               LEVEL3 'Only one set of profiles is present.'
               info%one_profile = .true.
               curvar => info%first_variable
               do while (associated(curvar))
                  curvar%data = info%prof1(:,curvar%index)
                  curvar => curvar%next
               end do
            else
               FATAL 'Error reading profiles around line #',info%lines
               stop 'gotm_fabm_input:get_observed_profiles'
            end if
            exit
         else
            info%nprofiles = info%nprofiles + 1
            call julian_day(yy,mm,dd,info%jul2)
            info%secs2 = hh*3600 + min*60 + ss
            if(time_diff(info%jul2,info%secs2,jul,secs)>0) exit
         end if
      end do
      if( .not. info%one_profile) then
         dt = time_diff(info%jul2,info%secs2,info%jul1,info%secs1)
         info%alpha = (info%prof2-info%prof1)/dt
      end if
   end if

!  Do the time interpolation - only if more than one profile
   if( .not. info%one_profile) then
      t = time_diff(jul,secs,info%jul1,info%secs1)
      curvar => info%first_variable
      do while (associated(curvar))
         curvar%data = info%prof1(:,curvar%index) + t*info%alpha(:,curvar%index)
         curvar => curvar%next
      end do
   end if

   end subroutine get_observed_profiles
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize a single input file with horizontal (0D) variables.
!
! !INTERFACE:
   subroutine initialize_timeseries_file(info)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !INPUT PARAMETERS:
   type (type_timeseries_file),intent(inout) :: info
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_0d_variable),pointer :: curvar
   integer :: nvar
   integer :: rc
!
!-----------------------------------------------------------------------
!BOC
!  Open the input file.
   open(next_unit_no,file=info%path,status='old',err=80)

!  Opening was successful - store the file unit, and increment the next unit with 1.
   info%unit = next_unit_no
   next_unit_no = next_unit_no + 1

!  Determine the maximum number of column that we need to read.
   nvar = 0
   curvar => info%first_variable
   do while (associated(curvar))
      nvar = max(nvar,curvar%index)
      curvar => curvar%next
   end do
   
   allocate(info%obs1(nvar),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input::initialize_timeseries_file: Error allocating memory (obs1)'
   info%obs1 = _ZERO_

   allocate(info%obs2(nvar),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input::initialize_timeseries_file: Error allocating memory (obs2)'
   info%obs2 = _ZERO_

   allocate(info%alpha(nvar),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input::initialize_timeseries_file: Error allocating memory (alpha)'
   info%alpha = _ZERO_

   return

80 FATAL 'Unable to open "',trim(info%path),'" for reading'
   stop 'input::initialize_timeseries_file'

   end subroutine initialize_timeseries_file
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read 0D data from a single input file
!
! !INTERFACE:
   subroutine get_observed_scalars(info,jul,secs)
!
! !DESCRIPTION:
!  Get observations for the current time from a single input file.
!  This reads in new observations if necessary (and available),
!  and performs linear interpolation in time.
!
! !USES:
   use time, only: time_diff,julian_day
   use observations, only: read_obs
!
! !INPUT PARAMETERS:
   integer,                    intent(in)    :: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
   type(type_timeseries_file), intent(inout) :: info
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                      :: rc
   integer                      :: yy,mm,dd,hh,min,ss
   REALTYPE                     :: t,dt
   type (type_0d_variable),pointer :: curvar
!
!-----------------------------------------------------------------------
!BOC
   if (info%unit==-1) call initialize_timeseries_file(info)

!  This part reads in new values if necessary.
   if(time_diff(info%jul2,info%secs2,jul,secs)<0) then
      do
         info%jul1 = info%jul2
         info%secs1 = info%secs2
         info%obs1 = info%obs2
         call read_obs(info%unit,yy,mm,dd,hh,min,ss,size(info%obs2),info%obs2,rc)
         call julian_day(yy,mm,dd,info%jul2)
         info%secs2 = hh*3600 + min*60 + ss
         if(time_diff(info%jul2,info%secs2,jul,secs)>0) exit
      end do
      dt = time_diff(info%jul2,info%secs2,info%jul1,info%secs1)
      info%alpha = (info%obs2-info%obs1)/dt
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,info%jul1,info%secs1)
   curvar => info%first_variable
   do while (associated(curvar))
      curvar%data = info%obs1(curvar%index) + t*info%alpha(curvar%index)
      curvar => curvar%next
   end do

   end subroutine get_observed_scalars
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Close inputs
!
! !INTERFACE:
   subroutine close_input()
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_profile_file),     pointer :: profile_file,next_profile_file
   type (type_timeseries_file),  pointer :: timeseries_file,next_scalar_file
   type (type_1d_variable),pointer :: curvar_1d,nextvar_1d
   type (type_0d_variable),pointer :: curvar_0d,nextvar_0d
!
!-----------------------------------------------------------------------
!BOC

   profile_file => first_profile_file
   do while (associated(profile_file))
      curvar_1d => profile_file%first_variable
      do while (associated(curvar_1d))
         nextvar_1d => curvar_1d%next
         if (allocated(curvar_1d%data)) deallocate(curvar_1d%data)
         deallocate(curvar_1d)
         curvar_1d => nextvar_1d
      end do

      next_profile_file => profile_file%next
      if (profile_file%unit/=-1) close(profile_file%unit)
      if (allocated(profile_file%prof1)) deallocate(profile_file%prof1)
      if (allocated(profile_file%prof2)) deallocate(profile_file%prof2)
      if (allocated(profile_file%alpha)) deallocate(profile_file%alpha)
      deallocate(profile_file)

      profile_file => next_profile_file
   end do
   nullify(first_profile_file)

   timeseries_file => first_timeseries_file
   do while (associated(timeseries_file))
      curvar_0d => timeseries_file%first_variable
      do while (associated(curvar_0d))
         nextvar_0d => curvar_0d%next
         deallocate(curvar_0d)
         curvar_0d => nextvar_0d
      end do

      next_scalar_file => timeseries_file%next
      if (timeseries_file%unit/=-1) close(timeseries_file%unit)
      if (allocated(timeseries_file%obs1))  deallocate(timeseries_file%obs1)
      if (allocated(timeseries_file%obs2))  deallocate(timeseries_file%obs2)
      if (allocated(timeseries_file%alpha)) deallocate(timeseries_file%alpha)
      deallocate(timeseries_file)

      timeseries_file => next_scalar_file
   end do
   nullify(first_timeseries_file)

   end subroutine close_input
!EOC

!-----------------------------------------------------------------------

   end module input


!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


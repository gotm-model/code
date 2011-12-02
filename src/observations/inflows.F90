#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: initialises everything related to the inflows
!
! !INTERFACE:
   subroutine get_inflows(unit,jul,secs,nlev,z)
!
! !DESCRIPTION:
!  This routine is responsible for providing sane values to `observed'
!  inflows.
!  The subroutine is called in the {\tt get\_all\_obs()} subroutine
!  as part of the main integration loop.
!  In case of observations from file the temporal interpolation is
!  done in this routine.
!
! !USES:
   use time
   use observations, only: init_saved_vars,read_profiles,inflows

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: unit
   integer, intent(in)                 :: jul,secs
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev)
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: rc,n
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,dt
   integer, save             :: jul1,secs1
   integer, save             :: jul2,secs2
   integer, parameter        :: cols=2
   integer, save             :: lines
   integer, save             :: nprofiles
   logical, save             :: one_profile
   REALTYPE, save, dimension(:,:), allocatable :: prof1,prof2,alpha
!
!-----------------------------------------------------------------------
!BOC
   if (init_saved_vars) then
      jul2=0
      secs2=0
      lines=0
      nprofiles=0
      one_profile=.false.

      if (allocated(prof1)) deallocate(prof1)
      allocate(prof1(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (prof1)'
      prof1 = _ZERO_

      if (allocated(prof2)) deallocate(prof2)
      allocate(prof2(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (prof2)'
      prof2 = _ZERO_

      if (allocated(alpha)) deallocate(alpha)
      allocate(alpha(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (alpha)'
      
      if (allocated(inflows)) deallocate(inflows)
      allocate(inflows(cols,0:nlev),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows)'
   end if

!  This part initialises and reads in new values if necessary.
   if(.not. one_profile .and. time_diff(jul2,secs2,jul,secs) .lt. 0) then
      do
         jul1 = jul2
         secs1 = secs2
         prof1 = prof2
         call read_profiles(unit,nlev,cols,yy,mm,dd,hh,min,ss,z,prof2,lines,rc)
         if(rc .ne. 0) then
            if(nprofiles .eq. 1) then
               LEVEL3 'Only one salinity profile present.'
               one_profile = .true.
               do n=1,cols
                  inflows(:,n) = prof1(:,n)
               end do
            else
               FATAL 'Error reading inflows around line # ',lines
               stop 'get_inflows'
            end if
            EXIT
         else
            nprofiles = nprofiles + 1
            call julian_day(yy,mm,dd,jul2)
            secs2 = hh*3600 + min*60 + ss
            if(time_diff(jul2,secs2,jul,secs) .gt. 0) EXIT
         end if
      end do
      if( .not. one_profile) then
         dt = time_diff(jul2,secs2,jul1,secs1)
         alpha = (prof2-prof1)/dt
      end if
   end if
!  Do the time interpolation - only if more than one profile
   if( .not. one_profile) then
      t  = time_diff(jul,secs,jul1,secs1)
      do n=1,cols
         inflows(n,:) = prof1(:,n) + t*alpha(:,n)
      end do
   end if

   do n=0, nlev
      write(*,*) "n = ", n, "inflows = ", inflows(:,n)
   end do

   return
   end subroutine get_inflows
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Initial read in of the inflows from specified file
!
! !INTERFACE:
   subroutine read_inflows(unit,ierr)
!
!  !DESCRIPTION:
!  Reads in the inflows from file at "unit" and saves everything
!  to the *_input variables.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)       :: unit
! !OUTPUT PARAMETERS:
   integer, intent(out)      :: ierr
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   integer                   :: rc
   integer                   :: up_down
   integer, save             :: lines
   REALTYPE                  :: x
!-----------------------------------------------------------------------
!BOC
   ierr = 0
   read(unit,*,ERR=100,END=110) N_input,up_down
   lines = 1

   if (allocated(depth_input)) deallocate(depth_input)
   allocate(depth_input(0:N_input),stat=rc)
   if (rc /= 0) stop 'read_hypsography: Error allocating memory (depth_input)'
   depth_input = _ZERO_
   if (allocated(A_input)) deallocate(A_input)
   allocate(A_input(0:N_input),stat=rc)
   if (rc /= 0) then
      stop 'read_hypsography: Error allocating memory (A_input)'
   end if
   A_input = _ZERO_

   select case (up_down)
      case(1)  ! surface ref, read from bottom
         do i=1,N_input
            lines = lines+1
            read(unit,*,ERR=100,END=110) depth_input(i), A_input(i)
         end do
      case(2)  ! surface ref, read from surface
         do i=N_input,1,-1
            lines = lines+1
            read(unit,*,ERR=100,END=110) depth_input(i), A_input(i)
         end do
      case(3)  ! bottom ref, read from bottom
         do i=1,N_input
            lines = lines+1
            read(unit,*,ERR=100,END=110) depth_input(i), A_input(i)
         end do
         do i=1,N_input/2
            x = depth_input(i)
            depth_input(i) = -depth_input(N_input-(i-1))
            depth_input(N_input-(i-1)) = -x
         end do
      case(4)  ! bottom ref, read from surface
         do i=N_input,1,-1
            lines = lines+1
            read(unit,*,ERR=100,END=110) depth_input(i), A_input(i)
         end do
         do i=1,N_input/2
            x = depth_input(i)
            depth_input(i) = -depth_input(N_input-(i-1))
            depth_input(N_input-(i-1)) = -x
         end do
      case default
   end select

   return
!  READ_ERROR = -2
100 ierr = -2
   FATAL 'Error reading inflows (READ_ERROR)'
   return
!  END_OF_FILE = -1
110 ierr = -1
   FATAL 'Error reading inflows (END_OF_FILE)'
   return
   end subroutine read_inflows
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: calculate inflows
!
! !INTERFACE:
   subroutine update_inflows()
!
! !DESCRIPTION:
!  TODO!
!
! !USES:
   IMPLICIT NONE
!EOP
!
!-----------------------------------------------------------------------
!BOC

   end subroutine update_inflows
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

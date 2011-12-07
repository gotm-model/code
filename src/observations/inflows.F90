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
   REALTYPE, dimension(0:nlev) :: depth
   REALTYPE, save, dimension(:), allocatable :: depth_input
   REALTYPE, save, dimension(:,:), allocatable :: inflows_input1,inflows_input2
   REALTYPE, save, dimension(:,:), allocatable :: alpha

   integer                   :: ierr
   integer                   :: N_input
   integer                   :: i,j
   integer                   :: up_down
   REALTYPE                  :: x
   character(len=128)        :: cbuf
   character                 :: c1,c2,c3,c4
!
!-----------------------------------------------------------------------
!BOC
   if (init_saved_vars) then
      jul2=0
      secs2=0
      lines=0
      nprofiles=0
      one_profile=.false.
      ierr = 0
      read(unit,'(A72)',ERR=100,END=110) cbuf
      read(cbuf,900,ERR=100,END=110) yy,c1,mm,c2,dd,hh,c3,min,c4,ss
      read(cbuf(20:),*,ERR=100,END=110) N_input,up_down

      if (allocated(depth_input)) deallocate(depth_input)
      allocate(depth_input(1:N_input),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (depth_input)'
      depth_input = _ZERO_

      if (allocated(inflows_input2)) deallocate(inflows_input2)
      allocate(inflows_input2(cols,1:N_input),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows_input2)'

      if (allocated(inflows_input1)) deallocate(inflows_input1)
      allocate(inflows_input1(cols,1:N_input),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows_input1)'

      if (allocated(inflows)) deallocate(inflows)
      allocate(inflows(cols,1:N_input),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows)'

      if (allocated(alpha)) deallocate(alpha)
      allocate(alpha(cols,1:N_input),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (alpha)'
   end if

!  This part initialises and reads in new values if necessary.
   if(.not. one_profile .and. time_diff(jul2,secs2,jul,secs) .lt. 0) then
      do
         jul1 = jul2
         secs1 = secs2
         inflows_input1 = inflows_input2

         ierr = 0
         read(unit,'(A72)',ERR=100,END=110) cbuf
         read(cbuf,900,ERR=100,END=110) yy,c1,mm,c2,dd,hh,c3,min,c4,ss
         read(cbuf(20:),*,ERR=100,END=110) N_input,up_down
         lines = 1

         select case (up_down)
            case(1)  ! surface ref, read from bottom
               do i=1,N_input
                  lines = lines+1
                  read(unit,*,ERR=100,END=110) depth_input(i),(inflows_input2(i,j),j=1,cols)
               end do
            case(2)  ! surface ref, read from surface
               do i=N_input,1,-1
                  lines = lines+1
                  read(unit,*,ERR=100,END=110) depth_input(i),(inflows_input2(j,i),j=1,cols)
               end do
            case(3)  ! bottom ref, read from bottom
               do i=1,N_input
                  lines = lines+1
                  read(unit,*,ERR=100,END=110) depth_input(i),(inflows_input2(i,j),j=1,cols)
               end do
               do i=1,N_input/2
                  x = depth_input(i)
                  depth_input(i) = -depth_input(N_input-(i-1))
                  depth_input(N_input-(i-1)) = -x
               end do
            case(4)  ! bottom ref, read from surface
               do i=N_input,1,-1
                  lines = lines+1
                  read(unit,*,ERR=100,END=110) depth_input(i),(inflows_input2(i,j),j=1,cols)
               end do
               do i=1,N_input/2
                  x = depth_input(i)
                  depth_input(i) = -depth_input(N_input-(i-1))
                  depth_input(N_input-(i-1)) = -x
               end do
            case default
         end select
         if(rc .ne. 0) then
            if(nprofiles .eq. 1) then
               LEVEL3 'Only one inflow profile present.'
               one_profile = .true.
               do n=1,cols
                  inflows(n,:) = inflows_input1(n,:)
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
         alpha = (inflows_input2-inflows_input1)/dt
      end if
   end if

!  Do the time interpolation - only if more than one profile
   if( .not. one_profile) then
      t  = time_diff(jul,secs,jul1,secs1)
      do n=1,cols
         inflows(n,:) = inflows_input1(n,:) + t*alpha(n,:)
      end do
   end if

   write(*,*) inflows
   write(*,*)

   return
!  READ_ERROR = -2
100 ierr = -2
   FATAL 'Error reading inflows (READ_ERROR)'
   return
!  END_OF_FILE = -1
110 ierr = -1
   FATAL 'Error reading inflows (END_OF_FILE)'
   return
900 format(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)

   end subroutine get_inflows
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

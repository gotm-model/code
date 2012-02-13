#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: inflows
!
! !INTERFACE:
   module inflows
!
! !DESCRIPTION:
! TODO: write descr.
!
! !USES
!  !PUBLIC DATA MEMBERS:
   IMPLICIT NONE
   public                                :: init_inflows,clean_inflows
   public                                :: get_inflows,update_inflows

!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   integer                                     :: rc
   integer, parameter                          :: cols=2
   integer                                     :: N_input
   REALTYPE, save, dimension(:), allocatable   :: A_input,depth_input
   REALTYPE, save, dimension(:,:), allocatable :: inflows_input1,inflows_input2
   REALTYPE, save, dimension(:,:), allocatable :: alpha
   REALTYPE, save, dimension(:), allocatable :: Q
   REALTYPE, save                              :: tauI
   REALTYPE, save                              :: tauI_left
   REALTYPE, save                              :: SI,TI
   REALTYPE, save                              :: VI_step
   REALTYPE, save                              :: V_diff
   REALTYPE, save                              :: Q_I
! !DEFINED PARAMETERS:
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: initialises everything related to the inflows
!
! !INTERFACE:
   subroutine init_inflows(nlev)
!
!  !DESCRIPTION:
!  Initialises everything related to the lake model, e.g. allocating memory
!  for arrays.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in) :: nlev
!
!EOP
!-----------------------------------------------------------------------
!BOC
      SI = 10.5d0
      TI = 4.0d0
      tauI = _ZERO_
      V_diff = _ZERO_
      Q_I = _ZERO_
      if (allocated(Q)) deallocate(Q)
      allocate(Q(0:nlev),stat=rc)
      if (rc /= 0) stop 'init_inflows: Error allocating memory (Q)'
      Q = _ZERO_
!112 FATAL 'Unable to open "',trim(hypsography_file),'" for reading'
!      stop 'init_hypsography'

   end subroutine init_inflows
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: initialises everything related to the inflows
!
! !INTERFACE:
   subroutine get_inflows(unit,init_saved_vars,jul,secs,inflows_input)
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

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: unit
   logical, intent(in)                 :: init_saved_vars
   integer, intent(in)                 :: jul,secs
   REALTYPE, intent(inout), dimension(:,:), allocatable :: inflows_input
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: rc,n
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,dt
   integer, save             :: jul1,secs1
   integer, save             :: jul2,secs2
   integer, save             :: lines
   integer, save             :: nprofiles
   logical, save             :: one_profile
   integer                   :: ierr
   integer                   :: N_input_old
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
      N_input_old = N_input
      read(unit,'(A72)',ERR=100,END=110) cbuf
      read(cbuf,900,ERR=100,END=110) yy,c1,mm,c2,dd,hh,c3,min,c4,ss
      read(cbuf(20:),*,ERR=100,END=110) N_input,up_down
      !go back one "read-command" in file
      backspace(unit)

      ! only allocate memory if the size of the arrays will change
      if (N_input .ne. N_input_old) then
         if (allocated(depth_input)) deallocate(depth_input)
         allocate(depth_input(1:N_input),stat=rc)
         if (rc /= 0) stop 'get_inflows: Error allocating memory (depth_input)'
         depth_input = _ZERO_

         if (allocated(inflows_input2)) deallocate(inflows_input2)
         allocate(inflows_input2(cols,1:N_input),stat=rc)
         if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows_input2)'
         inflows_input2 = _ZERO_

         if (allocated(inflows_input1)) deallocate(inflows_input1)
         allocate(inflows_input1(cols,1:N_input),stat=rc)
         if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows_input1)'
         inflows_input1 = _ZERO_

         if (allocated(inflows_input)) deallocate(inflows_input)
         allocate(inflows_input(cols,1:N_input),stat=rc)
         if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows_input)'
         inflows_input = _ZERO_

         if (allocated(alpha)) deallocate(alpha)
         allocate(alpha(cols,1:N_input),stat=rc)
         if (rc /= 0) stop 'get_inflows: Error allocating memory (alpha)'
         alpha = _ZERO_
      else
         depth_input = _ZERO_
         inflows_input2 = _ZERO_
         inflows_input1 = _ZERO_
         inflows_input = _ZERO_
         alpha = _ZERO_
      end if
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
                  read(unit,*,ERR=100,END=110) &
                     depth_input(i),(inflows_input2(i,j),j=1,cols)
               end do
            case(2)  ! surface ref, read from surface
               do i=N_input,1,-1
                  lines = lines+1
                  read(unit,*,ERR=100,END=110) &
                     depth_input(i),(inflows_input2(j,i),j=1,cols)
               end do
            case(3)  ! bottom ref, read from bottom
               do i=1,N_input
                  lines = lines+1
                  read(unit,*,ERR=100,END=110) &
                     depth_input(i),(inflows_input2(i,j),j=1,cols)
               end do
               do i=1,N_input/2
                  x = depth_input(i)
                  depth_input(i) = -depth_input(N_input-(i-1))
                  depth_input(N_input-(i-1)) = -x
               end do
            case(4)  ! bottom ref, read from surface
               do i=N_input,1,-1
                  lines = lines+1
                  read(unit,*,ERR=100,END=110) &
                     depth_input(i),(inflows_input2(i,j),j=1,cols)
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
                  inflows_input(n,:) = inflows_input1(n,:)
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
         inflows_input(n,:) = inflows_input1(n,:) + t*alpha(n,:)
      end do
   end if

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
   subroutine update_inflows(nlev,dt,S,T,h,Ac,Af,inflows_input,VI, &
                             qs,qt,FQ)
!
! !DESCRIPTION:
!  TODO!
!
! !USES:
!   use meanflow, only: S, T
!   use meanflow, only: h, Ac
   use eqstate, only: unesco

   IMPLICIT NONE

! !INPUT PARAMETERS:
   integer, intent(in)                  :: nlev
   REALTYPE, intent(in)                 :: dt
   REALTYPE, intent(in)                 :: S(0:nlev), T(0:nlev)
   REALTYPE, intent(in)                 :: h(0:nlev), Ac(0:nlev), Af(0:nlev)
   REALTYPE, intent(inout), dimension(:,:), allocatable :: inflows_input
   ! TODO: should this be an argument or a module variable!?
   REALTYPE, intent(inout)              :: VI
   REALTYPE, intent(inout)              :: qs(0:nlev), qt(0:nlev)
   REALTYPE, intent(inout)              :: FQ(0:nlev)
!EOP
!
! !LOCAL VARIABLES:
   integer              :: i,n
   REALTYPE             :: rhoI,rho
   REALTYPE             :: depth
   REALTYPE             :: zI_min,zI_max
   REALTYPE             :: VI_basin
   integer              :: index_min
   REALTYPE             :: threshold
!
!-----------------------------------------------------------------------
!BOC
   threshold = 17.0848d0

   ! check if an inflow will occure
   do i=1,N_input
      if (inflows_input(1,i) >= threshold) then
         write(*,*) "!!!!!!!!!!!!!!!!!!!!"
         write(*,*) "inflow was triggered"
         write(*,*) "!!!!!!!!!!!!!!!!!!!!"
         !2 weeks
         tauI = 1209600d0
         !1 day
         !tauI = 3 * 86400d0
         tauI_left = TauI
         VI = 50d09
         VI_step = (VI / tauI) * dt
         Q_I = VI / tauI
      end if
   end do

   ! inflow triggered or still in progress
   if (tauI_left .gt. _ZERO_) then
      ! calculate depth of water column
      depth = _ZERO_
      do i=1,nlev
         depth = depth - h(i)
      end do

      ! find minimal depth where the inflow will take place
      zI_min = _ZERO_
      index_min = 0
      do i=1,nlev
         depth = depth + h(i)
         rhoI = unesco(SI,TI,depth/10.0d0,.false.)
         rho = unesco(S(i),T(i),depth/10.0d0,.false.)
         ! if the density of the inflowing water is greater than the
         ! ambient water then the lowest interleaving depth is found
         if (rhoI > rho) then
            zI_min = depth
            index_min = i
            exit
         end if
      end do

      ! find the z-levels in which the water will interleave
      VI_basin = _ZERO_
      zI_max = zI_min
      n = index_min
      do while (VI_basin < VI_step)
         VI_basin = VI_basin + Ac(n) * h(n)
         zI_max = zI_max - h(n)
         n = n+1
         ! for debugging only
         if (n .gt. nlev) then
            write(*,*) "Warning: Too much water flowing into the basin."
            n = nlev
            exit
         end if
      end do
!      ! VI_basin is now too big so go back one step
      n = n-1
!      zI_max = zI_max + h(n)
!      VI_basin = VI_basin - Ac(n) * h(n)

      do i=0,nlev
         qs(i) = _ZERO_
         qt(i) = _ZERO_
         Q(i) = _ZERO_
         FQ(i) = _ZERO_
      end do
      ! calculate the source terms
      ! "+1" because loop includes both n and index_min
      do i=index_min,n
         Q(i) = Q_I / (n-index_min+1)
         !TODO check the +1.0d0 -> needed for alternating signs in salinity
         qs(i) = SI * Q(i) / (Af(i) - Af(i-1))
         qt(i) = SIGN(TI * Q(i) / (Af(i) - Af(i-1)), TI-T(i+1)+1.0d0)
      end do

      ! calculate the vertical flux terms
      FQ(index_min) = Q(index_min)
      do i=index_min+1,nlev-1
         FQ(i) = FQ(i-1) + Q(i)
      end do

      ! calculate the sink term at sea surface
      qs(nlev) = -S(nlev) * FQ(nlev-1) / (Af(nlev) - Af(nlev-1))
      qt(nlev) = SIGN(T(nlev) * FQ(nlev-1) / (Af(nlev) - Af(nlev-1)), &
                      -T(nlev))
      VI = VI - VI_step
      tauI_left = tauI_left - dt
   else
      do i=1,nlev
         qs(i) = _ZERO_
         qt(i) = _ZERO_
         FQ(i) = _ZERO_
      end do
   end if

   end subroutine update_inflows
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: cleaning up
!
! !INTERFACE:
   subroutine clean_inflows()
!
! !DESCRIPTION:
!  De-allocates all memory allocated via init\_inflows()
!
! !USES:
   IMPLICIT NONE
!EOP
!
!-----------------------------------------------------------------------
!BOC
      if (allocated(depth_input)) deallocate(depth_input)
      if (allocated(inflows_input2)) deallocate(inflows_input2)
      if (allocated(inflows_input1)) deallocate(inflows_input1)
      if (allocated(alpha)) deallocate(alpha)

      return
   end subroutine clean_inflows

   end module inflows
!EOC
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

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
   REALTYPE, save                              :: tauI
   REALTYPE, save                              :: tauI_left
   REALTYPE, save                              :: SI,TI
   REALTYPE, save                              :: mI
   REALTYPE, save                              :: VIn
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
      mI = 0.0d0
      SI = 12.0d0
      TI = 10.0d0
      tauI = _ZERO_
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
   subroutine get_inflows(unit,init_saved_vars,jul,secs,nlev,z,inflows_input)
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
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev)
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
   REALTYPE, dimension(0:nlev) :: depth

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
   subroutine update_inflows(inflows_input,VI,Qs,Qt,FQs,FQt,nlev,dt)
!
! !DESCRIPTION:
!  TODO!
!
! !USES:
   use meanflow, only: S, T
   use meanflow, only: h, Ac
   use eqstate, only: unesco

   IMPLICIT NONE

! !INPUT PARAMETERS:
   REALTYPE, intent(inout), dimension(:,:), allocatable :: inflows_input
   REALTYPE, intent(inout)             :: VI
   REALTYPE, intent(inout)             :: Qs(0:nlev), Qt(0:nlev)
   REALTYPE, intent(inout)             :: FQs(0:nlev), FQt(0:nlev)
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt
!EOP
!
! !LOCAL VARIABLES:
   integer              :: i,n
   logical              :: trigger
   REALTYPE             :: rhoI,rho
   REALTYPE             :: depth
   REALTYPE             :: zI_min,zI_max
   REALTYPE             :: V_basin
   REALTYPE             :: QI
   integer              :: index_min
   REALTYPE             :: threshold
!
!-----------------------------------------------------------------------
!BOC
   threshold = 17.0848d0
   trigger = .false.

   ! check if an inflow will occure
   do i=1,N_input
      if (inflows_input(1,i) >= threshold) then
         write(*,*) "!!!!!!!!!!!!!!!!!!!!"
         write(*,*) "inflow was triggered"
         write(*,*) "!!!!!!!!!!!!!!!!!!!!"
         trigger = .true.
         !2 weeks
         tauI = 1209600
         !1 day
         !tauI = 86400d0
         tauI_left = TauI
         !tauI = tauI + 1200
         VI = VI + 50d10
         VIn = (VI / tauI) * dt
      end if
   end do

   ! inflow triggered or still in progress
   if ((trigger .or. VI .gt. _ZERO_) .and. tauI_left .gt. _ZERO_) then
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
!         write(*,*) "depth = ", depth
!         write(*,*) "S_inflow = ", SI
!         write(*,*) "T_inflow = ", TI
!         write(*,*) "rho_inflow = ", rhoI
!         write(*,*) "S_ambient = ", S(i)
!         write(*,*) "T_ambient = ", T(i)
!         write(*,*) "rho_ambient = ", rho
         ! if the density of the inflowing water is greate than the
         ! ambient water then the lowest interleaving depth is found
         if (rhoI > rho) then
            write(*,*) "interleaving depth found"
            write(*,*) depth
            zI_min = depth
            index_min = i
            exit
         end if
      end do

      write(*,*) ""
      write(*,*) "z_inflow min = ", zI_min, " | index_min = ", index_min
      write(*,*) ""
      write(*,*) "dt = ", dt, " | tau_inflow_left = ", tauI_left

      ! find the z-levels in which the water will interleave
      V_basin = _ZERO_
      zI_max = zI_min
      n = index_min
      do while (V_basin < VIn)
         V_basin = V_basin + Ac(n) * h(n)
         zI_max = zI_max - h(n)
         n = n+1
         if (n .gt. nlev) then
            write(*,*) "Warning: Too much water flowing into the basin."
            n = nlev
            exit
         end if
      end do

      write(*,*) ""
      write(*,*) "V_inflow = ", VI, " | V_inflow_timestep = ", VIn, &
         " | V_basin = ", V_basin

      ! calculate the source terms
      do i=index_min,n
         Qs(i) = (SI - S(i)) / tauI
         Qt(i) = (TI - T(i)) / tauI
      end do
      ! calculate the vertical flux terms
      FQs(0) = Qs(0)
      FQt(0) = Qt(0)
      do i=1,nlev
         FQs(i) = FQs(i-1) + Qs(i)
         FQt(i) = FQt(i-1) + Qt(i)
      end do
      VI = VI - VIn
      tauI_left = tauI_left - dt
   else
      do i=0,nlev
         Qs(i) = _ZERO_
         Qt(i) = _ZERO_
         FQs(i) = _ZERO_
         FQt(i) = _ZERO_
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
!  TODO!
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

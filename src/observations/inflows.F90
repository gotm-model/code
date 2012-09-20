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
! This module is responsible for all calculations related to inflows. This means
! reading in prescribed values from a given file, calculating the depth where
! the inflowing water masses interleave, and calculating vertical fluxes. These
! vertical fluxes are used by other routines to calculate vertical advection
! velocities.
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
   integer, parameter                          :: cols=3
   REALTYPE, save, dimension(:), allocatable   :: inflows_input1,inflows_input2
   REALTYPE, save, dimension(:), allocatable   :: alpha
   REALTYPE, save, dimension(:), allocatable   :: Q
   REALTYPE                                    :: QI,SI,TI
   integer, save                               :: inflows_method
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
   subroutine init_inflows(nlev, method)
!
!  !DESCRIPTION:
!  Initialises everything related to the lake model, e.g. allocating memory
!  for arrays.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer             :: rc
   integer, intent(in) :: nlev
   integer, intent(in) :: method
!
!EOP
!-----------------------------------------------------------------------
!BOC
   inflows_method = method

   if (allocated(Q)) deallocate(Q)
   allocate(Q(0:nlev),stat=rc)
   if (rc /= 0) stop 'get_inflows: Error allocating memory (Q)'
   Q = _ZERO_

   if (allocated(inflows_input1)) deallocate(inflows_input1)
   allocate(inflows_input1(cols),stat=rc)
   if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows_input1)'
   inflows_input1 = _ZERO_

   if (allocated(inflows_input2)) deallocate(inflows_input2)
   allocate(inflows_input2(cols),stat=rc)
   if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows_input2)'
   inflows_input2 = _ZERO_

   if (allocated(alpha)) deallocate(alpha)
   allocate(alpha(cols),stat=rc)
   if (rc /= 0) stop 'get_inflows: Error allocating memory (alpha)'
   alpha = _ZERO_

   end subroutine init_inflows
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Reads in the necessary data for the inflows from file.
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
   REALTYPE, intent(inout), dimension(:), allocatable :: inflows_input
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
      !go back one "read-command" in file
      backspace(unit)

      if (allocated(inflows_input)) deallocate(inflows_input)
      allocate(inflows_input(cols),stat=rc)
      if (rc /= 0) stop 'get_inflows: Error allocating memory (inflows_input)'
      inflows_input = _ZERO_
      inflows_input1 = _ZERO_
      inflows_input2 = _ZERO_
      alpha = _ZERO_
      Q = _ZERO_
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

         read(cbuf(20:),*,ERR=100,END=110) inflows_input2
         if(rc .ne. 0) then
            if(nprofiles .eq. 1) then
               LEVEL3 'Only one inflow profile present.'
               one_profile = .true.
               inflows_input = inflows_input1
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
         inflows_input = inflows_input1 + t*alpha
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
   subroutine update_inflows(lake,nlev,dt,S,T,h,Ac,inflows_input, &
                             Qs,Qt,FQ)
!
! !DESCRIPTION:
!  Calculates the depth where the inflow occurs and
!  what effects it has on the water column. The resulting flux variables
!  can then be used by routines like salinity or temperature to calculate the
!  vertical advection induced by the inflows.
!
! !USES:
   use eqstate, only: unesco

   IMPLICIT NONE

! !INPUT PARAMETERS:
   logical, intent(in)                    :: lake
   integer, intent(in)                    :: nlev
   REALTYPE, intent(in)                   :: dt
   REALTYPE, intent(in)                   :: S(0:nlev), T(0:nlev)
   REALTYPE, intent(in)                   :: h(0:nlev), Ac(0:nlev)
   REALTYPE, intent(inout), dimension(:), allocatable :: inflows_input
   REALTYPE, intent(inout)                :: Qs(0:nlev), Qt(0:nlev)
   REALTYPE, intent(inout)                :: FQ(0:nlev)
!EOP
!
! !LOCAL VARIABLES:
   integer              :: i,n
   REALTYPE             :: rhoI,rho
   REALTYPE             :: depth
   REALTYPE             :: VI_basin
   integer              :: index_min
!
!-----------------------------------------------------------------------
!BOC

   ! check if an inflow will occur
   if (inflows_method .eq. 2) then
      if (lake) then
         QI = inflows_input(1)
         SI = inflows_input(2)
         TI = inflows_input(3)
         ! inflow triggered or still in progress
         if (QI .gt. _ZERO_) then
            ! calculate depth of water column
            depth = _ZERO_
            do i=1,nlev
               depth = depth - h(i)
            end do

            do i=0,nlev
               Qs(i) = _ZERO_
               Qt(i) = _ZERO_
               Q(i) = _ZERO_
               FQ(i) = _ZERO_
            end do

            ! find minimal depth where the inflow will take place
            index_min = 0
            do i=1,nlev
               depth = depth + h(i)
               rhoI = unesco(SI,TI,depth/10.0d0,.false.)
               rho = unesco(S(i),T(i),depth/10.0d0,.false.)
               ! if the density of the inflowing water is greater than the
               ! ambient water then the lowest interleaving depth is found
               if (rhoI > rho) then
                  index_min = i
                  exit
               end if
            end do

            !density of the inflowing water is too small -> no inflow
            if (index_min .eq. 0) then
               return
            endif

            ! find the z-levels in which the water will interleave
            VI_basin = _ZERO_
            n = index_min
            do while (VI_basin < QI*dt)
               VI_basin = VI_basin + Ac(n) * h(n)
               n = n+1
               if (n .gt. nlev) then
                  !if inflow at surface -> no inflow
                  !debug output only
                  write(*,*) "Warning: Too much water flowing into the basin."
                  return
               end if
            end do
            ! VI_basin is now too big so go back one step
            n = n-1

            ! calculate the source terms
            ! "+1" because loop includes both n and index_min
            do i=index_min,n
               Q(i) = QI / (n-index_min+1)
               Qs(i) = SI * Q(i) / (Ac(i) * h(i))
               Qt(i) = TI * Q(i) / (Ac(i) * h(i))
            end do

            ! calculate the vertical flux terms
            FQ(index_min) = Q(index_min)
            do i=index_min+1,nlev-1
               FQ(i) = FQ(i-1) + Q(i)
            end do

            ! calculate the sink term at sea surface
            Qs(nlev) = -S(nlev) * FQ(nlev-1) / (Ac(nlev) * h(nlev))
            Qt(nlev) = -T(nlev) * FQ(nlev-1) / (Ac(nlev) * h(nlev))
         else
            do i=1,nlev
               Qs(i) = _ZERO_
               Qt(i) = _ZERO_
               Q(i) = _ZERO_
               FQ(i) = _ZERO_
            end do
         end if
      end if
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
      if (allocated(Q)) deallocate(Q)
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

!$Id: read_hypsography.F90,v 0.1 2011-04-19 16:24:48 schueler Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_hypsography
!
! !INTERFACE:
   subroutine read_hypsography(unit,ierr)
!
! !DESCRIPTION:
!  This routine is responsible for reading in values for the hypography from a
!  file specified in the meanflow namelist.
!
! !USES:
   use meanflow, only: N_input,depth_input,hypsography_input
   use meanflow, only: hypsography,hypsography_slope
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
!
!-----------------------------------------------------------------------
!BOC
   ierr = 0
   read(unit,*,ERR=100,END=110) N_input,up_down
   lines = 1

   if (allocated(depth_input)) deallocate(depth_input)
   allocate(depth_input(0:N_input+1),stat=rc)
   if (rc /= 0) stop 'read_hypsography: Error allocating memory (depth_input)'
   depth_input = _ZERO_
   if (allocated(hypsography_input)) deallocate(hypsography_input)
   allocate(hypsography_input(0:N_input+1),stat=rc)
   if (rc /= 0) then
      stop 'read_hypsography: Error allocating memory (hypsography_input)'
   end if
   hypsography_input = _ZERO_

   if(up_down .eq. 1) then
      do i=1,N_input
         lines = lines+1
         read(unit,*,ERR=100,END=110) depth_input(i), hypsography_input(i)
      end do
   else
      do i=N_input,1,-1
         lines = lines+1
         read(unit,*,ERR=100,END=110) depth_input(i), hypsography_input(i)
      end do
   end if

   return
!  maybe these parameters should be defined in meanflow.F90
!  like in observations.F90
!  READ_ERROR = -2
100 ierr = -2
   FATAL 'Error reading hypsography (READ_ERROR)'
   return
!  END_OF_FILE = -1
110 ierr = -1
   FATAL 'Error reading hypsography (END_OF_FILE)'
   return
   end subroutine read_hypsography
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_hypsography
!
! !INTERFACE:
   subroutine update_hypsography(nlev,z,h)
!
! !DESCRIPTION:
!  This routine is responsible for updating the hypsography every timestep.
!  This includes interpolating the hypsography to the current grid and
!  calculating the derivative dA(z)/dz.
!
! !USES:
   use meanflow, only: N_input,depth_input,hypsography_input
   use meanflow, only: hypsography,hypsography_slope
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in):: nlev
   REALTYPE, intent(in):: z(0:nlev)
   REALTYPE, intent(in):: h(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   integer                   :: rc
   REALTYPE,allocatable,dimension(:) :: prof
   REALTYPE                  :: zPrime(0:nlev+1)
!
!-----------------------------------------------------------------------
!BOC
!  z' defined on the interfaces, not as z on the grid centers
!  let it start at "1", because the gridinterpol-routine-loop starts at "1"
!  hypsography is defined at grid interfaces
   zPrime(nlev+1) = _ZERO_
   zPrime(nlev) = -h(nlev)
   do i = nlev-1, 1, -1
      zPrime(i) = zPrime(i+1) - h(i)
   end do

   if (allocated(prof)) deallocate(prof)
      allocate(prof(0:nlev+1),stat=rc)
   if (rc /= 0) stop 'read_hypsography: Error allocating memory (prof)'
      prof = _ZERO_

!  interpolate hypsography to grid used by GOTM
   call gridinterpol(N_input,1,depth_input,hypsography_input,nlev+1,z,prof)

   do i = 0, nlev
      hypsography(i) = prof(i+1)
   end do

   if (allocated(prof)) deallocate(prof)

!  calculate the derivative of the hypsography wrt z
!  both hypsography & hypsography_slope are defined at the grid interfaces
!  forward diff at bottom
   hypsography_slope(0) = (hypsography(1) - hypsography(0)) / h(1)
!  backward diff at surface
   hypsography_slope(nlev) = (hypsography(nlev) - hypsography(nlev-1)) / &
                        h(nlev)
!  the rest central diff
   do i = 1, nlev-1
      hypsography_slope(i) = (hypsography(i+1) - hypsography(i-1)) / &
                        (h(i+1) + h(i))
   end do

   end subroutine update_hypsography
!EOC
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

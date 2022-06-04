#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Apply density correction
!
! !INTERFACE:
   subroutine density_correction(nlev, rho_corr)
!
! !DESCRIPTION:
!
! !USES:
   use meanflow,   only: h, rho, buoy, NN
   use meanflow,   only: gravity, rho_0
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: nlev

!  density correction
   REALTYPE, intent(in)                :: rho_corr(1:nlev)

!
! !OUTPUT PARAMETERS:

!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
!EOP
!
! !LOCAL VARIABLES:
   integer                     :: i
   REALTYPE                    :: r2b, dz
   REALTYPE, allocatable, save :: buoy_corr(:)
   logical, save               :: first=.true.
!
!-----------------------------------------------------------------------
!BOC

   if (first) then
      allocate(buoy_corr(1:nlev))
      first = .false.
   end if

   r2b = -gravity / rho_0
   do i=1,nlev
      rho      (i) = rho(i) + rho_corr(i)
      buoy_corr(i) = rho_corr(i) * r2b
      buoy     (i) = buoy(i) + buoy_corr(i)
   end do
   do i=1,nlev-1
      dz    = _HALF_ * ( h(i) + h(i+1) )
      NN(i) = NN(i) + ( buoy_corr(i+1) - buoy_corr(i) ) / dz
   end do

   return
   end subroutine density_correction
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

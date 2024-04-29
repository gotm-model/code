#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Light within the water column
!
! !INTERFACE:
   subroutine light(nlev,I_0,rad,swr_abs)
!
! !DESCRIPTION:
!
! !USES:
   use observations, only: A_,g1_,g2_
   use meanflow,     only: h,bioshade

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)                 :: nlev

!  surface short waves radiation  (W/m^2)
   REALTYPE, intent(in)                :: I_0
!
! !OUTPUT PARAMETERS:
!  shortwave radiation at interfaces (W/m^2)
   REALTYPE                            :: rad(0:nlev)

!  shortwave absoption per layer (W/m^2)
   REALTYPE                            :: swr_abs(1:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer  :: i
   REALTYPE :: z
!
!-----------------------------------------------------------------------
!BOC
!
!  compute short wave radiation (two-band model; downwelling stream only)
   rad(nlev)  = I_0
   z          =_ZERO_
   do i=nlev-1,0,-1
      z=z+h(i+1)
      rad(i)=I_0*(A*exp(-z/g1_%value)+(1.-A_%value)*exp(-z/g2_%value)*bioshade(i+1))
   end do

!  light absorption per layer (W m-2) is the difference between shortwave at the top and bottom interface
   do i=1,nlev
      swr_abs(i) = rad(i) - rad(i-1)
   end do

   end subroutine light
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Wrapper for air-sea fluxes calculations \label{sec:airsea-fluxes}
!
! !INTERFACE:
   subroutine airsea_fluxes(method,rain_impact,calc_evaporation, &
                            sst,airt,u10,v10,precip,evap,taux,tauy,qe,qh)
!
! !DESCRIPTION:
!  A wrapper around the different methods for calculating momentum
!  fluxes and sensible and latent heat fluxes at the air-sea interface.
!  To have a complete air-sea exchange also the short wave radiation
!  and back-wave radiation must be calculated.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: method
   integer, intent(in)                 :: rain_impact,calc_evaporation
   REALTYPE, intent(in)                :: sst,airt,u10,v10,precip
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)             :: evap
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: taux,tauy,qe,qh
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (method)
      case (1) ! Kondo
         call kondo(rain_impact,calc_evaporation, &
                    sst,airt,u10,v10,precip,evap,taux,tauy,qe,qh)
      case (2) ! Fairall et. all
         call fairall(rain_impact,calc_evaporation, &
                      sst,airt,u10,v10,precip,evap,taux,tauy,qe,qh)
      case default
   end select

   return
   end subroutine airsea_fluxes
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------


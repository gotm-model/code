#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The total water balance \label{sec:water_balance}
!
! !INTERFACE:
   subroutine water_balance(nlev)
!
! !DESCRIPTION:
!
! !USES:
   use meanflow,      only: water_balance_method,WATER_BALANCE_NONE
   use meanflow,      only: WATER_BALANCE_SURFACE,WATER_BALANCE_ALLLAYERS
   use meanflow,      only: int_flows,net_water_balance,int_fwf
   use meanflow,      only: lake,Af,Ac,h
   use streams,       only: int_inflow,int_outflow
   use airsea,        only: int_net_precip,evap,precip
   use observations,  only: Q,Qres,FQ
!
   IMPLICIT NONE

! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer              :: k
   REALTYPE             :: Vc(0:nlev)
!
!-----------------------------------------------------------------------
!BOC

   if (lake) then
#if 0
      int_flows = int_inflow + int_outflow
#else
      int_flows = (int_inflow + int_outflow)/Af(size(Af)-1)
#endif
      int_fwf = int_flows + int_net_precip
   else
      int_fwf = int_net_precip
   end if

   if (lake) then
      net_water_balance = sum(Q(1:nlev)) + Af(nlev)*(evap + precip)
!KBSTDERR 'net ',net_water_balance
      select case (water_balance_method)
         case(WATER_BALANCE_SURFACE)
            Qres(nlev) = -net_water_balance
         case(WATER_BALANCE_ALLLAYERS)
            Vc = Ac * h
            Qres = -net_water_balance * Vc / sum(Vc(1:nlev))
      end select
      ! calculate the vertical flux terms
      do k=1,nlev-1
         FQ(k) = FQ(k-1) + Q(k) + Qres(k)
      end do
   end if

   return
   end subroutine water_balance
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

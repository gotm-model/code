#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The total water balance \label{sec:water_balance}
!
! !INTERFACE:
   subroutine water_balance()
!
! !DESCRIPTION:
!
! !USES:
   use meanflow,      only: int_flows,int_fwf,lake,Af
   use inflows,       only: int_inflow,int_outflow
   use airsea,        only: int_net_precip
!
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
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

   return
   end subroutine water_balance
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

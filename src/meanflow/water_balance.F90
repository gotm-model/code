#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The total water balance \label{sec:water_balance}
!
! !INTERFACE:
   subroutine water_balance(nlev,dt)
!
! !DESCRIPTION:
!
! !USES:
   use meanflow,      only: water_balance_method,WATER_BALANCE_NONE
   use meanflow,      only: WATER_BALANCE_SURFACE,WATER_BALANCE_ALLLAYERS
   use meanflow,      only: WATER_BALANCE_ZETA
   use meanflow,      only: int_flows,net_water_balance,int_fwf
   use meanflow,      only: int_water_balance
   use meanflow,      only: lake,Af,Vc
   use streams,       only: int_inflow,int_outflow
   use airsea,        only: evap,precip
   use observations,  only: Qlayer,Qres,zeta_method,zeta
   use hypsograph,    only: Vc2zi,zi2Vc
!
   IMPLICIT NONE

! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)                 :: nlev
!  timestep
   REALTYPE, intent(in)                :: dt
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE             :: zi1(0:1),Af1(0:1),Vc1(1)
   REALTYPE             :: sumVc,sumVcn
!
!-----------------------------------------------------------------------
!BOC

   if (lake) then
#if 0
      int_flows = int_inflow + int_outflow
#else
      int_flows = (int_inflow + int_outflow)/Af(size(Af)-1)
#endif
!STDERR Af
!STDERR size(Af),nlev
      int_fwf = int_fwf + int_flows

      sumVc = sum(Vc(1:nlev))
      net_water_balance = sum(Qlayer(1:nlev)) + Af(nlev)*(evap + precip)
      if (zeta_method.eq.1 .or. zeta_method.eq.2) then
!        zeta is already updated by observations
         zi1(1) = zeta
         call zi2Vc(1,zi1,Af1,Vc1)
         sumVcn = Vc1(1)
         net_water_balance = net_water_balance - (sumVcn-sumVc)/dt
      end if
!KBSTDERR 'net ',net_water_balance
      select case (water_balance_method)
         case(WATER_BALANCE_SURFACE)
            Qres(nlev) = -net_water_balance
         case(WATER_BALANCE_ALLLAYERS)
            Qres = -net_water_balance * Vc / sumVc
         case(WATER_BALANCE_ZETA)
            if (zeta_method .ne. 3) stop 'WATER_BALANCE_ZETA requires zeta_method=3'
            Vc1(1) = sumVc + dt*net_water_balance
            call Vc2zi(1,Vc1,zi1)
            zeta = zi1(1)
      end select
      int_water_balance = int_water_balance + dt*net_water_balance
   else
      net_water_balance = evap + precip
      int_water_balance = int_water_balance + dt*net_water_balance
   end if


   return
   end subroutine water_balance
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Heat and momemtum fluxes computed with constant transfert coefs
!
! !INTERFACE:
   subroutine constant_cd(sst,airt,u10,v10,precip,evap,taux,tauy,qe,qh)
!
! !DESCRIPTION:
!  Based on the model sea surface temperature, the wind vector
!  at 10 m height, the air pressure at 2 m, the dry air
!  temperature and the air pressure at 2 m, and the relative
!  humidity (either directly given or recalculated from the
!  wet bulb or the dew point temperature),
!  this routine the surface
!  momentum flux vector, $(\tau_x^s,\tau_y^s)$,
!  the latent heat flux, $Q_e$,
!  and the sensible heat flux, $Q_h$, according to the 
!  bulk formulae.
!
!  \begin{equation}
!  \begin{array}{rcl}
!  \tau_x^s &=& c_{dd} \rho_a W_x W \\ \\
!  \tau_y^s &=& c_{dd} \rho_a W_y W \\ \\
!  Q_e &=& c_{ed} L \rho_a W (q_s-q_a) \\ \\
!  Q_h &=& c_{hd} C_{pa} \rho_a W (T_w-T_a)
!  \end{array}
!  \end{equation}
!
!  with the air density $\rho_a$, the wind speed at 10 m, $W$,
!  the $x$- and the $y$-component of the wind velocity vector,
!  $W_x$ and $W_y$, respectively, the specific evaporation heat of sea water,
!  $L$, the specific saturation humidity, $q_s$, the actual
!  specific humidity $q_a$, the specific heat capacity of air at constant
!  pressure, $C_{pa}$, the sea surface temperature, $T_w$ and the
!  dry air temperature, $T_a$.
!
! !USES:
   use airsea_variables, only: kelvin,const06,rgas,rho_0
   use airsea_variables, only: qs,qa,rhoa
   use airsea_variables, only: cpa,cpw
   use airsea_variables, only: rain_impact,calc_evaporation
   use airsea_variables, only: const_cdd, const_ced, const_chd
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: sst,airt,u10,v10,precip
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)             :: evap
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: taux,tauy,qe,qh
!
! !REVISION HISTORY:
!  Original author(s): Bruno Deremble
!
! !LOCAL VARIABLES:
   REALTYPE                  :: w,L

   REALTYPE                  :: x1,x2,x3
   REALTYPE                  :: ta,ta_k,tw,tw_k
   REALTYPE                  :: cdd,chd,ced
   REALTYPE                  :: tmp,rainfall,cd_rain
   REALTYPE, parameter       :: eps=1.0e-12
!EOP
!-----------------------------------------------------------------------
!BOC
   w = sqrt(u10*u10+v10*v10)
   L = (2.5-0.00234*sst)*1.e6

   if (sst .lt. 100.) then
      tw  = sst
      tw_k= sst+kelvin
   else
      tw  = sst-kelvin
      tw_k= sst
   end if

   if (airt .lt. 100.) then
      ta_k  = airt + kelvin
      ta = airt
   else
      ta  = airt - kelvin
      ta_k = airt
   end if


   cdd = const_cdd
   chd = const_chd
   ced = const_ced


   qh=-chd*cpa*rhoa*w*(sst-airt)       ! sensible
   qe=-ced*L*rhoa*w*(qs-qa)            ! latent

!  compute sensible heatflux correction due to rain fall
   if (rain_impact) then
!     units of qs and qa - should be kg/kg
      rainfall=precip * 1000. ! (convert from m/s to kg/m2/s)
      x1 = 2.11e-5*(ta_k/kelvin)**1.94
      x2 = 0.02411*(1.0+ta*(3.309e-3-1.44e-6*ta))/(rhoa*cpa)
      x3 = qa * L /(rgas * ta_K * ta_K)
      cd_rain = 1.0/(1.0+const06*(x3*L*x1)/(cpa*x2))
      cd_rain = cd_rain*cpw*((tw-ta) + (qs-qa)*L/cpa)
      qh = qh - rainfall * cd_rain
   end if

!  calculation of evaporation/condensation in m/s
   if (rain_impact .and. calc_evaporation) then
!     ced from latent heatflux for moisture flux
      evap = rhoa/rho_0*ced*w*(qa-qs)
   else
      evap = _ZERO_
   end if

   tmp = cdd*rhoa*w
   taux = tmp*u10
   tauy = tmp*v10

!  Compute momentum flux (N/m2) due to rainfall (kg/m2/s).
!  according to Caldwell and Elliott (1971, JPO)
   if ( rain_impact ) then
      tmp  = 0.85d0 * rainfall
      taux  = taux + tmp * u10
      tauy  = tauy + tmp * v10
   end if

   return
   end subroutine constant_cd
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

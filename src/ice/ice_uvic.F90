#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: ice_uvic --- Flato thermodynamic ice model
! \label{sec:ice_uvic}
!
! !INTERFACE:
   module ice_uvic
!
! !DESCRIPTION:
!  UVic people please fill in here - kb
!
   implicit none
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public                              :: init_ice_uvic,do_ice_uvic
#if 0
   public                              :: ice_optics
#endif
!
! !PUBLIC DATA MEMBERS:
#if 0
   public :: DS, DI, CW, DW, LI, MU_TS, SI, TFI, KMELT
#endif
!
! !DEFINED PARAMETERS:
!  UVic people - please define all parameters below like example given - kb
#if 0
!  thermal conductivity of snow [W/(mK)]
   REALTYPE, parameter       :: KS=0.31
#endif
!
! !REVISION HISTORY:
!  Original author: Flato
!  Author(s): Nadja Steiner, Hakase ..., Eric Mortensen and Karsten Bolding
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the uvic--ice model \label{sec:init-uvic-ice}
!
! !INTERFACE:
   subroutine init_ice_uvic(namlst)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: namlst
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!
! !LOCAL VARIABLES:
#if 0
   namelist /uvic_ice/ nlayers
#endif
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL3 'initializing ...'

#if 0
!  Read namelist variables from file.
   open(namlst,file='uvic_ice.nml',action='read',status='old',err=90)
   read(namlst,nml=uvic_ice,err=91)
   close(namlst)
#endif

   return

#if 0
90 FATAL 'I could not open uvic_ice.nml'
   stop 'init_ice_uvic'
#endif

   end subroutine init_ice_uvic
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculate ice thermodynamics \label{sec:do_ice_uvic}
!
! !INTERFACE:
   subroutine do_ice_uvic(dt,h,julianday,secondsofday,lon,lat, &
                          I_0,airt,airp,rh,u10,v10,precip,cloud, &
                          T,S,rho,rho_0, &
                          back_radiation_method,hum_method,fluxes_method, &
                          hi,hs,t1,t2,ts,alb,heat,tmelt,bmelt)
!
! !DESCRIPTION:
!  UVic people please replace this description - kb
!  This subroutine updates the sea ice prognostic variables. The updated
!  variables are upper ice layer temperature (T1), lower ice layer temperature
!  (T2), snow thickness (hs), and ice thickness (hi).
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)      :: dt    ! timestep (sec)
   REALTYPE, intent(in)      :: h     ! sea surface layer thickness
   integer, intent(in)       :: julianday ! this julian day
   integer, intent(in)       :: secondsofday ! seconds for this day
   REALTYPE, intent(in)      :: lon   ! longitude for this point
   REALTYPE, intent(in)      :: lat   ! latitude for this point
   REALTYPE, intent(inout)   :: I_0   ! shortwave radiation at sea surface
   REALTYPE, intent(in)      :: airt  ! 2m temperature
   REALTYPE, intent(in)      :: airp  ! sea surface pressure
   REALTYPE, intent(in)      :: rh    ! relative humidity
   REALTYPE, intent(in)      :: u10   ! 10 m wind u-component
   REALTYPE, intent(in)      :: v10   ! 10 m wind v-component
   REALTYPE, intent(inout)   :: precip! freshwater precipitation (m/s)
   REALTYPE, intent(in)      :: cloud ! cloud cover
   REALTYPE, intent(inout)   :: T     ! sea surface temperature
   REALTYPE, intent(in)      :: S     ! sea surface salinity
   REALTYPE, intent(in)      :: rho   ! sea surface layer density
   REALTYPE, intent(in)      :: rho_0 ! reference density
   integer, intent(in)       :: back_radiation_method ! method for LW
   integer, intent(in)       :: hum_method ! method for humidity
   integer, intent(in)       :: fluxes_method ! method for fluxes
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)   :: hi    ! ice thickness (m)
   REALTYPE, intent(inout)   :: hs    ! snow thickness (m)
   REALTYPE, intent(inout)   :: t1    ! upper ice temperature (deg-C)
   REALTYPE, intent(inout)   :: t2    ! lower ice temperature (deg-C)
   REALTYPE, intent(inout)   :: ts    ! ice surface temperature (deg-C)
   REALTYPE, intent(inout)   :: alb   ! surface albedo - water or ice
   REALTYPE, intent(inout)   :: heat  ! surface heat flux
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)     :: tmelt ! accumulated top melting energy  (J/m^2)
   REALTYPE, intent(out)     :: bmelt ! accumulated bottom melting energy (J/m^2)
!
! !LOCAL VARIABLES:
!  Declare any local variables
#if 0
   REALTYPE        :: tfw   ! seawater freezing temperature (deg-C)
#endif
!
! !LOCAL PARAMETERS:
   REALTYPE        :: kelvin=273.16 ! absolute zero
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL0 'do_ice_uvic'
   STDERR T,S
!

#if 0
!     Recalculate surface fluxes for sea ice conditions
      call humidity(hum_method,rh,airp,ts,airt)
      call back_radiation(back_radiation_method, &
                          lat,ts+kelvin,airt+kelvin,cloud,qb)
      call airsea_fluxes(fluxes_method,.false.,.false., &
                         ts,airt,u10,v10,precip,evap,tx,ty,qe,qh)
      heat = (qb+qe+qh)
      STDERR 'heat ice =', heat
!
!     Calculate derivative of heat with respect to sea ice surface temp.
      dts = 0.01
      call humidity(hum_method,rh,airp,ts-dts,airt)
      call back_radiation(back_radiation_method, &
                          lat,ts+kelvin-dts,airt+kelvin,cloud,qbm)
      call airsea_fluxes(fluxes_method,.false.,.false., &
                         ts-dts,airt,u10,v10,precip,evap,tx,ty,qem,qhm)
      B = (-heat + (qbm+qem+qhm))/dts
      STDERR 'd(-heat)/d(ts) =', B
   endif
   A = -heat
   STDERR A, B
#endif
!
#if 0
   call do_thermodynamics(A,B,I,tfw,fb,precip,dt,hs,hi,t1,t2,evap,tmelt, &
                                bmelt,ts)
#endif

end subroutine do_ice_uvic

! put any internal subroutines below here
#if 0
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculate ice thermodynamics \label{sec:do_ice_uvic}
!
! !INTERFACE:
   subroutine do_thermodynamics(A,B,I,tfw,fb,precip,dt,hs,hi,t1,t2,evap,tmelt, &
                                bmelt,ts)
!
! !DESCRIPTION:
!  This subroutine updates the sea ice prognostic variables. The updated
!  variables are upper ice layer temperature (T1), lower ice layer temperature
!  (T2), snow thickness (hs), and ice thickness (hi).
!
!  The ice model performs this in two steps. First the temperatures are updated
!  and secondly the changes in ice and snow thickness are calculated.
!
!  Any surplus energy that is not used for melting is returned in tmelt and
!  bmelt.
!
!  Evaporation and bottom ablation formation are not included in
!  this version of the model. Furthermore we do not keep an explicit water
!  and salt budget for the sea ice and how that affects the water and salt
!  budgets in the ocean.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)      :: A     ! net surface heat flux (+ up) at ts=0 (W/m^2)
   REALTYPE, intent(in)      :: B     ! d(sfc heat flux)/d(ts) [W/(m^2 deg-C)]
   REALTYPE, intent(in)      :: I     ! solar absorbed by upper ice (W/m^2)
   REALTYPE, intent(in)      :: tfw   ! seawater freezing temperature (deg-C)
   REALTYPE, intent(in)      :: fb    ! heat flux from ocean to ice bottom (W/m^2)
   REALTYPE, intent(in)      :: precip! freshwater precipitatin (m/s)
   REALTYPE, intent(in)      :: dt    ! timestep (sec)
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)   :: hs    ! snow thickness (m)
   REALTYPE, intent(inout)   :: hi    ! ice thickness (m)
   REALTYPE, intent(inout)   :: t1    ! upper ice temperature (deg-C)
   REALTYPE, intent(inout)   :: t2    ! lower ice temperature (deg-C)
   REALTYPE, intent(inout)   :: evap  ! evaporation of ice (m/s)
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)     :: tmelt ! accumulated top melting energy  (J/m^2)
   REALTYPE, intent(out)     :: bmelt ! accumulated bottom melting energy (J/m^2)
   REALTYPE, intent(out)     :: ts    ! surface temperature (deg-C)
!
! !LOCAL VARIABLES:
   REALTYPE        :: tsf
   REALTYPE        :: K12
   REALTYPE        :: hi2, hie
   REALTYPE        :: A10, B10, A1, B1, C1
   REALTYPE        :: h1, h2
   REALTYPE        :: dh
   REALTYPE        :: f1
   REALTYPE        :: hw
   REALTYPE        :: snow
   REALTYPE        :: snow_to_ice
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL0 'do_thermodynamics'
!
!  initialize surface temperature to zero (just to avoid strange output)
   ts = _ZERO_
!
!  initialize accumulated top and bottom melting energies
   tmelt = _ZERO_
   bmelt = _ZERO_
!
!  prevent thin ice inaccuracy (mw)
   hie = max(hi, H_LO_LIM);
!
!  temperature update is only performed when there is ice
   if (hi > _ZERO_) then
      STDERR 'do_thermodynamics: sea ice is present:', hi
!
!     set surface temperature to snow temperature or seawater freezing temp.
!     TODO: refactor into a sub called update_t1_and_ts(hs, hie, dt, t1, t2)
      if (hs > _ZERO_) then
         tsf = _ZERO_
      else
         tsf = TFI
      endif
!
!     compute upper ice and surface temperatures
      K12 = 4*KI*KS/(KS+4*KI*hs/hie)
      hi2 = hie*hie
!
      A10 = DI*hi2*CI/(2*dt) + 2*KI*(4*dt*2*KI+DI*hi2*CI)/(6*dt*2*KI+DI*hi2*CI)
      B10 = -DI*hi2*(CI*t1+LI*TFI/t1)/(2*dt) - I*hie                       &
            -2*KI*(4*dt*2*KI*tfw+DI*hi2*CI*t2)/(6*dt*2*KI+DI*hi2*CI)
!
      A1 = A10+K12*B*hie/(K12+B*hie)
      B1 = B10+A*K12*hie/(K12+B*hie)
      C1 = DI*hi2*LI*TFI/(2*dt)
      t1 = -(sqrt(B1*B1-4*A1*C1)+B1)/(2*A1)
      ts = (K12*t1-A*hie)/(K12+B*hie)
!
!     check if the snow (if present) or upper layer ice is melting
!     if this is the case the temperatures are recalculted using modified
!     A1 and B1 coefficients (see eqs. (19) - (20))
      if (ts > tsf) then
         A1 = A10+K12
         B1 = B10-K12*tsf
         t1 = -(sqrt(B1*B1-4*A1*C1)+B1)/(2*A1)
         ts = tsf
!        we will now save the surplus energy which we will use below for
!        melting ice from above, see also eq. (22)
         tmelt = tmelt + (K12*(t1-ts)/hi-(A+B*ts))*dt
      endif
!
!     update lower ice temperature we perform the update using temperature
!     deviations from tfw for better thin ice precision
!     TODO: refactor into a sub called update_t2(hs, hie, dt, t1, t2)
!     convert to temperature deviations for better thin ice precision
      t1 = t1-tfw;
      t2 = t2-tfw;
!     perform lower ice temperature update, see eq. (15)
      t2 = (2*dt*2*KI*t1+DI*hi2*CI*t2)/(6*dt*2*KI+DI*hi2*CI)
!     convert to real temperatures again
      t1 = t1+tfw;
      t2 = t2+tfw;
!
!     calculate energy flux for bottom melting or freezing according to
!     eq. (23). The oceanic heat flux to the ice bottom is expected
!     to be calculated by the ocean model and passed in as fb
      bmelt = bmelt + (fb+4*KI*(t2-tfw)/hie)*dt
!
!     the temperature update can lead to a situation where the sea ice
!     temperatures end above the freezing temperature. In this case use
!     the excess energy to melt ice
!     put excess lower ice energy into bmelt
      if (t2 > TFI) then
         bmelt = bmelt + melt_energy(h2=hie/2,t2=TFI) - melt_energy(h2=hie/2,t2=t2)
         t2 = TFI
      endif
!
!     put excess upper ice energy into tmelt
      if (t1 > TFI) then
         tmelt = tmelt + melt_energy(h1=hie/2,t1=TFI) - melt_energy(h1=hie/2,t1=t1)
         t1 = TFI
      endif
!
!     temperature update complete - check consistency
!KB      call ice_consistency(ts, hs, hi, t1, t2, bmelt, tmelt)
   endif
!
!  update snow and sea ice thicknesses and accompanying temperature updates
   h1 = hi/2
   h2 = h1
!
!  calculate snow rate [m/s] and add it to snow height
   snow = precip*DFW/DS
   hs = hs + snow*dt
!
!  apply freezing
   if (hi <= _ZERO_ .and. fb < _ZERO_) then
      LEVEL0 'ice_uvic: frazil ice formation', fb, bmelt
      bmelt = bmelt + fb*dt
      t1 = tfw
      t2 = tfw
   endif
!
!  Apply freezing
   if (bmelt < _ZERO_) then
      call add_to_bot(-bmelt/melt_energy(h2=_ONE_,t2=tfw), &
                     & tfw, h2, t2)
      bmelt = _ZERO_
   endif
!
!  apply atmospheric evaporation
   if (evap <= hs*DS) then
      hs = hs - evap/DS
   else if (evap-hs*DS<=h1*DI) then
      hs = _ZERO_
      h1 = h1 - (evap-DS*hs)/DI
   else if (evap-hs*DS-h1*DI<=h2*DI) then
      hs = _ZERO_
      h1 = _ZERO_
      h2 = h2 - (evap-hs*DS-h1*DI)/DI
   else
      hs = _ZERO_
      h1 = _ZERO_
      h2 = _ZERO_
   end if
!
!  in sea ice the temperatures (t1 and t2) are always < 0. But in the case
!  where we have no sea ice (h1 == 0) we still divide by t1. We therefore
!  set it to the sea water freezing temperature in this case
   if (h1 == _ZERO_) t1 = tfw
!
!  apply energy fluxes at top
!  TODO: refactor into a sub called apply_surface_flux(hs, h1, h2, tmelt, t1, t2)
   if (tmelt <= melt_energy(hs=hs)) then
!     only melting snow layer
      hs = hs - tmelt/melt_energy(hs=_ONE_)
      tmelt = _ZERO_
   else if (tmelt <= melt_energy(hs,h1,t1)) then
!     melting snow layer and part of top ice layer
      h1 = h1 - (tmelt-melt_energy(hs))/melt_energy(h1=_ONE_,t1=t1)
      hs = _ZERO_
      tmelt = _ZERO_
   else if (tmelt <= melt_energy(hs,h1,t1,h2,t2)) then
!     melting snow layer, top ice layer and part of bottom ice layer
      h2 = h2 - (tmelt - melt_energy(hs,h1,t1))/melt_energy(h2=_ONE_,t2=t2)
      hs = _ZERO_
      h1 = _ZERO_
      tmelt = _ZERO_
   else
!     melting all layers
      hs = _ZERO_
      h1 = _ZERO_
      h2 = _ZERO_
      tmelt = tmelt - melt_energy(hs,h1,t1,h2,t2)
   endif
!
!  apply energy fluxes at bottom
!  TODO: refactor into a sub called apply_bottom_flux(hs, h1, h2, tmelt, t1, t2)
   if (bmelt > _ZERO_) then
      if (bmelt < melt_energy(h2=h2,t2=t2)) then
!        only melting part of bottom ice layer
         h2 = h2 - bmelt/melt_energy(h2=_ONE_,t2=t2)
         bmelt = _ZERO_
      else if (bmelt < melt_energy(h1=h1,t1=t1,h2=h2,t2=t2)) then
!        melting bottom ice layer and part of top ice layer
         h1 = h1-(bmelt-melt_energy(h2=h2,t2=t2))/melt_energy(h1=_ONE_,t1=t1)
         h2 = _ZERO_
         bmelt = _ZERO_
      else if (bmelt < melt_energy(hs,h1,t1,h2,t2)) then
!        melting bottom and top ice layers and part of snow layer
         hs = hs - (bmelt-melt_energy(h1=h1,t1=t1,h2=h2,t2=t2)) &
            & / melt_energy(hs=_ONE_)
         h1 = _ZERO_
         h2 = _ZERO_
         bmelt = _ZERO_
      else
!        melting all layers
         hs = _ZERO_
         h1 = _ZERO_
         h2 = _ZERO_
         bmelt = bmelt - melt_energy(hs,h1,t1,h2,t2)
      endif
   endif
!
!  calculate updated sea ice thickness
   hi = h1 + h2
!
!  determine the water line by taking the mass (per unit square)
!  of the snow and ice and dividing it by the density of seawater.
   hw = (DI*hi+DS*hs)/DW
!
!  convert snow to ice to maintain ice at waterline
   if (hw > hi) then
      snow_to_ice = (hw-hi)*DI
      hs = hs - snow_to_ice/DS
!     the snow is added to the top ice layer preserving enthalpy
!     t1 is therefore also changed during the consersion, see eq. (38).
      call add_to_top(hw-hi, TFI, h1, t1)
   endif
!
!  Even up layer thicknesses and t2 according to eq. (40)
   call even_up(h1, t1, h2, t2)
   hi = h1 + h2
!
!  FIXME: Karsten, is this comparison really safe? I would have used:
!  if (hi <= _ZERO_) then ! and possibly added hi = _ZERO_ in the if block
   if (hi == _ZERO_) then
      t1 = _ZERO_
      t2 = _ZERO_
   endif
!
!  postconditions
!KB   call ice_consistency(ts, hs, hi, t1, t2, bmelt, tmelt)

   LEVEL0 'end do_thermodynamics'
   return
   end subroutine do_thermodynamics
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: ice_consistency
! !INTERFACE:
   subroutine ice_consistency(ts, hs, hi, t1, t2, bmelt, tmelt)
!
! !DESCRIPTION:
!  ice_consistency - checks that the sea ice fields look ok
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in) :: ts ! surface temperature
   REALTYPE, intent(in) :: hs ! snow thickness
   REALTYPE, intent(in) :: hi ! ice layer thickness (m)
   REALTYPE, intent(in) :: t1 ! Upper ice layer temperature (degC)
   REALTYPE, intent(in) :: t2 ! Lower ice layer temperature (degC)
   REALTYPE, intent(in) :: bmelt ! accumulated bottom melting energy (J/m^2)
   REALTYPE, intent(in) :: tmelt ! accumulated top melting energy  (J/m^2)
!EOP
!-----------------------------------------------------------------------
!BOC
   if (ts>_ZERO_ .or. t1>TFI .or. t2>_ZERO_ .or. hs<_ZERO_ .or. hs>100.0 &
     & .or. hi<_ZERO_ .or. hi>100.0 .or. abs(bmelt)>100.0*DI*LI &
     & .or. tmelt<_ZERO_ .or. tmelt>100.0*DI*LI) then
      FATAL 'UNREASONABLE ICE: hs=',hs,'hi=',hi,'t1=',t1,'t2=',t2,'ts=', &
     &        ts,'tmelt=',tmelt,'bmelt=',bmelt
      stop 'ice_consistency'
   end if
end subroutine ice_consistency
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: melting energy
!
! !INTERFACE:
   REALTYPE function melt_energy(hs, h1, t1, h2, t2)
!
! !DESCRIPTION:
!  melt_energy - energy needed to entirely melt a given snow/ice configuration
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, optional, intent(in) :: hs ! snow thickness
   REALTYPE, optional, intent(in) :: h1 ! Upper ice layer thickness (m)
   REALTYPE, optional, intent(in) :: t1 ! Upper ice layer temperature (degC)
   REALTYPE, optional, intent(in) :: h2 ! Lower ice layer thickness (m)
   REALTYPE, optional, intent(in) :: t2 ! Lower ice layer temperature (degC)
!   REALTYPE :: melt_energy ! Amount of melt energy needed (J)
!EOP
!-----------------------------------------------------------------------
!BOC
   melt_energy = _ZERO_
!  energy needed for melting snow layer
   if (present(hs)) melt_energy = melt_energy+DS*LI*hs
!  energy needed for melting upper ice layer
   if (present(h1).and.present(t1)) then
      melt_energy = melt_energy+DI*h1*(CI-LI/t1)*(TFI-t1)
   endif
!  energy needed for melting lower ice layer
   if (present(h2).and.present(t2)) then
      melt_energy = melt_energy+DI*h2*(LI+CI*(TFI-t2))
   endif
   return
   end function melt_energy
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate albedo, penetrating solar and transmissivity
!
! !INTERFACE
   subroutine ice_optics(alb, pen, trn, hs, hi, ts, tfw)
!
! !DESCRIPTION:
!  ice_optics - set albedo, penetrating solar, and ice/snow transmissivity
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)      :: hs  ! snow thickness (m-snow)
   REALTYPE, intent(in)      :: hi  ! ice thickness (m-ice)
   REALTYPE, intent(in)      :: ts  ! surface temperature
   REALTYPE, intent(in)      :: tfw ! seawater freezing temperature
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)     :: alb ! ice surface albedo (0-1)
   REALTYPE, intent(out)     :: pen ! fraction of down solar penetrating the ice
   REALTYPE, intent(out)     :: trn ! ratio of down solar at bottom to top of ice
!
! !LOCAL VARIABLES:
   REALTYPE        :: as, ai, cs, fh
!EOP
!-----------------------------------------------------------------------
!BOC
   as = ALB_SNO
   ai = ALB_ICE
!
!  determine how large a fraction of the sea ice is covered by snow
   cs = hs/(hs+0.02)
!
!  reduce albedo for thin ice using the same form as in
!  Community Sea Ice Model (CSIM4)
   fh = min(atan(5.0*hi)/atan(5.0*0.5),1.0)
!
!  reduce albedo for melting as in CSIM4 assuming 0.53/0.47 vis/ir
   if (ts+T_RANGE_MELT > TFI) then
      as = as-0.1235*min((ts+T_RANGE_MELT-TFI)/T_RANGE_MELT,1.0)
      ai = ai-0.075 *min((ts+T_RANGE_MELT-TFI)/T_RANGE_MELT,1.0)
   endif
!  reduce albedo for thin ice
   ai = fh*ai+(1-fh)*0.06
!
!  calculate output values
   alb = cs*as+(1-cs)*ai
   pen = (1-cs)*PEN_ICE
   trn = exp(-hi/OPT_DEP_ICE);
   return
   end subroutine ice_optics
!EOC

!------------------------------------------------------------------------------!
!BOP
!
! !IROUTINE: Add sea ice to top layer
!
! !INTERFACE:
   subroutine add_to_top(h, t, h1, t1)
   IMPLICIT NONE
!
! !DESCRIPTION:
! This subroutine adds sea ice to the top layer. It calculates the new
! temperature of the top layer according to eq. (30) in:
!
! Michael Winton (2001): FMS Sea Ice Simulator
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in   ) :: h ! amount to add to top ice layer
   REALTYPE, intent(in   ) :: t ! temperature of added ice
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout) :: h1 ! top layer thickness
   REALTYPE, intent(inout) :: t1 ! top layer temperature
!
! !LOCAL VARIABLES:
   REALTYPE        :: f1
!EOP
!-----------------------------------------------------------------------
   f1 = h1/(h1+h)
   t1 = f1*(t1+LI*TFI/(CI*t1))+(1-f1)*t
   t1 = (t1-sqrt(t1*t1-4*TFI*LI/CI))/2
   h1 = h1+h
   return
   end subroutine add_to_top
!EOC

!------------------------------------------------------------------------------!
!BOP
!
! !IROUTINE: Add sea ice to bottom layer
!
! !INTERFACE:
   subroutine add_to_bot(h, t, h2, t2)
!
! !DESCRIPTION:
! This subroutine adds sea ice to the bottom layer. It calculates the new
! temperature of the top layer according to eq. (32) in:
!
! Michael Winton (2001): FMS Sea Ice Simulator
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)      :: h ! amount to add to bottom ice layer
   REALTYPE, intent(in)      :: t ! temperature of added ice
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)   :: h2 ! bottom layer thickness
   REALTYPE, intent(inout)   :: t2 ! bottom layer temperature
!EOP
!------------------------------------------------------------------------------!
   t2 = (h2*t2+h*t)/(h2+h)
   h2 = h2+h
   return
   end subroutine add_to_bot
!EOC

!------------------------------------------------------------------------------!
!BOP
!
! !IROUTINE: Even up the ice layers
!
! !INTERFACE:
   subroutine even_up(h1, t1, h2, t2)
!
! !DESCRIPTION:
! The sea ice model uses two ice layers internally. But they are assumed to have
! the same thickness allowing us to represent them with a single prognostic
! variable. This subroutine transfers mass and energy from the thicker layer to
! the thinner layer to maintain equal thickness.
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)   :: h1, t1, h2, t2
!
! !LOCAL VARIABLES:
   REALTYPE        :: dh
!EOP
!------------------------------------------------------------------------------!
!BOC
   if (h1 > (h1+h2)/2) then
      call add_to_bot(h1-(h1+h2)/2, t1+LI*TFI/(CI*t1), h2, t2)
      h1 = h2
   else if (h2 > (h1+h2)/2) then
      call add_to_top(h2-(h1+h2)/2, t2, h1, t1)
      h2 = h1
   endif
   if (t2>TFI) then
      ! use extra energy to melt both layers evenly
      dh = h2*CI*(t2-TFI)*t1/(LI*t1+(CI*t1-LI)*(TFI-t1))
      t2 = TFI
      h1 = h1-dh
      h2 = h2-dh
   endif
   return
   end subroutine even_up
!EOC

#endif
! any internal subroutines
!
!-----------------------------------------------------------------------
!
   end module ice_uvic
!
!-----------------------------------------------------------------------
! Copyright by the GETM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: ice_winton --- Winton thermodynamic ice model 
! \label{sec:ice_winton}
!
! !INTERFACE:
   module ice_winton
!
! !DESCRIPTION:
!  The model consists of a zero heat capacity snow layer overlying two equally 
!  thick sea ice layers. The upper ice layer has a variable heat capacity to 
!  represent brine pockets. The lower ice layer has a fixed heat capacity.
!  The prognostic variables are hs (snow layer thickness), hi (ice layer 
!  thickness), T1 and T2, the upper and lower ice layer temperatures located 
!  at the midpoints of the layers. The ice model performs two functions, the 
!  first is to calculate the ice temperature and the second is to calculate 
!  changes in the thickness of ice and snow.
!
!------------------------------------------------------------------------------!
!                                                                              !
!                       THREE-LAYER VERTICAL THERMODYNAMICS                    !
!                                                                              !
! Reference:  M. Winton, 2000: "A reformulated three-layer sea ice model",     !
!            Journal of Atmospheric and Oceanic Technology, 17, 525-531.       !
!                                                                              !
!                                                                              !
!        -> +---------+ <- ts - diagnostic surface temperature ( <= 0C )       !
!       /   |         |                                                        !
!     hs    |  snow   | <- 0-heat capacity snow layer                          !
!       \   |         |                                                        !
!        => +---------+                                                        !
!       /   |         |                                                        !
!      /    |         | <- t1 - upper 1/2 ice temperature; this layer has      !
!     /     |         |         a variable (T/S dependent) heat capacity       !
!   hi      |...ice...|                                                        !
!     \     |         |                                                        !
!      \    |         | <- t2 - lower 1/2 ice temp. (fixed heat capacity)      !
!       \   |         |                                                        !
!        -> +---------+ <- base of ice fixed at seawater freezing temp.        !
!                                                                              !
!                                                     Mike Winton (mw@gfdl.gov)!
!------------------------------------------------------------------------------!
!  Note: in this implementation the equations are multiplied by hi to improve 
!  thin ice accuracy
!
   implicit none
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
!   public                              :: init_ice_winton
   public                              :: do_ice_winton
!   public                              :: ice_size
!   public                              :: melt_energy
!   public                              :: ice_optics
!
! !PUBLIC DATA MEMBERS:
   public :: DS, DI, CW, DW, LI, MU_TS, SI, TFI
!
! !DEFINED PARAMETERS:
!  properties of ice, snow, and seawater (NCAR CSM values)
!  thermal conductivity of snow [W/(mK)]
   REALTYPE, parameter       :: KS=0.31      
!  density of snow [kg/(m^3)]
   REALTYPE, parameter       :: DS=330.0     
!  thermal conductivity of ice [W/(mK)]
!  a smaller value should be more appropriate
   REALTYPE, parameter       :: KI=2.03      
!  density of ice [kg/(m^3)]
   REALTYPE, parameter       :: DI=905.0     
!  heat cap. of fresh ice [J/(kg K)]
   REALTYPE, parameter       :: CI=2100.0    
!  salinity of sea ice
   REALTYPE, parameter       :: SI=1.0       
!  relates freezing temp. to salinity
   REALTYPE, parameter       :: MU_TS=0.0545    
!  sea ice freezing temp. = -mu*salinity
   REALTYPE, parameter       :: TFI=-MU_TS*SI 
!  heat capacity of seawater?
   REALTYPE, parameter       :: CW=4.2e3     
!  density of water for waterline [kg/(m^3)]
   REALTYPE, parameter       :: DW=1025.0    
!  latent heat of fusion [J/(kg-ice)]
   REALTYPE, parameter       :: LI=334e3     
!  albedos are from CSIM4 assumming 0.53 visible and 0.47 near-ir insolation
!  albedo of snow (not melting)
   REALTYPE                  :: ALB_SNO=0.85       
!  albedo of ice (not melting)
   REALTYPE                  :: ALB_ICE=0.5826     
!  ice surface penetrating solar fraction
   REALTYPE                  :: PEN_ICE=0.3        
!  ice optical depth [m]
   REALTYPE                  :: OPT_DEP_ICE=0.67   
!  ice optical extinction [1/m]
   REALTYPE                  :: OPT_EXT_ICE=1.5    
!  snow optical extinction  [1/m]
   REALTYPE                  :: OPT_EXT_SNOW=15.0  
!
! !REVISION HISTORY:
!  Original author(s): Adolf Stips, Jesper Larsen and Karsten Bolding
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculate ICE thermodynamics \label{sec:do_ice_winton}
!
! !INTERFACE:
#define _STUB_
#ifdef _STUB_
   subroutine do_ice_winton()
#else
   subroutine do_ice_winton(hs, hi, t1, t2, ts, A, B, I, tfw, fb, dt, tmelt, bmelt)
#endif
!
! !DESCRIPTION:
!  This subroutine updates the sea ice prognostic variables. The updated
!  variables are upper ice layer temperature (T1), lower ice layer temperature
!  (T2), snow thickness (hs), and ice thickness (hi).
!
!  The ice model performs this in two steps. First the temperatures are updated
!  and secondly the changes in ice and snow thickness are calculated.
!
! !USES:
   IMPLICIT NONE
!
#ifndef _STUB_
! !INPUT PARAMETERS:
   REALTYPE, intent(in)      :: A     ! net surface heat flux (+ up) at ts=0 (W/m^2)
   REALTYPE, intent(in)      :: B     ! d(sfc heat flux)/d(ts) [W/(m^2 deg-C)]
   REALTYPE, intent(in)      :: I     ! solar absorbed by upper ice (W/m^2)
   REALTYPE, intent(in)      :: tfw   ! seawater freezing temperature (deg-C)
   REALTYPE, intent(in)      :: fb    ! heat flux from ocean to ice bottom (W/m^2)
   REALTYPE, intent(in)      :: dt    ! timestep (sec)
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)   :: hs    ! snow thickness (m)
   REALTYPE, intent(inout)   :: hi    ! ice thickness (m)
   REALTYPE, intent(inout)   :: t1    ! upper ice temperature (deg-C)
   REALTYPE, intent(inout)   :: t2    ! lower ice temperature (deg-C)
   REALTYPE, intent(inout)   :: tmelt ! accumulated top melting energy  (J/m^2)
   REALTYPE, intent(inout)   :: bmelt ! accumulated bottom melting energy (J/m^2)
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)     :: ts    ! surface temperature (deg-C)
#endif
!
! !REVISION HISTORY:
!  Original author(s): Adolf Stips, Jesper Larsen and Karsten Bolding
!
! !LOCAL VARIABLES:
   REALTYPE        :: TSF
   REALTYPE        :: K12
   REALTYPE        :: hi2, hie
   REALTYPE        :: A10, B10, A1, B1, C1
   REALTYPE        :: h1, h2
   REALTYPE        :: dh
   REALTYPE        :: f1
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef _STUB_
   LEVEL0 'do_ice_winton'
#else
   if (hs > _ZERO_) then
      TSF = _ZERO_
   else
      TSF = TFI
   endif
!
!  Compute upper ice and surface temperatures
!  hie = max(hi, H_LO_LIM); ! prevent thin ice inaccuracy (mw)
   hie = max(hi, 0.01); ! prevent thin ice inaccuracy (mw)

   write(*,*) KI, KS, hs, hi
   K12 = 4*KI*KS/(KS+4*KI*hs/hie)
   hi2 = hie*hie

   write(*,*) t1, dt, KI, DI, hi2, CI
   A10 = DI*hi2*CI/(2*dt) + 2*KI*(4*dt*2*KI+DI*hi2*CI)/(6*dt*2*KI+DI*hi2*CI)
   B10 = -DI*hi2*(CI*t1+LI*TFI/t1)/(2*dt) - I*hi                       &
         -2*KI*(4*dt*2*KI*tfw+DI*hi2*CI*t2)/(6*dt*2*KI+DI*hi2*CI)

   A1 = A10+K12*B*hi/(K12+B*hi)
   B1 = B10+A*K12*hi/(K12+B*hi)
   C1  = DI*hi2*LI*TFI/(2*dt)
   t1 = -(sqrt(B1*B1-4*A1*C1)+B1)/(2*A1)
   ts = (K12*t1-A*hi)/(K12+B*hi)
# if defined debug_ice
   write(stdout,*) 'k12,a10,b10,a1,b1,c1',k12,a10,b10,a1,b1,c1,B1*B1-4*A1*C1
# endif
   if (ts > tsf) then       ! slightly different equation for melting conditions
      A1 = A10+K12
      B1 = B10-K12*tsf
      t1 = -(sqrt(B1*B1-4*A1*C1)+B1)/(2*A1)
      ts = tsf
      tmelt = tmelt + (K12*(t1-ts)/hi-(A+B*ts))*dt
   endif
!
!  set lower ice temp. -- use tfw as reference for thin ice precision
# if defined debug_ice
   write(stdout,*) 'hs,hi,ts,t1,t2,tmelt',hs,hi,ts,t1,t2,tmelt
# endif
   t1 = t1-tfw; 
   t2 = t2-tfw;
   t2 = (2*dt*2*KI*t1+DI*hi2*CI*t2)/(6*dt*2*KI+DI*hi2*CI)
   t1 = t1+tfw; 
   t2 = t2+tfw;

   bmelt = bmelt + (fb+4*KI*(t2-tfw)/hi)*dt
# if defined debug_ice
   write(stdout,*) 'bmelt,ki,t2,tfw,hi,dt',bmelt,ki,t2,tfw,hi,dt
# endif

   if (tmelt<0) then
      print *,'neg. tmelt=',tmelt,ts,t1,hs,hi,K12*(t1-ts)/hi,-(A+B*ts)
      print *,'K12=',K12
      print *,'A/B/I=',A,B,I
      print *,'A/B/C=',A1,B1,C1,A1*t1*t1+B1*t1+C1,A1*t1*t1,B1*t1,C1
   end if
!
!  resizing the ice immediately AS
   h1=hi/2
   h2=h1
!  top first
   if ( tmelt .le. hs*DS*LI) then
      hs = hs - tmelt/(DS*LI)
   else
     ! FIXME: Check parantheses (jla)
     h1 = h1 - (tmelt-hs*DS*LI)/(DI*(CI-LI/t1)*(TFI-t1))
   endif
!  bottom next
   if (bmelt .lt. _ZERO_) then
      dh = -bmelt/(DI*(LI+CI*(TFI-TFW)))
      t2 = (h2*t2+dh*TFW)/(h2+dh)
      h2 =  h2 +dh
   else
      h2=h2-bmelt/(DI*(LI+CI*(TFI-t2)))
   endif
   hi = h1 + h2
!  if ice remains, even up 2 layers, else pass negative energy back in snow
   if ( hi .gt. _ZERO_) then 
      if (h1 .gt. hi*0.5) then
         f1 = 1.0 - 2.0*h2/hi
         t2 = f1*(t1+LI*TFI/(CI*t1))+(1.0-f1)*t2
      else
         f1 = 2.0 * h1/hi
         t1 = f1*(t1+LI*TFI/(CI*t1))+(1.0-f1)*t2
         t1 = 0.5*(t1 - sqrt(t1*t1-4.0*TFI*LI/CI))
      endif
   else
      hs = hs + (h1*(CI*(t1-TFI)-LI*(1.0-TFI/t1)) \
          +h2*(CI*(t2-TFI)-LI))/LI
      hi = _ZERO_
      t1 = TFW
      t2 = TFW
   endif
#endif
   return
   end subroutine do_ice_winton
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
!  Energy needed for melting snow layer
   if (present(hs)) melt_energy = melt_energy+DS*LI*hs
!  Energy needed for melting upper ice layer
   if (present(h1).and.present(t1)) then
      melt_energy = melt_energy+DI*h1*(CI-LI/t1)*(TFI-t1)
   endif
!  Energy needed for melting lower ice layer
   if (present(h2).and.present(t2)) then
      melt_energy = melt_energy+DI*h2*(LI+CI*(TFI-t2))
   endif
   return
   end function melt_energy
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: ice optics
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
   REALTYPE        :: as, ai, cs
!EOP
!-----------------------------------------------------------------------
!BOC
   as = ALB_SNO
   ai = ALB_ICE
   cs = hs/(hs+0.04)                        ! thin snow partially covers ice
   if (hi < 0.5) ai = 0.06+(ai-0.06)*hi/0.5 ! reduce albedo for thin ice
   if (ts+5.0 > TFI) then                   ! reduce albedo for melting as in
      as = as-0.0247*max(ts+5-TFI,5.0)      ! CSIM4 assuming 0.53/0.47 vis/ir
      ai = ai-0.015 *max(ts+5-TFI,5.0)
   endif
   alb = cs*as+(1-cs)*ai
!  allow short wave penetration through snow (RCO, Sahlberg 1988)
!  pen = PEN_ICE; if (hs>0.0) pen = 0.0
!  trn = exp(-hi/OPT_DEP_ICE);
   pen = PEN_ICE;
   trn = exp(-hs*OPT_EXT_SNOW)*exp(-hi*OPT_EXT_ICE);

   return
   end subroutine ice_optics
!EOC

!------------------------------------------------------------------------------!
!BOP
!
! !IROUTINE: add to top (?)
!
! !INTERFACE:
   subroutine add_to_top(h, t, h1, t1)
   IMPLICIT NONE
!
! !DESCRIPTION:
!  add_to_top - add some ice to the top ice layer                               !
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
! TODO: Insert rationale and reference for this calculation
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
! !IROUTINE: add to bottom (?)
! !INTERFACE:
   subroutine add_to_bot(h, t, h2, t2)
!
! !DESCRIPTION:
!  add_to_bot - add some ice to the bottom ice layer                            !
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
! !IROUTINE: even up
!
! !INTERFACE:
   subroutine even_up(h1, t1, h2, t2)
!
! !DESCRIPTION:
!  even_up - transfer mass/energy between ice layers to maintain equal thickness!
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

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ice_size - ice & snow thickness change 
!
!
! !INTERFACE:
   subroutine ice_size(hs, hi, t1, t2, snow, frazil, evap, tmelt, bmelt, &
                       tfw, heat_to_ocn, wat_to_ocn, wat_from_ocn,       &
                       wat_from_atm, snow_to_ice, bablt)
! !DESCRIPTION:
!  This routine calculates the change in ice and snow thickness
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)      :: snow        ! new snow (kg/m^2-snow)
   REALTYPE, intent(in)      :: frazil      ! frazil in energy units
   REALTYPE, intent(in)      :: evap        ! ice evaporation (kg/m^2)
   REALTYPE, intent(in)      :: tmelt       ! top melting energy (J/m^2)
   REALTYPE, intent(in)      :: bmelt       ! bottom melting energy (J/m^2)
   REALTYPE, intent(in)      :: tfw         ! seawater freezing temperature (deg-C)
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)   :: hs          ! snow thickness (m-snow)
   REALTYPE, intent(inout)   :: hi          ! ice thickness (m-ice)
   REALTYPE, intent(inout)   :: t1          ! temperature of upper ice (deg-C)
   REALTYPE, intent(inout)   :: t2          ! temperature of lower ice (deg-C)
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)     :: heat_to_ocn ! energy left after ice all melted (J/m^2)
   REALTYPE, intent(out)     :: wat_to_ocn  ! liquid water flux to ocean (kg/m^2)
   REALTYPE, intent(out)     :: wat_from_ocn! evaporation flux from ocean (kg/m^2)
   REALTYPE, intent(out)     :: wat_from_atm! water flux from atmosphere to ice (kg/m^2)
   REALTYPE, intent(out)     :: snow_to_ice ! snow below waterline becomes ice
   REALTYPE, intent(out),optional :: bablt  ! bottom ablation (kg/m^2)
!
! !LOCAL VARIABLES:
   REALTYPE        :: h1,h2,hw 
!EOP
!-----------------------------------------------------------------------
!BOC
   heat_to_ocn  = _ZERO_
   wat_to_ocn   = DS*hs+DI*hi
   wat_from_ocn = _ZERO_
   snow_to_ice  = _ZERO_

#if defined debug_ice
   write(stdout,*) 'ice_size: tmelt,bmelt,heat_to_ocn',tmelt,bmelt,heat_to_ocn
#endif

!  snowfall & evaporation over snow/ice is water exchange with the atmosphere
!  it does NOT contribute to the water exchange between ice and ocean
!  only the excess evaporation goes directly to the ocean via wat_from_ocn
!  we save the effect of snowfall & evaporation in wat_from_atm
   h1 = hi/2
   h2 = hi/2
!  add snow ...
   hs = hs + snow/DS
   wat_from_atm = snow/DS
!  ... and frazil
   call add_to_bot(frazil/melt_energy(h2=_ONE_,t2=tfw), tfw, h2, t2)
! atmospheric evaporation
   if (evap <= hs*DS) then
      hs = hs - evap/DS
      wat_from_atm = wat_from_atm - evap/DS
   else if (evap-hs*DS<=h1*DI) then
      hs = _ZERO_
      h1 = h1-(evap-DS*hs)/DI
      wat_from_atm = wat_from_atm - (evap-DS*hs)/DI
   else if (evap-hs*DS-h1*DI<=h2*DI) then
      hs = _ZERO_
      h1 = _ZERO_
      h2 = h2 - (evap-hs*DS-h1*DI)/DI
      wat_from_atm = wat_from_atm - (evap-hs*DS-h1*DI)/DI
   else
      wat_from_atm = wat_from_atm - hs*DS - (h1+h2)*DI
      wat_from_ocn = evap-hs*DS-(h1+h2)*DI
      hs = _ZERO_
      h1 = _ZERO_
      h2 = _ZERO_
   end if
!
   if (bmelt < _ZERO_) call add_to_bot(-bmelt/melt_energy(h2=_ONE_,t2=tfw), tfw, h2, t2)
!
   if (h1 == _ZERO_) t1 = tfw  ! need this, below we divide by t1 even when h1 == 0
  !
  ! apply energy fluxes ... top ...
  !
   if (tmelt <= melt_energy(hs)) then
      hs = hs - tmelt/melt_energy(hs=_ONE_)
   else if (tmelt <= melt_energy(hs,h1,t1)) then
      h1 = h1 - (tmelt-melt_energy(hs))/melt_energy(h1=_ONE_,t1=t1)
      hs = _ZERO_
   else if (tmelt <= melt_energy(hs,h1,t1,h2,t2)) then
      h2 = h2 - (tmelt-melt_energy(hs,h1,t1))/melt_energy(h2=_ONE_,t2=t2)
      hs = _ZERO_
      h1 = _ZERO_
   else
     heat_to_ocn = heat_to_ocn+tmelt-melt_energy(hs,h1,t1,h2,t2)
     hs = _ZERO_
     h1 = _ZERO_
     h2 = _ZERO_
   endif
# if defined debug_ice
  write(stdout,*) 'ice_size: tmelt,bmelt,heat_to_ocn',tmelt,bmelt,heat_to_ocn
# endif
!  ... and bottom
   if (present(bablt)) bablt = DS*hs+DI*(h1+h2)

   if (bmelt > _ZERO_) then
      if (bmelt < melt_energy(h2=h2,t2=t2)) then
         h2 = h2 - bmelt/melt_energy(h2=_ONE_,t2=t2)
      else if (bmelt < melt_energy(h1=h1,t1=t1,h2=h2,t2=t2)) then
         h1 = (bmelt-melt_energy(h2=h2,t2=t2))/melt_energy(h1=_ONE_,t1=t1)
         h2 = _ZERO_
      else if (bmelt < melt_energy(hs,h1,t1,h2,t2)) then
         hs = hs - (bmelt-melt_energy(h1=h1,t1=t1,h2=h2,t2=t2))/melt_energy(hs=_ONE_)
         h1 = _ZERO_
         h2 = _ZERO_
      else
         heat_to_ocn = heat_to_ocn+bmelt-melt_energy(hs,h1,t1,h2,t2)
         hs = _ZERO_
         h1 = _ZERO_
         h2 = _ZERO_
      endif
   endif
   if (present(bablt)) bablt = bablt-DS*hs-DI*(h1+h2)
# if defined debug_ice_1
   write(stdout,*) 'ice_size: tmelt,bmelt,heat_to_ocn',tmelt,bmelt,heat_to_ocn
# endif
!
   hi = h1 + h2
   hw = (DI*hi+DS*hs)/DW
   if (hw>hi) then           ! convert snow to ice to maintain ice at waterline
      snow_to_ice = (hw-hi)*DI
      hs = hs - snow_to_ice/DS
      call add_to_top(hw-hi, TFI, h1, t1)
   endif
!
   call even_up(h1, t1, h2, t2)
   hi = h1+h2
   if (hi==_ZERO_) then
      t1 = _ZERO_
      t2 = _ZERO_
   endif
!
   wat_to_ocn = wat_to_ocn - DS*hs - DI*hi + wat_from_atm

   return
   end subroutine ice_size
!EOC

!-----------------------------------------------------------------------

   end module ice_winton

!-----------------------------------------------------------------------
! Copyright by the GETM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------


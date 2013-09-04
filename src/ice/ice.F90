#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: ice --- atmospheric fluxes \label{sec:ice}
!
! !INTERFACE:
   module ice
!
! !DESCRIPTION:
!  To be done
!
! !USES:
  use ice_winton,       only: do_ice_winton, KMELT, CW
  use meanflow,         only: T,S,rho
  use airsea,           only: heat,I_0
!
   IMPLICIT NONE

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public                              :: init_ice
   public                              :: do_ice
   public                              :: clean_ice
#ifdef _PRINTSTATE_
   public                              :: print_state_ice
#endif
!
! !PUBLIC DATA MEMBERS:
   integer, public                     :: ice_method
!  Winton ice model
   REALTYPE, public                    :: ice_hs,ice_hi,ice_T1,ice_T2
   REALTYPE, public                    :: ice_tmelt,ice_bmelt
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the icea module \label{sec:init-ice}
!
! !INTERFACE:
   subroutine init_ice(namlst)
!
! !DESCRIPTION:
!  To be done
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
! !LOCAL VARIABLES:
   namelist /ice/ ice_method
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_ice'

!  initialize namelist variables to reasonable defaults.
   ice_method=0

!  Read namelist variables from file.
   open(namlst,file='ice.nml',action='read',status='old',err=90)
   read(namlst,nml=ice,err=91)
   close(namlst)

!  The different ice models
   select case (ice_method)
      case (0)
         LEVEL2 'No ice calculations included'
      case (1)
         LEVEL2 'Clip heat-fluxes if SST < freezing point (function of S)'
      case (2)
         LEVEL2 'Thermodynamic ice model adapted from Winton'
         ice_hs=_ZERO_;ice_hi=_ZERO_;ice_T1=_ZERO_;ice_T2=_ZERO_
         ice_tmelt=_ZERO_;ice_bmelt=_ZERO_
      case default
   end select

   return

90 FATAL 'I could not open ice.nml'
   stop 'init_ice'
91 FATAL 'I could not read ice namelist'
   stop 'init_ice'

   end subroutine init_ice
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Obtain the air--sea fluxes
!
! !INTERFACE:
   subroutine do_ice()
!
! !DESCRIPTION:
!  To be done
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Jesper Larsen
!
! !LOCAL VARIABLES:
   REALTYPE        :: tfw
   REALTYPE        :: fb=_ZERO_
REALTYPE        :: dt=3600.
REALTYPE        :: ts
   integer         :: n
!EOP
!-----------------------------------------------------------------------
!BOC

!  The different ice models
   select case (ice_method)
      case (0)
         LEVEL2 'ice_method=',ice_method
      case (1)
!         LEVEL0 T(10),S(10),heat
         n = ubound(S,1)
         if (heat .gt. _ZERO_ .and. T(n) .le. -0.0575*S(n)) then
            heat = _ZERO_
            LEVEL0 'do_ice: heat clipped to',heat
         end if
      case (2)
#if 0
         call do_ice_winton(ice_hs,ice_hi,ice_t1,ice_t2)
#else
         n = ubound(S,1)
         tfw = -0.0575*S(n)
         if (heat .gt. _ZERO_ .and. T(n) .le. tfw) then
            if (ice_hi .eq. _ZERO_) then
               ice_bmelt = -heat*dt
            endif
            heat = _ZERO_
            LEVEL0 'do_ice: heat clipped to',heat,ice_bmelt
         end if
!
         if (T(n) .le. tfw) then
!           during freezing conditions all available energy is converted
!           to bottom freezing energy
            ice_bmelt = ((T(n) - tfw)*rho(n)*CW)*dt
!           the freezing heats the surface water to the freezing point
            T(n) = tfw
            STDERR 'do_ice: frazil ice formation', ice_bmelt, tfw, T(n)
         else if (ice_hi .gt. _ZERO_) then
!           when sea ice is present there is an ocean to sea ice heat flux, see eq. (23)
!           with the linear form described in eq. (15) in "FMS Sea Ice Simulator"
            ice_bmelt = KMELT*(T(n) - tfw)*dt
!           TODO (KBK?): the surface water temperature should be changed according to
!           how much energy is extracted
            STDERR 'do_ice: ocean to bottom ice melting', ice_bmelt
         end if
         call do_ice_winton(heat,_ZERO_,I_0,tfw,fb,dt, &
                            ice_hs,ice_hi,ice_t1,ice_t2, &
                            ice_tmelt,ice_bmelt,ts)
         if (ice_hi .gt. _ZERO_) then
            heat = _ZERO_
         endif
!         STDERR heat,I_0,tfw,fb
!        TODO (KBK?): the returned quantities in ice_tmelt and ice_bmelt are
!        surplus energy that was not used for melting or was released during
!        freezing. This energy should be used to modify T(n)
         ice_tmelt = _ZERO_
         ice_bmelt = _ZERO_
!         STDERR ice_hs,ice_hi,ice_t1,ice_t2
!         STDERR ice_tmelt,ice_bmelt,ts
#endif
      case default
   end select

   end subroutine do_ice
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the air--sea interactions
!
! !INTERFACE:
   subroutine clean_ice
!
! !DESCRIPTION:
!  All files related to air-sea interaction which have been opened
!  are now closed by this routine.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC

   end subroutine clean_ice
!EOC

   end module ice

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

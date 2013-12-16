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
   use time,             only: julianday, secondsofday
   use airsea_variables, only: emiss,bolz,kelvin
   use ice_winton,       only: do_ice_winton, ice_optics, KMELT, CW
   use meanflow,         only: h,T,S,rho,rho_0
   use airsea,           only: heat,I_0,albedo,precip,evap,cloud,swr_method,airt, &
                               airp,rh,u10,v10,back_radiation_method,hum_method, &
                               fluxes_method
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
!  Simple 'ice model'
   REALTYPE, public                    :: ice_layer
!  Winton ice model
   REALTYPE, public                    :: ice_hs,ice_hi,ice_T1,ice_T2
   REALTYPE, public                    :: ice_tmelt,ice_bmelt
   REALTYPE, public                    :: ice_ts
!
!  !PRIVATE DATA MEMBERS:
   REALTYPE                            :: lat, lon

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
   subroutine init_ice(namlst,latitude,longitude)
!
! !DESCRIPTION:
!  To be done
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: namlst
   REALTYPE, intent(in)                :: latitude,longitude

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
   lat = latitude
   lon = longitude

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
         ice_layer=_ZERO_
      case (2)
         LEVEL2 'Thermodynamic ice model adapted from Winton'
         ice_hs=_ZERO_;ice_hi=_ZERO_;ice_T1=_ZERO_;ice_T2=_ZERO_
         ice_ts=_ZERO_;ice_tmelt=_ZERO_;ice_bmelt=_ZERO_
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
   REALTYPE        :: dt=3600.
   integer         :: n
   logical         :: has_ice
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
         tfw = -0.0575*S(n)
         if (heat .gt. _ZERO_ .and. T(n) .le. tfw) then
            heat = _ZERO_
            LEVEL0 'do_ice: heat clipped to',heat
            ice_layer=_ONE_
         else
            ice_layer=_ZERO_
         end if
      case (2)
         if (swr_method .ne. 3) then
            STDERR 'Ice model currently only support swr_method 3'
            stop 'Ice model currently only support swr_method 3'
         endif
         n = ubound(S,1)
         call do_ice_winton(julianday,secondsofday,lon,lat, &
                            cloud,airt,airp,rh,u10,v10, &
                            S(n),rho(n),rho_0,h(n), &
                            back_radiation_method,hum_method, &
                            fluxes_method,dt, &
                            T(n),heat,I_0,precip, &
                            ice_hs,ice_hi,ice_t1,ice_t2, &
                            ice_ts,albedo,ice_tmelt,ice_bmelt)
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

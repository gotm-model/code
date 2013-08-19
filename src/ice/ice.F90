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
  use ice_winton,       only: do_ice_winton
  use meanflow,         only: T,S,rho
  use airsea,           only: heat
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
!  Original author(s): Karsten Bolding
!EOP
!-----------------------------------------------------------------------
!BOC

!  The different ice models
   select case (ice_method)
      case (0)
         LEVEL2 'ice_method=',ice_method
      case (1)
!         LEVEL0 T(10),S(10),heat
         if (heat .gt. _ZERO_ .and. T(10) .le. -0.0575*S(10)) then
            heat = _ZERO_
            LEVEL0 'do_ice: heat clipped to',heat
         end if
      case (2)
         call do_ice_winton()
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

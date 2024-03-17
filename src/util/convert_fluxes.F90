#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
! !ROUTINE: Convert between buoyancy fluxes and others \label{sec:convertFluxes}
!
! !INTERFACE:
    subroutine  convert_fluxes(nlev,swf,shf,ssf,rad,Tsrf,Ssrf,tFlux,sFlux,btFlux,bsFlux,tRad,bRad)

    
! !DESCRIPTION:
!  This subroutine computes the buoyancy fluxes that are due
!  to
!  \begin{enumerate}
!    \item the surface heat flux,
!    \item the surface salinity flux caused by the value of
!          P-E (precipitation-evaporation),
!    \item and the short wave radiative flux.
!  \end{enumerate}
!  Additionally, it outputs the temperature flux ({\tt tFlux})
!  corresponding to the surface heat flux, the salinity flux
!  ({\tt sFlux})  corresponding to the value P-E, and the profile
!  of the temperature flux ({\tt tRad}) corresponding to the profile
!  of the radiative heat flux.
!
! This function is only called when the KPP turbulence model is used.
! When you call the KPP routines from another model outside GOTM, you
! are on your own in computing the  fluxes required by the KPP model, because
! they have to be consistent with the equation of state used in your model.
!
!
! !USES:
   use meanflow,       only: gravity 
   use density,        only: rho0,cp
   use density,        only: get_alpha,get_beta
   use density,        only: alpha,beta          
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
  integer,  intent(in)                :: nlev
  REALTYPE, intent(in)                :: swf,shf,ssf
  REALTYPE, intent(in)                :: Tsrf
  REALTYPE, intent(in)                :: Ssrf
  REALTYPE, intent(in)                :: rad(0:nlev)
!
! !OUTPUT PARAMETERS:
  REALTYPE, intent(out)               :: tFlux,sFlux
  REALTYPE, intent(out)               :: btFlux,bsFlux
  REALTYPE, intent(out)               :: tRad(0:nlev)
  REALTYPE, intent(out)               :: bRad(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: alpha0,beta0
!
!-----------------------------------------------------------------------
!BOC

   ! alpha, beta at surface
   alpha0  = get_alpha(Ssrf,Tsrf,_ZERO_)
   beta0   = get_beta(Ssrf,Tsrf,_ZERO_)

   ! temperature flux and associated buoyancy flux
   tFlux   = -Tsrf*swf - shf/(rho0*cp)
   btFlux  =  gravity*alpha0*tFlux
   
   ! salinity flux and associated buoyancy flux
   sFlux   = -Ssrf*swf - ssf     
   bsFlux  = -gravity*beta0*sFlux   

   ! radiative temperature and buoyancy flux profiles
   tRad    =  rad/(rho0*cp)
   bRad    = gravity*alpha*tRad  


   return
   end subroutine convert_fluxes
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

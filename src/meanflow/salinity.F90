!$Id: salinity.F90,v 1.7 2004-08-18 11:43:10 lars Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The salinity equation \label{sec:salinity}
!
! !INTERFACE:
   subroutine salinity(nlev,dt,cnpar,nuh) 
!
! !DESCRIPTION:
! This subroutine computes the balance of salinity in the form
!  \begin{equation}
!   \label{SEq}
!    \dot{S}
!    = {\cal D}_S
!    - \frac{1}{\tau^S_R}(S-S_{obs})
!    \comma
!  \end{equation}
!  where $\dot{S}$ denotes the material derivative of the salinity $S$, and
!  ${\cal D}_S$ is the sum of the turbulent and viscous transport
!  terms modelled according to
!  \begin{equation}
!   \label{DS}
!    {\cal D}_S 
!    = \frstder{z} 
!     \left( 
!        \left( \nu^S_t + \nu^S \right) \partder{S}{z} - \tilde{\Gamma}_S 
!        \right) 
!    \point
!  \end{equation}
!  In this equation, $\nu^S_t$ and $\nu^S$ are the turbulent and 
!  molecular diffusivities of salinity, respectively, 
!  and $\tilde{\Gamma}_S$
!  denotes the so-called counter-gradient flux of salinity, see 
!  \sect{sec:turbulenceIntro}. In the current version of GOTM,
!  we set $\nu^S_t = \nu'_t$ for simplicity.
!
!  Horizontal advection is optionally
!  included  (see {\tt obs.inp}) by means of prescribed
!  horizontal gradients $\partial_xS$ and $\partial_yS$ and 
!  calculated horizontal velocities $u$ and $v$.
!  Relaxation with the time scale $\tau^S_R$ 
!  towards a precribed (changing in time)
!  profile $S_{obs}$ is possible. 

!  Inner sources or sinks are not considered. 
!  The surface freshwater flux is given by means of the precipitation
!  - evaporation data read in as $p_e$ through the {\tt airsea.inp} namelist:
!  \begin{equation}
!    {\cal D}_S = - S p_e,
!    \qquad \mbox{at } z=\zeta,
!  \end{equation}
!  with $p_e$ given as a velocity.
!  Diffusion is numerically treated implicitly, 
!  see equations (\ref{sigmafirst})-
!  (\ref{sigmalast}).   
!  The tri-diagonal matrix is solved then by a simplified Gauss elimination.
!  Vertical advection is included for accounting for adaptive grids,
!  see {\tt adaptivegrid.F90}.
!
! !USES:
   use meanflow, only: avmols
   use meanflow, only: h,ho,u,v,S,avh,w,grid_method,w_grid
   use observations, only: dsdx,dsdy,s_adv,w_adv,w_adv_discr,w_adv_method
   use observations, only: sprof,SRelaxTau
   use airsea, only: p_e
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt,cnpar
   REALTYPE, intent(in)                :: nuh(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: salinity.F90,v $
!  Revision 1.7  2004-08-18 11:43:10  lars
!  updated documentation
!
!  Revision 1.6  2004/01/07 12:17:47  lars
!  Removed latex bug
!
!  Revision 1.5  2003/06/13 09:27:15  hb
!  Implemented freshwater fluxes
!
!  Revision 1.4  2003/03/28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.3  2003/03/28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.2  2003/03/10 08:50:07  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: Qsour(0:nlev)
   REALTYPE                  :: Sup,Sdw,saltot
   integer                   :: i,Bcup,Bcdw,flag
   logical                   :: surf_flux,bott_flux
!
!-----------------------------------------------------------------------
!BOC
! hard coding of parameters, to be included into namelist for gotm2.0 
   Bcup=1                    !BC Neumann
   Sup=-S(nlev)*p_e          !freshwater flux
   Bcdw=1                    !BC Neumann
   Sdw=0.                    !No flux
   surf_flux=.false.                      
   bott_flux=.false.

   do i=1,nlev-1
      avh(i)=nuh(i)+avmolS 
!      w(i)=w_adv
   end do
   do i=1,nlev
      Qsour(i)=0.
      if (s_adv) Qsour(i)=Qsour(i)-u(i)*dsdx(i)-v(i)*dsdy(i)
   end do

   flag=1  ! divergence correction for vertical advection
   
   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Sup,Sdw,SRelaxTau,h,ho,avh,w,        &
              QSour,sprof,w_adv_method,w_adv_discr,S,surf_flux,bott_flux,  &
              grid_method,w_grid,flag)

   return
   end subroutine salinity 
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

!$Id: temperature.F90,v 1.9 2003-07-23 12:33:21 hb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The temperature equation \label{sec:temperature}
!
! !INTERFACE:
   subroutine temperature(nlev,dt,cnpar,I_0,heat,nuh,rad)
!
! !DESCRIPTION:
! This subroutine computes the balance of heat in the form
!  \begin{equation}
!   \label{TEq}
!    \dot{\theta}
!    = {\cal D}_\theta
!    - \frac{1}{\tau_R(\theta)}(\theta-\theta_{obs})
!    + \frac{1}{C_p \rho_0} \partder{I}{z}
!    \comma
!  \end{equation}
!  where $\dot{\theta}$ denotes the material derivative of the potential 
!  temperature $\theta$, and
!  ${\cal D}_\theta$ is the sum of the turbulent and viscous transport
!  terms modelled according to
!  \begin{equation}
!   \label{DT}
!    {\cal D}_\theta 
!    = \frstder{z} 
!     \left( 
!        \left( \nu'_t + \nu^\theta \right) \partder{\theta}{z}
!      \right) 
!    \point
!  \end{equation}
!  In this equation, $\nu'_t$ and $\nu^\theta$ are the turbulent and 
!  molecular diffusivities of heat, respectively. The computation
!  of $\nu'_t$ is discussed in \sect{sec:turbulenceIntro}.
!
!  Horizontal advection is optionally
!  included  (see {\tt obs.inp}) by means of prescribed
!  horizontal gradients $\partial_x\theta$ and $\partial_y\theta$ and 
!  calculated horizontal velocities $u$ and $v$.
!  Relaxation with the time scale $\tau_R (\theta)$ 
!  towards a precribed (changing in time)
!  profile $\theta_{obs}$ is possible. 
!
!  The sum of latent, sensible, and longwave radiation is treated
!  as a boundary condition. Solar radiation is treated as an inner 
!  source, $I(z)$. It is computed according the
!  exponential law (see \cite{PaulsonSimpson77})
!  \begin{equation}
!    \label{Iz}
!    I(z) = I_0 \bigg(Ae^{-\eta_1z}+(1-A)e^{-\eta_2z}\bigg).
!  \end{equation}
!  The absorbtion coefficients $\eta_1$ and $\eta_2$ depend on the water type
!  and have to be prescribed either by means of choosing a \cite{Jerlov68} class
!  (see \cite{PaulsonSimpson77}) or by reading in a file through the namelist
!  {\tt extinct} in {\tt obs.inp}. 

!  Diffusion is numerically treated implicitly, see equations (\ref{sigmafirst})-
!  (\ref{sigmalast}).
!  The tri--diagonal matrix is solved then by a simplified Gauss elimination.
!  Vertical advection is included for accounting for adaptive grids,
!  see {\tt adaptivegrid.F90}.
!
! !USES:
   use meanflow, only: avmolt,rho_0,cp
   use meanflow, only: h,ho,u,v,T,avh,w,grid_method,w_grid
   use meanflow, only: bioshade
   use observations, only: dtdx,dtdy,t_adv,w_adv,w_adv_discr,w_adv_method
   use observations, only: tprof,TRelaxTau
   use observations, only: A,g1,g2
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt,cnpar
   REALTYPE, intent(in)                :: I_0,heat
   REALTYPE, intent(in)                :: nuh(0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE                            :: rad(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: temperature.F90,v $
!  Revision 1.9  2003-07-23 12:33:21  hb
!  fixed bioshade init and use
!
!  Revision 1.7  2003/04/05 07:01:16  kbk
!  moved bioshade variable to meanflow - to compile properly
!
!  Revision 1.6  2003/04/04 14:25:52  hb
!  First iteration of four-compartment geobiochemical model implemented
!
!  Revision 1.5  2003/03/28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.4  2003/03/28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.3  2003/03/10 08:50:07  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.2  2001/11/18 11:50:37  gotm
!  Cleaned
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i,Bcup,Bcdw,flag
   REALTYPE                  :: Qsour(0:nlev) 
   REALTYPE                  :: Tup,Tdw,z
   logical                   :: surf_flux,bott_flux
!
!-----------------------------------------------------------------------
!BOC
!  hard coding of parameters, to be included into namelist for gotm2.0 
   Bcup=1!BC Neumann 
   Tup=-heat/(rho_0*cp)!Heat flux (positive upward)
   Bcdw=1!BC Neumann
   Tdw=0.!No flux
   surf_flux=.false.                     
   bott_flux=.false.

   rad(nlev)=I_0
   z=0. 
   do i=nlev-1,0,-1 
      z=z+h(i+1)
      rad(i)=I_0*(A*exp(-z/g1)+(1-A)*exp(-z/g2))*bioshade(i)
      avh(i)=nuh(i)+avmolT 
   end do

   do i=1,nlev
      Qsour(i)=(rad(i)-rad(i-1))/(rho_0*cp)/h(i) 
      if (t_adv) Qsour(i)=Qsour(i)-u(i)*dtdx(i)-v(i)*dtdy(i) 
   end do

   flag=1  ! divergence correction for vertical advection

   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Tup,Tdw,TRelaxTau,h,ho,avh,w,        &
              Qsour,tprof,w_adv_method,w_adv_discr,T,surf_flux,bott_flux,  &
              grid_method,w_grid,flag)
   return
   end subroutine temperature
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

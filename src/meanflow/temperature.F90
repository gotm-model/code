#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The temperature equation \label{sec:temperature}
!
! !INTERFACE:
   subroutine temperature(nlev,dt,cnpar,I_0,heat,nuh,gamh,rad)
!
! !DESCRIPTION:
! This subroutine computes the balance of heat in the form
!  \begin{equation}
!   \label{TEq}
!    \dot{\Theta}
!    = {\cal D}_\Theta
!    - \frac{1}{\tau^\Theta_R}(\Theta-\Theta_{obs})
!    + \frac{1}{C_p \rho_0} \partder{I}{z}
!    \comma
!  \end{equation}
!  where $\dot{\Theta}$ denotes the material derivative of the mean  potential
!  temperature $\Theta$, and
!  ${\cal D}_\Theta$ is the sum of the turbulent and viscous transport
!  terms modelled according to
!  \begin{equation}
!   \label{DT}
!    {\cal D}_\Theta
!    = \frstder{z}
!     \left(
!        \left( \nu^\Theta_t + \nu^\Theta \right) \partder{\Theta}{z}
!               - \tilde{\Gamma}_\Theta
!        \right)
!    \point
!  \end{equation}
!  In this equation, $\nu^\Theta_t$ and $\nu^\Theta$ are the turbulent and
!  molecular diffusivities of heat, respectively, and $\tilde{\Gamma}_\Theta$
!  denotes the non-local flux of heat, see \sect{sec:turbulenceIntro}.
!
!  Horizontal advection is optionally
!  included  (see {\tt obs.nml}) by means of prescribed
!  horizontal gradients $\partial_x\Theta$ and $\partial_y\Theta$ and
!  calculated horizontal mean velocities $U$ and $V$.
!  Relaxation with the time scale $\tau^\Theta_R$
!  towards a precribed profile $\Theta_{obs}$, changing in time, is possible.
!
!  The sum of latent, sensible, and longwave radiation is treated
!  as a boundary condition. Solar radiation is treated as an inner
!  source, $I(z)$. It is computed according the
!  exponential law (see \cite{PaulsonSimpson77})
!  \begin{equation}
!    \label{Iz}
!    I(z) = I_0 \bigg(Ae^{z/\eta_1}+(1-A)e^{z/\eta_2}\bigg)B(z).
!  \end{equation}
!  The absorbtion coefficients $\eta_1$ and $\eta_2$ depend on the water type
!  and have to be prescribed either by means of choosing a \cite{Jerlov68} class
!  (see \cite{PaulsonSimpson77}) or by reading in a file through the namelist
!  {\tt extinct} in {\tt obs.nml}. The damping term due to bioturbidity,
!  $B(z)$ is calculated in the biogeochemical routines, see section
!  \ref{sec:bio-intro}.

!  Diffusion is numerically treated implicitly, see equations (\ref{sigmafirst})-
!  (\ref{sigmalast}).
!  The tri-diagonal matrix is solved then by a simplified Gauss elimination.
!  Vertical advection is included, and it must be non-conservative,
!  which is ensured by setting the local variable {\tt adv\_mode=0},
!  see section \ref{sec:advectionMean} on page \pageref{sec:advectionMean}.
!
! !USES:
   use meanflow,     only: avmolt,rho_0,cp
   use meanflow,     only: h,u,v,w,T,S,avh
   use meanflow,     only: bioshade
#ifndef _ICE_
   use meanflow,     only: Hice
#endif
   use meanflow,     only: bioshade
   use observations, only: dtdx,dtdy,t_adv
   use observations, only: w_adv_discr,w_adv
   use observations, only: tprof,TRelaxTau
   use observations, only: A_,g1_,g2_
   use util,         only: Dirichlet,Neumann
   use util,         only: oneSided,zeroDivergence

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)                 :: nlev

!  time step (s)
   REALTYPE, intent(in)                :: dt

!  numerical "implicitness" parameter
   REALTYPE, intent(in)                :: cnpar

!  surface short waves radiation  (W/m^2)
   REALTYPE, intent(in)                :: I_0

!  surface heat flux (W/m^2)
!  (negative for heat loss)
   REALTYPE, intent(in)                :: heat

!  diffusivity of heat (m^2/s)
   REALTYPE, intent(in)                :: nuh(0:nlev)

!  non-local heat flux (Km/s)
   REALTYPE, intent(in)                :: gamh(0:nlev)
!
! !OUTPUT PARAMETERS:
!  shortwave radiation profile (W/m^2)
   REALTYPE                            :: rad(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: adv_mode=0
   integer                   :: posconc=0
   integer                   :: i
   integer                   :: DiffBcup,DiffBcdw
   integer                   :: AdvBcup,AdvBcdw
   REALTYPE                  :: DiffTup,DiffTdw
   REALTYPE                  :: AdvTup,AdvTdw
   REALTYPE                  :: Lsour(0:nlev)
   REALTYPE                  :: Qsour(0:nlev)
   REALTYPE                  :: z
!
!-----------------------------------------------------------------------
!BOC
!
!  set boundary conditions
   DiffBcup       = Neumann
   DiffBcdw       = Neumann
#ifndef _ICE_
! simple sea ice model: surface heat flux switched off for sst < freezing temp
   if (T(nlev) .le. -0.0575*S(nlev)) then
       DiffTup    = max(_ZERO_,heat/(rho_0*cp))
       Hice = _ONE_
   else
       DiffTup    = heat/(rho_0*cp)
       Hice = _ZERO_
   end if
#else
   DiffTup    = heat/(rho_0*cp)
#endif
   DiffTdw        = _ZERO_

   AdvBcup        = oneSided
   AdvBcdw        = oneSided
   AdvTup         = _ZERO_
   AdvTdw         = _ZERO_

!  initalize radiation
   rad(nlev)  = I_0
   z          =_ZERO_

   do i=nlev-1,0,-1

      z=z+h(i+1)

!     compute short wave radiation
      rad(i)=I_0*(A_%value*exp(-z/g1_%value)+(1.-A_%value)*exp(-z/g2_%value)*bioshade(i+1))

!     compute total diffusivity
      avh(i)=nuh(i)+avmolT
   end do


!  add contributions to source term
   Lsour=_ZERO_
   Qsour=_ZERO_

   Qsour(nlev)=(I_0-rad(nlev-1))/(rho_0*cp*h(nlev))
   do i=1,nlev-1
!     from radiation
      Qsour(i) = (rad(i)-rad(i-1))/(rho_0*cp*h(i))
   enddo

   do i=1,nlev
!     from non-local turbulence
#ifdef NONLOCAL
      Qsour(i) = Qsour(i) - ( gamh(i) - gamh(i-1) )/h(i)
#endif
   end do

!  ... and from lateral advection
   if (t_adv) then
      do i=1,nlev
         Qsour(i) = Qsour(i) - u(i)*dtdx%data(i) - v(i)*dtdy%data(i)
      end do
   end if

!  do advection step
   if (w_adv%method.ne.0) then
      call adv_center(nlev,dt,h,h,w,AdvBcup,AdvBcdw,                    &
                          AdvTup,AdvTdw,w_adv_discr,adv_mode,T)
   end if

!  do diffusion step
   call diff_center(nlev,dt,cnpar,posconc,h,DiffBcup,DiffBcdw,          &
                    DiffTup,DiffTdw,avh,Lsour,Qsour,TRelaxTau,tProf%data,T)
   return
   end subroutine temperature
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The V-momentum equation\label{sec:vequation}
!
! !INTERFACE:
   subroutine vequation(nlev,dt,cnpar,ty,num,gamv,Method)
!
! !DESCRIPTION:
!  This subroutine computes the transport of momentum in
!  $y$-direction according to
!  \begin{equation}
!   \label{vEq}
!    \dot{V}
!    = {\cal D}_V
!    - g \partder{\zeta}{y} + \int_z^{\zeta} \partder{B}{y} \,dz'
!    - \frac{1}{\tau^V_R}(V-V_{obs})-C_f V \sqrt{U^2+V^2}
!    \comma
!  \end{equation}
!  where $\dot{V}$ denotes the material derivative of $V$, $\zeta$
!  the free surface elevation and $B$ the mean buoyancy defined
!  in  \eq{DefBuoyancy}. ${\cal D}_V$ is the sum of the turbulent
!  and viscous transport terms modelled according to
!  \begin{equation}
!   \label{Dv}
!    {\cal D}_V
!    = \frstder{z}
!     \left(
!        \left( \nu_t + \nu \right) \partder{V}{z}
!               - \tilde{\Gamma}_V
!      \right)
!    \point
!  \end{equation}
!  In this equation, $\nu_t$ and $\nu$ are the turbulent and
!  molecular diffusivities of momentum, respectively, and
!  $\tilde{\Gamma}_V$ denotes the non-local flux of momentum,
!  see \sect{sec:turbulenceIntro}.
!
!  Coriolis rotation is accounted for as described in
!  \sect{sec:coriolis}. All other terms are completely analogous
!  to those described in \sect{sec:uequation}.
!
! If lake is true additional "bottom" friction over the whole water column is
! included.
!
! !USES:
   use meanflow,     only: gravity,avmolu
   use meanflow,     only: lake
   use meanflow,     only: h,Vco,Vc,Afo,Af
   use meanflow,     only: v,vo,u,w,avh
   use meanflow,     only: drag,SS,runtimev
   use observations, only: w_adv_method,w_adv_discr
   use observations, only: vprof,vel_relax_tau,vel_relax_ramp
   use observations, only: idpdy,dpdy
   use observations, only: wq
   use util,         only: Dirichlet,Neumann
   use util,         only: oneSided,zeroDivergence,flux

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)                 :: nlev

!  time step (s)
   REALTYPE, intent(in)                :: dt

!  numerical "implicitness" parameter
   REALTYPE, intent(in)                :: cnpar

!  wind stress in y-direction
!  divided by rho_0 (m^2/s^2)
   REALTYPE, intent(in)                :: ty

!  diffusivity of momentum (m^2/s)
   REALTYPE, intent(in)                :: num(0:nlev)

!  non-local flux of momentum (m^2/s^2)
   REALTYPE, intent(in)                :: gamv(0:nlev)

!  method to compute external
!  pressure gradient
   integer, intent(in)                 :: method
!
! !DEFINED PARAMETERS:
   REALTYPE, parameter                 :: long=1.0D15

! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!                      (re-write after first version of
!                       Hans Burchard and Karsten Bolding)
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: adv_mode=0
   integer                   :: posconc=0
   integer                   :: i
   integer                   :: DiffBcup,DiffBcdw
   integer                   :: AdvBcup,AdvBcdw
   REALTYPE                  :: DiffVup,DiffVdw
   REALTYPE                  :: AdvVup,AdvVdw
   REALTYPE                  :: dzetady
   REALTYPE                  :: Lsour(0:nlev)
   REALTYPE                  :: Qsour(0:nlev)
   REALTYPE                  :: VRelaxTau(0:nlev)
   logical                   :: call_adv
!
!-----------------------------------------------------------------------
!BOC
!  save old value
   vo = v

!  set boundary conditions
   DiffBcup       = Neumann
   DiffBcdw       = Neumann
   DiffVup        = ty
   DiffVdw        = _ZERO_   ! bottom friction treated as a source term

   AdvBcup        = oneSided
   AdvBcdw        = oneSided
   AdvVup         = _ZERO_
   AdvVdw         = _ZERO_

!  set external pressure gradient
   if (method .eq. 0) then
      dzetady = dpdy
   else
      dzetady = _ZERO_
   endif

!  set vector of relaxation times
   if (vel_relax_ramp .lt. long) then
      runtimev=runtimev+dt
      if (runtimev .lt. vel_relax_ramp) then
         VRelaxTau=vel_relax_tau*vel_relax_ramp/(vel_relax_ramp-runtimev)
      else
         VRelaxTau=vel_relax_tau
      end if
   else
      VRelaxTau=vel_relax_tau
   end if

!  compute total diffusivity
   avh=num+avmolu

   if (lake) then
      call_adv = ANY( wq(1:nlev-1) .ne. _ZERO_ )
      if (call_adv) then
         Lsour = _ZERO_
         Qsour = _ZERO_
         call adv_center(nlev,dt,h,Vco,Vc,Afo,wq,flux,flux,             &
                         _ZERO_,_ZERO_,Lsour,Qsour,w_adv_discr,1,V)
      end if
   end if

!  do advection step
   if (w_adv_method.ne.0) then
      Lsour = _ZERO_
      Qsour = _ZERO_
      call adv_center(nlev,dt,h,Vc,Vc,Af,w,AdvBcup,AdvBcdw,             &
                      AdvVup,AdvVdw,Lsour,Qsour,w_adv_discr,adv_mode,V)
   end if

   do i=1,nlev
      Qsour(i) = _ZERO_
      Lsour(i) = _ZERO_

!     add external and internal pressure gradients
      Qsour(i) = Qsour(i) - gravity*dzetady + idpdy(i)

!     implement bottom friction as source term
      Lsour(i) = -drag(i)/h(i)*sqrt(u(i)*u(i)+v(i)*v(i))

!     add non-local fluxes
#ifdef NONLOCAL
!      Qsour(i) = Qsour(i) - ( gamv(i) - gamv(i-1) )/h(i)
#endif
   end do

!  do diffusion step
   call diff_center(nlev,dt,cnpar,posconc,h,Vc,Af,DiffBcup,DiffBcdw,    &
                    DiffVup,DiffVdw,avh,Lsour,Qsour,VRelaxTau,vprof,V)

   return
   end subroutine vequation
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

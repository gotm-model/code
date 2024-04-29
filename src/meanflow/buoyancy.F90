#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The buoyancy equation
!
! !INTERFACE:
   subroutine buoyancy(nlev,dt,cnpar,nub,gamb)
!
! !DESCRIPTION:
!  This subroutine solves a transport equation for the mean
!  potential buoyancy,
!  \begin{equation}
!    \label{DefBuoyancy}
!    B=-g\frac{\mean{\rho}-\rho_0}{\rho_0}
!    \comma
!  \end{equation}
!  where $g$ is the accelaration of gravity, and $\mean{\rho}$ and $\rho_0$
!  are the mean potential density and the reference density, respectively.
!  A simplified transport equation for $B$ can be written as
!  \begin{equation}
!   \label{bEq}
!    \dot{B}
!    = {\cal D}_B
!    \comma
!  \end{equation}
!  where $\dot{B}$ denotes the material derivative of $B$, and
!  ${\cal D}_b$ is the sum of the turbulent and viscous transport
!  terms modelled according to
!  \begin{equation}
!   \label{Db}
!    {\cal D}_B
!    = \frstder{z}
!     \left( (\nu^B_t+\nu^B) \partder{B}{z} - \tilde{\Gamma}_B \right)
!    \point
!  \end{equation}
!  In this equation, $\nu^B_t$ and $\nu^B$ are the turbulent and molecular
!  diffusivities of buoyancy, respectively, and $\tilde{\Gamma}_B$
!  denotes the non-local flux of buoyancy, see
!  \sect{sec:turbulenceIntro}. In the current version of GOTM,
!  we set $\nu^B_t = \nu^\Theta_t$ for simplicity. Source and sink
!  terms are completely disregarded, and thus \eq{bEq} mainly serves
!  as a convenient tool for some idealized test cases in
!  GOTM.
!
!  Diffusion is treated implicitly in space (see equations (\ref{sigmafirst})-
!  (\ref{sigmalast})), and then solved by a
!  simplified Gauss elimination.
!  Vertical advection is included, and it must be non-conservative,
!  which is ensured by setting the local variable {\tt adv\_mode=0},
!  see section \ref{sec:advectionMean} on page \pageref{sec:advectionMean}.
!
! !USES:
   use meanflow,      only: h,w,buoy,T,avh,init_buoyancy
   use meanflow,      only: grid_method
   use observations,  only: b_obs_NN,b_obs_surf,b_obs_sbf
   use observations,  only: w_adv_discr,w_adv
   use util,          only: Dirichlet,Neumann
   use util,          only: oneSided,zeroDivergence
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!  number of vertical layers
   integer, intent(in)                 :: nlev

!   time step (s)
   REALTYPE, intent(in)                :: dt

!  numerical "implicitness" parameter
   REALTYPE, intent(in)                :: cnpar

!  diffusivity of buoyancy (m^2/s)
   REALTYPE, intent(in)                :: nub(0:nlev)

!  non-local buoyancy flux (m^2/s^3)
   REALTYPE, intent(in)                :: gamb(0:nlev)
!
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
   REALTYPE                  :: DiffBup,DiffBdw
   REALTYPE                  :: AdvBup,AdvBdw
   REALTYPE                  :: Lsour(0:nlev)
   REALTYPE                  :: Qsour(0:nlev)
   REALTYPE                  :: BRelaxTau(0:nlev)
   REALTYPE                  :: zz
!
!-----------------------------------------------------------------------
!BOC

!  no diffusive flux at bottom and surface
   DiffBcup      = Neumann
   DiffBcdw      = Neumann
   DiffBup       = _ZERO_
   DiffBdw       = _ZERO_

   AdvBcup       = oneSided
   AdvBcdw       = oneSided
   AdvBup        = _ZERO_
   AdvBdw        = _ZERO_

!  compute total diffusivity
   do i=0,nlev
      avh(i)=nub(i)
   end do

!  construct initial linear profile from information in namelist
   if (init_buoyancy) then

      zz=_ZERO_
      do i=nlev,1,-1
         zz=zz+0.5*h(i)
         buoy(i)  = b_obs_surf - zz*b_obs_NN
         zz=zz+0.5*h(i)
      end do

      init_buoyancy=.false.
   end if

!  compose source term
   do i=1,nlev
      Lsour(i) = _ZERO_
      Qsour(i) = - (gamB(i)-gamB(i-1))/h(i)
   end do

!  no relaxation to observed values
   BRelaxTau = 1.e15

!  do advection step
   if (w_adv%method .ne. 0) then
      call adv_center(nlev,dt,h,h,w,AdvBcup,AdvBcdw,                    &
                      AdvBup,AdvBdw,w_adv_discr,adv_mode,buoy)
   end if

!  do diffusion step
   call diff_center(nlev,dt,cnpar,posconc,h,DiffBcup,DiffBcdw,          &
                    DiffBup,DiffBdw,avh,Lsour,Qsour,                    &
                    BRelaxTau,buoy,buoy)

!   T = buoy

   return
   end subroutine buoyancy
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

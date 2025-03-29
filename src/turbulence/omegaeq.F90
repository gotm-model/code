#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The dynamic epsilon-equation \label{sec:dissipationeq}
!
! !INTERFACE:
   subroutine omegaeq(nlev,dt,u_taus,u_taub,z0s,z0b,h,NN,SS)

! !DESCRIPTION:
! The $k$-$\omega$ model described by \cite{UmlaufEtAl3003} solves
! a transport equation for the inverse turbulence time scale,
! $ \omega = (c_\mu^0)^4 \varepsilon /k$, of the following form:     
! \begin{equation}
!   \label{omega}
!   \dot{\omega}
!   =
!   {\cal D}_\omega
!   + \frac{\omega}{k} ( c_{\omega 1} P + c_{\omega 3} G
!                        - c_{\omega 2} \varepsilon )
!   \comma
! \end{equation}
! where $\dot{\omega}$ denotes the material derivative of $\omega$.
! The production terms $P$ and $G$ follow from \eq{PandG} and
! ${\cal D}_\omega$ represents the sum of the viscous and turbulent
! transport terms.
!
! For horizontally homogeneous flows, the transport term ${\cal D}_\omega$
! appearing in \eq{dissipation} is presently expressed by a simple
! gradient formulation,
! \begin{equation}
!   \label{diffusionOmega}
!   {\cal D}_\omega = \frstder{z}
!    \left( \dfrac{\nu_t}{\sigma_\omega} \partder{\omega}{z} \right)
!  \comma
! \end{equation}
! where $\sigma_\omega$ is the constant Schmidt-number for $\omega$.
!
! Model constants are summarized in \tab{tab:KW_constants}. Similar
! to the two-equations models, the model parameter $c_{omega 3}$
! determines the value of the stationory Richardson number. It is
! computed numerically by solving \eq{Ri_st}.
! \begin{table}[ht]
!   \begin{center}
! \begin{tabular}{cccccc}
!     & $c_\mu^0$ & $\sigma_k$  & $\sigma_\omega$
!     & $c_{\omega 1}$ & $c_{\omega 2}$  \\[1mm] \hline
!     \cite{Rodi87} & $0.55$ & $2.0$ &  $2.0$ & $0.56$ & $0.83$ \\
!   \end{tabular}
!   \caption{\label{tab:KW_constants} Constants appearing in
!    \eq{omega} and \eq{diffusionOmega}.}
!   \end{center}
! \end{table}
!
! At the end of this routine the length-scale can be constrained according to a
! suggestion of \cite{Galperinetal88}. This feature is optional and can be activated
! by setting {\tt length\_lim = .true.} in {\tt gotm.yaml}.     
!
! !USES:
   use turbulence, only: P,B,PSTK,num
   use turbulence, only: tke,tkeo,k_min,eps,eps_min,L
   use turbulence, only: cw1,cw2,cw3plus,cw3minus,cw4
   use turbulence, only: cm0,cde,galp,length_lim
   use turbulence, only: omega_bc, psi_ubc, psi_lbc, ubc_type, lbc_type
   use turbulence, only: sig_w
   use util,       only: Dirichlet,Neumann

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: nlev

!  time step (s)
   REALTYPE, intent(in)                :: dt

!  surface and bottom
!  friction velocity (m/s)
   REALTYPE, intent(in)                :: u_taus,u_taub

!  surface and bottom
!  roughness length (m)
   REALTYPE, intent(in)                :: z0s,z0b

!  layer thickness (m)
   REALTYPE, intent(in)                :: h(0:nlev)

!  square of shear and buoyancy
!  frequency (1/s^2)
   REALTYPE, intent(in)                :: NN(0:nlev),SS(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: DiffOmgUp,DiffOmgDw,pos_bc
   REALTYPE                  :: prod,buoyan,diss
   REALTYPE                  :: prod_pos,prod_neg,buoyan_pos,buoyan_neg
   REALTYPE                  :: ki,epslim,OmgOverTke,NN_pos
   REALTYPE                  :: cnpar=_ONE_
   REALTYPE                  :: omega(0:nlev)   
   REALTYPE                  :: avh(0:nlev)
   REALTYPE                  :: Lsour(0:nlev),Qsour(0:nlev)
   REALTYPE                  :: cw3

   integer                   :: i
!
!------------------------------------------------------------------------
!BOC
!
!  re-construct omega at "old" timestep
   do i=0,nlev
      omega(i) = 1./cm0 * tkeo(i)**0.5 / L(i)
   end do
   
!  compute RHS
   do i=1,nlev-1

!     compute epsilon diffusivity
      avh(i) = num(i)/sig_w

!     compute production terms in eps-equation
      if (B(i).gt.0) then
         cw3=cw3plus
      else
         cw3=cw3minus
      end if

      OmgOverTke  = omega(i)/tkeo(i)
      prod        = cw1*OmgOverTke*P(i) + cw4*OmgOverTke*PSTK(i)
      buoyan      = cw3*OmgOverTke*B(i)
      diss        = cw2*OmgOverTke*eps(i)

      if (prod+buoyan.gt.0) then
         Qsour(i) = prod+buoyan
         Lsour(i) = -diss/omega(i)
      else
         Qsour(i)  = prod
         Lsour(i) = -(diss-buoyan)/omega(i)
      end if

   end do


!  TKE and position for upper BC
   if (psi_ubc.eq.Neumann) then
!     tke at center "nlev"
      ki = tke(nlev-1)

!     flux at center "nlev"
      pos_bc = 0.5*h(nlev)
   else
!     tke at face "nlev-1"
      ki = tke(nlev-1)

!     value at face "nlev-1"
      pos_bc = h(nlev)
   end if

!  obtain BC for upper boundary of type "ubc_type"
   DiffOmgUp  = omega_bc(psi_ubc,ubc_type,pos_bc,ki,z0s,u_taus)


!  TKE and position for lower BC
   if (psi_lbc.eq.Neumann) then
!     tke at center "1"
      ki = tke(1)

!     flux at center "1"
      pos_bc = 0.5*h(1)
   else
!     tke at face "1"
      ki = tke(1)

!     value at face "1"
      pos_bc = h(1)
   end if

!  obtain BC for lower boundary of type "lbc_type"
   DiffOmgDw  = omega_bc(psi_lbc,lbc_type,pos_bc,ki,z0b,u_taub)


!  do diffusion step
   call diff_face(nlev,dt,cnpar,h,psi_ubc,psi_lbc,                          &
                  DiffOmgUp,DiffOmgDw,avh,Lsour,Qsour,omega)


!  fill top and bottom value with something nice
!  (only for output)
   omega(nlev)  = omega_bc(Dirichlet,ubc_type,z0s,tke(nlev),z0s,u_taus)
   omega(0   )  = omega_bc(Dirichlet,lbc_type,z0b,tke(0   ),z0b,u_taub)

   do i=0,nlev
!     recover dissipation rate from k and omega
      eps(i)=cm0**4. * tke(i) * omega(i)

!     clip at eps_min
      eps(i) = max(eps(i),eps_min)
   enddo
   
!  limit dissipation rate under stable stratification,
!  see Galperin et al. (1988)
   if (length_lim) then
      do i=0,nlev

 !       look for N^2 > 0
         NN_pos = 0.5*( NN(i) + abs( NN(i) ) )

!        compute limit
         epslim = cde/sqrt(2.)/galp*tke(i)*sqrt(NN_pos)

!        clip at limit
         eps(i) = max(eps(i),epslim)

      end do
   endif

      do i=0,nlev
!        compute dissipative scale
         L(i) = cde*sqrt(tke(i)*tke(i)*tke(i))/eps(i)
      enddo

   return
   end subroutine omegaeq
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

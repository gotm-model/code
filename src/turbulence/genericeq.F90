#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The dynamic psi-equation  \label{sec:genericeq}
!
! !INTERFACE:
   subroutine genericeq(nlev,dt,u_taus,u_taub,z0s,z0b,h,NN,SS)

! !DESCRIPTION:
! This model has been formulated by \cite{UmlaufBurchard2003},
! who introduced a `generic' variable,
! \begin{equation}
!   \label{psi_l}
!   \psi = (c_\mu^0)^p k^m l^n
!   \comma
! \end{equation}
! where $k$ is the turbulent kinetic energy computed from \eq{tkeA} and
! $l$ is the dissipative length-scale defined in \eq{epsilon}.
! For appropriate choices of the exponents $p$, $m$, and $n$, the variable
! $\psi$ can be directly identified with the classic length-scale determining
! variables like the rate of dissipation, $\epsilon$, or the product
! $kl$ used by \cite{MellorYamada82} (see \sect{sec:lengthscaleeq}
! and \sect{sec:dissipationeq}).
!  Some examples are compiled in \tab{tab:psi}.
! \begin{table}[ht]
!   \begin{center}
!     \begin{tabular}{clccc}
!       $\psi$     & two-equation model by:      &  $p$   & $m$           & $n$    \\[2mm]  \hline
!       $\omega$   & \cite{Wilcox88}             &  $-1$ & $\frac{1}{2}$  & $-1$   \\[1mm]
!       $k l$      & \cite{MellorYamada82}       &  $0$  & $1$            & $1$    \\[1mm]
!       $\epsilon$ & \cite{Rodi87}               &  $3$  & $\frac{3}{2}$  & $-1$   \\[1mm]
!       $k \tau$   & \cite{ZeiermanWolfshtein86} &  $-3$ & $\frac{1}{2}$  & $1$    \\
!     \end{tabular}
!    \caption{\label{tab:psi}Exponents $p$, $n$, $m$ defined in  \eq{psi_l}, and
!     their relation to the variable of the second equation in some well-known
!     two-equation models.}
!  \end{center}
!\end{table}
!
! The transport equation for $\psi$ can written as
! \begin{equation}
!   \label{generic}
!   \dot{\psi} = {\cal D}_\psi
!   + \frac{\psi}{k} (  c_{\psi_1} P + c_{\psi_3} G
!                     + c_{\psi x} P_x
!                     + c_{\psi 4} P_s
!    - c_{\psi 2} \epsilon )
!   \comma
! \end{equation}
! where $\dot{\psi}$ denotes the material derivative of $\psi$,
! see \cite{UmlaufBurchard2003}.
! The production terms $P$ and $G$ follow from \eq{PandG}.
! $P_s$ is Stokes shear production defined in \eq{computePs}
! and $P_x$ accounts for extra turbulence production.
! ${\cal D}_\psi$ represents the sum of the viscous and turbulent
! transport terms. The rate of dissipation can computed by solving
! \eq{psi_l} for $l$ and inserting the result into \eq{epsilon}.
!
! For horizontally homogeneous flows, the transport terms ${\cal D}_\psi$
! appearing in \eq{generic} are expressed by a simple
! gradient formulation,
! \begin{equation}
!   \label{diffusionGeneric}
!   {\cal D}_\psi = \frstder{z}
!   \left( \dfrac{\nu_t}{\sigma_\psi} \partder{\psi}{z} \right)
!  \point
! \end{equation}
!
! For appropriate choices of the parameters, most of the classic transport
! equations can be directly recovered from the generic equation \eq{generic}.
! An example is the transport equation for the inverse turbulent time scale,
! $\omega \propto \epsilon / k$, which has been formulated by \cite{Wilcox88}
! and extended to buoyancy affected flows by \cite{Umlaufetal2003}. The precise
! definition of $\omega$ follows from \tab{tab:psi}, and its transport
! equation can be written as
! \begin{equation}
!   \label{KW}
!   \dot{\omega}
!   =
!   {\cal D}_\omega
!   + \frac{\omega}{k} (  c_{\omega_1} P + c_{\omega_3} G
!   - c_{\omega 2} \epsilon )
!   \comma
! \end{equation}
! which is clearly a special case of \eq{generic}. Model constants for this
! and other traditional models are given in \tab{tab:constants}.
! \begin{table}[ht]
!   \begin{center}
!     \begin{tabular}{lccccccc}
!       & $c_\mu^0$
!       & $\sigma_k^\psi$
!       & $\sigma_\psi$
!       & $c_{\psi 1}$
!       & $c_{\psi 2}$
!       & $c_{\psi 3}$  \\[2mm] \hline
!       $k$-$\epsilon$,  \cite{Rodi87}             :
!       & $0.5477$ & $1.0$  &  $1.3$  & $1.44$  & $1.92$  & (see eq.\ (\ref{Ri_st})) \\[1mm]
!       $k$-$kl$,       \cite{MellorYamada82}      :
!       & $0.5544$ & $1.96$ &  $1.96$ & $0.9$   & $0.5$   & $0.9$ &    \\[1mm]
!       $k$-$\omega$,   \cite{Wilcox88}            :
!       & $0.5477$ & $2$    &  $2$    & $0.555$ & $0.833$ & (see eq.\ (\ref{Ri_st})) \\[1mm]
!       $k$-$\tau$     \cite{ZeiermanWolfshtein86}:
!        & $0.5477$ & $1.46$ &  $10.8$ & $0.173$ & $0.225$ & (---)      \\
!     \end{tabular}
!     \caption{\label{tab:constants} Model constants of some standard models,
! converted to the notation used here. The Schmidt-numbers for the model of
!    \cite{MellorYamada82} are valid only in the logarithmic boundary-layer,
!    because the diffusion models \eq{diffusionMYTKE} and \eq{diffusionMYlength}
!    are slightly different from \eq{diffusionTKE} and \eq{diffusionGeneric}.
!    There is no indication that one class of  diffusion models is superior.}
!   \end{center}
! \end{table}
! Apart from having to code only one equation to recover all of the
! traditional models, the main advantage of the generic equation is its
! flexibility. After choosing meaningful values for physically relevant
! parameters like  the von K{\'a}rm{\'a}n constant, $\kappa$, the temporal
! decay rate for homogeneous turbulence, $d$, some parameters related to
! breaking surface waves, etc, a two-equation model can be generated,
! which has exactly the required properties. This is discussed in
! great detail in  \cite{UmlaufBurchard2003}. All algorithms have been
! implemented in GOTM and are described in \sect{sec:generate}.
!
! !USES:
   use turbulence, only: P,B,Px,PSTK,num
   use turbulence, only: tke,tkeo,k_min,eps,eps_min,L
   use turbulence, only: cpsi1,cpsi2,cpsi3plus,cpsi3minus,cpsix,cpsi4,sig_psi
   use turbulence, only: gen_m,gen_n,gen_p
   use turbulence, only: cm0,cde,galp,length_lim
   use turbulence, only: psi_bc, psi_ubc, psi_lbc, ubc_type, lbc_type
   use util,       only: Dirichlet,Neumann

   IMPLICIT NONE

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
!  Original author(s): Lars Umlauf and Hans Burchard
!
!EOP
!------------------------------------------------------------------------
!
! !LOCAL VARIABLES:
   REALTYPE                  :: DiffPsiup,DiffPsidw,pos_bc
   REALTYPE                  :: prod,buoyan,diss
   REALTYPE                  :: prod_pos,prod_neg,buoyan_pos,buoyan_neg
   REALTYPE                  :: ki,epslim,PsiOverTke,NN_pos
   REALTYPE                  :: cnpar=_ONE_
   REALTYPE                  :: exp1,exp2,exp3
   REALTYPE                  :: psi(0:nlev)
   REALTYPE                  :: avh(0:nlev)
   REALTYPE                  :: Lsour(0:nlev),Qsour(0:nlev)
   REALTYPE                  :: cpsi3

   integer                   :: i
!
!------------------------------------------------------------------------
!BOC

!  compute some parameters
   exp1 = 3.0 + gen_p/gen_n
   exp2 = 1.5 + gen_m/gen_n
   exp3 =       - 1.0/gen_n

!  re-construct psi at "old" timestep
   do i=0,nlev
      psi(i) = cm0**gen_p * tkeo(i)**gen_m * L(i)**gen_n
   end do

!  compute RHS
   do i=1,nlev-1

!     compute psi diffusivity
      avh(i) = num(i)/sig_psi

!     compute production terms in psi-equation
      if (B(i).gt.0) then
         cpsi3=cpsi3plus
      else
         cpsi3=cpsi3minus
      end if

!     compute production terms in psi-equation
      PsiOverTke  = psi(i)/tkeo(i)
      prod        = PsiOverTke * ( cpsi1*P(i) + cpsix*Px(i) + cpsi4*PSTK(i) )
      buoyan      = cpsi3*PsiOverTke*B(i)
      diss        = cpsi2*PsiOverTke*eps(i)

      if (prod+buoyan.gt.0) then
         Qsour(i)  = prod+buoyan
         Lsour(i) = -diss/psi(i)
      else
         Qsour(i)  = prod
         Lsour(i) = -(diss-buoyan)/psi(i)
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
   DiffPsiup  = psi_bc(psi_ubc,ubc_type,pos_bc,ki,z0s,u_taus)


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
   DiffPsidw  = psi_bc(psi_lbc,lbc_type,pos_bc,ki,z0b,u_taub)


!  do diffusion step
   call diff_face(nlev,dt,cnpar,h,psi_ubc,psi_lbc,                          &
                  DiffPsiup,DiffPsidw,avh,Lsour,Qsour,psi)


!  fill top and bottom value with something nice
!  (only for output)
   psi(nlev)  = psi_bc(Dirichlet,ubc_type,z0s,tke(nlev),z0s,u_taus)
   psi(0   )  = psi_bc(Dirichlet,lbc_type,z0b,tke(0   ),z0b,u_taub)

   do i=0,nlev
!     recover dissipation rate from k and psi
      eps(i)=cm0**exp1 * tke(i)**exp2 * psi(i)**exp3

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
!     compute dissipative scale
      L(i) = cde * sqrt( tke(i) * tke(i) * tke(i) ) / eps(i)
   end do


   return
   end subroutine genericeq
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The generic $\psi$--equation  \label{sec:genericeq}
! 
! !INTERFACE:
   subroutine genericeq(N,dt,u_taus,u_taub,z0s,z0b,h,P,B,NN,num)

! !DESCRIPTION:
! This model has been formulated by \cite{UmlaufBurchard2003},
! who introduced a `generic' variable,
! \begin{equation}
!   \label{psi_l}
!   \psi = (c_\mu^0)^p k^m l^n
!   \comma
! \end{equation}
! where $k$ is the turbulent kinetic energy computed from \eq{tkeA} and
! $l$ is the dissipative length--scale defined in \eq{epsilon}.
! For appropriate choices of the exponents $p$, $m$, and $n$, the variable
! $\psi$ can be directly identified with the classic length--scale determining
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
!     their relation to the variable of the second equation in some well--known
!     two--equation models.}
!  \end{center}
!\end{table}
!
! The transport equation for $\psi$ can written as
! \begin{equation}
!   \label{generic}
!   \dot{\psi} = {\cal D}_\psi
!   + \frac{\psi}{k} (  c_{\psi_1} P + c_{\psi_3} B 
!    - c_{\psi 2} \epsilon )   
!   \comma
! \end{equation}
! where $\dot{\psi}$ denotes the material derivative of $\psi$, 
! see \cite{UmlaufBurchard2003}.
! The production terms $P$ and $B$ follow from \eq{PandG}. 
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
!   + \frac{\omega}{k} (  c_{\omega_1} P + c_{\omega_3} B 
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
! converted to the notation used here. The Schmidt--numbers for the model of
!    \cite{MellorYamada82} are valid only in the logarithmic boundary--layer, 
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
! breaking surface waves, etc, a two--equation model can be generated, 
! which has exactly the required properties. This is discussed in
! great detail in  \cite{UmlaufBurchard2003}. All algorithms have been
! implemented in GOTM and are described in \sect{sec:generate}.
! 
! !USES:
   use mTridiagonal
   use turbulence, only: cpsi1,cpsi2,cpsi3plus,cpsi3minus,sig_psi
   use turbulence, only: gen_m,gen_n,gen_p
   use turbulence, only: tkeo,tke,k_min,eps,eps_min,L
   use turbulence, only: cm0,cde,galp,length_lim
   use turbulence, only: psi_bc, psi_ubc, psi_lbc, ubc_type, lbc_type
   IMPLICIT NONE

! !INPUT PARAMETERS:
   integer, intent(in)       :: N
   REALTYPE, intent(in)      :: dt
   REALTYPE, intent(in)      :: u_taus,u_taub,z0s,z0b
   REALTYPE, intent(in)      :: h(0:N)
   REALTYPE, intent(in)      :: P(0:N),B(0:N),NN(0:N)
   REALTYPE, intent(in)      :: num(0:N)
!
! !DEFINED PARAMETERS:
!  boundary conditions 
   integer, parameter        :: Dirichlet=0
   integer, parameter        :: Neumann=1
   integer, parameter        :: viscous=0
   integer, parameter        :: logarithmic=1
   integer, parameter        :: injection=2
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf, Hans Burchard
!
!  $Log: genericeq.F90,v $
!  Revision 1.3  2003-03-10 09:02:05  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: psi(0:N)
   REALTYPE                  :: avh(0:N)
   REALTYPE                  :: pminus(0:N),pplus(0:N)
   REALTYPE                  :: prod,buoyan,diss,epslim
   REALTYPE                  :: cpsi3
   REALTYPE                  :: bc_tmp
   REALTYPE                  :: ki
!
!------------------------------------------------------------------------
!BOC
! compute diffusivities at levels of the mean variables
   do i=2,N-1
      avh(i)=0.5/sig_psi*(num(i-1)+num(i))
   end do

! for Neumann boundary conditions set the boundary fluxes preliminary to zero
   if (psi_ubc.eq.Neumann) then
      avh(N)=0
   end if

   if (psi_lbc.eq.Neumann) then
      avh(1)=0
   end if

! construct the variable psi at the old timestep
   do i=0,N
      psi(i) = cm0**gen_p * tkeo(i)**gen_m * L(i)**gen_n 
   end do

! prepare the production terms
   do i=N-1,1,-1

      if (B(i).gt.0) then
         cpsi3=cpsi3plus 
      else
         cpsi3=cpsi3minus 
      end if

      prod   = cpsi1*psi(i)/tkeo(i)*P(i)
      buoyan = cpsi3*psi(i)/tkeo(i)*B(i)
      diss   = cpsi2*psi(i)/tkeo(i)*eps(i)

      if (prod+buoyan.gt.0) then
         pplus(i)  = prod+buoyan
         pminus(i) = diss
      else
         pplus(i)  = prod
         pminus(i) = diss-buoyan
      end if
   end do

! construct the matrix
   do i=1,N-1
      au(i) = -2.*dt*avh(i)/(h(i)+h(i+1))/h(i)
      cu(i) = -2.*dt*avh(i+1)/(h(i)+h(i+1))/h(i+1)
      bu(i) =  1.-au(i)-cu(i)+pminus(i)*dt/psi(i)
      du(i) = (1+pplus(i)*dt/psi(i))*psi(i)
   end do

   
! impose upper boundary conditions
   if (psi_ubc.eq.Neumann) then
      ! value of k at the top cell
      ki      = 0.5*(tke(N-1)+tke(N))
      ! compute the BC
      bc_tmp  = psi_bc(Neumann,ubc_type,0.5*h(N),ki,z0s,u_taus)
      ! insert the BC into system
      du(N-1) = du(N-1)+bc_tmp*dt/(0.5*(h(N)+h(N-1)))
   else
      ! prepare matrix 
      bu(N-1) = 1.
      au(N-1) = 0.
      ! value of k at the top cell
      ki      = tke(N-1)
      ! compute the BC
      bc_tmp  = psi_bc(Dirichlet,ubc_type,h(N),ki,z0s,u_taus)
      ! insert the BC into system
      du(N-1) = bc_tmp
   end if

! impose lower boundary conditions
   if (psi_lbc.eq.Neumann) then
      ! value of k at the bottom cell
      ki     = 0.5*(tke(1)+tke(2))
      ! compute the BC
      bc_tmp  = psi_bc(Neumann,lbc_type,0.5*h(1),ki,z0b,u_taub)
      ! insert the BC into system
      du(1)   = du(1)+bc_tmp*dt/(0.5*(h(1)+h(2)))
   else
      ! prepare matrix
      cu(1)   = 0.
      bu(1)   = 1.
 
      ! value of k at the bottom cell
      ki      = tke(1)
      ! compute the BC
      bc_tmp  = psi_bc(Dirichlet,lbc_type,h(1),ki,z0b,u_taub)
      ! insert the BC into system
      du(1)   = bc_tmp
   end if

   ! solve the system
   call tridiagonal(N,1,N-1,psi)

   !overwrite the uppermost value
   psi(N)  = psi_bc(Dirichlet,ubc_type,z0s,tke(N),z0s,u_taus)
   !overwrite the lowest value
   psi(0)  = psi_bc(Dirichlet,lbc_type,z0b,tke(0),z0b,u_taub)



! finish
   do i=0,N

!     recover the length scale and the dissipation rate
      L(i)=( psi(i) * cm0**(-gen_p) * tke(i)**(-gen_m) )**(1./gen_n)
      eps(i)=cde*sqrt(tke(i)*tke(i)*tke(i))/L(i)
      
      ! substitute minimum value
      if ((NN(i).gt.0).and.(length_lim)) then
         epslim = cde/sqrt(2.)/galp*tke(i)*sqrt(NN(i))
      else
         epslim = eps_min
      endif

      if (eps(i).lt.epslim) then
         eps(i) = epslim
           L(i) = cde*sqrt(tke(i)*tke(i)*tke(i))/epslim
      endif
   enddo

   return
   end subroutine genericeq
!EOC

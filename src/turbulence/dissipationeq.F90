#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The dynamic $\epsilon$--equation \label{sec:dissipationeq}
! 
! !INTERFACE:
   subroutine dissipationeq(N,dt,u_taus,u_taub,z0s,z0b,  &
                            h,P,B,NN,num)

! !DESCRIPTION:
! The $k$-$\epsilon$ model in its form suggested by \cite{Rodi87} has been
! implemented in GOTM.
! In this model, the rate of dissipation is balanced according to
! \begin{equation}
!   \label{dissipation}
!   \dot{\epsilon}
!   =
!   {\cal D}_\epsilon
!   + \frac{\epsilon}{k} ( c_{\epsilon 1} P + c_{\epsilon 3} B 
!                        - c_{\epsilon 2} \epsilon ) 
!   \comma
! \end{equation}
! where $\dot{\epsilon}$ denotes the material derivative of $\epsilon$.
! The production terms $P$ and $B$ follow from \eq{PandG} and 
! ${\cal D}_\epsilon$ represents the sum of the viscous and turbulent 
! transport terms.
!
! For horizontally homogeneous flows, the transport term ${\cal D}_\epsilon$
! appearing in \eq{dissipation} is presently expressed by a simple
! gradient formulation,
! \begin{equation}
!   \label{diffusionEps}
!   {\cal D}_\epsilon = \frstder{z} 
!    \left( \dfrac{\nu_t}{\sigma_\epsilon} \partder{\epsilon}{z} \right)
!  \comma
! \end{equation}
! where $\sigma_\epsilon$ is the constant Schmidt--number for $\epsilon$.
! 
! It should be pointed out that not all authors retain the buoyancy term
! in \eq{dissipation}, see e.g.\ \cite{GibsonLaunder76}.  Similar to the
! model of \cite{MellorYamada82}, \cite{Craftetal96a} set 
! $c_{\epsilon 1}=c_{\epsilon 3}$. 
! However, in both cases, the $k$--$\epsilon$ model cannot
! predict a proper state of full equilibrium in stratified flows at a
! predefined value of the Richardson number (see
! \cite{Umlaufetal2003} and discussion around \eq{Ri_st}). Model constants are 
! summarised in \tab{tab:KE_constants}.
! \begin{table}[ht]
!   \begin{center}
! \begin{tabular}{cccccc} 
!     & $c_\mu^0$ & $\sigma_k$  & $\sigma_\epsilon$ 
!     & $c_{\epsilon 1}$ & $c_{\epsilon 2}$  \\[1mm] \hline
!     \cite{Rodi87} & $0.5577$ & $1.0$ &  $1.3$ & $1.44$ & $1.92$ \\
!   \end{tabular}
!   \caption{\label{tab:KE_constants} Constants appearing in 
!    \eq{dissipation} and \eq{epsilon}.}
!   \end{center}
! \end{table}
! 
! At the end of this routine the length-scale can be constrained according to a 
! suggestion of \cite{Galperinetal88}. This feature is optional and can be activated
! by setting {\tt length\_lim = .true.} in {\tt gotmturb.inp}.
! 
! !USES:
   use mTridiagonal
   use turbulence, ONLY: ce1,ce2,ce3plus,ce3minus,sig_e
   use turbulence, only: tkeo,tke,k_min,eps,eps_min,L
   use turbulence, ONLY: cm0,cde,galp,length_lim,galp
   use turbulence, only: epsilon_bc, psi_ubc, psi_lbc, ubc_type, lbc_type
   use turbulence, only: sig_e0,sig_peps
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: N
   REALTYPE, intent(in) :: dt
   REALTYPE, intent(in) :: u_taus,u_taub,z0s,z0b
   REALTYPE, intent(in) :: h(0:N)
   REALTYPE, intent(in) :: P(0:N),B(0:N),NN(0:N)
   REALTYPE, intent(in) :: num(0:N)
!
! !DEFINED PARAMETERS:
!  boundary conditions 
   integer, parameter   :: Dirichlet=0
   integer, parameter   :: Neumann=1
   integer, parameter   :: viscous=0
   integer, parameter   :: logarithmic=1
   integer, parameter   :: injection=2
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding,
!                      Lars Umlauf
!
!  $Log: dissipationeq.F90,v $
!  Revision 1.3  2003-03-10 09:02:04  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE             :: avh(0:N)
   REALTYPE             :: pminus(0:N),pplus(0:N)
   REALTYPE             :: peps,sig_eff(0:N)
   REALTYPE             :: prod,buoyan,diss,epslim
   REALTYPE             :: ce3
   REALTYPE             :: bc_tmp
   REALTYPE             :: ki
   integer              :: i
!
!------------------------------------------------------------------------
!BOC
!  Determination of the turbulent Schmidt number for the Craig & Banner (1994)
!  parameterisation for breaking surface waves suggested by Burchard (2001):

   if (sig_peps) then          ! With wave breaking
      sig_eff(N)=sig_e0
      do i=1,N-1
         peps=(P(i)+B(i))/eps(i)
         if (peps .gt. 1.) peps=1.
         sig_eff(i)=peps*sig_e+(1.-peps)*sig_e0
      end do
      sig_eff(0)=sig_e
   else                        ! No wave breaking
      sig_eff=sig_e
   end if

! compute diffusivities at levels of the mean variables
   do i=2,N-1
      avh(i)=0.5*(num(i-1)/sig_eff(i-1)+num(i)/sig_eff(i))
   end do

! for Neumann boundary conditions set the boundary fluxes preliminary to zero
   if (psi_ubc.eq.Neumann) then
      avh(N)=0
   end if

   if (psi_lbc.eq.Neumann) then
      avh(1)=0
   end if

! prepare the production terms
   do i=N-1,1,-1

      if (B(i).gt.0) then
         ce3=ce3plus 
      else
         ce3=ce3minus 
      end if

      prod   = ce1*eps(i)/tkeo(i)*P(i)
      buoyan = ce3*eps(i)/tkeo(i)*B(i)
      diss   = ce2*eps(i)*eps(i)/tkeo(i)

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
      bu(i) =  1.-au(i)-cu(i)+pminus(i)*dt/eps(i)
      du(i) = (1+pplus(i)*dt/eps(i))*eps(i)
   end do

! impose upper boundary conditions
   if (psi_ubc.eq.Neumann) then
      ki      = 0.5*(tke(N-1)+tke(N))! value of k at the top cell
      bc_tmp  = epsilon_bc(Neumann,ubc_type,0.5*h(N),ki,z0s,u_taus)
      ! compute the BC
      du(N-1) = du(N-1)+bc_tmp*dt/(0.5*(h(N)+h(N-1)))
      ! insert the BC into system
   else
      bu(N-1) = 1.
      ! prepare matrix 
      au(N-1) = 0.
      ! prepare matrix 

      ! value of k at the top cell
      ki      = tke(N-1)
      ! compute the BC
      bc_tmp  = epsilon_bc(Dirichlet,ubc_type,h(N),ki,z0s,u_taus)
      ! insert the BC into system
      du(N-1) = bc_tmp
   end if

!  impose lower boundary conditions
   if (psi_lbc.eq.Neumann) then
      ! value of k at the bottom cell
      ki = 0.5*(tke(1)+tke(2))
      !compute the BC
      bc_tmp  = epsilon_bc(Neumann,lbc_type,0.5*h(1),ki,z0b,u_taub)
      ! insert the BC into system
      du(1) = du(1)+bc_tmp*dt/(0.5*(h(1)+h(2)))
   else
      ! prepare matrix
      cu(1) = 0.
      bu(1) = 1.
      ! value of k at the bottom cell
      ki = tke(1)
      ! compute the BC
      bc_tmp = epsilon_bc(Dirichlet,lbc_type,h(1),ki,z0b,u_taub)
      ! insert the BC into system
      du(1) = bc_tmp
   end if

   ! solve the system
   call tridiagonal(N,1,N-1,eps)

   ! overwrite the uppermost value
   eps(N)  = epsilon_bc(Dirichlet,ubc_type,z0s,tke(N),z0s,u_taus)
   ! overwrite the lowest value
   eps(0)  = epsilon_bc(Dirichlet,lbc_type,z0b,tke(0),z0b,u_taub)


! finish
   do i=0,N

!     limit dissipation rate under stable stratification,
!     see Galperin et al. (1988)  
      if ((NN(i).gt.0).and.(length_lim)) then
         epslim = cde/sqrt(2.)/galp*tke(i)*sqrt(NN(i))
      else
         epslim = eps_min
      endif

     ! compute length scale and clip
      if (eps(i).lt.epslim) then
         eps(i) = epslim
      endif

      L(i) = cde*sqrt(tke(i)*tke(i)*tke(i))/eps(i)
   end do

   return
   end subroutine dissipationeq
!EOC

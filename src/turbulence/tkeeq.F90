#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The dynamic $k$--equation \label{sec:tkeeq} 
! 
! !INTERFACE:
   subroutine tkeeq(N,dt,u_taus,u_taub,z0s,z0b,h,P,B,num)

! !DESCRIPTION: 
! The transport equation for the turbulent kinetic energy, $k$, 
! follows immediately from the contraction of the Reynolds--stress
! tensor. In the case of a Boussinesq--fluid, this equation can
! be written as
! \begin{equation}
!   \label{tkeA}
!   \dot{k}
!   = 
!   {\cal D}_k +  P + B  - \epsilon 
!   \comma
! \end{equation}
! where $\dot{k}$ denotes the material derivative of $k$. $P$ and $B$ are
! the production of $k$ by mean shear and buoyancy, respectively, and
! $\epsilon$ the rate of dissipation. ${\cal D}_k$ represents the sum of
! the viscous and turbulent transport terms.
! For horizontally homogeneous flows, the transport term ${\cal D}_k$
! appearing in \eq{tkeA} is presently expressed by a simple
! gradient formulation,
! \begin{equation}
!   \label{diffusionTKE}
!   {\cal D}_k = \frstder{z} \left( \dfrac{\nu_t}{\sigma_k} \partder{k}{z} \right)
!  \comma
! \end{equation}
! where $\sigma_k$ is the constant Schmidt--number for $k$.
! 
! In horizontally homogeneous flows, the shear and the buoyancy
! production, $P$ and $B$, can be written as
! \begin{equation}
!   \label{PandG}
!   \begin{array}{rcl}
!   P &=& - \mean{u'w'} \partder{u}{z} - \mean{v'w'} \partder{v}{z}  \comma \\[3mm]  
!   B &=&  g \alpha \mean{w' \theta'}                                \point
!   \end{array}
! \end{equation}
! There computation is discussed in \sect{sec:production}.
!
! The rate of dissipation, $\epsilon$, can be either obtained directly
! from its parameterised transport equation as discussed in 
! \sect{sec:dissipationeq}, or from any other model yielding
! an appropriate description of the dissipative length-scale, $l$.
! Then, $\epsilon$ follows from the well--known cascading relation
! of turbulence,
! \begin{equation}
!   \label{epsilon}
!   \epsilon = (c_\mu^0)^3 \frac{k^{\frac{3}{2}}}{l}
!   \comma
! \end{equation}
! where $c_\mu^0$ is a constant of the model.
! 
! !USES:
   use mTridiagonal
   use turbulence, only: tkeo,tke,k_min,eps
   use turbulence, only: k_bc, k_ubc, k_lbc, ubc_type, lbc_type
   use turbulence, only: sig_k
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: N
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: u_taus,u_taub,z0s,z0b
   REALTYPE, intent(in)                :: h(0:N)
   REALTYPE, intent(in)                :: P(0:N),B(0:N)
   REALTYPE, intent(in)                :: num(0:N)
!
! !DEFINED PARAMETERS:
!  boundary conditions 
   integer, parameter                  :: Dirichlet=0
   integer, parameter                  :: Neumann=1
   integer, parameter                  :: viscous=0
   integer, parameter                  :: logarithmic=1
   integer, parameter                  :: injection=2
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding,
!                      Lars Umlauf
!
!  $Log: tkeeq.F90,v $
!  Revision 1.4  2003-03-10 09:02:06  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: avh(0:N)
   REALTYPE                  :: pminus(0:N),pplus(0:N)
   REALTYPE                  :: prod,buoyan,diss
   integer                   :: i
   REALTYPE                  :: bc_tmp
!
!------------------------------------------------------------------------
!BOC
! save old time step (needed for the length-scale equation)
   tkeo=tke

! compute diffusivities at levels of the mean variables
   do i=2,N-1
      avh(i)=0.5/sig_k*(num(i-1)+num(i))
   end do

! for Neumann boundary conditions set the boundary fluxes preliminary to zero
   if (k_ubc.eq.Neumann) then
      avh(N)=0
   end if

   if (k_lbc.eq.Neumann) then
      avh(1)=0
   end if

! prepare the production terms
   do i=N-1,1,-1
      prod   = P(i)
      buoyan = B(i)
      diss   = eps(i)
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
      bu(i) =  1.-au(i)-cu(i)+pminus(i)*dt/tke(i)
      du(i) = (1+pplus(i)*dt/tke(i))*tke(i)
   end do

! impose upper boundary conditions
   if (k_ubc.eq.Neumann) then
      ! compute the BC
      bc_tmp  = k_bc(Neumann,ubc_type,0.5*h(N),z0s,u_taus)
      ! insert the BC into system
      du(N-1) = du(N-1)+bc_tmp*dt/(0.5*(h(N)+h(N-1)))
   else
      ! prepare matrix 
      bu(N-1) = 1.
      au(N-1) = 0.
      ! compute the BC
      bc_tmp  = k_bc(Dirichlet,ubc_type,h(N),z0s,u_taus)
      ! insert the BC into system
      du(N-1) = bc_tmp
   end if

! impose lower boundary conditions
   if (k_lbc.eq.Neumann) then
      ! compute the BC            
      bc_tmp  = k_bc(Neumann,lbc_type,0.5*h(1),z0b,u_taub)
      ! insert the BC into system 
      du(1)   = du(1)+bc_tmp*dt/(0.5*(h(1)+h(2)))
   else
      ! prepare matrix        
      cu(1)   = 0.
      bu(1)   = 1.
      ! compute the BC
      bc_tmp  = k_bc(Dirichlet,lbc_type,h(1),z0b,u_taub)
      ! insert the BC into system
      du(1)   = bc_tmp
   end if

   ! solve the system
   call tridiagonal(N,1,N-1,tke)

   ! overwrite the uppermost value
   tke(N)  = k_bc(Dirichlet,ubc_type,z0s,z0s,u_taus)
   ! overwrite the lowest value
   tke(0)  = k_bc(Dirichlet,lbc_type,z0b,z0b,u_taub)

   ! substitute minimum value
   where (tke .lt. k_min) tke = k_min

   return
   end subroutine tkeeq
!EOC

#$Id: q2over2eq.F90,v 1.1 2003-03-10 09:00:36 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The dynamic $q^2/2$--equation \label{sec:q2over2eq} 
! 
! !INTERFACE:
   subroutine q2over2eq(N,dt,u_taus,u_taub,z0s,z0b,h,P,B)

! !DESCRIPTION:
! The transport equation for the TKE $q^2/2=k$ can be written as
! \begin{equation}
!   \label{tkeB}
!   \dot{\overline{q^2/2}}
!   = 
!   {\cal D}_q +  P + B  - \epsilon 
!   \comma
! \end{equation}
! where $\dot{\overline{q^2/2}}$ denotes the material derivative of $q^2/2$. 
! With $P$ and $B$ following from \eq{PandG}, evidently, this equation is
! formally identical to \eq{tkeA}. The only reason why it is discretized
! seperately here, is the slightly different down--gradient model for the
! transport term, 
! \begin{equation}
!   \label{diffusionMYTKE}
!   {\cal D}_q = \frstder{z} \left( q l S_q \partder{q^2/2}{z} \right)
!  \comma
! \end{equation}
! where $S_q$ is a model constant. The notation has been chosen according
! to that introduced by \cite{MellorYamada82}. Using their notation, 
! also \eq{epsilon} can be expressed in mathematically identical from 
! as
! \begin{equation}
!   \label{epsilonMY}
!   \epsilon = \frac{q^3}{B_1 l}
!   \comma
! \end{equation}
! where $B_1$ is a constant of the model. Note, that the equivalence of
! \eq{epsilon} and \eq{epsilonMY} require
! \begin{equation}
!   \label{B1}
!   (c_\mu^0)^{-2} = \frac{1}{2} B_1^\frac{2}{3}
!   \point
! \end{equation}
!
! !USES:
   use mTridiagonal
   use turbulence, only: tkeo,tke,eps,L
   use turbulence, only: k_min
   use turbulence, only: q2over2_bc, k_ubc, k_lbc, ubc_type, lbc_type
   use turbulence, only: sq
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: N
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: u_taus,u_taub,z0s,z0b
   REALTYPE, intent(in)                :: h(0:N)
   REALTYPE, intent(in)                :: P(0:N),B(0:N)
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
!  Original author(s): Lars Umlauf
!
!  $Log: q2over2eq.F90,v $
!  Revision 1.1  2003-03-10 09:00:36  gotm
!  Part of new generic turbulence model
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
      avh(i) = 0.5*sq*( sqrt(2.*tke(i-1))*L(i-1) + sqrt(2.*tke(i))*L(i) )
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
      bc_tmp  = q2over2_bc(Neumann,ubc_type,0.5*h(N),z0s,u_taus)
      ! insert the BC into system
      du(N-1) = du(N-1)+bc_tmp*dt/(0.5*(h(N)+h(N-1)))
   else
      ! prepare matrix 
      bu(N-1) = 1.
      au(N-1) = 0.
      ! compute the BC
      bc_tmp  = q2over2_bc(Dirichlet,ubc_type,h(N),z0s,u_taus)
      ! insert the BC into system
      du(N-1) = bc_tmp
   end if

! impose lower boundary conditions
   if (k_lbc.eq.Neumann) then
      ! compute the BC            
      bc_tmp  = q2over2_bc(Neumann,lbc_type,0.5*h(1),z0b,u_taub)
      ! insert the BC into system 
      du(1)   = du(1)+bc_tmp*dt/(0.5*(h(1)+h(2)))
   else
      ! prepare matrix        
      cu(1)   = 0.
      bu(1)   = 1.
      ! compute the BC
      bc_tmp  = q2over2_bc(Dirichlet,lbc_type,h(1),z0b,u_taub)
      ! insert the BC into system
      du(1)   = bc_tmp
   end if

   ! solve the system
   call tridiagonal(N,1,N-1,tke)

   ! overwrite the uppermost value
   tke(N)  = q2over2_bc(Dirichlet,ubc_type,z0s,z0s,u_taus)
   ! overwrite the lowest value
   tke(0)  = q2over2_bc(Dirichlet,lbc_type,z0b,z0b,u_taub)

   ! substitute minimum value
   where (tke .lt. k_min) tke = k_min

   return
   end subroutine q2over2eq
!EOC

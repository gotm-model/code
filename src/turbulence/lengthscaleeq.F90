!$Id: lengthscaleeq.F90,v 1.3 2003-03-10 09:02:05 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The dynamic $q^2l$--equation  \label{sec:lengthscaleeq}
! 
! !INTERFACE:
   subroutine lengthscaleeq(N,dt,u_taus,u_taub,z0s,z0b,   &
                            h,depth,P,B,NN) 
!
! !DESCRIPTION:
! Following suggestions of \cite{Rotta51a}, \cite{MellorYamada82}
! proposed an equation for the product $q^2 l$ expressed by
! \begin{equation}
!   \label{MY}
!   \dot{\overline{q^2 l}}
!   = {\cal D}_l + l ( E_1  P + E_3 B - E_2  F \epsilon ) 
!   \comma
! \end{equation}
! where $\dot{\overline{q^2 l}}$ denotes the material derivative of $q^2 l$.
! The production terms $P$ and $B$ follow from \eq{PandG}, and $\epsilon$
! can be computed either directly from \eq{epsilonMY}, or from \eq{epsilon}
! with the help \eq{B1}.
! 
! The so-called wall function, $F$, appearing in \eq{MY} is defined by
! \begin{equation}
!   \label{F}
!   F = 1 + E_2 \left( \dfrac{l}{\kappa {\cal L}_z} \right)^2
!   \comma
! \end{equation}
! $\kappa$ being the von K{\'a}rm{\'a}n constant and ${\cal L}_z$ some
! measure for the distance from the wall. Different possiblities 
! for  ${\cal L}_z$ are implemented in GOTM, which can be activated
! be setting the parameter {\tt MY\_length} in {\tt gotmturb.inp} to 
! appropriate values. Close to the wall, however, one always has
! ${\cal L}_z= \overline{z}$, where $\overline{z}$ is the distance from
! the wall.
!
! For horizontally homogeneous flows, the transport term ${\cal D}_l$
! appearing in \eq{MY} is expressed by a simple gradient formulation,
! \begin{equation}
!   \label{diffusionMYlength}
!   {\cal D}_l = \frstder{z} \left( q l S_l \partder{q^2 l}{z} \right)
!  \comma
! \end{equation}
! where $S_l$ is a constant of the model. The values for the model
! constants recommended by \cite{MellorYamada82} are displayed in 
! \tab{tab:MY_constants}. They can be set in {\tt gotmturb.inp}. Note, 
! that the parameter $E_3$ in stably stratifed flows is in principle
! a function of the so--called steady state Richardson--number,
! as discussed by \cite{Burchard2001c}, see discussion in the context
! of \eq{Ri_st}.
! \begin{table}[ht]
!   \begin{center}
! \begin{tabular}{ccccccc} 
!                           & $B_1$  & $S_q$ & $S_l$ & $E_1$ & $E_2$ & $E_3$    \\[1mm]
!      \hline
!     \cite{MellorYamada82} & $16.6$ & $0.2$ & $0.2$ & $1.8$ & $1.33$ & $1.8$\\
!   \end{tabular}
!   \caption{\label{tab:MY_constants} Constants appearing in \eq{MY}
!     and \eq{epsilonMY}}
!   \end{center}
! \end{table}
! 
! At the end of this routine the length-scale can be constrained according to a 
! suggestion of \cite{Galperinetal88}. This feature is optional and can be activated
! by setting {\tt length\_lim = .true.} in {\tt gotmturb.inp}.
! 
! !USES:
   use mTridiagonal
   use turbulence, ONLY: kappa,e1,e2,e3,b1
   use turbulence, only: tkeo,tke,k_min,eps_min,eps,L
   use turbulence, ONLY: MY_length,cm0,cde,galp,length_lim
   use turbulence, only: q2l_bc, psi_ubc, psi_lbc, ubc_type, lbc_type
   use turbulence, only: sl
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: n
   REALTYPE, intent(in)                :: dt,depth
   REALTYPE, intent(in)                :: u_taus,u_taub,z0s,z0b
   REALTYPE, intent(in)                :: h(0:N)
   REALTYPE, intent(in)                :: P(0:N),B(0:N),NN(0:N)
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
!  Original author(s): Hans Burchard, Karsten Bolding
!                      Lars Umlauf
!
!  $Log: lengthscaleeq.F90,v $
!  Revision 1.3  2003-03-10 09:02:05  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: avh(0:N),q2l(0:N),q3(0:N)
   REALTYPE                  :: pminus(0:N),pplus(0:N)
   REALTYPE                  :: Lz(0:N)
   REALTYPE                  :: prod,buoyan,diss,ds,db,Lcrit
   REALTYPE                  :: bc_tmp
   REALTYPE                  :: ki,l_min
!
!------------------------------------------------------------------------
!BOC

! compute lower bound for length scale
  l_min = cde*k_min**1.5/eps_min


! some quantities in Mellor-Yamada notation
   do i=1,N-1  
      q2l(i)=2.*tkeo(i)*L(i)
      q3 (i)=sqrt(8.*tke(i)*tke(i)*tke(i))
   end do

! compute diffusivities at levels of the mean variables
   do i=2,N-1
      avh(i) = 0.5*sl*( sqrt(2.*tke(i-1))*L(i-1) + sqrt(2.*tke(i))*L(i) )
   end do

! for Neumann boundary conditions set the boundary fluxes preliminary to zero
   if (psi_ubc.eq.Neumann) then
      avh(N)=0
   end if

   if (psi_lbc.eq.Neumann) then
      avh(1)=0
   end if

   ! Diagnostic Length Scale for wall proximity function
   db=0.0      
   ds=0.0
   do i=1,N-1
      db=db+h(i)
      ds=depth-db
      ! Parabola shape
      if (MY_length.eq.1) Lz(i)=kappa*(ds+z0s)*(db+z0b)/(ds+z0s+db+z0b)
      ! Triangle shape
      if (MY_length.eq.2) Lz(i)=kappa*min(ds+z0s,db+z0b)
      ! For infinite depth
      if (MY_length.eq.3) Lz(i)=kappa*(ds+z0s)
   end do

! prepare the production terms
   do i=1,N-1
      prod=e1*L(i)*P(i)
      buoyan=e3*L(i)*B(i)
      diss=-q3(i)/b1*(1.+e2*(L(i)/Lz(i))*(L(i)/Lz(i)))
      if (prod+buoyan .gt. 0) then
         pplus(i)=prod+buoyan
         pminus(i)=-diss
      else
         pplus(i)=prod
         pminus(i)=-buoyan-diss
      end if
   end do

! construct the matrix
   do i=1,N-1
      au(i)=-2.*dt*avh(i)/(h(i)+h(i+1))/h(i)
      cu(i)=-2.*dt*avh(i+1)/(h(i)+h(i+1))/h(i+1)
      bu(i)=1.-au(i)-cu(i)+pminus(i)*dt/q2l(i)
      du(i)=(1+pplus(i)*dt/q2l(i))*q2l(i)
   end do

! impose upper boundary conditions

   if (psi_ubc.eq.Neumann) then
      ! value of k at the top cell
      ki      = 0.5*(tke(N-1)+tke(N))
      ! compute the BC
      bc_tmp  = q2l_bc(Neumann,ubc_type,0.5*h(N),ki,z0s,u_taus)
      ! insert the BC into system
      du(N-1) = du(N-1)+bc_tmp*dt/(0.5*(h(N)+h(N-1)))
   else
      ! prepare matrix
      bu(N-1) = 1.
      au(N-1) = 0.

      ! value of k at the top cell
      ki      = tke(N-1)
      ! compute the BC
      bc_tmp  = q2l_bc(Dirichlet,ubc_type,h(N),ki,z0s,u_taus)
      ! insert the BC into system
      du(N-1) = bc_tmp
   end if

! impose lower boundary conditions

   if (psi_lbc .eq. Neumann) then
      ! value of k at the bottom cell
      ki      = 0.5*(tke(1)+tke(2))
      ! compute the BC
      bc_tmp  = q2l_bc(Neumann,lbc_type,0.5*h(1),ki,z0b,u_taub)
      ! insert the BC into system
      du(1)   = du(1)+bc_tmp*dt/(0.5*(h(1)+h(2)))
   else
     ! prepare matrix
     cu(1)   = 0.
     bu(1)   = 1.

     ! value of k at the bottom cell
     ki      = tke(1)
     ! compute the BC
     bc_tmp  = q2l_bc(Dirichlet,lbc_type,h(1),ki,z0b,u_taub)
     ! insert the BC into system
     du(1)   = bc_tmp
  end if

  ! solve the system
  call tridiagonal(N,1,N-1,q2l)

  ! overwrite the uppermost value
  q2l(N)  = q2l_bc(Dirichlet,ubc_type,z0s,tke(N),z0s,u_taus)
  ! overwrite the lowest value
  q2l(0)  = q2l_bc(Dirichlet,lbc_type,z0b,tke(0),z0b,u_taub)


  ! compute L and epsilon
  do i=0,N
     L(i)=q2l(i)/(2.*tke(i))

     ! apply the length-scale clipping of Galperin et al. (1988) 
     if ((NN(i).gt.0).and.(length_lim)) then
        Lcrit=sqrt(2*galp*galp*tke(i)/NN(i))
        if (L(i).gt.Lcrit) L(i)=Lcrit
     end if

     ! compute dissipation rate
     eps(i) = cde*sqrt(tke(i)*tke(i)*tke(i))/L(i)

     ! check for very small lengh scale
     if (L(i).lt.l_min) L(i)=l_min

     ! substitute minimum value
     if (eps(i).lt.eps_min) then
        eps(i) = eps_min
          L(i) = cde*sqrt(tke(i)*tke(i)*tke(i))/eps_min
     endif
  end do

  return
  end subroutine lengthscaleeq
!EOC

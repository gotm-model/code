!$Id: algebraiclength.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: 'Simple' algebraic length scales.
!
! !INTERFACE:
   subroutine algebraiclength(method,nlev,z0b,z0s,depth,h,NN) 
!
! !DESCRIPTION:
!  These subroutine computes the vertical profile of the mixing lengthscale
!  from an algebraic relation.
!
!  1) Parabola
!  \begin{equation}
!  l=\kappa \frac {l_b l_s} {l_b+l_s}
!  \end{equation}
!  2) Triangle
!  \begin{equation}
!  l=\kappa max(l_b,l_s)
!  \end{equation}
!  3) Distorted Parabola. Robert-Ouellet
!  \begin{equation}
!  l=\kappa l_b (1-\frac {l_b} {D})
!  \end{equation}
!  4) Xing, parabolic with exponential $d_b$
!  \begin{equation}
!  l_b=\kappa l_b e^{-\beta l_b}
!  \end{equation}
!  5) Blackadar (in the presence of two boundaries)
!  \begin{equation}
!  l=\kappa \frac {1} {\frac {1} {l_b}+\frac {1} {l_s}+\frac {1} {l_a}} \\
!  l_a=\gamma_0 \frac {\int_{0}^{D} k^{1/2} zdz} {\int_{0}^{D} k^{1/2} dz}
!  \end{equation}
!  6) see ispralength.f
!
!  At the end of the subroutine, the dissipation rate is calculated using:
!
!  \begin{equation}\label{DefDissip}
!  \varepsilon = (c_{\mu}^0)^3 \frac{k^{3/2}}{L}. 
!  \end{equation}
!
! !USES:
   use turbulence, ONLY: L,eps,tkeo,tke,L_min,eps_min,cde,galp,kappa,length_lim
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: method,nlev
   REALTYPE, intent(in)	:: z0b,z0s
   REALTYPE, intent(in)	:: depth,h(0:nlev),NN(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: algebraiclength.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer 		:: i
   REALTYPE 		:: ds,db,dbxing
   REALTYPE 		:: beta,gamma,La,La_up,La_down
   REALTYPE 		:: Lcrit

   integer, parameter	:: Parabola=1
   integer, parameter	:: Triangle=2
   integer, parameter	:: Xing=3
   integer, parameter	:: RobertOuellet=4
   integer, parameter	:: Blackadar=5
   integer, parameter	:: ispra_length=7
!
!EOP
!-----------------------------------------------------------------------
!BOC
   db=0.0
!
! Parabola shape
!
   select case (method)
      case(parabola)
         do i=1,nlev-1
            db=db+h(i)
            ds=depth-db
            L(i)=kappa*(ds+z0s)*(db+z0b)/(ds+db+z0b+z0s)
         end do
      case(triangle)
         do i=1,nlev-1
            db=db+h(i)
            ds=depth-db
            L(i)=kappa*min(ds+z0s,db+z0b)
         end do
!  
! Xing and Davies (1995). 
! Modification of parabolic mixing length. db changes:
!
      case(Xing)
         beta=-2.
         do i=1,nlev-1
           db=db+h(i)
           ds=depth-db
           dbxing=db*exp(-beta*db)
           L(i)=kappa*(ds+z0s)*(dbxing+z0b)/(ds+dbxing+z0s+z0b)
         end do
!
! Robert and Ouellet(1987). Similar to parabolic
!
      case(RobertOuellet)
         do i=1,nlev-1
            db=db+h(i)
            ds=depth-db
            L(i)= kappa*(db+z0b)*sqrt((ds+z0s)/(ds+db+z0b+z0s)) 
            L(i)= kappa*(db+z0b)*sqrt(1-db/(ds+db)) 
         end do
!
! Blackadar (1962). 
! In the form suggested by Luyten et al. (1996) for two boundary layers.
!
      case(Blackadar)
         La_up=0.
         La_down=0.
         do i=1,nlev-1
            db=db+h(i) 
            La_up=La_up+sqrt(tkeo(i))*(db+z0b)*h(i)
            La_down=La_down+sqrt(tkeo(i))*h(i) 
         end do
         gamma=0.2
         La=gamma*La_up/La_down
         db=0.0
         do i=1,nlev-1
            db=db+h(i)
            ds=depth-db
             L(i)=1/(1/(kappa*(ds+z0s))+1/(kappa*(db+z0b))+1/La)
         end do
!
!  Ispramix
!
      case(ispra_length)
         call ispralength(nlev,NN,h,depth) 
      case default
   end select

! Boundary conditions for L
!
   L(0)=kappa*z0b
   L(nlev)=kappa*z0s 
 
   do i=0,nlev
      if ((NN(i).gt.0).and.(length_lim)) then
         Lcrit=sqrt(2*galp*galp*tke(i)/NN(i))
         if (L(i).gt.Lcrit) L(i)=Lcrit
      end if
      if (L(i).lt.L_min) L(i)=L_min
      eps(i)=cde*sqrt(tke(i)*tke(i)*tke(i))/L(i)
      if (eps(i).lt.eps_min) eps(i)=eps_min
   end do  

   return
   end subroutine algebraiclength 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard & Karsten Bolding.

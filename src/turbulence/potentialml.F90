!$Id: potentialml.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include"cppdefs.h"
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: PotentialML - length sacle using 2 master lenghth scales
!
! !INTERFACE:
   subroutine potentialml(nlev,z0b,z0s,h,depth,NN)

! !DESCRIPTION:
!  Computes the length scale by defining two master 
!  length scales $l_u$ and $l_d$
!  \begin{equation}
!  \begin{array}{l}
!  \int_{z_0}^{z_0+l_u(z_0)} [b(z_0)-b(z)] dz =k(z_0) \\
!  \int_{z_0-l_d(z_0)}^{z_0} [b(z)-b(z_0)] dz =k(z_0)
!  \end{array}
!  \end{equation}
! 
!   From $l_u$ and $l_d$ two length scales are defined $l_k$ 
!   (characteristic mixing length)
!   and $l_\epsilon$ (characteristic dissipation length):
!   \begin{equation}
!   \begin{array}{l}
!   l_k(z_0)= min[l_d(z_0),l_u(z_0)] \\
!   l_{\epsilon}(z_0)={[l_d(z_0)l_u(z_0)]}^{1/2}
!   \end{array}
!   \end{equation}
! 
!   $l_k$ is used in kolpran() to compute eddy viscosity/difussivity  
!   (is transported as L()). $l_{\epsilon}$ is ed to compute $\epsilon$:
!   \begin{equation}
!   \epsilon=C_{\epsilon}k^{3/2}l_{\epsilon}^{-1}, with C_{\epsilon}=0.7
!   \end{equation}
!
! !USES:
   use turbulence, ONLY: L,eps,tkeo,tke,L_min,eps_min,cde,galp,kappa,length_lim
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev 
   REALTYPE, intent(in)	:: h(0:nlev),depth,NN(0:nlev)
   REALTYPE, intent(in)	:: z0b,z0s
!
! !OUTPUT PARAMETERS:
!
! !BUGS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: potentialml.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   REALTYPE		:: ds(0:nlev),db(0:nlev)
   REALTYPE		:: lu(0:nlev),ld(0:nlev)
   REALTYPE		:: lk(0:nlev),leps(0:nlev)
   REALTYPE, parameter	:: NNmin=1.e-8
   REALTYPE		:: Lcrit,buoydiff,integral,ceps
   integer 		:: i,j
!EOP
!-------------------------------------------------------------------------
!BOC
   db(0)=0.
   ds(nlev)=0.
   do i=1,nlev-1
      db(i)=db(i-1)+h(i)      ! distance of intercace i from bottom 
      ds(i)=depth-db(i)       ! distance of intercace i from surface 
   end do
!
!  Calculation of lu and ld by solving the integral equation following 
!  Gaspar (1990). Some other approximations of the integral equation 
!  are possible.
!
! Computation of lupward
!
   do i=1,nlev-1
      lu(i)=0.
      integral=0.
      buoydiff=0.
      do j=i+1,nlev
         buoydiff=buoydiff+NN(j-1)*0.5*(h(j)+h(j-1))
         integral=integral+buoydiff*h(j)
         if (integral.ge.tkeo(i)) then
            if(j.ne.nlev) then
               if(j.ne.i+1) then
                  lu(i)=lu(i)-(integral-tkeo(i))/buoydiff
               else 
!           To avoid lu(i) from becoming too large if NN(i) is too small
	          if(NN(i).gt.NNmin) then
	             lu(i)=sqrt(2.)*sqrt(tkeo(i))/sqrt(NN(i))
                  else
	             lu(i)=h(i)
                  end if
               end if 
               goto 600
            end if
         end if
         lu(i)=lu(i)+h(j)
      end do 
600   continue
!     Implicitely done in the do loop: if (lu(i).gt.ds(i)) lu(i)=ds(i) 
!     lu limited by distance to surface 
   end do

!  Computation of ldownward
   do i=nlev-1,1,-1
      ld(i)=0.
      integral=0.
      buoydiff=0.
      do j=i-1,1,-1 
         buoydiff=buoydiff+NN(j)*0.5*(h(j+1)+h(j))
         integral=integral-buoydiff*h(j)
         if (integral.ge.tkeo(i)) then
            if(j.ne.0) then
               if(j.ne.i-1) then
                  ld(i)=ld(i)-(integral-tkeo(i))/buoydiff
               else
!              To avoid ld(i) from becoming too large if NN(i) is too small
                  if(NN(i).gt.NNmin) then
                     ld(i)=sqrt(2.)*sqrt(tkeo(i))/sqrt(NN(i))
                  else
                     ld(i)=h(i)
                  end if
               end if 
               goto 610
            end if
         end if
         ld(i)=ld(i)+h(j)
      end do
610   continue
!     if (ld(i).gt.db(i)) ld(i)=db(i) !ld limited by distance to bottom
   end do         

!   Calculation of lk and leps, mixing and dissipation lengths
   do i=nlev-1,1,-1 
!  Suggested by Gaspar:        lk(i)   = min(lu(i),ld(i))
      lk(i)=sqrt(lu(i)*ld(i))
      leps(i) = sqrt(lu(i)*ld(i)) 
   end do

!  We set L=lk because it is the one we use to calculate num and nuh
   ceps=0.7
   do i=1,nlev-1
      L(i)=lk(i)
   end do      
     
   L(0)=kappa*z0b
   L(nlev)=kappa*z0s
!  Gaspar uses null gradient
   do i=0,nlev
      if ((NN(i).gt.0).and.(length_lim)) then
         Lcrit=sqrt(2*galp*galp*tke(i)/NN(i))
         if (L(i).gt.Lcrit) L(i)=Lcrit
      end if
      if (L(i).lt.L_min) L(i)=L_min
      eps(i)=cde*sqrt(tke(i)*tke(i)*tke(i))/L(i)
      if(eps(i).lt.eps_min) eps(i)=eps_min
   end do  
 
   return
   end
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard.

!$Id: lengthscaleeq.F90,v 1.2 2002-02-08 08:59:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: An equation for the length scale. 
! 
! !INTERFACE:
   subroutine lengthscaleeq(nlev,dt,z0b,z0s,depth,h,NN,P,B,numtke) 
!
! !DESCRIPTION:
!  This subroutine calculates the lengthscale equation according to
!  Mellor and Yamada [1982]:
!
!  \begin{equation}\label{kL_eq}
!  \partial_t (kL) - \partial_z\left(\nu_L\partial_z (kL)\right) =
!  L \left(c_{L1}P + c_{L3}B
!  -  \left(1 + E_2\left(\frac{L}{L_z}\right)^2\right)\varepsilon \right).
!  \end{equation}
!
!  At the end of the subroutine, the Galperin et al. [1988] length
!  limitation is applied and the disispation rate calculated. 
!
! !USES:
   use mtridiagonal
   use turbulence, ONLY: kappa,cde,galp,length_lim
   use turbulence, ONLY: MY_length,e1,e2,e3,b1
   use turbulence, ONLY: num,L,L_min,eps,eps_min,tkeo,tke
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: dt
   REALTYPE, intent(in)	:: z0b,z0s
   REALTYPE, intent(in)	:: depth,h(0:nlev)
   REALTYPE, intent(in)	:: NN(0:nlev)
   REALTYPE, intent(in)	:: P(0:nlev),B(0:nlev)
   REALTYPE, intent(in)	:: numtke(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard, Karsten Bolding 
!                      & Manuel Ruiz Villarreal
!
!  $Log: lengthscaleeq.F90,v $
!  Revision 1.2  2002-02-08 08:59:58  gotm
!  Added Manuel as author and copyright holder
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
! !LOCAL VARIABLES:
   REALTYPE 		:: avh(0:nlev),q2l(0:nlev),q3(0:nlev)
   REALTYPE 		:: phi_minus(0:nlev),phi_plus(0:nlev)
   REALTYPE 		:: Lz(0:nlev)
   REALTYPE             :: ds,db,prod,buoyan,diss,Lcrit 
   integer 		:: i 
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      q2l(i)=2.*tkeo(i)*L(i)
      q3 (i)=sqrt(8.*tke(i)*tke(i)*tke(i))
   end do
 
   do i=1,nlev
      avh(i)=0.5*(numtke(i-1)+numtke(i))
   end do
     
   db=0.0    ! Diagnostic Length Scale  
   ds=0.0 
   do i=1,nlev-1   
      db=db+h(i) 
      ds=depth-db
      ! Parabola shape
      if (MY_length.eq.1) Lz(i)=kappa*(ds+z0s)*(db+z0b)/(ds+z0s+db+z0b)
      ! Triangle shape
      if (MY_length.eq.2) Lz(i)=kappa*min(ds+z0s,db+z0b)
      ! For infinite depth
      if (MY_length.eq.3) Lz(i)=kappa*(ds+z0s)
   end do
       
   do i=1,nlev-1
      prod=e1*L(i)*P(i)
      buoyan=e3*L(i)*B(i)
      diss=-q3(i)/b1*(1.+e2*(L(i)/Lz(i))*(L(i)/Lz(i)))
      if (prod+buoyan .gt. 0) then
         phi_plus(i)=prod+buoyan
         phi_minus(i)=-diss
      else
         phi_plus(i)=prod
         phi_minus(i)=-buoyan-diss
      end if
   end do
 
   do i=2,nlev-2
      au(i)=-2.*dt*avh(i)/(h(i)+h(i+1))/h(i)
      cu(i)=-2.*dt*avh(i+1)/(h(i)+h(i+1))/h(i+1)
      bu(i)=1.-au(i)-cu(i)+phi_minus(i)*dt/q2l(i)
      du(i)=(1+phi_plus(i)*dt/q2l(i))*q2l(i)
   end do
 
   cu(1)=0
   bu(1)=1.
   du(1)=2.*tke(1)*kappa*(z0b+h(1)) 

   bu(nlev-1)=1.
   au(nlev-1)=0
   du(nlev-1)=2.*tke(nlev-1)*kappa*(z0s+h(nlev)) 

   call Tridiagonal(nlev,1,nlev-1,q2l)
 
   do i=1,nlev-1
     L(i)=q2l(i)/(2.*tke(i))
     if ((NN(i).gt.0).and.(length_lim)) then
       Lcrit=sqrt(2*galp*galp*tke(i)/NN(i)) 
       if (L(i).gt.Lcrit) L(i)=Lcrit  
     end if
     if (L(i).lt.L_min) L(i)=L_min
     eps(i)=cde*sqrt(tke(i)*tke(i)*tke(i))/L(i)
     if (eps(i).lt.eps_min) eps(i)=eps_min
   end do

   return
   end subroutine lengthscaleeq
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard, Karsten Bolding 
!                     & Manuel Ruiz Villarreal.
!-----------------------------------------------------------------------

!$Id: friction.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Friction calculation. 
!
! !INTERFACE:
   subroutine friction(kappa,avmolu,tx,ty)
!
! !USES:
   use meanflow, only:	h,z0b,h0b,MaxItz0b,z0s
   use meanflow, only:	u,v,gravity
   use meanflow, only:	u_taub,u_taus,drag
   use meanflow, only:	charnok,charnok_val,z0s_min
   IMPLICIT NONE
!
! !DESCRIPTION:
!  This subroutine updates the bottom roughness
!
!  \begin{equation}\label{Defz0b}
!  z_0^b = 0.1 \frac{\nu}{u_*^b} + 0.03 h_0^b, 
!  \end{equation}
!
!  calculates the product of bottom drag coefficient and current speed
!
!  \begin{equation}
!  r=\left(
!  \frac{\kappa}{\mbox{ln}\left(\frac{0.5h_1+z_0^b}{z^b_0}\right)}\right)^2
!  \sqrt{u_1^2+v_1^2} 
!  \end{equation}
!
!  and calculates the bottom friction velocity. 
!
!  The surface roughness may be calculated according to the 
!  {\it Charnok} [1955] formula:
!
!  \begin{equation}\label{Charnok}
!  z_0^s=\alpha \frac{(u_*^s)^2}{g}. 
!  \end{equation}
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)	:: kappa,avmolu,tx,ty
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: friction.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: count
   REALTYPE		:: dd
!
!EOP
!-----------------------------------------------------------------------
!BOC
   drag = 0.

   if (charnok) then 
      z0s=charnok_val*u_taus**2/gravity
      if (z0s.lt.z0s_min) z0s=z0s_min
   else
      z0s=z0s_min
   end if 
    
!  Iterate bottom roughness length MaxItz0b times  -- need to be changed KBK
   count=0
333   count=count+1
   if (avmolu.le.0) then
      z0b=0.03*h0b 
   else
      z0b=0.1*avmolu/max(avmolu,u_taub)+0.03*h0b 
   end if
   dd=kappa/(log((z0b+h(1)/2)/z0b))
! The following two lines would use the mean velocity instead of the velocity
! at z=h_1/2 for calculating the bottom. Just un-uncomment in order 
! to activate it.
!   frac=(z0b+h(1))/z0b
!   dd=kappa/((z0b+h(1))/h(1)*log(frac)-1.)
   dd=dd*dd 
   u_taub=sqrt(dd*(u(1)*u(1)+v(1)*v(1)))
!   write(*,*) u_taub,u(1),z0b 
   if (count.lt.MaxItz0b) goto 333 

!  Add bottom friction with fric(1) contribution.      
   drag(1) = drag(1) +  dd

   u_taus=(tx**2+ty**2)**(1./4.)

   return
   end subroutine friction 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

!$Id: friction.F90,v 1.2 2003-03-10 08:50:06 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The vertical friction \label{sec:friction}
!
! !INTERFACE:
   subroutine friction(kappa,avmolu,tx,ty)
!
! !DESCRIPTION:
!  This subroutine updates the bottom roughness
!  \begin{equation}\label{Defz0b}
!    z_0^b = 0.1 \frac{\nu}{u_*^b} + 0.03 h_0^b \point
!  \end{equation}
!  Then, it uses the well--known law--of--the--wall relations
!  to compute the friction velocity
! \begin{equation}
!  \label{uStar}
!   u_*^b = r \sqrt{u_1^2 + v_1^2}
!   \point
! \end{equation}
! Here, we used the abbreviation
!  \begin{equation}
!    r=\frac{\kappa}{\ln \left( \frac{0.5h_1+z_0^b}{z^b_0} \right)}
!    \comma
!  \end{equation}
!  where $\kappa$ is the von K{\'a}rm{\'a}n constant and
!  the index `1' indicates values at the center of the first
!  grid box at the bottom (version 1). Another expression for $r$ can be 
!  derived using the mean value of the velocity in the lowest
!  grid box, and not its value in the middle of the box (version 2). Also 
!  this method is supported in {\tt friction()} and can be activated by
!  uncommenting one line in this subroutine.
!
!  If no wave--breaking is considered, the same formula holds at 
!  the same formual holds at the surface. The surface roughness may
!  be calculated according to the \cite{Charnok55} formula,
!  \begin{equation}
!   \label{Charnok}
!    z_0^s=\alpha \frac{(u_*^s)^2}{g}
!   \point
!  \end{equation}
!
! !USES:
   use meanflow, only: h,z0b,h0b,MaxItz0b,z0s
   use meanflow, only: u,v,gravity
   use meanflow, only: u_taub,u_taus,drag
   use meanflow, only: charnok,charnok_val,z0s_min
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)	               :: kappa,avmolu,tx,ty
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: friction.F90,v $
!  Revision 1.2  2003-03-10 08:50:06  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE		:: rr
!
!-----------------------------------------------------------------------
!BOC

   drag = 0.

!  use the Charnok formula to compute the surface roughness
   if (charnok) then 
      z0s=charnok_val*u_taus**2/gravity
      if (z0s.lt.z0s_min) z0s=z0s_min
   else
      z0s=z0s_min
   end if 
    
!  iterate bottom roughness length MaxItz0b times 
   do i=1,MaxItz0b

      if (avmolu.le.0) then
         z0b=0.03*h0b 
      else
         z0b=0.1*avmolu/max(avmolu,u_taub)+0.03*h0b 
      end if
      
      !  compute the factor r (version 1, with log-law)
      rr=kappa/(log((z0b+h(1)/2)/z0b))
      
      !  compute the factor r (version 2, with meanvalue log-law)
      !   frac=(z0b+h(1))/z0b
      !   rr=kappa/((z0b+h(1))/h(1)*log(frac)-1.)
      
      !  compute the friction velocity at the bottom
      u_taub = rr*sqrt( u(1)*u(1) + v(1)*v(1) )
      
   end do
      
!  add bottom friction as source term for the momentum equation
   drag(1) = drag(1) +  rr*rr

!  be careful: tx and ty are the surface shear-stresses 
!  already divided by rho!
   u_taus=(tx**2+ty**2)**(1./4.)

   return
   end subroutine friction 
!EOC

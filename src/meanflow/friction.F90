!$Id: friction.F90,v 1.11 2010-09-17 12:53:48 jorn Exp $
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
!  \begin{equation}
!    \label{Defz0b}
!    z_0^b = 0.1 \frac{\nu}{u_*^b} + 0.03 h_0^b + z_a \point
!  \end{equation}
!  The first term on the right hand side of \eq{Defz0b} represents
!  the limit for hydraulically smooth surfaces, the second term the limit
!  for completely rough surfaces. Note that the third term, $z_a$,
!  is the contribution of suspended sediments to the
!  roughness length, see \cite{SmithMcLean77}. It is updated during calls
!  to the sediment-routines.
!
! The law-of-the-wall relations are used to compute the friction velocity
! \begin{equation}
!  \label{uStar}
!   u_*^b = r \sqrt{U_1^2 + V_1^2}
!   \comma
! \end{equation}
! where $U_1$ and $V_1$ are the components of the mean velocity
! at the center of the lowest cell.
! We used the abbreviation
!  \begin{equation}
!    \label{rParam}
!    r=\frac{\kappa}{\ln \left( \frac{0.5h_1+z_0^b}{z^b_0} \right)}
!    \comma
!  \end{equation}
!  where $\kappa$ is the von K{\'a}rm{\'a}n constant and
!  the index `1' indicates values at the center of the first
!  grid box at the bottom (version 1). Another expression for $r$ can be
!  derived using the mean value of the velocity in the lowest
!  grid box, and not its value in the middle of the box (version 2). Also
!  this method is supported in {\tt friction()} and can be activated by
!  uncommenting one line in the code.
!
!  If no breaking surface waves are considered, the law of the wall 
!  also holds at the surface. The surface roughness length may 
!  be calculated according to the \cite{Charnock55} formula,
!  \begin{equation}
!   \label{Charnock}
!    z_0^s=\alpha \frac{(u_*^s)^2}{g}
!   \point
!  \end{equation}
!  The model constant $\alpha$ is read in as {\tt charnock\_val} from
!  the {\tt meanflow} namelist.
!
! !USES:
   use meanflow,      only: h,z0b,h0b,MaxItz0b,z0s,za
   use meanflow,      only: u,v,gravity
   use meanflow,      only: u_taub,u_taus,drag
   use meanflow,      only: charnock,charnock_val,z0s_min
   use meanflow,      only: lake

!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: kappa,avmolu,tx,ty
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: friction.F90,v $
!  Revision 1.11  2010-09-17 12:53:48  jorn
!  extensive code clean-up to ensure proper initialization and clean-up of all variables
!
!  Revision 1.10  2006-11-20 17:28:58  kbk
!  [Cc]harnok -> [Cc]harnock - A. Jenkins
!
!  Revision 1.9  2006-11-20 17:26:15  kbk
!  [Cc]harnok -> [Cc]harnock - A. Jenkins
!
!  Revision 1.8  2005/08/11 12:31:54  lars
!  corrected error in documentation. Thanks to Patrizio Mariani
!
!  Revision 1.7  2005/06/27 13:44:07  kbk
!  modified + removed traling blanks
!
!  Revision 1.6  2004/08/18 12:33:30  lars
!  updated documentation
!
!  Revision 1.5  2004/01/13 08:39:49  lars
!  included roughness due to suspended sediments
!
!  Revision 1.4  2003/03/28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.3  2003/03/28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.2  2003/03/10 08:50:06  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                             :: i,j,j_max
   REALTYPE                            :: rr
!
!-----------------------------------------------------------------------
!BOC

   drag = _ZERO_

!  use the Charnock formula to compute the surface roughness
   if (charnock) then
      z0s=charnock_val*u_taus**2/gravity
      if (z0s.lt.z0s_min) z0s=z0s_min
   else
      z0s=z0s_min
   end if

!  iterate bottom roughness length MaxItz0b times
!  for lake model the friction has to be calculacted at every depth
!  drag(j) = drag(j) * dAdz(j)/Ac(j) is done implicitly when solving
!  the diffusion equation, see u-,v-equation.F90 and diff_center_hypso.F90
   if (lake) then
      j_max = sizeof(h) / sizeof(h(1)) - 1
   else
      j_max = 1
   end if
!  iterate from nlev to 1 so that u_taub is located at 1 at the end
!  this is important for other modules
   do j=j_max,1,-1
      do i=1,MaxItz0b

         if (avmolu.le.0) then
            z0b=0.03*h0b + za
         else
            z0b=0.1*avmolu/max(avmolu,u_taub)+0.03*h0b + za
         end if

!        compute the factor r (version 1, with log-law)
         rr=kappa/(log((z0b+h(j)/2)/z0b))

!        compute the factor r (version 2, with meanvalue log-law)
!        frac=(z0b+h(j))/z0b
!        rr=kappa/((z0b+h(j))/h(j)*log(frac)-1.)

!        compute the friction velocity at every grid cell
         u_taub = rr*sqrt( u(j)*u(j) + v(j)*v(j) )

      end do
!     add bottom friction as source term for the momentum equation
      drag(j) = drag(j) +  rr*rr
   end do

!  be careful: tx and ty are the surface shear-stresses
!  already divided by rho!
   u_taus=(tx**2+ty**2)**(1./4.)

   return
   end subroutine friction
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

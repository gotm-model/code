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
!  If lake is true, this module calculates the bottom friction at every grid
!  cell.
!
! !USES:
   use meanflow,      only: h,z0b,h0b,MaxItz0b,z0s,za
   use meanflow,      only: u,v,rho_0,gravity
   use meanflow,      only: u_taub,u_taus,drag,taub
   use meanflow,      only: charnock,charnock_val,z0s_min
   use meanflow,      only: lake,Af,Ac

!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: kappa,avmolu,tx,ty
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                             :: i,j,j_max
   REALTYPE                            :: rr,vel
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
      j_max = SIZE(h) - 1
   else
      j_max = 1
   end if
!  iterate from nlev to 1 so that u_taub is located at 1 at the end
!  this is important for other modules
   do j=j_max,1,-1
      vel = sqrt( u(j)*u(j) + v(j)*v(j) )

      z0b = 0.03d0*h0b + za

!     compute the factor r (version 1, with log-law)
      !rr=kappa/(log((z0b+h(j)/2)/z0b))
      rr = kappa / log(_ONE_+_HALF_*h(j)/z0b)

!     compute the factor r (version 2, with meanvalue log-law)
!     frac=(z0b+h(j))/z0b
!     rr=kappa/((z0b+h(j))/h(j)*log(frac)-1.)

      if ( avmolu.gt._ZERO_ .and. vel.gt._ZERO_ ) then
         do i=1,MaxItz0b
            !z0b=0.1*avmolu/max(avmolu,u_taub)+0.03*h0b + za
            z0b = 0.1d0*avmolu/(rr*vel) + 0.03d0*h0b + za

!           compute the factor r (version 1, with log-law)
            !rr=kappa/(log((z0b+h(j)/2)/z0b))
            rr = kappa / log(_ONE_+_HALF_*h(j)/z0b)

!           compute the factor r (version 2, with meanvalue log-law)
!           frac=(z0b+h(j))/z0b
!           rr=kappa/((z0b+h(j))/h(j)*log(frac)-1.)

         end do
      end if

!     add bottom friction as source term for the momentum equation
      drag(j) = drag(j) +  rr*rr * ( Af(j) - Af(j-1) ) / Ac(j)
   end do

   drag(1) = drag(1) + rr*rr

!  compute the friction velocity at every grid cell
   u_taub = rr*vel

!  calculate bottom stress, which is used by sediment resuspension models
   taub = u_taub*u_taub*rho_0

!  be careful: tx and ty are the surface shear-stresses
!  already divided by rho!
   u_taus=(tx**2+ty**2)**(1./4.)

   return
   end subroutine friction
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

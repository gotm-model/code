#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The vertical friction \label{sec:friction}
!
! !INTERFACE:
   subroutine friction(nlev,kappa,avmolu,tx,ty,plume_type)
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
!  the {\tt gotm.yaml}.
!
! !USES:
   use density,       only: rho0
   use meanflow,      only: h,z0b,h0b,MaxItz0b,z0s,za
   use meanflow,      only: u,v,gravity
   use meanflow,      only: u_taub,u_taubo,u_taus,drag,taub
   use meanflow,      only: calc_bottom_stress
   use meanflow,      only: charnock,charnock_val,z0s_min
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!  number of vertical layers
   integer, intent(in)                 :: nlev

   REALTYPE, intent(in)                :: kappa,avmolu,tx,ty
   integer, intent(in)                 :: plume_type
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                             :: i
   REALTYPE                            :: rr_s,rr_b
   logical, save                       :: first=.true.
!
!-----------------------------------------------------------------------
!BOC

   drag = _ZERO_
   rr_s = _ZERO_
   rr_b = _ZERO_

!  use the Charnock formula to compute the surface roughness
   if (charnock) then
      z0s=charnock_val*u_taus**2/gravity
      if (z0s.lt.z0s_min) z0s=z0s_min
   else
      z0s=z0s_min
   end if

   if (calc_bottom_stress) then
      if (first) then
         u_taub = u_taubo
         first = .false.
      else
         u_taubo = u_taub
      end if
!     iterate bottom roughness length MaxItz0b times
      do i=1,MaxItz0b

         if (avmolu.le.0) then
            z0b=0.03*h0b + za
         else
            z0b=0.1*avmolu/max(avmolu,u_taub)+0.03*h0b + za
         end if

!        compute the factor r (version 1, with log-law)
         rr_b=kappa/(log((z0b+h(1)/2)/z0b))

!        compute the factor r (version 2, with meanvalue log-law)
!        frac=(z0b+h(1))/z0b
!        rr=kappa/((z0b+h(1))/h(1)*log(frac)-1.)

!        compute the friction velocity at the bottom
         u_taub = rr_b*sqrt( u(1)*u(1) + v(1)*v(1) )

       end do
   end if

!  compute the factor r (version 1, with log-law)
   if (plume_type .eq. 1) rr_s=kappa/(log((z0s+h(nlev)/2)/z0s))

!  calculate bottom stress, which is used by sediment resuspension models
   taub = u_taub*u_taub*rho0

!  add bottom friction as source term for the momentum equation
   drag(1) = drag(1) +  rr_b*rr_b
    
!  add for surface plume scenario surface friction as source term for the momentum equation
   if (plume_type .eq. 1) drag(nlev) = drag(nlev) +  rr_s*rr_s

!  be careful: tx and ty are the surface shear-stresses
!  already divided by rho!
   if (plume_type == 1) then
      u_taus=rr_s*sqrt( u(nlev)*u(nlev) + v(nlev)*v(nlev) )     
   else
      u_taus=(tx**2+ty**2)**(1./4.)
   endif

   return
   end subroutine friction
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

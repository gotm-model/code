#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The external pressure-gradient \label{sec:extpressure}
!
! !INTERFACE:
   subroutine extpressure(method,nlev)
!
! !DESCRIPTION:
!
!  This subroutine calculates the external pressure-gradient. Two methods
!  are implemented here, relating either to the velocity vector at a
!  given height above bed prescribed or to the vector for the vertical mean
!  velocity. In the first case, {\tt dpdx} and {\tt dpdy} are $x$-
!  and $y$-components of the prescribed velocity vector at the
!  height {\tt h\_press} above the bed. The velocity profile will in
!  this routive be shifted by a vertically constant vector such that the
!  resulting profile has an (interpolated) velocity at {\tt h\_press}
!  which is identical to the prescribed value. In the second case,
!  {\tt dpdx} and {\tt dpdy} are $x$- and $y$-components of the
!  prescribed vertical mean velocity vector, and {\tt h\_press} is
!  not used. Here the velocity profile is shifted in such a way that
!  the resulting mean velocty vector is identical to {\tt dpdx} and {\tt dpdy}.
!
!  For both cases, this is a recalculation of the external pressure gradient,
!  since at all points the same acceleration has been applied in this
!  operator split method.
!
!  If the external pressure-gradient is prescribed by the
!  surface slope, then it is directly inserted in \eq{uEq} and \eq{vEq}.
!
!  For details of this method, see \cite{Burchard99}.
!
! !USES:
   use meanflow,     only: u,v,h
   use observations, only: dpdx,dpdy,h_press
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  method to compute external
!  pressure gradient
   integer, intent(in)                 :: method

!  number of vertical layers
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                             :: i
   REALTYPE                            :: z(0:nlev)
   REALTYPE                            :: rat,uint,vint,hint
!
!-----------------------------------------------------------------------
!BOC
   select case (method)
      case (1)
!        current measurement at h_press above bed
         z(1)=0.5*h(1)
         i   =0
222      i=i+1
         z(i+1)=z(i)+0.5*(h(i)+h(i+1))
         if ((z(i+1).lt.h_press%value).and.(i.lt.nlev)) goto 222
         rat=(h_press%value-z(i))/(z(i+1)-z(i))
         uint=rat*u(i+1)+(1-rat)*u(i)
         vint=rat*v(i+1)+(1-rat)*v(i)
         do i=1,nlev
            u(i)=u(i)+dpdx%value-uint
            v(i)=v(i)+dpdy%value-vint
         end do
      case (2)
!     vertical mean of current prescribed
         uint=_ZERO_
         vint=_ZERO_
         hint=_ZERO_
         do i=1,nlev
            hint=hint+h(i)
            uint=uint+h(i)*u(i)
            vint=vint+h(i)*v(i)
         end do
         uint=uint/hint
         vint=vint/hint
         do i=1,nlev
            u(i)=u(i)+dpdx%value-uint
            v(i)=v(i)+dpdy%value-vint
         end do
      case default
!     do nothing if method=0, because then
!     pressure gradient is applied directly
!     in uequation() and vequation()
   end select

   return
   end subroutine extpressure
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

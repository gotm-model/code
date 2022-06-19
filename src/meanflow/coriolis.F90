#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The Coriolis rotation \label{sec:coriolis}
!
! !INTERFACE:
   subroutine coriolis(nlev,dt)
!
! !DESCRIPTION:
!  This subroutine carries out the Coriolis rotation by applying a
!  $2\times 2$ rotation matrix with the angle $f\Delta t$ on the
!  horizontal velocity vector $(U,V)$.
!
! !USES:
   USE meanflow, only: u,v,cori
   USE stokes_drift, only: usprof, vsprof
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: ua,omega,cosomega,sinomega
   REALTYPE                  :: ul, vl
!
!-----------------------------------------------------------------------
!BOC

   omega=cori*dt
   cosomega=cos(omega)
   sinomega=sin(omega)

   do i=1,nlev
!     KK-TODO: move calculation of Lagrangian velocities to a more
!              central place.
      ul = u(i) + usprof%data(i)
      vl = v(i) + vsprof%data(i)

      ua = ul
      ul =  ul*cosomega + vl*sinomega
      vl = -ua*sinomega + vl*cosomega

!     KK-TODO: In GETM we distinguish between old and new Stokes drift.
      u(i) = ul - usprof%data(i)
      v(i) = vl - vsprof%data(i)
   end do

   return
   end subroutine coriolis
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

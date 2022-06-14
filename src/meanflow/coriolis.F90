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
   REALTYPE                  :: ul(0:nlev), vl(0:nlev)
!
!-----------------------------------------------------------------------
!BOC

   omega=cori*dt
   cosomega=cos(omega)
   sinomega=sin(omega)

!  KK-TODO: move calculation of Lagrangian velocities to a more central
!           place.
   do i=1,nlev
      ul(i) = u(i) + usprof%data(i)
      vl(i) = v(i) + vsprof%data(i)
   end do

   do i=1,nlev
      ua = ul(i)
      ul(i) =  ul(i)*cosomega + vl(i)*sinomega
      vl(i) = -ua   *sinomega + vl(i)*cosomega
   end do

!  KK-TODO: In GETM we distinguish between old and new Stokes drift.
   do i=1,nlev
      u(i) = ul(i) - usprof%data(i)
      v(i) = vl(i) - vsprof%data(i)
   end do

   return
   end subroutine coriolis
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

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
!
!-----------------------------------------------------------------------
!BOC

   omega=cori*dt
   cosomega=cos(omega)
   sinomega=sin(omega)

   do i=1,nlev
      ua=u(i)
      u(i)= u(i) *cosomega+v(i)*sinomega
      v(i)=-ua   *sinomega+v(i)*cosomega
   end do

   return
   end subroutine coriolis
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

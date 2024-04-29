#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: stokes_drift_exp
!
! !INTERFACE:
   subroutine stokes_drift_exp(nlev,z,zi)
!
! !DESCRIPTION:
!  Calculate the Stokes drift profile from surface Stokes drift and the
!  Stokes penetration depth, assuming exponential profile.
!
! !USES:

   use stokes_drift, only:   usprof, vsprof
   use stokes_drift, only:   us0, vs0, ds

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev), zi(0:nlev)
!
! !OUTPUT PARAMETERS:

! !REVISION HISTORY:
!  Original author(s): Qing Li
!
!EOP
! !LOCAL VARIABLES:
   integer                             :: k
   REALTYPE                            :: tmp
   REALTYPE                            :: kdz, dz
!
!-----------------------------------------------------------------------
!BOC
!  initialization
   usprof%data = _ZERO_
   vsprof%data = _ZERO_
!  Stokes drift averaged over the grid cell, z(0) is not used
   do k=1,nlev
      dz = zi(k)-zi(k-1)
      kdz = 0.5*dz/ds%value
      if (kdz .lt. 100.) then
          tmp = sinh(kdz)/kdz*exp(z(k)/ds%value)
      else
          tmp = exp(z(k)/ds%value)
      end if
      usprof%data(k) = tmp*us0%value
      vsprof%data(k) = tmp*vs0%value
   end do

   return

   end subroutine stokes_drift_exp
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

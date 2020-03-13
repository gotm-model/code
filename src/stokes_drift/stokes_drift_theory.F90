#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: stokes_drift_theory
!
! !INTERFACE:
   subroutine stokes_drift_theory(nlev,z,zi,u10,v10,gravity)
! !DESCRIPTION:
!   Calculate the Stokes drift profile from surface wind using the 'theory-wave'
!    approximation described in Li et al., 2017.
!
! !USES:

   use stokes_drift, only:   usprof, vsprof
   use stokes_drift, only:   us0, vs0, ds

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev), zi(0:nlev)
   REALTYPE, intent(in)                :: u10, v10, gravity
!
! !OUTPUT PARAMETERS:

! !REVISION HISTORY:
!  Original author(s): Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   ! ratio of surface Stokes drift to U10
   REALTYPE, parameter :: us0_to_u10 = 0.0162

   integer :: k
   REALTYPE :: stokes_srf(0:nlev), wind_speed, xcomp, ycomp, tmp
!
!-----------------------------------------------------------------------
!BOC
!  initialization
   usprof%data = _ZERO_
   vsprof%data = _ZERO_
   us0%value = _ZERO_
   vs0%value = _ZERO_
   ds%value = _ZERO_

!  wind direction
   wind_speed = sqrt(u10**2+v10**2)
   xcomp = u10 / wind_speed
   ycomp = v10 / wind_speed

!  compute surface layer averaged Stokes drift
   do k=0,nlev
      call stokes_drift_theory_srf(wind_speed,zi(k),gravity,stokes_srf(k))
   enddo

!  compute Stokes drift profile
   do k=1,nlev
      tmp = ( stokes_srf(k-1)*zi(k-1)-stokes_srf(k)*zi(k) ) / (zi(k-1) - zi(k))
      usprof%data(k) = tmp * xcomp
      vsprof%data(k) = tmp * ycomp
   enddo
   us0%value = usprof%data(nlev)
   vs0%value = vsprof%data(nlev)
   ds%value = stokes_srf(0)*abs(zi(0))/max(SMALL, sqrt(us0%value**2.+vs0%value**2.))

   return

   end subroutine stokes_drift_theory
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: stokes_drift_theory_srf
!
! !INTERFACE:
   subroutine stokes_drift_theory_srf(u10,z_srf,gravity,stokes_srf)
! !DESCRIPTION:
!   Return the Stokes drift averaged over the surface layer
!
! !USES:
   use stokes_drift, only:   pi

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
! 10 meter wind (m/s)
   REALTYPE, intent(in)                 :: u10
! depth of  the layer for averaging (m)
   REALTYPE, intent(in)                 :: z_srf
! gravity
   REALTYPE, intent(in)                 :: gravity
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)                :: stokes_srf

! !REVISION HISTORY:
!  Original author(s): Qing Li
!  (Re)added to GOTM: Brandon Reichl
!
!EOP
! !LOCAL VARIABLES:
   REALTYPE, parameter :: &
   ! ratio of U19.5 to U10 (Holthuijsen, 2007)
   u19p5_to_u10 = 1.075, &
   ! ratio of mean frequency to peak frequency for
   ! Pierson-Moskowitz spectrum (Webb, 2011)
   fm_to_fp = 1.296, &
   ! ratio of surface Stokes drift to U10
   us0_to_u10 = 0.0162, &
   ! loss ratio of Stokes transport
   r_loss = 0.667

   REALTYPE :: us0, hm0, fm, fp, stokes_trans, kphil, kstar
   REALTYPE :: z0, z0i, r1, r2, r3, r4, tmp
!
!-----------------------------------------------------------------------
!BOC
   stokes_srf = _ZERO_

   if (u10 .gt. _ZERO_) then

      ! surface Stokes drift
      us0 = us0_to_u10 * u10
      !
      ! significant wave height from Pierson-Moskowitz
      ! spectrum (Bouws, 1998)
      hm0 = 0.0246 * u10**2
      !
      ! peak frequency (PM, Bouws, 1998)
      tmp = 2.0 *  pi * u19p5_to_u10 * u10
      fp = 0.877 * gravity/tmp
      !
      ! mean frequency
      fm = fm_to_fp * fp
      !
      ! total Stokes transport (a factor r_loss is applied to account
      !  for the effect of directional spreading, multidirectional waves
      !  and the use of PM peak frequency and PM significant wave height
      !  on estimating the Stokes transport)
      stokes_trans = 0.125 * pi * r_loss * fm * hm0**2
      !
      ! the general peak wavenumber for Phillips' spectrum
      ! (Breivik et al., 2016) with correction of directional spreading
      kphil = 0.176 * us0 / stokes_trans
      !
      ! surface layer averaged Stokes dirft with Stokes drift profile
      ! estimated from Phillips' spectrum (Breivik et al., 2016)
      ! the directional spreading effect from Webb and Fox-Kemper, 2015
      ! is also included
      kstar = kphil * 2.56
      !
      ! stokes integral to z_srf
      z0 = abs(z_srf)
      if (z0>1.e-4) then
         z0i = abs(_ONE_/z0)
         ! term 1 to 4
         r1 = (0.151 / kphil * z0i - 0.84) &
              *(_ONE_ - exp(-2.0 * kphil * z0))
         r2 = -(0.84 + 0.0591 / kphil * z0i) &
              *sqrt(2.0 * pi * kphil * z0) &
              *erfc(sqrt(2.0 * kphil * z0))
         r3 = (0.0632 / kstar * z0i + 0.125) &
              *(_ONE_ - exp(-2.0 * kstar * z0))
         r4 = (0.125 + 0.0946 / kstar * z0i) &
              *sqrt(2.0 * pi * kstar * z0) &
              *erfc(sqrt(2.0 * kstar * z0))
         stokes_srf = us0*(0.715 + r1 + r2 + r3 + r4)
      else
         stokes_srf = _ZERO_
      endif
   endif

   return

   end subroutine stokes_drift_theory_srf
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: stokes_drift_dhh85
!
! !INTERFACE:
   subroutine stokes_drift_dhh85(nlev,z,zi,u10,v10,gravity)
!
! !DESCRIPTION:
!  Compute grid cell-averaged Stokes drift profile from the empirical wave
!  spectrum of Donelan et al., 1985
!
! !USES:

   use stokes_drift, only:   usprof, vsprof
   use stokes_drift, only:   us0, vs0, ds
   use stokes_drift, only:   wave_age

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev), zi(0:nlev)
   REALTYPE, intent(in)                :: u10, v10, gravity
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   integer, parameter      :: nomega = 2000
   REALTYPE, parameter     :: min_omega = 0.1, max_omega = 20.0
!
   integer                 :: i, k
   REALTYPE                :: xcomp, ycomp, wind_speed
   REALTYPE                :: domega, sd_omega, tmp, dz, ustran
   REALTYPE                :: stokes_drift_kernel_dhh85
!
!-----------------------------------------------------------------------
!BOC

!  initialization
   usprof%data = _ZERO_
   vsprof%data = _ZERO_
   ustran = _ZERO_

!  wind direction
   wind_speed = sqrt(u10**2+v10**2)
   xcomp = u10 / wind_speed
   ycomp = v10 / wind_speed
!
!  stokes drift at u-levels
   domega = ( max_omega - min_omega ) / real(nomega)
   do  k = 1, nlev
      tmp = _ZERO_
      sd_omega = min_omega + 0.5 * domega
      dz = zi(k)-zi(k-1)
      do  i = 1, nomega
         tmp = tmp +                                                       &
            domega * stokes_drift_kernel_dhh85(sd_omega, z(k), dz,         &
                                               wind_speed, wave_age%value, gravity)
         sd_omega = sd_omega + domega
      enddo
      usprof%data(k) = xcomp * tmp
      vsprof%data(k) = ycomp * tmp
      ustran = ustran + dz * tmp
   enddo
   us0%value = usprof%data(nlev)
   vs0%value = vsprof%data(nlev)
   ds%value = ustran/max(SMALL, sqrt(us0%value**2.+vs0%value**2.))
! TODO: add contribution from a f^-5 tail <20200304, Qing Li> !

   return

   end subroutine stokes_drift_dhh85
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: stokes_drift_kernel_dhh85
!
! !INTERFACE:
   function stokes_drift_kernel_dhh85(sd_omega,sd_z,sd_dz,wind_speed,waveage,gravity)
!
! !DESCRIPTION:
!  Evaluate kernel of the Stokes integral for the Donelan et al., 1985 spectrum
!
! !USES:

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)               :: sd_omega, sd_z, sd_dz
   REALTYPE, intent(in)               :: wind_speed, waveage, gravity
!
! !REVISION HISTORY:
!  Original author(s): Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE  :: dhh_omega_p, dhh_alpha, dhh_sigma, dhh_gamma1, dhh_gamma2
   REALTYPE  :: wave_spec, sd_filter, kdz, iwa
   REALTYPE  :: stokes_drift_kernel_dhh85
!-----------------------------------------------------------------------
!BOC

!  DHH 85 spectrum
   iwa = _ONE_ / waveage !< inverse wave age
   dhh_omega_p = gravity * iwa / wind_speed !< peak frequency
   dhh_alpha  = 0.006 * iwa**(0.55)
   dhh_sigma  = 0.08 * ( _ONE_ + 4.0 * waveage**3 )
   if ( iwa .le. _ONE_) then
      dhh_gamma1 = 1.7
   else
      dhh_gamma1 = 1.7 + 6.0 * log10( iwa )
   endif
   dhh_gamma2 = exp( -0.5 * ( sd_omega - dhh_omega_p )**2 /              &
                    dhh_sigma**2 / dhh_omega_p**2 )
   wave_spec  = dhh_alpha * gravity**2 / (dhh_omega_p * sd_omega**4 ) *  &
                exp( -( dhh_omega_p / sd_omega )**4 ) *                  &
                dhh_gamma1**dhh_gamma2
!
!--Stokes drift integral kernel
   kdz = sd_omega**2 * sd_dz / gravity
   if ( kdz .lt. 10.0 ) then
      sd_filter = sinh(kdz) / kdz
   else
      sd_filter = _ONE_
   endif
   stokes_drift_kernel_dhh85 = 2.0 * ( wave_spec * sd_omega**3 ) *       &
          sd_filter * exp( 2.0 * sd_omega**2 * sd_z / gravity ) / gravity
   return

   end function stokes_drift_kernel_dhh85
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

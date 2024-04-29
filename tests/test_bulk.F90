#include "cppdefs.h"
!
   program test_kondo
!
! !DESCRIPTION:
!  program to test the Kondo transfere coefficients
!
!  To build:
!  make test_kondo
!  To execute:
!  ./test_kondo
!  To plot:
!  python $GOTM_BASE/scr/plot_bulk.py
!
! !USES:
   use airsea_variables ! only for printing variables
   implicit none
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding

! !LOCAL VARIABLES
!  basic airsea interaction variables
   integer  :: hum_method=1 ! 2,3,4
   REALTYPE :: rh=90.
   REALTYPE :: airp=101325.
   REALTYPE :: tw=10.,tw_k
   REALTYPE :: ta_k
!KB   REALTYPE :: ta=15.,ta_k
!  back-radiation
   integer  :: back_radiation_method=1 ! 2,3,4,5,6...
   REALTYPE :: dlat=45.
   REALTYPE :: cloud=0.5
   REALTYPE :: qb
!  heat and momentum fluxes
   integer  :: fluxes_method=1 ! 2....
   !logical  :: rain_impact=.false.,calc_evaporation=.true.
   REALTYPE :: u10=5.,ssu=0.
   REALTYPE :: v10=-5.,ssv=0.
   REALTYPE :: precip=0.
   REALTYPE :: evap,tx,ty,qe,qh
!  short wave radiation
   integer  :: yday=183.,n
   REALTYPE :: hh
   REALTYPE :: dlon=0.
   REALTYPE :: albedo, albedo_water
   REALTYPE :: swr,short_wave_radiation
   REALTYPE :: z1,z2,z3,z4,solar_zenith_angle
   REALTYPE :: s1,s2,s3,s4
   REALTYPE :: kondo_cdd,kondo_ced,kondo_chd
   REALTYPE :: fairall_cdd,fairall_ced,fairall_chd

   integer  :: i
!EOP
!-----------------------------------------------------------------------
!BOC
   ta = tw+0.001
   tw_k = tw+kelvin
   ta_k = ta+kelvin
   STDERR 'basic variables:'
   STDERR 'rh=   ',rh
   STDERR 'airp= ',airp
   STDERR 'tw=   ',tw
   STDERR 'ta=   ',ta
   STDERR

   STDERR 'humidity related variables:'
   L = 2.5e6
   call humidity(hum_method,rh,airp,tw,ta)
   STDERR 'rhoa= ',rhoa
   STDERR 'L=    ',L
   STDERR 'qs=   ',qs
   STDERR 'qa=   ',qa
   STDERR

   write(200,*) 'Wind Kondo_cdd Fairall_cdd Kondo_chd Fairall_chd Kondo_ced Fairall_ced'
#if 0
   do n=1,2400000
      u10=n*0.0000125
#else
   do n=1,240
      u10=n*0.125
#endif
      v10=n*0.0
      call airsea_fluxes(1, &
                      tw,ta,u10-ssu,v10-ssv,precip,evap,tx,ty,qe,qh)
#if 1
      kondo_cdd = tx/(rhoa*u10*u10)
      kondo_chd = -qh/(cpa*rhoa*u10*(tw-ta))
      kondo_ced = -qe/(L*rhoa*u10*(qs-qa))
      call airsea_fluxes(2, &
                      tw,ta,u10-ssu,v10-ssv,precip,evap,tx,ty,qe,qh)
      fairall_cdd = tx/(rhoa*u10*u10)
      fairall_chd = -qh/(cpa*rhoa*u10*(tw-ta))
      fairall_ced = -qe/(L*rhoa*u10*(qs-qa))
      write(200,"(7F9.6)") u10,kondo_cdd,fairall_cdd,kondo_chd,fairall_chd,kondo_ced,fairall_ced
!      write(200,*) u10,kondo_cdd,fairall_cdd
#endif
   end do

   end program test_kondo
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

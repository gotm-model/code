#include "cppdefs.h"
!
   program test_airsea
!
! !DESCRIPTION:
!  program to test the airsea inter action routines
!
!  To execute:
!  make test_airsea
!
! !USES:
   use airsea_variables ! only for printing variables
   implicit none
!
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
   logical  :: rain_impact=.false.,calc_evaporation=.true.
   REALTYPE :: u10=5.,ssu=0.
   REALTYPE :: v10=-5.,ssv=0.
   REALTYPE :: precip=0.
   REALTYPE :: evap,tx,ty,qe,qh
!  short wave radiation
   integer  :: yday=183.
   REALTYPE :: hh
   REALTYPE :: dlon=0.
   REALTYPE :: albedo, albedo_water
   REALTYPE :: swr, short_wave_radiation

   integer  :: i
!EOP
!-----------------------------------------------------------------------
!BOC
   ta = 15.
   tw_k = tw+kelvin
   ta_k = ta+kelvin
   STDERR 'basic variables:'
   STDERR 'rh=   ',rh
   STDERR 'airp= ',airp
   STDERR 'tw=   ',tw
   STDERR 'ta=   ',ta
   STDERR 'lon= ',dlon,'lat= ',dlat
   STDERR 'cloud=',cloud
   STDERR 'u10=  ',u10
   STDERR 'v10=  ',v10
   STDERR

   STDERR 'humidity related variables:'
   call humidity(hum_method,rh,airp,tw,ta)
   STDERR 'es=   ',es
   STDERR 'ea=   ',ea
   STDERR 'qs=   ',qs
   STDERR 'qa=   ',qa
   STDERR 'L=    ',L
   STDERR 'rhoa= ',rhoa

   STDERR
   STDERR 'back radiation:'
   call back_radiation(1,dlat,tw_k,ta_k,cloud,qb)
   STDERR 'Clark=      ',qb
   call back_radiation(2,dlat,tw_k,ta_k,cloud,qb)
   STDERR 'Hastenrath= ',qb
   call back_radiation(3,dlat,tw_k,ta_k,cloud,qb)
   STDERR 'Bignami=    ',qb
   call back_radiation(4,dlat,tw_k,ta_k,cloud,qb)
   STDERR 'Berliand=   ',qb
   call back_radiation(5,dlat,tw_k,ta_k,cloud,qb)
   STDERR 'Josey 1 =   ',qb
   call back_radiation(6,dlat,tw_k,ta_k,cloud,qb)
   STDERR 'Josey 2 =   ',qb
!   call back_radiation(7,dlat,tw_k,ta_k,cloud,qb)
!   STDERR 'not available - yet :-)'
   STDERR

   STDERR
   STDERR 'heat and momentum fluxes:'
#if 0
   call airsea_fluxes(1,rain_impact,calc_evaporation, &
                   tw,ta,u10-ssu,v10-ssv,precip,evap,tx,ty,qe,qh)
#else
   call airsea_fluxes(1, &
                   tw,ta,u10-ssu,v10-ssv,precip,evap,tx,ty,qe,qh)
#endif
   STDERR 'Kondo:'
   STDERR 'evap= ',evap
   STDERR 'tx=   ',tx
   STDERR 'ty=   ',ty
   STDERR 'qe=   ',qe
   STDERR 'qh=   ',qh
#if 0
   call airsea_fluxes(2,rain_impact,calc_evaporation, &
                   tw,ta,u10-ssu,v10-ssv,precip,evap,tx,ty,qe,qh)
#else
   call airsea_fluxes(2, &
                   tw,ta,u10-ssu,v10-ssv,precip,evap,tx,ty,qe,qh)
#endif
   STDERR 'Fairall:'
   STDERR 'evap= ',evap
   STDERR 'tx=   ',tx
   STDERR 'ty=   ',ty
   STDERR 'qe=   ',qe
   STDERR 'qh=   ',qh
   STDERR

   STDERR 'short wave radiation and albedo:'
   STDERR 'yday=   ',yday
   STDERR 'dlon=   ',dlon
   STDERR 'dlat=   ',dlat
   do i=0,24
      hh=i*_ONE_
      swr = short_wave_radiation(yday,hh,dlon,dlat,cloud)
      albedo = albedo_water(1,yday,hh,dlon,dlat)
      STDERR hh,swr,albedo
   end do

   end program test_airsea
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

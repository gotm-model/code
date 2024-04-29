#include "cppdefs.h"
!
   program test_airsea
!
! !DESCRIPTION:
!  program to test the airsea inter action routines
!
!  To build:
!  make test_airsea
!  To execute:
!  ./test_airsea
!  To plot:
!  python $GOTM_BASE/scr/plot_airsea.py
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
!  longwave-radiation
   integer  :: longwave_radiation_method=1 ! 2,3,4,5,6...
   REALTYPE :: dlat=45.
   REALTYPE :: cloud=0.5
   REALTYPE :: ql
!  heat and momentum fluxes
   integer  :: fluxes_method=1 ! 2....
   REALTYPE :: u10=5.,ssu=0.
   REALTYPE :: v10=-5.,ssv=0.
   REALTYPE :: precip=0.
   REALTYPE :: evap,tx,ty,qe,qh
!  short wave radiation
   integer  :: yday=183.
   REALTYPE :: hh
   REALTYPE :: dlon=0.
   REALTYPE :: albedo, albedo_water
   REALTYPE :: swr,shortwave_radiation
   REALTYPE :: z1,z2,z3,z4,solar_zenith_angle
   REALTYPE :: s1,s2,s3,s4
   REALTYPE :: ql1,ql2,ql3,ql4,ql5,ql6

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
   STDERR 'net longwave radiation:'
   call longwave_radiation(1,dlat,tw_k,ta_k,cloud,ql)
   STDERR 'Clark=      ',ql
   call longwave_radiation(2,dlat,tw_k,ta_k,cloud,ql)
   STDERR 'Hastenrath= ',ql
   call longwave_radiation(3,dlat,tw_k,ta_k,cloud,ql)
   STDERR 'Bignami=    ',ql
   call longwave_radiation(4,dlat,tw_k,ta_k,cloud,ql)
   STDERR 'Berliand=   ',ql
   call longwave_radiation(5,dlat,tw_k,ta_k,cloud,ql)
   STDERR 'Josey 1 =   ',ql
   call longwave_radiation(6,dlat,tw_k,ta_k,cloud,ql)
   STDERR 'Josey 2 =   ',ql
!   call longwave_radiation(7,dlat,tw_k,ta_k,cloud,ql)
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

   STDERR 'solar zenith andgle and short wave radiation (during 1 day):'
   STDERR 'yday=   ',yday
   STDERR 'dlon=   ',dlon
   STDERR 'dlat is varying'
   write(100,'(a5,4a8)') 'hour','00','30','60','90'
   write(101,'(a5,4a8)') 'hour','00','30','60','90'
   do i=0,48
      hh=i*_HALF_
      z1 = solar_zenith_angle(yday,hh,dlon,_ONE_*0.)
      z2 = solar_zenith_angle(yday,hh,dlon,_ONE_*30.)
      z3 = solar_zenith_angle(yday,hh,dlon,_ONE_*60.)
      z4 = solar_zenith_angle(yday,hh,dlon,_ONE_*90.)
      s1 = shortwave_radiation(z1,yday,dlon,_ONE_*0. ,cloud)
      s2 = shortwave_radiation(z2,yday,dlon,_ONE_*30.,cloud)
      s3 = shortwave_radiation(z3,yday,dlon,_ONE_*60.,cloud)
      s4 = shortwave_radiation(z4,yday,dlon,_ONE_*90.,cloud)
      write(100,'(F5.1,5F8.4)') hh,z1,z2,z3,z4
      write(101,'(F5.1,5F8.2)') hh,s1,s2,s3,s4
   end do

   STDERR 'solar zenith andgle and short wave radiation (during 1 year):'
   STDERR 'dlon=   ',dlon
   STDERR 'dlat is varying'
   write(200,'(a5,4a8)') 'hour','00','30','60','90'
   write(201,'(a5,4a8)') 'hour','00','30','60','90'
   do i=1,365
      hh= 12.
      z1 = solar_zenith_angle(i,hh,dlon,_ONE_*0.)
      z2 = solar_zenith_angle(i,hh,dlon,_ONE_*30.)
      z3 = solar_zenith_angle(i,hh,dlon,_ONE_*60.)
      z4 = solar_zenith_angle(i,hh,dlon,_ONE_*90.)
      s1 = shortwave_radiation(z1,i,dlon,_ONE_*0. ,cloud)
      s2 = shortwave_radiation(z2,i,dlon,_ONE_*30.,cloud)
      s3 = shortwave_radiation(z3,i,dlon,_ONE_*60.,cloud)
      s4 = shortwave_radiation(z4,i,dlon,_ONE_*90.,cloud)
      write(200,'(i5,5F8.4)') i,z1,z2,z3,z4
      write(201,'(i5,5F8.2)') i,s1,s2,s3,s4
   end do

   cloud = 1.0
   tw=283.15
   STDERR
   STDERR 'net longwave radiation:'
   STDERR 'dlat=   ',dlat
   STDERR 'tw=     ',tw
   STDERR 'air temperature is varying'
   write(300,'(a)') 'ta-tw clark hastenrath bignami berliand josey_1 josey_2'
   do i=-20,20
      ta= i*0.5+tw
      call longwave_radiation(1,dlat,tw,ta,cloud,ql1)
      call longwave_radiation(2,dlat,tw,ta,cloud,ql2)
      call longwave_radiation(3,dlat,tw,ta,cloud,ql3)
      call longwave_radiation(4,dlat,tw,ta,cloud,ql4)
      call longwave_radiation(5,dlat,tw,ta,cloud,ql5)
      call longwave_radiation(6,dlat,tw,ta,cloud,ql6)
      write(300,'(7F8.1)') ta-tw,ql1,ql2,ql3,ql4,ql5,ql6
   end do

   end program test_airsea
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

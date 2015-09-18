#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Variable registration 
!
! !INTERFACE:
   subroutine register_all_variables(lat,lon,nlev,fm)
!
! !DESCRIPTION:
!
! !USES:
   use field_manager
   use airsea_variables, only: es,ea,qs,qa,rhoa
   use airsea
   use meanflow
   use turbulence
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in) :: lat,lon
   integer, intent(in)  :: nlev

! !INPUT/OUTPUT PARAMETERS:
!KB   class (type_field_manager), target intent(inout) :: fm
   type (type_field_manager), target :: fm
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'register_all_variables()'

!  register - dimension
   call fm%register_dimension('lon',1,id=id_dim_lon)
   call fm%register_dimension('lat',1,id=id_dim_lat)
   call fm%register_dimension('z',nlev,id=id_dim_z)
   call fm%register_dimension('z1',nlev,id=id_dim_z1)
   call fm%register_dimension('time',id=id_dim_time)
   call fm%initialize(prepend_by_default=(/id_dim_lon,id_dim_lat/),append_by_default=(/id_dim_time/))

   call fm%register('lon','degrees_east','longitude',dimensions=(/id_dim_lon/),no_default_dimensions=.true.,data0d=lon,coordinate_dimension=id_dim_lon)
   call fm%register('lat','degrees_north','latitude',dimensions=(/id_dim_lat/),no_default_dimensions=.true.,data0d=lat,coordinate_dimension=id_dim_lat)

!  register - airsea_variables
   call fm%register('es', 'Pascal', 'saturation water vapor pressure', standard_name='', data0d=es)
   call fm%register('ea', 'Pascal', 'actual water vapor presure', standard_name='', data0d=ea)
   call fm%register('qs', 'kg/kg', 'saturation specific humidity', standard_name='', data0d=qs)
   call fm%register('qa', 'kg/kg', 'specific humidity', standard_name='', data0d=qa)
   call fm%register('rhoa', 'kg/m3', 'air density', standard_name='', data0d=rhoa)

!  register - airsea
   call fm%register('I_0', 'W/m2', 'incoming short wave radiation', standard_name='', data0d=I_0)
   call fm%register('albedo', '', 'albedo', standard_name='', data0d=albedo)
   call fm%register('qe', 'W/m2', 'sensible heat', standard_name='', data0d=qe)
   call fm%register('qh', 'W/m2', 'latent heat', standard_name='', data0d=qh)
   call fm%register('qb', 'W/m2', 'long-wave back radiation', standard_name='', data0d=qb)
   call fm%register('heat', 'W/m2', 'surface heat fluxes', standard_name='', data0d=heat)
   call fm%register('tx', 'Pa', 'wind stress (x)', standard_name='', data0d=tx)
   call fm%register('ty', 'Pa', 'wind stress (y)', standard_name='', data0d=ty)
   call fm%register('precip', 'm/s', 'precipitation', standard_name='', data0d=precip)
   call fm%register('evap', 'm/s', 'evaporation', standard_name='', data0d=evap)
   call fm%register('sst', 'Celsius', 'sea surface temperature', standard_name='sea_surface_temperature', data0d=sst)
   call fm%register('sst_obs', 'Celsius', 'observed sea surface temperature', standard_name='sea_surface_temperature', data0d=sst_obs)
   call fm%register('sss', 'PSU', 'sea surface salinity', standard_name='sea_surface_salinity', data0d=sss)
   call fm%register('cloud', '', 'cloud cover', standard_name='', data0d=cloud)
   select case (hum_method)
      case (1) ! relative humidity in % given
         call fm%register('hum', '%', 'relative humidity', standard_name='', data0d=rh)
      case (2)  ! Specific humidity from wet bulb temperature
         call fm%register('hum', 'Celsius', 'wet bulb temperature', standard_name='', data0d=twet)
      case (3)  ! Specific humidity from dew point temperature
         call fm%register('hum', 'Celsius', 'dew point temperature', standard_name='', data0d=tdew)
      case (4)  ! Specific humidity given
!KB - check data source
         call fm%register('hum', 'kg/kg', 'specific humidity', standard_name='', data0d=rh)
   end select
   call fm%register('airt', 'Celsius', '2m air temperature', standard_name='', data0d=airt)
   call fm%register('u10', 'm/s', '10m wind (x)', standard_name='', data0d=u10)
   call fm%register('v10', 'm/s', '10m wind (y)', standard_name='', data0d=v10)
   call fm%register('airp', 'Pa', 'air pressure', standard_name='', data0d=airp)
   call fm%register('int_precip', 'm', 'integrated precipitation', standard_name='', data0d=int_precip)
   call fm%register('int_evap','m', 'integrated evaporation', standard_name='', data0d=int_evap)
   call fm%register('int_fwf','m', 'integrated fresh water fluxes', standard_name='', data0d=int_fwf)
   call fm%register('int_swr','J/m2', 'integrated short wave radiation', standard_name='', data0d=int_swr)
   call fm%register('int_heat','J/m2', 'integrated surface heat fluxes', standard_name='', data0d=int_heat)
   call fm%register('int_total','J/m2', 'integrated total surface heat exchange', standard_name='', data0d=int_total)

!  register - meanflow
   call fm%register('ga', '', 'coordinate scaling', standard_name='??', dimensions=(/id_dim_z/), data1d=ga(1:nlev),category='meanflow')
   call fm%register('z', 'm', 'depth', standard_name='??', dimensions=(/id_dim_z/), data1d=z(1:nlev), coordinate_dimension=id_dim_z,category='meanflow')
   call fm%register('h', 'm', 'layer thickness', standard_name='cell_thickness', dimensions=(/id_dim_z/), data1d=h(1:nlev),category='meanflow')
   call fm%register('ho', 'm', 'layer thickness - old time step', standard_name='cell_thickness', dimensions=(/id_dim_z/), data1d=h(1:nlev),category='meanflow')
   call fm%register('u', 'm/s', 'x-velocity', standard_name='??', dimensions=(/id_dim_z/), data1d=u(1:nlev), category='meanflow/velocities')
   call fm%register('uo', 'm/s', 'x-velocity - old time step', standard_name='??', dimensions=(/id_dim_z/), data1d=uo(1:nlev), category='meanflow/velocities', output_level=output_level_debug)
   call fm%register('v', 'm/s', 'y-velocity', standard_name='??', dimensions=(/id_dim_z/), data1d=v(1:nlev), category='meanflow/velocities')
   call fm%register('vo', 'm/s', 'y-velocity - old time step', standard_name='??', dimensions=(/id_dim_z/), data1d=vo(1:nlev), category='meanflow/velocities', output_level=output_level_debug)
!KB   call fm%register('w', 'm/s', 'z-velocity', standard_name='??', dimensions=(/id_dim_z/), data1d=w(1:nlev), category='meanflow/velocities,')
   call fm%register('fric', '', 'extra friction coefficient in water column', standard_name='??', dimensions=(/id_dim_z/), data1d=fric(1:nlev),category='meanflow')
   call fm%register('drag', '', 'drag coefficient in water column', standard_name='??', dimensions=(/id_dim_z/), data1d=drag(1:nlev),category='meanflow')
   call fm%register('temp', 'Celsius', 'potential temperature', standard_name='sea_water_temperature', dimensions=(/id_dim_z/), data1d=T(1:nlev),category='meanflow')
   call fm%register('salt', 'g/kg', 'salinity', standard_name='sea_water_practical_salinity', dimensions=(/id_dim_z/), data1d=S(1:nlev),category='meanflow')
   call fm%register('rho', 'kg/m3', 'potential density', standard_name='??', dimensions=(/id_dim_z/), data1d=rho(1:nlev),category='meanflow')
   call fm%register('NN', '1/s**2', 'buoyancy frequency squared', standard_name='??', dimensions=(/id_dim_z/), data1d=NN(1:nlev),category='meanflow')
   call fm%register('NNT', '1/s**2', 'contribution of T-gradient to buoyancy frequency squared', standard_name='??', dimensions=(/id_dim_z/), data1d=NNT(1:nlev),category='meanflow')
   call fm%register('NNS', '1/s**2', 'contribution of S-gradient to buoyancy frequency squared', standard_name='??', dimensions=(/id_dim_z/), data1d=NNS(1:nlev),category='meanflow')
   call fm%register('SS', '1/s**2', 'shear frequency squared', standard_name='??', dimensions=(/id_dim_z/), data1d=SS(1:nlev),category='meanflow')

!  SSU and SSV are both in meanflow and airsea
!KB   call fm%register('SSU', '1/s**2', 'x-contribution to shear frequency squared', standard_name='??', dimensions=(/id_dim_z/), data1d=SSU(1:nlev),category='meanflow')
!KB   call fm%register('SSV', '1/s**2', 'y-contribution to shear frequency squared', standard_name='??', dimensions=(/id_dim_z/), data1d=SSV(1:nlev),category='meanflow')
   call fm%register('xP', 'm**2/s**3', 'extra turbulence production', standard_name='??', dimensions=(/id_dim_z/), data1d=xP(1:nlev),category='meanflow')
   call fm%register('buoy', 'm/s**2', 'buoyancy', standard_name='??', dimensions=(/id_dim_z/), data1d=buoy(1:nlev),category='meanflow')
   call fm%register('rad', 'W/m**2', 'short-wave radiation', standard_name='??', dimensions=(/id_dim_z/), data1d=rad(1:nlev),category='meanflow')
   call fm%register('avh', 'm**2/s', 'eddy diffusivity', standard_name='??', dimensions=(/id_dim_z/), data1d=avh(1:nlev),category='meanflow')
   call fm%register('bioshade', '', 'degree of bio-shading', standard_name='??', dimensions=(/id_dim_z/), data1d=bioshade(1:nlev),category='meanflow')
#ifdef EXTRA_OUTPUT
   call fm%register('mean1', '??', '1. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean1(1:nlev),category='meanflow',output_level=output_level_debug)
   call fm%register('mean2', '??', '2. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean2(1:nlev),category='meanflow',output_level=output_level_debug)
   call fm%register('mean3', '??', '3. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean3(1:nlev),category='meanflow',output_level=output_level_debug)
   call fm%register('mean4', '??', '4. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean4(1:nlev),category='meanflow',output_level=output_level_debug)
   call fm%register('mean5', '??', '5. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean5(1:nlev),category='meanflow',output_level=output_level_debug)
#endif

!  register - turbulence
   call fm%register('tke', 'm2/s2', 'turbulent kinetic energy', standard_name='??', dimensions=(/id_dim_z1/), data1d=tke(1:nlev),category='turbulence')
   call fm%register('tkeo', 'm2/s2', 'turbulent kinetic energy - old time step', standard_name='??', dimensions=(/id_dim_z1/), data1d=tkeo(1:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('eps', 'm2/s3', 'energy dissipation rate', standard_name='??', dimensions=(/id_dim_z1/), data1d=eps(1:nlev),category='turbulence')
   call fm%register('L', 'm', 'turbulence length scale', standard_name='??', dimensions=(/id_dim_z1/), data1d=L(1:nlev),category='turbulence')
   call fm%register('kb', 'm2/s4', '(half) buoyancy variance', standard_name='??', dimensions=(/id_dim_z1/), data1d=kb(1:nlev),category='turbulence')
   call fm%register('epsb', 'm2/s5', 'destruction rate of buoyancy variance', standard_name='??', dimensions=(/id_dim_z1/), data1d=epsb(1:nlev),category='turbulence')
   call fm%register('P', 'm2/s3', 'shear production', standard_name='??', dimensions=(/id_dim_z1/), data1d=P(1:nlev),category='turbulence')
   call fm%register('G', 'm2/s3', 'buoyancy production', standard_name='??', dimensions=(/id_dim_z1/), data1d=B(1:nlev),category='turbulence')
   call fm%register('Pb', 'm2/s5', 'production of kb', standard_name='??', dimensions=(/id_dim_z1/), data1d=Pb(1:nlev),category='turbulence')
   call fm%register('num', 'm2/s', 'turbulent diffusivity of momentum', standard_name='??', dimensions=(/id_dim_z1/), data1d=num(1:nlev),category='turbulence')
   call fm%register('nuh', 'm2/s', 'turbulent diffusivity of heat', standard_name='??', dimensions=(/id_dim_z1/), data1d=nuh(1:nlev),category='turbulence')
   call fm%register('nus', 'm2/s', 'turbulent diffusivity of salt', standard_name='??', dimensions=(/id_dim_z1/), data1d=nus(1:nlev),category='turbulence')
   call fm%register('gamu', 'm2/s2', 'non-local flux of u-momentum', standard_name='??', dimensions=(/id_dim_z1/), data1d=gamu(1:nlev),category='turbulence')
   call fm%register('gamv', 'm2/s2', 'non-local flux of v-momentum', standard_name='??', dimensions=(/id_dim_z1/), data1d=gamv(1:nlev),category='turbulence')
   call fm%register('gamb', 'm2/s3', 'non-local  buoyancy flux', standard_name='??', dimensions=(/id_dim_z1/), data1d=gamb(1:nlev),category='turbulence')
   call fm%register('gamh', 'K m/s', 'non-local heat flux', standard_name='??', dimensions=(/id_dim_z1/), data1d=gamh(1:nlev),category='turbulence')
   call fm%register('gams', 'g/kg m/s', 'non-local salinity flux', standard_name='??', dimensions=(/id_dim_z1/), data1d=gams(1:nlev),category='turbulence')
   call fm%register('cmue1', '', 'stability function for momentum diffusivity', standard_name='??', dimensions=(/id_dim_z1/), data1d=cmue1(1:nlev),category='turbulence')
   call fm%register('cmue2', '', 'stability function for scalar diffusivity', standard_name='??', dimensions=(/id_dim_z1/), data1d=cmue2(1:nlev),category='turbulence')
   call fm%register('gam', '', 'non-dimensinsional non-local buoyancy flux', standard_name='??', dimensions=(/id_dim_z1/), data1d=gam(1:nlev),category='turbulence')
   call fm%register('an', '', 'non-dimensional buoyancy time scale', standard_name='??', dimensions=(/id_dim_z1/), data1d=an(1:nlev),category='turbulence')
   call fm%register('as', '', 'non-dimensional shear time scale', standard_name='??', dimensions=(/id_dim_z1/), data1d=as(1:nlev),category='turbulence')
   call fm%register('at', '', 'non-dimensional buoyancy variance', standard_name='??', dimensions=(/id_dim_z1/), data1d=at(1:nlev),category='turbulence')
   call fm%register('r', '', 'turbulent time scale ratio', standard_name='??', dimensions=(/id_dim_z1/), data1d=r(1:nlev),category='turbulence')
   call fm%register('Rig', '', 'gradient Richardson number', standard_name='??', dimensions=(/id_dim_z1/), data1d=Rig(1:nlev),category='turbulence')
   call fm%register('xRf', '', 'flux Richardson number', standard_name='??', dimensions=(/id_dim_z1/), data1d=xRf(1:nlev),category='turbulence')
   call fm%register('uu', 'm2/s2', 'variance of u-fluctuations', standard_name='??', dimensions=(/id_dim_z1/), data1d=uu(1:nlev),category='turbulence')
   call fm%register('vv', 'm2/s2', 'variance of v-fluctuations', standard_name='??', dimensions=(/id_dim_z1/), data1d=vv(1:nlev),category='turbulence')
   call fm%register('ww', 'm2/s2', 'variance of w-fluctuations', standard_name='??', dimensions=(/id_dim_z1/), data1d=ww(1:nlev),category='turbulence')
#ifdef EXTRA_OUTPUT
   call fm%register('turb1', '', '1. turbulence dummy variable', dimensions=(/id_dim_z1/), data1d=turb1(1:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('turb2', '', '2. turbulence dummy variable', dimensions=(/id_dim_z1/), data1d=turb2(1:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('turb3', '', '3. turbulence dummy variable', dimensions=(/id_dim_z1/), data1d=turb3(1:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('turb4', '', '4. turbulence dummy variable', dimensions=(/id_dim_z1/), data1d=turb4(1:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('turb5', '', '5. turbulence dummy variable', dimensions=(/id_dim_z1/), data1d=turb5(1:nlev),category='turbulence',output_level=output_level_debug)
#endif

!  register - fabm
!         call fm%register(model%state_variables(i)%name, model%state_variables(i)%units, &
!         call fm%register(model%bottom_state_variables(i)%name, model%bottom_state_variables(i)%units, &
!         call fm%register(model%surface_state_variables(i)%name, model%surface_state_variables(i)%units, &
   return
   end subroutine register_all_variables
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

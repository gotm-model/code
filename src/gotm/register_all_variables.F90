#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: register_all_variables
!
! !INTERFACE:
   module register_all_variables
!
! !DESCRIPTION:
!
! !USES:
   use field_manager
   IMPLICIT NONE
!
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public :: do_register_all_variables
!
! !PUBLIC DATA MEMBERS:
   type (type_field_manager), public, target :: fm
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Jorn Bruggeman
!
! !PRIVATE DATA MEMBERS
   integer :: N=0
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: do_register_all_variables
!
! !INTERFACE:
   subroutine do_register_all_variables(lat,lon,nlev)
!
! !USES:
   IMPLICIT NONE
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: lat,lon
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Jorn Bruggeman
!
! !LOCAL VARIABLES:
!EOP
!-------------------------------------------------------------------------
!BOC
   LEVEL1 'register_all_variables()'
   call register_coordinate_variables(lat,lon)
   call register_meanflow_variables(nlev)
   call register_airsea_variables(nlev)
#ifdef _ICE_
   call register_stim_variables(nlev)
#endif
   call register_observation_variables(nlev)
   call register_stokes_drift_variables(nlev)
#if 0
   call register_stream_variables(nlev)
#endif
   call register_turbulence_variables(nlev)
   call register_diagnostic_variables(nlev)
!   LEVEL2 'registrated ',N,'variables'
   return
   end subroutine do_register_all_variables
!EOC

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Coordinate variable registration
!
! !INTERFACE:
   subroutine register_coordinate_variables(lat,lon)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in) :: lat,lon
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_coordinate_variables()'

!  register - dimension
   call fm%register('lon','degrees_east','longitude',dimensions=(/id_dim_lon/),no_default_dimensions=.true.,data0d=lon,coordinate_dimension=id_dim_lon)
   call fm%register('lat','degrees_north','latitude',dimensions=(/id_dim_lat/),no_default_dimensions=.true.,data0d=lat,coordinate_dimension=id_dim_lat)

   end subroutine register_coordinate_variables
!EOC

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: airsea variable registration
!
! !INTERFACE:
   subroutine register_airsea_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
   use airsea_variables, only: es,ea,qs,qa,rhoa
   use airsea_driver
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_airsea_variables()'
   call fm%register('u10', 'm/s', '10m wind (x)', standard_name='', data0d=u10%value, category='surface')
   call fm%register('v10', 'm/s', '10m wind (y)', standard_name='', data0d=v10%value, category='surface')
   call fm%register('airt', 'Celsius', '2m air temperature', standard_name='', data0d=airt%value, category='surface')
   call fm%register('airp', 'Pa', 'air pressure', standard_name='', data0d=airp%value, category='surface')
   select case (hum_method)
      case (1) ! relative humidity in % given
         call fm%register('hum', '%', 'relative humidity', standard_name='', data0d=hum%value, category='surface')
      case (2)  ! Specific humidity from wet bulb temperature
         call fm%register('hum', 'Celsius', 'wet bulb temperature', standard_name='', data0d=hum%value, category='surface')
      case (3)  ! Specific humidity from dew point temperature
         call fm%register('hum', 'Celsius', 'dew point temperature', standard_name='', data0d=hum%value, category='surface')
      case (4)  ! Specific humidity given
         call fm%register('hum', 'kg/kg', 'specific humidity', standard_name='', data0d=hum%value, category='surface')
   end select
   call fm%register('es', 'Pa', 'saturation water vapor pressure', standard_name='', data0d=es, category='surface')
   call fm%register('ea', 'Pa', 'actual water vapor presure', standard_name='', data0d=ea, category='surface')
   call fm%register('qs', 'kg/kg', 'saturation specific humidity', standard_name='', data0d=qs, category='surface')
   call fm%register('qa', 'kg/kg', 'specific humidity', standard_name='', data0d=qa, category='surface')
   call fm%register('rhoa', 'kg/m3', 'air density', standard_name='', data0d=rhoa, category='surface')
   call fm%register('cloud', '', 'cloud cover', standard_name='', data0d=cloud%value, category='surface')
   call fm%register('albedo', '', 'albedo', standard_name='', data0d=albedo, category='surface')
   call fm%register('precip', 'm/s', 'precipitation', standard_name='', data0d=precip%value, category='surface')
   call fm%register('evap', 'm/s', 'evaporation', standard_name='', data0d=evap, category='surface')

   call fm%register('int_precip', 'm', 'integrated precipitation', standard_name='', data0d=int_precip, category='surface')
   call fm%register('int_evap','m', 'integrated evaporation', standard_name='', data0d=int_evap, category='surface')
!KB   call fm%register('int_fwf','m', 'integrated fresh water fluxes', standard_name='', data0d=int_fwf)
   call fm%register('int_swr','J/m2', 'integrated short wave radiation', standard_name='', data0d=int_swr, category='surface')
   call fm%register('int_heat','J/m2', 'integrated surface heat fluxes', standard_name='', data0d=int_heat, category='surface')
   call fm%register('int_total','J/m2', 'integrated total surface heat exchange', standard_name='', data0d=int_total, category='surface')

   call fm%register('I_0', 'W/m2', 'incoming short wave radiation', standard_name='', data0d=I_0%value, category='surface/heat_fluxes')
   call fm%register('qh', 'W/m2', 'sensible heat flux', standard_name='', data0d=qh, category='surface/heat_fluxes')
   call fm%register('qe', 'W/m2', 'latent heat flux', standard_name='', data0d=qe, category='surface/heat_fluxes')
   call fm%register('ql', 'W/m2', 'net longwave radiation', standard_name='', data0d=ql%value, category='surface/heat_fluxes')
   call fm%register('heat', 'W/m2', 'net surface heat flux', standard_name='', data0d=heat%value, category='surface/heat_fluxes')
   call fm%register('tx', 'm2/s2', 'wind stress (x)', standard_name='', data0d=tx, category='surface')
   call fm%register('ty', 'm2/s2', 'wind stress (y)', standard_name='', data0d=ty, category='surface')
   call fm%register('sst', 'Celsius', 'sea surface temperature', standard_name='sea_surface_temperature', data0d=sst, category='surface')
   call fm%register('sst_obs', 'Celsius', 'observed sea surface temperature', standard_name='sea_surface_temperature', data0d=sst_obs%value, category='surface')
   call fm%register('sss', '1e-3', 'sea surface salinity', standard_name='sea_surface_salinity', data0d=sss%value, category='surface')

   end subroutine register_airsea_variables
!EOC

#ifdef _ICE_
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: ice variable registration
!
! !INTERFACE:
   subroutine register_stim_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
   use ice, only: ice_model
   use stim_variables, only: Tice_surface,Tice,Tf
   use stim_variables, only: Hice, Hfrazil, dHis, dHib
   use stim_variables, only: surface_ice_energy,bottom_ice_energy
   use stim_variables, only: ocean_ice_flux
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_stim_variables()'

   call fm%register('Tice_surface', 'celsius', 'ice temperature (surface)', standard_name='', data0d=Tice_surface, category='ice')
   if (ice_model .eq. 3) then
      call fm%register('T1', 'celsius', 'ice temperature (upper)', standard_name='', data0d=Tice(1), category='ice')
      call fm%register('T2', 'celsius', 'ice temperature (lower)', standard_name='', data0d=Tice(2), category='ice')
   end if
   call fm%register('Tf', 'celsius', 'ice freezing temperature', standard_name='', data0d=Tf, category='ice')
   call fm%register('Hice', 'm', 'ice thickness', standard_name='', data0d=Hice, category='ice')
   call fm%register('surface_ice_energy', 'J/m2', 'ice energy (surface)', standard_name='', data0d=surface_ice_energy, category='ice')
   call fm%register('bottom_ice_energy', 'J/m2', 'ice energy (bottom)', standard_name='', data0d=bottom_ice_energy, category='ice')
   call fm%register('ocean_ice_flux', 'W/m2', 'ocean-ice heat flux', standard_name='', data0d=ocean_ice_flux, category='ice')
   call fm%register('Hfrazil', 'm', 'ice thickness (frazil)', standard_name='', data0d=Hfrazil, category='ice')
   call fm%register('dHis', 'm', 'ice growth (surface)', standard_name='', data0d=dHis, category='ice')
   call fm%register('dHib', 'm', 'ice growth (bottom)', standard_name='', data0d=dHib, category='ice')

   return
   end subroutine register_stim_variables
!EOC
#endif

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: observation variable registration
!
! !INTERFACE:
   subroutine register_observation_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
  use observations
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_observation_variables()'
   call fm%register('temp_obs', 'Celsius', 'observed temperature', standard_name='sea_water_temperature', dimensions=(/id_dim_z/), data1d=tprof%data(1:nlev),category='temperature_and_salinity')
   call fm%register('salt_obs', 'psu', 'observed salinity', standard_name='sea_water_salinity', dimensions=(/id_dim_z/), data1d=sprof%data(1:nlev),category='temperature_and_salinity')
   call fm%register('u_obs', 'm/s', 'observed x-velocity', dimensions=(/id_dim_z/), data1d=uprof%data(1:nlev), category='velocities')
   call fm%register('v_obs', 'm/s', 'observed y-velocity', dimensions=(/id_dim_z/), data1d=vprof%data(1:nlev), category='velocities')
   call fm%register('zeta', 'm', 'sea surface elevation', standard_name='sea_surface_elevation', data0d=zeta%value,category='surface')
   if (epsprof%method /= 0) then
      call fm%register('eps_obs', 'm2/s3', 'observed dissipation', dimensions=(/id_dim_z/), data1d=epsprof%data(1:nlev), category='turbulence')
   end if

   return
   end subroutine register_observation_variables
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Stokes drift variable registration
!
! !INTERFACE:
   subroutine register_stokes_drift_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
   use stokes_drift
   use turbulence, only: turb_method
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_stokes_drift_variables()'
   call fm%register('us', 'm/s', 'Stokes drift x-component', dimensions=(/id_dim_z/), data1d=usprof%data(1:nlev),category='stokes_drift')
   call fm%register('vs', 'm/s', 'Stokes drift y-component', dimensions=(/id_dim_z/), data1d=vsprof%data(1:nlev),category='stokes_drift')
   call fm%register('dusdz', '1/s', 'Stokes drift shear x-component', dimensions=(/id_dim_z/), data1d=dusdz%data(1:nlev),category='stokes_drift')
   call fm%register('dvsdz', '1/s', 'Stokes drift shear y-component', dimensions=(/id_dim_z/), data1d=dvsdz%data(1:nlev),category='stokes_drift')
   call fm%register('us0', 'm/s', 'surface Stokes drift x-component', data0d=us0%value, category='stokes_drift')
   call fm%register('vs0', 'm/s', 'surface Stokes drift y-component', data0d=vs0%value, category='stokes_drift')
   call fm%register('ds', 'm', 'Stokes drift penetration depth', data0d=ds%value, category='stokes_drift')

#ifdef _CVMIX_
   if (turb_method .ne. 100) return

   call fm%register('La_Turb', '', 'Turbulent Langmuir number', data0d=La_Turb, category='stokes_drift')
   call fm%register('La_SL', '', 'Surface layer averaged Langmuir number', data0d=La_SL, category='stokes_drift')
   call fm%register('La_SLP_VR12', '', 'Surface layer averaged and projected Langmuir number (Van Roekel et al., 2012)', data0d=La_SLP_VR12, category='stokes_drift')
   call fm%register('La_SLP_RWH16', '', 'Surface layer averaged and projected Langmuir number (Reichl et al., 2016)', data0d=La_SLP_RWH16, category='stokes_drift')
   call fm%register('EFactor_LWF16', '', 'Enhancement factor for Langmuir mixing (Li et al., 2016)', data0d=EFactor_LWF16, category='stokes_drift')
   call fm%register('EFactor_RWH16', '', 'Enhancement factor for Langmuir mixing (Reichl et al., 2016)', data0d=EFactor_RWH16, category='stokes_drift')
   call fm%register('theta_WW', 'rad', 'Angle between wind and waves', data0d=theta_WW, category='stokes_drift')
   call fm%register('theta_WL', 'rad', 'Angle between wind and Langmiur cells', data0d=theta_WL, category='stokes_drift')
#endif

   return
   end subroutine register_stokes_drift_variables
!EOC

#if 0
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: stream variable registration
!
! !INTERFACE:
   subroutine register_stream_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
   use observations, only: Q, Qs, Qt, wq, FQ, Qres
   use streams
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
   integer :: istream
   type (type_stream), pointer :: current_stream
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_stream_variables()'
   if (nstreams>0) then
      call fm%register('Q', 'm3/s', 'inflows over water column', standard_name='??', dimensions=(/id_dim_z/), data1d=Q(1:nlev), category='streams')
      call fm%register('Qs', '1/s', 'salt inflow', standard_name='??', dimensions=(/id_dim_z/), data1d=Qs(1:nlev), category='streams')
      call fm%register('Qt', 'Celsius/s', 'temperature inflow', standard_name='??', dimensions=(/id_dim_z/), data1d=Qt(1:nlev), category='streams')
      call fm%register('wq', 'm/s', 'vertical water balance advection velocity', standard_name='??', dimensions=(/id_dim_z/), data1d=wq(1:nlev), category='streams')
      call fm%register('FQ', 'm3/s', 'vertical water balance flux', standard_name='??', dimensions=(/id_dim_z/), data1d=FQ(1:nlev), category='streams')
      call fm%register('Qres', 'm3/s', 'residual water balance inflows', standard_name='??', dimensions=(/id_dim_z/), data1d=Qres(1:nlev), category='streams')
   end if

   current_stream => first_stream
   do while (associated(current_stream))
      call fm%register('Q_'//trim(current_stream%name), 'm3/s', 'stream (Q): '//trim(current_stream%name), data0d=current_stream%QI, category='streams')
      if (current_stream%has_T) then
         call fm%register('T_'//trim(current_stream%name), 'Celsius', 'stream (T): '//trim(current_stream%name), data0d=current_stream%TI, category='streams')
      end if
      current_stream => current_stream%next
   end do

   call fm%register('int_inflow', 'm3/s', 'integrated inflow', data0d=int_inflow, category='streams')
   call fm%register('int_outflow', 'm3/s', 'integrated outflow', data0d=int_outflow, category='streams')

   return
   end subroutine register_stream_variables
!EOC
#endif

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: meanflow variable registration
!
! !INTERFACE:
   subroutine register_meanflow_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
   use meanflow
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_meanflow_variables()'
   call fm%register('temp', 'Celsius', 'potential temperature', standard_name='sea_water_temperature', dimensions=(/id_dim_z/), data1d=T(1:nlev),category='temperature_and_salinity', part_of_state=.true.)
   call fm%register('salt', 'g/kg', 'salinity', standard_name='sea_water_practical_salinity', dimensions=(/id_dim_z/), data1d=S(1:nlev),category='temperature_and_salinity', part_of_state=.true.)
   call fm%register('rho', 'kg/m3', 'potential density', standard_name='??', dimensions=(/id_dim_z/), data1d=rho(1:nlev),category='temperature_and_salinity')

   call fm%register('u', 'm/s', 'x-velocity', standard_name='??', dimensions=(/id_dim_z/), data1d=u(1:nlev), category='velocities', part_of_state=.true.)
   call fm%register('uo', 'm/s', 'x-velocity - old time step', standard_name='??', dimensions=(/id_dim_z/), data1d=uo(1:nlev), category='velocities', part_of_state=.true., output_level=output_level_debug)
   call fm%register('v', 'm/s', 'y-velocity', standard_name='??', dimensions=(/id_dim_z/), data1d=v(1:nlev), category='velocities', part_of_state=.true.)
   call fm%register('vo', 'm/s', 'y-velocity - old time step', standard_name='??', dimensions=(/id_dim_z/), data1d=vo(1:nlev), category='velocities', part_of_state=.true., output_level=output_level_debug)
!KB   call fm%register('w', 'm/s', 'z-velocity', standard_name='??', dimensions=(/id_dim_z/), data1d=w(1:nlev), category='velocities,')

   call fm%register('fric', '', 'extra friction coefficient in water column', standard_name='??', dimensions=(/id_dim_z/), data1d=fric(1:nlev),category='turbulence/shear')
   call fm%register('drag', '', 'drag coefficient in water column', standard_name='??', dimensions=(/id_dim_z/), data1d=drag(1:nlev),category='turbulence/shear')
   call fm%register('u_taus', 'm/s', 'surface friction velocity', data0d=u_taus,category='surface')
   call fm%register('u_taub', 'm/s', 'bottom friction velocity', data0d=u_taub,category='bottom')
   call fm%register('u_taubo', 'm/s', 'bottom friction velocity - old time step', data0d=u_taubo,category='bottom', part_of_state=.true., output_level=output_level_debug)
   call fm%register('taub', 'Pa', 'bottom stress', data0d=taub,category='bottom')
   call fm%register('NN', '1/s2', 'buoyancy frequency squared', standard_name='??', dimensions=(/id_dim_zi/), data1d=NN(0:nlev),category='turbulence/buoyancy')
   call fm%register('NNT', '1/s2', 'contribution of T-gradient to buoyancy frequency squared', standard_name='??', dimensions=(/id_dim_zi/), data1d=NNT(0:nlev),category='turbulence/buoyancy')
   call fm%register('NNS', '1/s2', 'contribution of S-gradient to buoyancy frequency squared', standard_name='??', dimensions=(/id_dim_zi/), data1d=NNS(0:nlev),category='turbulence/buoyancy')
   call fm%register('SS', '1/s2', 'shear frequency squared', standard_name='??', dimensions=(/id_dim_zi/), data1d=SS(0:nlev),category='turbulence/shear')
   call fm%register('SSU', '1/s2', 'x-contribution to shear frequency squared', standard_name='??', dimensions=(/id_dim_zi/), data1d=SSU(0:nlev),category='turbulence/shear', output_level=output_level_debug)
   call fm%register('SSV', '1/s2', 'y-contribution to shear frequency squared', standard_name='??', dimensions=(/id_dim_zi/), data1d=SSV(0:nlev),category='turbulence/shear', output_level=output_level_debug)
   call fm%register('xP', 'm2/s3', 'extra turbulence production', standard_name='??', dimensions=(/id_dim_z/), data1d=xP(1:nlev),category='turbulence', part_of_state=.true.)
   call fm%register('buoy', 'm/s2', 'buoyancy', standard_name='??', dimensions=(/id_dim_z/), data1d=buoy(1:nlev),category='turbulence/buoyancy')
   call fm%register('rad', 'W/m2', 'shortwave radiation', standard_name='??', dimensions=(/id_dim_zi/), data1d=rad(0:nlev),category='light')
   call fm%register('avh', 'm2/s', 'eddy diffusivity', standard_name='??', dimensions=(/id_dim_z/), data1d=avh(1:nlev),category='turbulence')
   call fm%register('bioshade', '-', 'fraction of visible light that is not shaded by overlying biogeochemistry', dimensions=(/id_dim_z/), data1d=bioshade(1:nlev),category='light')

   call fm%register('ga', '', 'coordinate scaling', standard_name='??', dimensions=(/id_dim_z/), data1d=ga(1:nlev),category='column_structure')
#if 0
   if (lake) then
      call fm%register('Af', 'm^2', 'hypsograph at grid interfaces', standard_name='??', dimensions=(/id_dim_z/), data1d=Af(1:nlev), category='column_structure')
   end if
#endif
   call fm%register('z', 'm', 'depth', standard_name='??', dimensions=(/id_dim_z/), data1d=z(1:nlev), coordinate_dimension=id_dim_z,category='column_structure')
   call fm%register('zi', 'm', 'interface depth', standard_name='??', dimensions=(/id_dim_zi/), data1d=zi(0:nlev), coordinate_dimension=id_dim_zi,category='column_structure')
   call fm%register('h', 'm', 'layer thickness', standard_name='cell_thickness', dimensions=(/id_dim_z/), data1d=h(1:nlev),category='column_structure',part_of_state=.true.)
   call fm%register('ho', 'm', 'layer thickness - old time step', standard_name='cell_thickness', dimensions=(/id_dim_z/), data1d=h(1:nlev),category='column_structure', part_of_state=.true., output_level=output_level_debug)
#ifdef EXTRA_OUTPUT
   call fm%register('mean1', '??', '1. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean1(1:nlev),category='meanflow',output_level=output_level_debug)
   call fm%register('mean2', '??', '2. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean2(1:nlev),category='meanflow',output_level=output_level_debug)
   call fm%register('mean3', '??', '3. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean3(1:nlev),category='meanflow',output_level=output_level_debug)
   call fm%register('mean4', '??', '4. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean4(1:nlev),category='meanflow',output_level=output_level_debug)
   call fm%register('mean5', '??', '5. mean dummy variable', standard_name='??', dimensions=(/id_dim_z/), data1d=mean5(1:nlev),category='meanflow',output_level=output_level_debug)
#endif
#ifndef _ICE_
   call fm%register('Hice', 'm', 'fake ice thickness', standard_name='', data0d=Hice, category='surface')
#endif
   return
   end subroutine register_meanflow_variables
!EOC

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: turbulence variable registration
!
! !INTERFACE:
   subroutine register_turbulence_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
   use turbulence
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_turbulence_variables()'

   if (turb_method.eq.99) return

   call fm%register('num', 'm2/s', 'turbulent diffusivity of momentum', standard_name='??', dimensions=(/id_dim_zi/), data1d=num(0:nlev),category='turbulence', part_of_state=.true.)
   call fm%register('nuh', 'm2/s', 'turbulent diffusivity of heat', standard_name='??', dimensions=(/id_dim_zi/), data1d=nuh(0:nlev),category='turbulence', part_of_state=.true.)
   call fm%register('nus', 'm2/s', 'turbulent diffusivity of salt', standard_name='??', dimensions=(/id_dim_zi/), data1d=nus(0:nlev),category='turbulence', part_of_state=.true.)
   call fm%register('gamu', 'm2/s2', 'non-local flux of u-momentum', standard_name='??', dimensions=(/id_dim_zi/), data1d=gamu(0:nlev),category='turbulence')
   call fm%register('gamv', 'm2/s2', 'non-local flux of v-momentum', standard_name='??', dimensions=(/id_dim_zi/), data1d=gamv(0:nlev),category='turbulence')
   call fm%register('gamh', 'K m/s', 'non-local heat flux', standard_name='??', dimensions=(/id_dim_zi/), data1d=gamh(0:nlev),category='turbulence')
   call fm%register('gams', 'g/kg m/s', 'non-local salinity flux', standard_name='??', dimensions=(/id_dim_zi/), data1d=gams(0:nlev),category='turbulence')
   call fm%register('Rig', '', 'gradient Richardson number', standard_name='??', dimensions=(/id_dim_zi/), data1d=Rig(0:nlev),category='turbulence')

#ifdef _CVMIX_
   if (turb_method .eq. 100) return
#endif

   call fm%register('tke', 'm2/s2', 'turbulent kinetic energy', standard_name='??', dimensions=(/id_dim_zi/), data1d=tke(0:nlev),category='turbulence', part_of_state=.true.)
   call fm%register('tkeo', 'm2/s2', 'turbulent kinetic energy - old time step', standard_name='??', dimensions=(/id_dim_zi/), data1d=tkeo(0:nlev),category='turbulence', part_of_state=.true., output_level=output_level_debug)
   call fm%register('eps', 'm2/s3', 'energy dissipation rate', standard_name='??', dimensions=(/id_dim_zi/), data1d=eps(0:nlev),category='turbulence', part_of_state=.true.)
   call fm%register('L', 'm', 'turbulence length scale', standard_name='??', dimensions=(/id_dim_zi/), data1d=L(0:nlev),category='turbulence')
   call fm%register('kb', 'm2/s4', '(half) buoyancy variance', standard_name='??', dimensions=(/id_dim_zi/), data1d=kb(0:nlev),category='turbulence/buoyancy')
   call fm%register('epsb', 'm2/s5', 'destruction of buoyancy variance', standard_name='??', dimensions=(/id_dim_zi/), data1d=epsb(0:nlev),category='turbulence/buoyancy')
   call fm%register('P', 'm2/s3', 'shear production', standard_name='??', dimensions=(/id_dim_zi/), data1d=P(0:nlev),category='turbulence/shear')
   call fm%register('G', 'm2/s3', 'buoyancy production', standard_name='??', dimensions=(/id_dim_zi/), data1d=B(0:nlev),category='turbulence/buoyancy')
   call fm%register('Pb', 'm2/s5', 'production of buoyancy variance', standard_name='??', dimensions=(/id_dim_zi/), data1d=Pb(0:nlev),category='turbulence/buoyancy')
   call fm%register('gamb', 'm2/s3', 'non-local  buoyancy flux', standard_name='??', dimensions=(/id_dim_zi/), data1d=gamb(0:nlev),category='turbulence')
   call fm%register('cmue1', '', 'stability function for momentum diffusivity', standard_name='??', dimensions=(/id_dim_zi/), data1d=cmue1(0:nlev),category='turbulence')
   call fm%register('cmue2', '', 'stability function for scalar diffusivity', standard_name='??', dimensions=(/id_dim_zi/), data1d=cmue2(0:nlev),category='turbulence')
   call fm%register('gam', '', 'non-dimensional non-local buoyancy flux', standard_name='??', dimensions=(/id_dim_zi/), data1d=gam(0:nlev),category='turbulence')
   call fm%register('an', '', 'non-dimensional buoyancy time scale', standard_name='??', dimensions=(/id_dim_zi/), data1d=an(0:nlev),category='turbulence')
   call fm%register('as', '', 'non-dimensional shear time scale', standard_name='??', dimensions=(/id_dim_zi/), data1d=as(0:nlev),category='turbulence')
   call fm%register('at', '', 'non-dimensional buoyancy variance', standard_name='??', dimensions=(/id_dim_zi/), data1d=at(0:nlev),category='turbulence')
   call fm%register('r', '', 'turbulent time scale ratio', standard_name='??', dimensions=(/id_dim_zi/), data1d=r(0:nlev),category='turbulence')
   call fm%register('xRf', '', 'flux Richardson number', standard_name='??', dimensions=(/id_dim_zi/), data1d=xRf(0:nlev),category='turbulence')
   call fm%register('uu', 'm2/s2', 'variance of u-fluctuations', standard_name='??', dimensions=(/id_dim_zi/), data1d=uu(0:nlev),category='turbulence/shear')
   call fm%register('vv', 'm2/s2', 'variance of v-fluctuations', standard_name='??', dimensions=(/id_dim_zi/), data1d=vv(0:nlev),category='turbulence/shear')
   call fm%register('ww', 'm2/s2', 'variance of w-fluctuations', standard_name='??', dimensions=(/id_dim_zi/), data1d=ww(0:nlev),category='turbulence/shear')
#ifdef EXTRA_OUTPUT
   call fm%register('turb1', '', '1. turbulence dummy variable', dimensions=(/id_dim_zi/), data1d=turb1(0:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('turb2', '', '2. turbulence dummy variable', dimensions=(/id_dim_zi/), data1d=turb2(0:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('turb3', '', '3. turbulence dummy variable', dimensions=(/id_dim_zi/), data1d=turb3(0:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('turb4', '', '4. turbulence dummy variable', dimensions=(/id_dim_zi/), data1d=turb4(0:nlev),category='turbulence',output_level=output_level_debug)
   call fm%register('turb5', '', '5. turbulence dummy variable', dimensions=(/id_dim_zi/), data1d=turb5(0:nlev),category='turbulence',output_level=output_level_debug)
#endif
   return
   end subroutine register_turbulence_variables
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: diagnostic variable registration
!
! !INTERFACE:
   subroutine register_diagnostic_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
   use diagnostics
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_diagnostic_variables()'
!   LEVEL3 'remeber to add the variables declared and calculated in diagnostics'
#if 0
   call fm%register('mld_surf','m', 'surface mixed layer depth',fill_value=_ZERO_, data0d=mld_surf,category='surface')
   call fm%register('mld_bott','m', 'bottom mixed layer depth',fill_value=_ZERO_, data0d=mld_bott,category='bottom')
#else
   call fm%register('mld_surf','m', 'surface mixed layer depth', data0d=mld_surf,category='surface')
   call fm%register('mld_bott','m', 'bottom mixed layer depth', data0d=mld_bott,category='bottom')
#endif
   call fm%register('taux',  'm2/s2', 'turbulent flux of momentum (x)', dimensions=(/id_dim_zi/), data1d=taux(0:nlev), category='turbulence')
   call fm%register('tauy',  'm2/s2', 'turbulent flux of momentum (y)', dimensions=(/id_dim_zi/), data1d=tauy(0:nlev), category='turbulence')
   call fm%register('Ekin',  'J', 'kinetic energy', data0d=ekin,category='column_integrals')
   call fm%register('Epot',  'J', 'potential energy', data0d=epot,category='column_integrals')
   call fm%register('Eturb', 'J', 'turbulent kinetic energy', data0d=eturb,category='column_integrals')

   return
   end subroutine register_diagnostic_variables
!EOC

!-----------------------------------------------------------------------

   end module register_all_variables

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#if 0
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: AAA variable registration
!
! !INTERFACE:
   subroutine register_AAA_variables(nlev)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: nlev
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_AAA_variables()'

   return
   end subroutine register_AAA_variables
!EOC
#endif

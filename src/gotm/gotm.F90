#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: gotm --- the general framework \label{sec:gotm}
!
! !INTERFACE:
   module gotm
!
! !DESCRIPTION:
! This is 'where it all happens'. This module provides the internal
! routines {\tt init\_gotm()} to initialise the whole model and
! {\tt time\_loop()} to manage the time-stepping of all fields. These
! two routines in turn call more specialised routines e.g.\ of the
! {\tt meanflow} and {\tt turbulence} modules to delegate the job.
!
! !USES:
   use field_manager
   use gsw_mod_toolbox, only: gsw_sa_from_sp, gsw_sp_from_sa
   use gsw_mod_toolbox, only: gsw_ct_from_t, gsw_t_from_ct
   use gsw_mod_toolbox, only: gsw_pt_from_ct, gsw_ct_from_pt, gsw_pt_from_t
   use register_all_variables, only: do_register_all_variables, fm
!KB   use output_manager_core, only:output_manager_host=>host, type_output_manager_host=>type_host,time_unit_second,type_output_category
   use output_manager_core, only:output_manager_host=>host, type_output_manager_host=>type_host,type_output_manager_file=>type_file,time_unit_second,type_output_item
   use output_manager
   use diagnostics
   use settings

   use density, only: init_density,do_density
   use density, only: density_method,T0,S0,p0,rho0,alpha0,beta0,cp
   use density, only: rho, rho_p
   use meanflow
   use input
   use input_netcdf
   use observations
   use stokes_drift
   use time

   use airsea_driver, only: init_airsea,post_init_airsea,do_airsea,clean_airsea
   use airsea_driver, only: surface_fluxes
   use airsea_driver, only: set_sst,set_ssuv,integrated_fluxes
   use airsea_driver, only: fluxes_method
   use airsea_driver, only: wind=>w,tx,ty,I_0,cloud_input,heat_input,precip_input,evap,airp_input,albedo
   use airsea_driver, only: bio_albedo,bio_drag_scale
   use airsea_driver, only: u10_input, v10_input
   use airsea_variables, only: qa,ta

#ifdef _ICE_
   use gotm_stim_driver, only: ice_model
   use gotm_stim_driver, only: init_ice, post_init_ice, do_ice, clean_ice, ice_cover
   use stim_variables, only: Hice, rho_ice
   use stim_variables, only: Tice_surface,albedo_ice,transmissivity
   use stim_variables, only: melt_rate,T_melt,S_melt
   use stim_variables, only: ocean_ice_heat_flux,ocean_ice_salt_flux
#endif

   use turbulence,  only: turb_method
   use turbulence,  only: init_turbulence,post_init_turbulence,do_turbulence
   use turbulence,  only: num,nuh,nus
   use turbulence,  only: const_num,const_nuh
   use turbulence,  only: gamu,gamv,gamh,gams
   use turbulence,  only: Rig
   use turbulence,  only: kappa
   use turbulence,  only: clean_turbulence

   use mtridiagonal,only: init_tridiagonal,clean_tridiagonal

#ifdef _CVMIX_
   use gotm_cvmix,  only: init_cvmix, post_init_cvmix, do_cvmix, clean_cvmix
   use gotm_cvmix,  only: zsbl, sbl_langmuir_method
#endif

#ifdef SEAGRASS
   use seagrass
#endif
#ifdef SPM
   use spm_var, only: spm_calc
   use spm, only: init_spm, set_env_spm, do_spm, end_spm
#endif
#ifdef _FABM_
   use gotm_fabm,only:configure_gotm_fabm,gotm_fabm_create_model,init_gotm_fabm,init_gotm_fabm_state,start_gotm_fabm,set_env_gotm_fabm,do_gotm_fabm,clean_gotm_fabm,fabm_calc
   use gotm_fabm,only:model_fabm=>model,standard_variables_fabm=>standard_variables
   use gotm_fabm, only: fabm_airp, fabm_calendar_date, fabm_julianday
   use gotm_fabm_input,only: configure_gotm_fabm_input, init_gotm_fabm_input
#endif

   IMPLICIT NONE
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public initialize_gotm, integrate_gotm, finalize_gotm

!
! !DEFINED PARAMETERS:
   integer, parameter                  :: namlst=10
#ifdef SEAGRASS
   integer, parameter                  :: unit_seagrass=62
#endif
#ifdef SPM
   integer, parameter                  :: unit_spm=64
#endif
#ifndef _ICE_
   integer, parameter :: ice_model=0
   integer, parameter :: ice_cover=0
#endif
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
!  private data members initialised via gotm.yaml
   character(len=80)         :: title
   integer                   :: nlev
   REALTYPE                  :: dt
   REALTYPE                  :: cnpar
   integer                   :: buoy_method
!  station description
   character(len=80)         :: name
   REALTYPE,target           :: latitude,longitude
   logical                   :: restart

   character(len=1024), public :: restart_file = 'restart'
   character(len=1024), public :: yaml_file = 'gotm.yaml'
   character(len=1024), public :: write_yaml_path = ''
   character(len=1024), public :: write_schema_path = ''
   character(len=1024), public :: output_id = ''
   integer, public             :: write_yaml_detail = display_normal
   logical, public             :: list_fields = .false.
   logical, public             :: ignore_unknown_config = .false.
   logical, public             :: generate_restart_file = .false.
   logical, public             :: force_restart_offline = .false.

   type,extends(type_output_manager_host) :: type_gotm_host
   contains
      procedure :: julian_day => gotm_host_julian_day
      procedure :: calendar_date => gotm_host_calendar_date
   end type

   REALTYPE :: swf=_ZERO_ ! surface water flux
   REALTYPE :: shf=_ZERO_ ! surface heat flux
   REALTYPE :: ssf=_ZERO_ ! surface salinity flux

   integer(kind=timestepkind):: progress
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the model \label{initGOTM}
!
! !INTERFACE:
   subroutine initialize_gotm()
!
! !DESCRIPTION:
!  This internal routine triggers the initialization of the model.
!  The first section reads {\tt gotm.yaml} with
!  the user specifications. Then, one by one each of the modules are
!  initialised with help of more specialised routines like
!  {\tt init\_meanflow()} or {\tt init\_turbulence()} defined inside
!  their modules, respectively.
!
!  Note that the KPP-turbulence model requires not only a call to
!  {\tt init\_kpp()} but before also a call to {\tt init\_turbulence()},
!  since there some fields (fluxes, diffusivities, etc) are declared and
!  the turbulence configuration is done.

! !USES:
  IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   class (type_settings), pointer :: branch, twig
   integer, parameter :: rk = kind(_ONE_)
   logical          ::    restart_allow_missing_variable = .false.
   logical          ::    restart_allow_perpetual = .true.
   logical          ::    restart_offline = .false.
   logical          ::    restart_online=.false.
   integer          ::    rc
   logical          ::    file_exists
   logical          ::    config_only=.false.
   integer          ::    n
   integer          ::    configuration_version=_CFG_VERSION_
   integer          ::    cfg_version
   type (type_header) :: yaml_header
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'initialize_gotm'
#if 0
   if (present(t1)) then
      restart_online = .true.
   end if
#endif

   config_only = write_yaml_path /= '' .or. write_schema_path /= ''
   STDERR LINE

   inquire(file=trim(yaml_file),exist=file_exists)
   if (file_exists) then
      LEVEL2 'Reading configuration from: ',trim(yaml_file)
      call settings_store%load(trim(yaml_file), namlst)
   elseif (config_only) then
      LEVEL2 'Configuration file ' // trim(yaml_file) // ' not found. Using default settings.'
   else
      FATAL 'Configuration file ' // trim(yaml_file) // ' not found.'
      LEVEL1 'To generate a file with default settings, specify --write_yaml gotm.yaml.'
      LEVEL1 'For more information, run gotm with -h.'
      stop 2
   end if

   if (config_only) then
      call settings_store%get(cfg_version, 'version', 'version of configuration file', default=configuration_version)
   else
      call settings_store%get(cfg_version, 'version', 'version of configuration file', default=-1)
      if (cfg_version == -1) then
         FATAL 'The configuration in ' // trim(yaml_file) //' does not have a version key and is therefore not compatible with this version of GOTM.'
         LEVEL1 'Please update your configuration by running <GOTMDIR>/scripts/python/update_setup.py.'
         stop 1
      end if
      if (cfg_version /= configuration_version) then
         FATAL 'The configuration in ' // trim(yaml_file) //' has version ',cfg_version,', which does not match version ',configuration_version,' required by this executable.'
         LEVEL1 'Please update your configuration by running <GOTMDIR>/scripts/python/update_setup.py.'
         stop 1
      end if
   end if

   call settings_store%get(title, 'title', 'simulation title used in output', &
                           default='GOTM simulation')

   branch => settings_store%get_child('location')
   call branch%get(name, 'name', 'station name used in output', &
                   default='GOTM site')
   call branch%get(latitude, 'latitude', 'latitude', 'degrees North', &
                   minimum=-90._rk, maximum=90._rk, default=0._rk)
   call branch%get(longitude, 'longitude', 'longitude', 'degrees East', &
                   minimum=-360._rk, maximum=360._rk, default=0._rk)
   call branch%get(depth, 'depth', 'water depth', 'm', &
                   minimum=0._rk, default=100._rk)

   branch => settings_store%get_child('time')
   call branch%get(timefmt, 'method', 'method to specify simulated period', default=2, &
                   options=(/option(1, 'number of time steps', 'MaxN'), option(2, 'start and stop', 'start_stop'), option(3, 'start and number of time steps', 'start_MaxN')/), display=display_advanced)
#if 0
   call branch%get(MaxN, 'MaxN', 'number of time steps', &
                   minimum=1,default=100, display=display_advanced)
#endif
   call branch%get(start, 'start', 'start date and time', units='yyyy-mm-dd HH:MM:SS', &
                   default='2017-01-01 00:00:00')
   call branch%get(stop, 'stop', 'stop date and time', units='yyyy-mm-dd HH:MM:SS', &
                   default='2018-01-01 00:00:00')
   if (generate_restart_file) stop = start
   call branch%get(dt, 'dt', 'time step for integration', 's', &
                   minimum=0.e-10_rk, default=3600._rk)
   call branch%get(cnpar, 'cnpar', '"implicitness" of diffusion scheme', '1', &
                   minimum=0._rk, maximum=1._rk, default=1._rk, display=display_advanced, &
                   description='constant for the theta scheme used for time integration of diffusion-reaction components. Typical values: 0.5 for Cranck-Nicholson (second-order accurate), 0 for Forward Euler (first-order accurate), 1 for Backward Euler (first-order accurate). Only 1 guarantees positive solutions for positive definite systems.')

   branch => settings_store%get_child('grid')
   call branch%get(nlev, 'nlev', 'number of layers', &
                   minimum=1, default=100)
   call branch%get(grid_method, 'method', 'layer thickness specification', &
                   options=(/option(0, 'equal by default with optional zooming', 'analytical'), option(1, 'prescribed relative fractions', 'file_sigma'), option(2, 'prescribed thicknesses', 'file_h')/), default=0) ! option(3, 'adaptive')
   call branch%get(ddu, 'ddu', 'surface zooming', '-', &
                   minimum=0._rk, default=0._rk)
   call branch%get(ddl, 'ddl', 'bottom zooming', '-', &
                   minimum=0._rk, default=0._rk)
   call branch%get(grid_file, 'file', 'path to file with layer thicknesses', &
                   default='')
#if 0
   twig => branch%get_child('adaptation')
   call twig%get(c1ad, 'c1ad', 'weighting factor for buoyancy frequency', '-', &
                 default=0.8_rk)
   call twig%get(c2ad, 'c2ad', 'weighting factor for shear frequency', '-', &
                 default=0.0_rk)
   call twig%get(c3ad, 'c3ad', 'weighting factor for surface distance', '-', &
                 default=0.1_rk)
   call twig%get(c4ad, 'c4ad', 'weighting factor for background', '-', &
                 default=0.1_rk)
   call twig%get(Tgrid, 'Tgrid', 'grid adaption time scale', 's', &
                 minimum=0._rk,default=3600._rk)
   call twig%get(NNnorm, 'NNnorm', 'normalisation factor for buoyancy frequency', '-', &
                 minimum=0._rk,default=0.2_rk)
   call twig%get(SSNorm, 'SSNorm', 'normalisation factor for shear frequency', '-', &
                 minimum=0._rk,default=0.2_rk)
   call twig%get(dsurf, 'dsurf', 'normalisation factor for surface distance', '-', &
                 minimum=0._rk,default=10._rk)
   call twig%get(dtgrid, 'dtgrid', 'time step (must be fraction of dt)', 's', &
                 minimum=0._rk,default=5._rk)
#endif


   ! Predefine order of top-level categories in gotm.yaml
   branch => settings_store%get_child('temperature')
   branch => settings_store%get_child('salinity')
   branch => settings_store%get_child('surface')
   branch => settings_store%get_child('bottom')
   branch => settings_store%get_child('light_extinction')
   branch => settings_store%get_child('turbulence')
   branch => settings_store%get_child('waves', 'surface waves', display=display_advanced)

   branch => settings_store%get_child('restart', order=998)
   call branch%get(restart_offline, 'load', &
                   'initialize simulation with state stored in restart.nc', &
                   default=.false.)
   if (force_restart_offline) restart_offline = .true.
   call branch%get(restart_allow_missing_variable, 'allow_missing_variable', &
                   'warn but not abort if a variable is missing from restart file', &
                   default=.false., display=display_advanced)
   branch => settings_store%get_child('output', order=999)

   LEVEL2 'configuring modules ....'
   call init_airsea()
#ifdef _ICE_
   call init_ice()
#else
   if (settings_store%ignore('surface/ice')) then
      LEVEL3 'WARNING: surface/ice section in ' // trim(yaml_file) // ' is ignored because GOTM was compiled without STIM.'
      LEVEL3 'To change this, specify -DGOTM_USE_STIM=ON when running cmake, then rebuild GOTM.'
   end if
#endif
   call init_observations()
   call init_stokes_drift()
   branch => settings_store%get_child('turbulence')
   call init_turbulence(branch)
#ifdef _CVMIX_
   branch => settings_store%get_child('cvmix')
   call init_cvmix(branch)
#else
   if (settings_store%ignore('cvmix')) then
      LEVEL3 'WARNING: cvmix section in ' // trim(yaml_file) // ' is ignored because GOTM was compiled without CVMix.'
      LEVEL3 'To change this, specify -DGOTM_USE_CVMIX=ON when running cmake, then rebuild GOTM.'
   end if
#endif
#ifdef _FABM_
   branch => settings_store%get_typed_child('fabm', 'Framework for Aquatic Biogeochemical Models')
   call configure_gotm_fabm(branch)
#else
   if (settings_store%ignore('fabm')) then
      LEVEL3 'WARNING: fabm section in ' // trim(yaml_file) // ' is ignored because GOTM was compiled without FABM.'
      LEVEL3 'To change this, specify -DGOTM_USE_FABM=ON when running cmake, then rebuild GOTM.'
   end if
#endif
   call init_meanflow()
#ifdef _ICE_
   if (ice_model > 0 .and. Hice > _ZERO_) zeta = -Hice*rho_ice/rho0
#endif

   LEVEL1 'init_eqstate_yaml'
   branch => settings_store%get_child('equation_of_state', 'equation of state')
   call branch%get(density_method, 'method', 'density formulation', &
                   options=(/option(1, 'TEOS-10', 'full_TEOS-10'), &
                             option(2, 'linearized from user-specified T0, S0, p0 (alpha, beta, cp calculated)', 'linear_teos-10'), &
                             option(3, 'linearized from user-specified T0, S0, rho0, alpha, beta, cp', 'linear_custom')/), &
                             default=1)
   call branch%get(rho0, 'rho0', 'reference density', 'kg/m3', default=1027._rk)
   twig => branch%get_child('linear')
   call twig%get(T0, 'T0', 'reference temperature', 'Celsius', &
                 minimum=-2._rk, default=15._rk)
   call twig%get(S0, 'S0', 'reference salinity', 'g/kg', &
                 minimum=0._rk, default=35._rk)
   call twig%get(p0, 'p0', 'reference pressure', 'dbar', &
                 minimum=0._rk, default=0._rk)
   call twig%get(alpha0, 'alpha', 'thermal expansion coefficient', '1/K', &
                 default=0.00020_rk)
   call twig%get(beta0, 'beta', 'saline contraction coefficient', 'kg/g', &
                 default=0.00075_rk)
   call twig%get(cp, 'cp', 'specific heat capacity', 'J/(kg/K)', &
                 minimum=0._rk,default=3991.86796_rk)


   LEVEL1 'init_density()'
   call init_density(nlev)

#ifdef _FABM_
   ! Allow FABM to create its model tree. After this we know all biogeochemical variables
   ! This must be done before gotm_fabm_input configuration.
   call gotm_fabm_create_model(namlst)

   call configure_gotm_fabm_input()
#endif

   ! Initialize field manager
   ! This is needed for the output manager to be able to read its configuration [output_manager_init]
   call fm%register_dimension('lon',1,id=id_dim_lon)
   call fm%register_dimension('lat',1,id=id_dim_lat)
   call fm%register_dimension('z',nlev,id=id_dim_z)
   call fm%register_dimension('zi',nlev+1,id=id_dim_zi)
   call fm%register_dimension('time',id=id_dim_time)
   call fm%initialize(prepend_by_default=(/id_dim_lon,id_dim_lat/),append_by_default=(/id_dim_time/))

   allocate(type_gotm_host::output_manager_host)
   branch => settings_store%get_child('output')
   call output_manager_init(fm,title,settings=branch,postfix=output_id)

   inquire(file='output.yaml',exist=file_exists)

   ! Make sure all elements in the YAML configuration file were recognized
   if (.not. settings_store%check_all_used()) then
      if (ignore_unknown_config) then
         LEVEL0 'Continuing because --ignore_unknown_config is specified.'
      else
         LEVEL0 'If you want GOTM to ignore unknown settings in ' // trim(yaml_file) // ','
         LEVEL0 'rerun with --ignore_unknown_config.'
         stop 1
      end if
   end if

   if (write_yaml_path /= '') then
      select case (write_yaml_detail)
      case (0)
         call yaml_header%append('This file was created with only the settings that differ from the default specified by GOTM.')
         call yaml_header%append('You can generate a configuration with all commonly used settings with: gotm --write_yaml <OUTFILE> --detail default')
         call yaml_header%append('To generate a configuration with every possible setting, use: gotm --write_yaml <OUTFILE> --detail full')
      case (1)
         call yaml_header%append('This file was created with only commonly used settings, plus those that differ from the default specified by GOTM.')
         call yaml_header%append('You can generate a configuration with every possible setting with: gotm --write_yaml <OUTFILE> --detail full')
         call yaml_header%append('To see only the settings that differ from the default, use: gotm --write_yaml <OUTFILE> --detail minimal')
      case (2)
         call yaml_header%append('This file was created with every possible GOTM setting.')
         call yaml_header%append('You can generate a configuration with just the commonly used settings with: gotm --write_yaml <OUTFILE> --detail default')
         call yaml_header%append('To see only the settings that differ from the default, use: gotm --write_yaml <OUTFILE> --detail minimal')
      end select
      call settings_store%save(trim(write_yaml_path), namlst, display=write_yaml_detail, header=yaml_header)
      LEVEL0 'Your configuration has been written to '//trim(write_yaml_path)//'.'
      if (file_exists) then
         LEVEL0 'Settings from output.yaml have been migrated to gotm.yaml, but output.yaml will still take priority.'
         LEVEL0 'Delete or rename output.yaml if you want output settings from gotm.yaml to take effect.'
      end if
   end if
   if (write_schema_path /= '') then
      call settings_store%write_schema_file(trim(write_schema_path), namlst, 'gotm-5.3')
      LEVEL0 'Schema file has been written to '//trim(write_schema_path)//'.'
   end if
   if (config_only) stop 0

   if (restart_online) then
      LEVEL3 'online restart - updating values read from gotm.yaml ...'
      LEVEL4 'orignal: ',start,' -> ',stop
!KB      start = t1
!KB      stop  = t2
      LEVEL4 'updated: ',start,' -> ',stop
   end if

!KB - rephrase
!  zeta is initialized to 0 - but might have been over written by the ice module.
!  The sea surface elevation (zeta) and vertical advection method (w_adv_method)
!  will be set by init_observations.
!  However, before that happens, it is already used in updategrid.
!  therefore, we set to to a reasonable default here.
   w_adv_input%method = 0

   restart = restart_online .or. restart_offline
   if (restart_online) restart_offline = .false.

!  initialize a few things from gotm.yaml
   timestep   = dt
   depth0     = depth

!  write information for this run
   LEVEL2 trim(title)
   LEVEL2 'Using ',nlev,' layers to resolve a depth of',depth
   LEVEL2 'The station ',trim(name),' is situated at (lat,long) ',      &
            latitude,longitude
   LEVEL2 trim(name)

   if (restart_offline) then
      LEVEL2 'Offline restart ....'
   end if

   LEVEL2 'initializing modules....'
   call init_input(nlev)
   call init_time(MinN,MaxN)

!  From here - each init_? is responsible for opening and closing the
!  namlst - unit.
   call post_init_meanflow(nlev,latitude)
   call init_tridiagonal(nlev)
   call updategrid(nlev,dt,zeta)

#ifdef SEAGRASS
      call init_seagrass(namlst,'seagrass.nml',unit_seagrass,nlev,h,fm)
#endif
#ifdef SPM
      call init_spm(namlst,'spm.nml',unit_spm,nlev)
#endif

!KB   call post_init_observations(julianday,secondsofday,depth,nlev,z,h,gravity,rho0)
   call post_init_observations(depth,nlev,z,zi,h,gravity)
   call get_all_obs(julianday,secondsofday,nlev,z)

!  Stokes drift
   call post_init_stokes_drift(nlev)

!  Call do_input to make sure observed profiles are up-to-date.
   call do_input(julianday,secondsofday,nlev,z)
   if (ice_model == 0) zeta = zeta_input%value

   ! Update the grid based on true initial zeta (possibly read from file by do_input).
   call updategrid(nlev,dt,zeta)

   call post_init_turbulence(nlev)

#ifdef _CVMIX_
   if (turb_method .eq. 100) then
      call post_init_cvmix(nlev,depth,gravity,rho0)
   endif
#endif

!  initialise mean fields
!GSW_KB
   select case (initial_salinity_type)
      case(1) ! Practical
         LEVEL1 "Initial salinity: practical"
         Sp(1:nlev) = sprof_input%data(1:nlev)
      case(2) ! Absolute
         LEVEL1 "Initial salinity: absolute"
         S(1:nlev) = sprof_input%data(1:nlev)
   end select
   select case (initial_temperature_type)
      case(1) ! In-situ
         LEVEL1 "Initial temperature: in-situ"
         Ti(1:nlev) = tprof_input%data(1:nlev)
      case(2) ! Potential
         LEVEL1 "Initial temperature: potential"
         Tp(1:nlev) = tprof_input%data(1:nlev)
      case(3) ! Conservative
         LEVEL1 "Initial temperature: conservative"
         T(1:nlev) = tprof_input%data(1:nlev)
   end select
   LEVEL1 ""
   Sobs = S
   Tobs = T

   select case (density_method)
      case (1,2,3)
         select case (initial_salinity_type)
            case(1) ! Practical --> Absolute
               S(1:nlev) = gsw_sa_from_sp(Sp(1:nlev),-z(1:nlev),longitude,latitude)
            case(2) ! Absolute --> Practical
               Sp(1:nlev) = gsw_sp_from_sa(S(1:nlev),-z(1:nlev),longitude,latitude)
         end select
         select case (initial_temperature_type)
            case(1) ! In-situ
               T(1:nlev) = gsw_ct_from_t(S(1:nlev),Ti(1:nlev),-z(1:nlev))
               Tp(1:nlev) = gsw_pt_from_t(S(1:nlev),Ti(1:nlev),-z(1:nlev),_ZERO_)
            case(2) ! Potential
               T(1:nlev) = gsw_ct_from_pt(S(1:nlev),Tp(1:nlev))
               Ti(1:nlev) = gsw_t_from_ct(S(1:nlev),T(1:nlev),-z(1:nlev))
            case(3) ! Conservative
               Tp(1:nlev) = gsw_pt_from_ct(S(1:nlev),T(1:nlev))
               Ti(1:nlev) = gsw_t_from_ct(S(1:nlev),T(1:nlev),-z(1:nlev))
         end select
   end select
!GSW_KB
   u(1:nlev) = uprof_input%data(1:nlev)
   v(1:nlev) = vprof_input%data(1:nlev)

   call post_init_airsea(latitude,longitude)
#if 0
#ifdef _ICE_
   call post_init_ice(ta,S(nlev))
   if(ice_cover .eq. 2) then
      z0s = z0i
   end if
#endif
#endif
   call init_diagnostics(nlev)

   call do_register_all_variables(latitude,longitude,nlev)

   !  initialize FABM module
#ifdef _FABM_

   if (fabm_calc) then
      call init_gotm_fabm(nlev,dt,fm)

!     Link relevant GOTM data to FABM.
!     This sets pointers, rather than copying data, and therefore needs to be done only once.
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_depth,depth)
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_depth_below_geoid,depth0)
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_roughness_length,z0b)
      if (fluxes_method /= 0) then
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_specific_humidity,qa)
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_air_pressure,airp_input%value)
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_temperature,ta)
      end if
      call set_env_gotm_fabm(latitude,longitude,dt,w_adv_input%method,w_adv_discr,t(1:nlev),s(1:nlev),rho(1:nlev), &
                             nuh,h,w,bioshade(1:nlev),I_0%value,cloud_input%value,taub,wind,precip_input%value,evap,z(1:nlev), &
                             A_input%value,g1_input%value,g2_input%value,yearday,secondsofday,SRelaxTau(1:nlev),sprof_input%data(1:nlev), &
                             bio_albedo,bio_drag_scale)

      ! Initialize FABM input (data files with observations)
      call init_gotm_fabm_input(nlev,h(1:nlev))

      ! Transfer optional dependencies to FABM coupler
      if (fluxes_method /= 0) fabm_airp => airp_input%value
      fabm_calendar_date => calendar_date
      fabm_julianday => julianday
   end if
#endif

   ! Now that all inputs have been registered (FABM added some), update them all by reading from file.
   call do_input(julianday,secondsofday,nlev,z)
   if (ice_model == 0) zeta = zeta_input%value

#ifdef _FABM_
!  Initialize FABM initial state (this is done after the first call to do_input,
!  to allow user-specified observed values to be used as initial state)
   if (fabm_calc) call init_gotm_fabm_state(nlev)
#endif

   if (restart) then
      if (restart_offline) then
         LEVEL1 'read_restart'
         call read_restart(restart_allow_perpetual, restart_allow_missing_variable)
         call friction(nlev,kappa,avmolu,tx,ty,plume_type)
      end if
      if (restart_online) then
      end if
   end if

!   allocate(type_gotm_host::output_manager_host)
!   call output_manager_init(fm,title)
   call setup_restart()
!   call init_output(title,nlev,latitude,longitude)

   call do_airsea(julianday,secondsofday)
#ifdef _ICE_
   call post_init_ice(ta,S(nlev))
#endif

   ! Call stratification to make sure density has sensible value.
   ! This is needed to ensure the initial density is saved correctly, and also for FABM.
   call shear(nlev,cnpar)
   call do_density(nlev,S,T,-z,-zi)
   buoy(1:) = -gravity*(rho_p(1:)-rho0)/rho0
   call stratification(nlev)


#ifdef _FABM_
!  Accept the current biogeochemical state and used it to compute derived diagnostics.
   if (fabm_calc) call start_gotm_fabm(nlev, fm)
#endif

   if (list_fields) then
      call fm%list()
      stop 0
   end if

   LEVEL2 'done.'
   STDERR LINE

   progress = (MaxN-MinN+1)/10
   if (progress < 1) progress = 1

#ifdef _PRINTSTATE_
   call print_state
#endif
   end subroutine initialize_gotm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Manage global time--stepping \label{timeLoop}
!
! !INTERFACE:
   subroutine integrate_gotm()
!
! !DESCRIPTION:
! This internal routine is the heart of the code. It contains
! the main time-loop inside of which all routines required
! during the time step are called. The following main processes are
! successively triggered.
! \begin{enumerate}
!  \item The model time is updated and the output is prepared.
!  \item Air-sea interactions (flux, SST) are computed.
!  \item The time step is performed on the mean-flow equations
!        (momentum, temperature).
!  \item Some quantities related to shear and stratification are updated
!        (shear-number, buoyancy frequency, etc).
!  \item Turbulence is updated depending on what turbulence closure
!        model has been specified by the user.
!  \item The results are written to the output files.
! \end{enumerate}
!
! Depending on macros set for the Fortran pre-processor, extra features
! like the effects of sea-grass or sediments are considered in this routine
! (see \sect{sec:extra}).
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   integer(kind=timestepkind):: n
   integer                   :: i=0

   REALTYPE                  :: tFlux,btFlux,sFlux,bsFlux
   REALTYPE                  :: tRad(0:nlev),bRad(0:nlev)

   REALTYPE                  :: Qsw, Qflux
   integer                   :: k
   REALTYPE                  :: La, EFactor
!-----------------------------------------------------------------------
!BOC
   if (i == 0 .and. .not. restart) then
      LEVEL1 'saving initial conditions'
      call output_manager_save(julianday,int(fsecondsofday),int(mod(fsecondsofday,_ONE_)*1000000),0)
      STDERR LINE
      LEVEL1 'integrate_gotm'
      STDERR LINE
   end if
   do n=MinN,MaxN

      if(mod(n,progress) .eq. 0 .or. i == 0) then
         LEVEL0 i,'%'
         i = i +10
      end if

!     prepare time and output
      call update_time(n)
      call output_manager_prepare_save(julianday, int(fsecondsofday), int(mod(fsecondsofday,_ONE_)*1000000), int(n))

!     all observations/data
      call do_input(julianday,secondsofday,nlev,z)
      call get_all_obs(julianday,secondsofday,nlev,z)
      if (ice_model == 0) zeta = zeta_input%value
      call do_stokes_drift(nlev,z,zi,gravity,u10_input%value,v10_input%value)

!     external forcing
      if(fluxes_method /= 0) then
         if(ice_cover .eq. 0) then
            call set_sst(gsw_t_from_ct(S(nlev), T(nlev), _ZERO_))
            call set_ssuv(u(nlev),v(nlev))
#ifdef _ICE_
         else
            call set_sst(Tice_surface) !GSW_KB - check for GSW alternative
            call set_ssuv(_ZERO_,_ZERO_)
#endif
         end if
      end if
      call do_airsea(julianday,secondsofday)

#ifdef _ICE_
      if(ice_cover .eq. 2) then
         albedo = albedo_ice
      end if
#endif
      I_0%value = I_0%value*(_ONE_-albedo-bio_albedo)

#ifdef _ICE_
      Qsw = I_0%value
      call do_ice(h(nlev),dt,u_taus,T(nlev),S(nlev),ta,precip_input%value,Qsw,surface_fluxes)
#endif

!     reset some quantities
      if(ice_cover .gt. 0) then
         shf = _ZERO_
         heat_input%value = _ZERO_
         tx = _ZERO_
         ty = _ZERO_
#ifdef _ICE_
         I_0%value = transmissivity*I_0%value
         swf=melt_rate
         shf=ocean_ice_heat_flux
         ssf=ocean_ice_salt_flux
#endif
      else
         swf=precip_input%value+evap
         shf=-heat_input%value ! temperature() changed to positive heat flux upwards (v7)
         tx = tx/rho0
         ty = ty/rho0
      end if

      call integrated_fluxes(dt)

!     meanflow integration starts
      call updategrid(nlev,dt,zeta)
      call wequation(nlev,dt)
      call coriolis(nlev,dt)

!     update velocity
      call uequation(nlev,dt,cnpar,tx,num,gamu,ext_press_mode)
      call vequation(nlev,dt,cnpar,ty,num,gamv,ext_press_mode)
      call external_pressure(ext_press_mode,nlev)
      call internal_pressure(nlev)
      call friction(nlev,kappa,avmolu,tx,ty,plume_type)

#ifdef SEAGRASS
      if(seagrass_calc) call do_seagrass(nlev,dt)
#endif

!     update temperature and salinity
      if (sprof_input%method .ne. 0) then
         call salinity(nlev,dt,cnpar,swf,ssf,nus,gams)
      endif
      if (tprof_input%method .ne. 0) then
         call temperature(nlev,dt,cnpar,I_0%value,swf,shf,nuh,gamh,rad)
      endif
!GSW
   select case (density_method)
      case (1,2,3)
         Sp(1:nlev) = gsw_sp_from_sa(S(1:nlev),-z(1:nlev),longitude,latitude)
         Ti(1:nlev) = gsw_t_from_ct(S(1:nlev),T(1:nlev),-z(1:nlev))
         Tp(1:nlev) = gsw_pt_from_ct(S(1:nlev),T(1:nlev))
         ! convert obs to conservative and absolute - for proper nudging
         if (sprof_input%method .ne. 0) then
            select case (initial_salinity_type)
               case(1) ! Practical --> Absolute
                  Sobs(1:nlev) = gsw_sa_from_sp(sprof_input%data(1:nlev),-z(1:nlev),longitude,latitude)
               case(2) ! Absolute
                  Sobs(1:nlev) = sprof_input%data(1:nlev)
            end select
         end if
         if (tprof_input%method .ne. 0) then
            select case (initial_temperature_type)
               case(1) ! In-situ
                  Tobs(1:nlev) = gsw_ct_from_t(Sobs(1:nlev),tprof_input%data(1:nlev),-z(1:nlev))
               case(2) ! Potential
                  Tobs(1:nlev) = gsw_ct_from_pt(Sobs(1:nlev),tprof_input%data(1:nlev))
               case(3) ! Conservative
                  Tobs(1:nlev) = tprof_input%data(1:nlev)
         end select
         end if
   end select
!GSW
!  update shear and stratification
   call shear(nlev,cnpar)
   call do_density(nlev,S,T,-z,-zi)
   buoy(1:nlev) = -gravity*(rho_p(1:nlev)-rho0)/rho0
   call stratification(nlev)

#ifdef SPM
      if (spm_calc) then
         call set_env_spm(nlev,rho0,depth,u_taub,h,u,v,nuh,tx,ty,Hs,Tz,Phiw)
         call do_spm(nlev,dt)
      end if
#endif
#ifdef _FABM_
      call do_gotm_fabm(nlev,real(n,kind(_ONE_)))
#endif

!     compute turbulent mixing
      select case (turb_method)
#ifdef _CVMIX_
      ! use KPP implemenatation in CVMix
      case (100)

         ! convert thermodynamic fluxes to what is needed by CVMix
         call convert_fluxes(nlev,gravity,swf,shf,ssf,rad,T(nlev),S(nlev),tFlux,sFlux,btFlux,bsFlux,tRad,bRad)

         ! update Langmuir number
         call langmuir_number(nlev,zi,Hs_input%value,u_taus,zi(nlev)-zsbl,u10_input%value,v10_input%value)

         select case(sbl_langmuir_method)
         case (0)
            efactor = _ONE_
            La = _ONE_/SMALL
         case (1)
            efactor = EFactor_LWF16
            La = La_SL
         case (2)
            efactor = EFactor_LWF16
            La = La_SL
         case (3)
            efactor = EFactor_RWH16
            La = La_SLP_RWH16
         end select

         !  update turbulence parameter from CVMix
         call do_cvmix(nlev,depth,h,rho,u,v,NN,NNT,NNS,SS,              &
                       u_taus,u_taub,tFlux,btFlux,sFlux,bsFlux,         &
                       tRad,bRad,cori,efactor,La)
#endif

      case default
!        update one-point models
# ifdef SEAGRASS
         call do_turbulence(nlev,dt,depth,u_taus,u_taub,z0s,z0b,h,      &
                            NN,SS,xP, SSCSTK=SSCSTK)
# else
         call do_turbulence(nlev,dt,depth,u_taus,u_taub,z0s,z0b,h,      &
                            NN,SS, SSCSTK=SSCSTK)
# endif
      end select

      call variances (nlev,SSU,SSV)
      call do_diagnostics(nlev)
      call output_manager_save(julianday,int(fsecondsofday),int(mod(fsecondsofday,_ONE_)*1000000),int(n))

   end do
   STDERR LINE

   return
   end subroutine integrate_gotm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: The run is over --- now clean up.
!
! !INTERFACE:
   subroutine finalize_gotm()
!
! !DESCRIPTION:
! This function is just a wrapper for the external routine
! {\tt close\_output()} discussed in \sect{sec:output}. All open files
! will be closed after this call.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef _PRINTSTATE_
   call print_state
#endif

   LEVEL1 'finalize_gotm'

   call clean_airsea()

#ifdef _ICE_
   call clean_ice()
#endif

   call clean_meanflow()

#ifdef _CVMIX_
   if (turb_method .eq. 100) call clean_cvmix()
#endif

   call clean_turbulence()

   call clean_observations()

   call clean_tridiagonal()

#ifdef SEAGRASS
   call end_seagrass
#endif

#ifdef _FABM_
   call clean_gotm_fabm()
#endif

   call close_input()

   call output_manager_clean()
   if (associated(output_manager_host)) deallocate(output_manager_host)

   call clean_diagnostics()

   call fm%finalize()

   call settings_store%finalize()

   return
   end subroutine finalize_gotm
!EOC

#ifdef _PRINTSTATE_
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Print the current state of all loaded gotm modules.
!
! !INTERFACE:
   subroutine print_state()
!
! !DESCRIPTION:
!  This routine writes the value of all module-level variables to screen.
!
! !USES:
   use airsea,    only: print_state_airsea
   use turbulence,only: print_state_turbulence

   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'state of gotm module'
   LEVEL2 'title',title
   LEVEL2 'nlev',nlev
   LEVEL2 'dt',dt
   LEVEL2 'cnpar',cnpar
   LEVEL2 'buoy_method',buoy_method
   LEVEL2 'name',name
   LEVEL2 'latitude',latitude
   LEVEL2 'longitude',longitude

   call print_state_time
   call print_state_meanflow
   call print_state_observations
   call print_state_airsea
   call print_state_turbulence

   end subroutine print_state
!EOC
#endif

   subroutine gotm_host_julian_day(self,yyyy,mm,dd,julian)
      class (type_gotm_host), intent(in) :: self
      integer, intent(in)  :: yyyy,mm,dd
      integer, intent(out) :: julian
      call julian_day(yyyy,mm,dd,julian)
   end subroutine

   subroutine gotm_host_calendar_date(self,julian,yyyy,mm,dd)
      class (type_gotm_host), intent(in) :: self
      integer, intent(in)  :: julian
      integer, intent(out) :: yyyy,mm,dd
      call calendar_date(julian,yyyy,mm,dd)
   end subroutine

   subroutine setup_restart()
#ifdef NETCDF_FMT
      use netcdf_output
      use output_manager_core
      use time, only: jul2,secs2

      class (type_netcdf_file), pointer :: file
      type (type_output_item),  pointer :: item

      allocate(file)
      file%path = 'restart'
      file%postfix = output_id
      file%time_unit = time_unit_day
      file%time_step = 1
      file%first_julian = jul2
      file%first_seconds = secs2
      call output_manager_add_file(fm,file)

      allocate(item)
      item%name = 'state'
      item%output_level = output_level_debug
      item%settings => file%create_settings()
      select type (settings=>item%settings)
      class is (type_netcdf_variable_settings)
         settings%xtype = NF90_DOUBLE
      end select
      call file%append_item(item)
#endif
   end subroutine setup_restart

   subroutine read_restart(restart_allow_perpetual, restart_allow_missing_variable)
      logical, intent(in) :: restart_allow_perpetual
      logical, intent(in) :: restart_allow_missing_variable

#ifdef NETCDF_FMT
      type (type_field_set)                 :: field_set
      class (type_field_set_member),pointer :: member

      call open_restart(trim(restart_file))

      if (.not. restart_allow_perpetual) then
         call check_restart_time('time')
      else
         LEVEL2 'allow perpetual restarts'
      end if

      field_set = fm%get_state()
      member => field_set%first
      do while (associated(member))
         ! This field is part of the model state. Its name is member%field%name.
         if (associated(member%field%data%p0d)) then
            ! Depth-independent variable with data pointed to by member%field%data%p0d
            ! Here you would read the relevant scalar (name: member%field%name) from the NetCDF file and assign it to member%field%data%p0d.
            call read_restart_data(trim(member%field%name),restart_allow_missing_variable,data_0d=member%field%data%p0d)
         elseif (associated(member%field%data%p1d)) then
            ! Depth-dependent variable with data pointed to by member%field%data%p1d
            ! Here you would read the relevant 1D variable (name: member%field%name) from the NetCDF file and assign it to member%field%data%p1d.
            call read_restart_data(trim(member%field%name),restart_allow_missing_variable,data_1d=member%field%data%p1d)
         else
            stop 'no data assigned to state field'
         end if
         member => member%next
      end do
      call field_set%finalize()

      call close_restart()
#else
      FATAL 'Restart reading requires NetCDF support. Please build with GOTM_USE_NetCDF=ON'
      stop 1
#endif
   end subroutine read_restart

!-----------------------------------------------------------------------

   end module gotm

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

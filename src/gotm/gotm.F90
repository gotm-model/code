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
!  Here is also the place for a few words on FORTRAN `units' we used.
!  The method of FORTRAN units is quite rigid and also a bit dangerous,
!  but lacking a better alternative we adopted it here. This requires
!  the definition of ranges of units for different purposes. In GOTM
!  we strongly suggest to use units according to the following
!  conventions.
!  \begin{itemize}
!     \item unit=10 is reserved for reading namelists.
!     \item units 20-29 are reserved for the {\tt airsea} module.
!     \item units 30-39 are reserved for the {\tt meanflow} module.
!     \item units 40-49 are reserved for the {\tt turbulence} module.
!     \item units 50-59 are reserved for the {\tt output} module.
!     \item units 60-69 are reserved for the {\tt extra} modules
!           like those dealing with sediments or sea-grass.
!     \item units 70- are \emph{not} reserved and can be used as you
!           wish.
!  \end{itemize}
!
! !USES:
   use field_manager
   use register_all_variables, only: do_register_all_variables, fm
!KB   use output_manager_core, only:output_manager_host=>host, type_output_manager_host=>type_host,time_unit_second,type_output_category
   use output_manager_core, only:output_manager_host=>host, type_output_manager_host=>type_host,type_output_manager_file=>type_file,time_unit_second,type_output_item
   use output_manager
   use diagnostics
   use settings

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
   use airsea_driver, only: wind=>w,tx,ty,I_0,cloud,heat,precip,evap,airp,albedo
   use airsea_driver, only: bio_albedo,bio_drag_scale
   use airsea_driver, only: u10, v10
   use airsea_variables, only: qa,ta

#ifdef _ICE_
   use ice,         only: init_ice, post_init_ice, do_ice, clean_ice, ice_cover
   use stim_variables, only: Tice_surface,albedo_ice,transmissivity
#endif

   use turbulence,  only: turb_method
   use turbulence,  only: init_turbulence,post_init_turbulence,do_turbulence
   use turbulence,  only: num,nuh,nus
   use turbulence,  only: const_num,const_nuh
   use turbulence,  only: gamu,gamv,gamh,gams
   use turbulence,  only: Rig
   use turbulence,  only: kappa
   use turbulence,  only: clean_turbulence

   use kpp,         only: init_kpp,do_kpp,clean_kpp

   use mtridiagonal,only: init_tridiagonal,clean_tridiagonal
   use eqstate,     only: init_eqstate

#ifdef _CVMIX_
   use gotm_cvmix,  only: init_cvmix, post_init_cvmix, do_cvmix, clean_cvmix
   use gotm_cvmix,  only: zsbl, kpp_langmuir_method
#endif

#ifdef SEAGRASS
   use seagrass
#endif
#ifdef SPM
   use spm_var, only: spm_calc
   use spm, only: init_spm, set_env_spm, do_spm, end_spm
#endif
#ifdef _FABM_
   use gotm_fabm,only:configure_gotm_fabm,configure_gotm_fabm_from_nml,gotm_fabm_create_model,init_gotm_fabm,init_gotm_fabm_state,start_gotm_fabm,set_env_gotm_fabm,do_gotm_fabm,clean_gotm_fabm,fabm_calc
   use gotm_fabm,only:model_fabm=>model,standard_variables_fabm=>standard_variables
   use gotm_fabm, only: fabm_airp, fabm_calendar_date, fabm_julianday
   use gotm_fabm_input,only: configure_gotm_fabm_input, configure_gotm_fabm_input_from_nml, init_gotm_fabm_input
#endif

   IMPLICIT NONE
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm, time_loop, clean_up

!
! !DEFINED PARAMETERS:
   integer, parameter                  :: namlst=10
#ifdef SEAGRASS
   integer, parameter                  :: unit_seagrass=62
#endif
#ifdef SPM
   integer, parameter                  :: unit_spm=64
#endif
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
!  private data members initialised via namelists
   character(len=80)         :: title
   integer                   :: nlev
   REALTYPE                  :: dt
   REALTYPE                  :: cnpar
   integer                   :: buoy_method
!  station description
   character(len=80)         :: name
   REALTYPE,target           :: latitude,longitude
   logical                   :: restart

   character(len=1024), public :: yaml_file = 'gotm.yaml'
   character(len=1024), public :: write_yaml_path = ''
   character(len=1024), public :: write_schema_path = ''
   character(len=1024), public :: output_id = ''
   logical, public             :: read_nml = .false.
   integer, public             :: write_yaml_detail = display_normal

   type,extends(type_output_manager_host) :: type_gotm_host
   contains
      procedure :: julian_day => gotm_host_julian_day
      procedure :: calendar_date => gotm_host_calendar_date
   end type
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the model \label{initGOTM}
!
! !INTERFACE:
!KB   subroutine init_gotm(t1,t2)
   subroutine init_gotm()
!
! !DESCRIPTION:
!  This internal routine triggers the initialization of the model.
!  The first section reads the namelists of {\tt gotmrun.nml} with
!  the user specifications. Then, one by one each of the modules are
!  initialised with help of more specialised routines like
!  {\tt init\_meanflow()} or {\tt init\_turbulence()} defined inside
!  their modules, respectively.
!
!  Note that the KPP-turbulence model requires not only a call to
!  {\tt init\_kpp()} but before also a call to {\tt init\_turbulence()},
!  since there some fields (fluxes, diffusivities, etc) are declared and
!  the turbulence namelist is read.

! !USES:
  IMPLICIT NONE
!
! !INPUT PARAMETERS:
!KB   character(len=*), intent(in), optional  :: t1,t2
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   class (type_settings), pointer :: branch, twig
   integer, parameter :: rk = kind(_ONE_)

   namelist /model_setup/ title,nlev,dt,restart_offline,restart_allow_missing_variable, &
                          restart_allow_perpetual,cnpar,buoy_method
   namelist /station/     name,latitude,longitude,depth
   namelist /time/        timefmt,MaxN,start,stop
   logical          ::    list_fields=.false.
   logical          ::    restart_online=.false.
   logical          ::    restart_offline = .false.
   logical          ::    restart_allow_missing_variable = .false.
   logical          ::    restart_allow_perpetual = .true.
   integer          ::    rc
   logical          ::    file_exists
   logical          ::    config_only=.false.
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm'
#if 0
   if (present(t1)) then
      restart_online = .true.
   end if
#endif

   config_only = write_yaml_path /= '' .or. write_schema_path /= ''
   STDERR LINE

   settings_store%path = ''
   if (.not. read_nml) then
      inquire(file=trim(yaml_file),exist=file_exists)
      if (file_exists) then
         LEVEL2 'Reading yaml configuration from: ',trim(yaml_file)
         call settings_store%load(trim(yaml_file), namlst)
      elseif (.not. config_only) then
         FATAL 'GOTM now reads its configuration from gotm.yaml.'
         LEVEL1 'To use namelists instead, specify --read_nml.'
         LEVEL1 'To migrate namelists to yaml, specify --read_nml --write_yaml gotm.yaml.'
         LEVEL1 'To generate gotm.yaml with default settings, specify --write_yaml gotm.yaml.'
         stop 2
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
                   options=(/option(1, 'number of time steps'), option(2, 'start and stop'), option(3, 'start and number of time steps')/), display=display_advanced)
#if 0
   call branch%get(MaxN, 'MaxN', 'number of time steps', &
                   minimum=1,default=100, display=display_advanced)
#endif
   call branch%get(start, 'start', 'start date and time', units='yyyy-mm-dd HH:MM:SS', &
                   default='2017-01-01 00:00:00')
   call branch%get(stop, 'stop', 'stop date and time', units='yyyy-mm-dd HH:MM:SS', &
                   default='2018-01-01 00:00:00')
   call branch%get(dt, 'dt', 'time step for integration', 's', &
                   minimum=0.e-10_rk, default=3600._rk)
   call branch%get(cnpar, 'cnpar', '"implicitness" of diffusion scheme', '1', &
                   minimum=0._rk, maximum=1._rk, default=1._rk, display=display_advanced, &
                   description='constant for the theta scheme used for time integration of diffusion-reaction components. Typical values: 0.5 for Cranck-Nicholson (second-order accurate), 0 for Forward Euler (first-order accurate), 1 for Backward Euler (first-order accurate). Only 1 guarantees positive solutions for positive definite systems.')

   branch => settings_store%get_child('grid')
   call branch%get(nlev, 'nlev', 'number of layers', &
                   minimum=1, default=100)
   call branch%get(grid_method, 'method', 'layer thicknesses', &
                   options=(/option(0, 'equal by default with optional zooming'), option(1, 'prescribed relative fractions'), option(2, 'prescribed thicknesses')/), default=0) ! option(3, 'adaptive')
   call branch%get(ddu, 'ddu', 'surface zooming', '-', &
                   minimum=0._rk, default=0._rk)
   call branch%get(ddl, 'ddl', 'bottom zooming', '-', &
                   minimum=0._rk, default=0._rk)
   call branch%get(grid_file, 'file', 'file with custom grid', &
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

   branch => settings_store%get_child('restart', order=998)
   call branch%get(restart_offline, 'load', &
                   'initialize simulation with state stored in restart.nc', &
                   default=.false.)
   call branch%get(restart_allow_missing_variable, 'allow_missing_variable', &
                   'warn but not abort if a variable is missing from restart file', &
                   default=.false., display=display_advanced)
   branch => settings_store%get_child('output', order=999)

   LEVEL2 'configuring modules ....'
   call init_airsea()
#ifdef _ICE_
   call init_ice()
#endif
   call init_observations()
   call init_stokes_drift()
   branch => settings_store%get_child('turbulence')
   call init_turbulence(branch)
#ifdef _CVMIX_
   branch => settings_store%get_child('cvmix')
   call init_cvmix(branch)
#endif
#ifdef _FABM_
   branch => settings_store%get_typed_child('fabm', 'Framework for Aquatic Biogeochemical Models')
   call configure_gotm_fabm(branch)
#endif
   call init_meanflow()

   branch => settings_store%get_child('buoyancy', display=display_advanced)
   call branch%get(buoy_method, 'method', 'method to compute mean buoyancy', &
                   options=(/option(1, 'equation of state'), option(2, 'prognostic equation')/), default=1)
   call branch%get(b_obs_surf, 'surf_ini', 'initial buoyancy at the surface', '-', &
                   default=0._rk)
   call branch%get(b_obs_NN, 'NN_ini', 'initial value of NN (=buoyancy gradient)', 's^-2', &
                   default=0._rk)
   call branch%get(b_obs_sbf, 'obs_sbf', 'observed constant surface buoyancy flux', '-', &
                   default=0._rk, display=display_hidden)

   branch => settings_store%get_child('eq_state', 'equation of state')
   call init_eqstate(branch)

!  open the namelist file.
   if (read_nml) then
      LEVEL2 'reading model setup namelists..'
      open(namlst,file='gotmrun.nml',status='old',action='read',err=90)

      read(namlst,nml=model_setup,err=91)
      read(namlst,nml=station,err=92)
      read(namlst,nml=time,err=93)
      call init_eqstate(namlst)
      close (namlst)

      call init_meanflow(namlst,'gotmmean.nml')

      call init_observations(namlst,'obs.nml')

      call init_stokes_drift(namlst,'stokes_drift.nml')

      call init_turbulence(namlst,'gotmturb.nml')
      if (turb_method.eq.99) call init_kpp(namlst,'kpp.nml',nlev,depth,h,gravity,rho_0)
#ifdef _CVMIX_
      if (turb_method .eq. 100) call init_cvmix(namlst,'cvmix.nml')
#endif

      call init_airsea(namlst)
   end if

#ifdef _FABM_
   if (read_nml) call configure_gotm_fabm_from_nml(namlst, 'gotm_fabm.nml')

   ! Allow FABM to cretae its model tree. After this we know all biogeochemical variables
   ! This must be done before gotm_fabm_input configuration.
   call gotm_fabm_create_model(namlst)

   call configure_gotm_fabm_input()
   if (read_nml) call configure_gotm_fabm_input_from_nml(namlst, 'fabm_input.nml')
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
   if (read_nml .and. .not. file_exists) then
      call deprecated_output(namlst,title,dt,list_fields)
   end if

   ! Make sure all elements in the YAML configuration file were recognized
   if (.not. settings_store%check_all_used()) then
      FATAL trim(yaml_file) // ' is invalid.'
      stop 1
   end if

   if (write_yaml_path /= '') then
      call settings_store%save(trim(write_yaml_path), namlst, display=write_yaml_detail)
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
      LEVEL3 'online restart - updating values in the time namelist ...'
      LEVEL4 'orignal: ',start,' -> ',stop
!KB      start = t1
!KB      stop  = t2
      LEVEL4 'updated: ',start,' -> ',stop
   end if

!  The sea surface elevation (zeta) and vertical advection method (w_adv_method)
!  will be set by init_observations.
!  However, before that happens, it is already used in updategrid.
!  therefore, we set to to a reasonable default here.
   zeta%value = _ZERO_
   w_adv%method = 0

   restart = restart_online .or. restart_offline
   if (restart_online) restart_offline = .false.

!  initialize a few things from  namelists
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
   call updategrid(nlev,dt,zeta%value)

#ifdef SEAGRASS
      call init_seagrass(namlst,'seagrass.nml',unit_seagrass,nlev,h,fm)
#endif
#ifdef SPM
      call init_spm(namlst,'spm.nml',unit_spm,nlev)
#endif

!KB   call post_init_observations(julianday,secondsofday,depth,nlev,z,h,gravity,rho_0)
   call post_init_observations(depth,nlev,z,h,gravity,rho_0)
   call get_all_obs(julianday,secondsofday,nlev,z)

!  Stokes drift
   call post_init_stokes_drift(nlev)

!  Call do_input to make sure observed profiles are up-to-date.
   call do_input(julianday,secondsofday,nlev,z)

   ! Update the grid based on true initial zeta (possibly read from file by do_input).
   call updategrid(nlev,dt,zeta%value)

   call post_init_turbulence(nlev)

#ifdef _CVMIX_
   if (turb_method .eq. 100) then
      call post_init_cvmix(nlev,depth,gravity,rho_0)
   endif
#endif

!  initialise mean fields
   s(1:nlev) = sprof%data(1:nlev)
   t(1:nlev) = tprof%data(1:nlev)
   u(1:nlev) = uprof%data(1:nlev)
   v(1:nlev) = vprof%data(1:nlev)

   call post_init_airsea(latitude,longitude)
#if 0
#ifdef _ICE_
   call post_init_ice(ta,S(nlev))
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
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_air_pressure,airp%value)
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_temperature,ta)
      end if
      call set_env_gotm_fabm(latitude,longitude,dt,w_adv%method,w_adv_discr,t(1:nlev),s(1:nlev),rho(1:nlev), &
                             nuh,h,w,bioshade(1:nlev),I_0%value,cloud%value,taub,wind,precip%value,evap,z(1:nlev), &
                             A_%value,g1_%value,g2_%value,yearday,secondsofday,SRelaxTau(1:nlev),sProf%data(1:nlev), &
                             bio_albedo,bio_drag_scale)

      ! Initialize FABM input (data files with observations)
      call init_gotm_fabm_input(nlev,h(1:nlev))

      ! Transfer optional dependencies to FABM coupler
      if (fluxes_method /= 0) fabm_airp => airp%value
      fabm_calendar_date => calendar_date
      fabm_julianday => julianday
   end if
#endif

   ! Now that all inputs have been registered (FABM added some), update them all by reading from file.
   call do_input(julianday,secondsofday,nlev,z)

#ifdef _FABM_
!  Initialize FABM initial state (this is done after the first call to do_input,
!  to allow user-specified observed values to be used as initial state)
   if (fabm_calc) call init_gotm_fabm_state(nlev)
#endif

   if (restart) then
      if (restart_offline) then
         LEVEL1 'read_restart'
#ifdef NETCDF_FMT
         if (.not. restart_allow_perpetual) then
            call check_restart_time('time')
         else
            LEVEL2 'allow perpetual restarts'
         end if
         call read_restart(restart_allow_missing_variable)
#else
         FATAL 'Restart reading requires NetCDF support. Please build with GOTM_USE_NetCDF=ON'
         stop 1
#endif
         call friction(kappa,avmolu,tx,ty)
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
   call stratification(nlev,buoy_method,dt,cnpar,nuh,gamh)

#ifdef _FABM_
!  Accept the current biogeochemical state and used it to compute derived diagnostics.
   if (fabm_calc) call start_gotm_fabm(nlev, fm)
#endif

   if (list_fields) call fm%list()

   LEVEL2 'done.'
   STDERR LINE

#ifdef _PRINTSTATE_
   call print_state
#endif
   return

90 FATAL 'I could not open gotmrun.nml for reading'
   stop 'init_gotm'
91 FATAL 'I could not read the "model_setup" namelist'
   stop 'init_gotm'
92 FATAL 'I could not read the "station" namelist'
   stop 'init_gotm'
93 FATAL 'I could not read the "time" namelist'
   stop 'init_gotm'
   end subroutine init_gotm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Manage global time--stepping \label{timeLoop}
!
! !INTERFACE:
   subroutine time_loop()
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
   integer(kind=timestepkind):: n,progress
   integer                   :: i

   REALTYPE                  :: tFlux,btFlux,sFlux,bsFlux
   REALTYPE                  :: tRad(0:nlev),bRad(0:nlev)
   character(8)              :: d_
   character(10)             :: t_

   REALTYPE                  :: Qsw, Qflux
   REALTYPE                  :: La, EFactor
!
!-----------------------------------------------------------------------
!BOC
   if (.not. restart) then
      LEVEL1 'saving initial conditions'
      call output_manager_save(julianday,int(fsecondsofday),int(mod(fsecondsofday,_ONE_)*1000000),0)
   end if
   STDERR LINE
   LEVEL1 'time_loop'
   progress = (MaxN-MinN+1)/10
   i=0
   do n=MinN,MaxN

      if(mod(n,progress) .eq. 0 .or. n .eq. MinN) then
#if 0
         call date_and_time(date=d_,time=t_)
         LEVEL0 i,'%: ',t_(1:2),':',t_(3:4),':',t_(5:10)
#else
         LEVEL0 i,'%'
#endif
         i = i +10
      end if

!     prepare time and output
      call update_time(n)
      call output_manager_prepare_save(julianday, int(fsecondsofday), int(mod(fsecondsofday,_ONE_)*1000000), int(n))

!     all observations/data
      call do_input(julianday,secondsofday,nlev,z)
      call get_all_obs(julianday,secondsofday,nlev,z)
      call do_stokes_drift(nlev,z,zi,gravity,u10%value,v10%value)

!     external forcing
      if(fluxes_method /= 0) then
#ifdef _ICE_
         if(ice_cover .eq. 0) then
#endif
         call set_sst(T(nlev))
         call set_ssuv(u(nlev),v(nlev))
#ifdef _ICE_
         else
            call set_sst(Tice_surface)
            call set_ssuv(_ZERO_,_ZERO_)
         end if
#endif
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
      call do_ice(h(nlev),dt,T(nlev),S(nlev),ta,precip%value,Qsw,surface_fluxes)
#endif

!     reset some quantities
#ifdef _ICE_
      if(ice_cover .gt. 0) then
         tx = _ZERO_
         ty = _ZERO_
         heat%value = _ZERO_
         I_0%value = transmissivity*I_0%value
      else
#endif
         tx = tx/rho_0
         ty = ty/rho_0
#ifdef _ICE_
      end if
#endif

      call integrated_fluxes(dt)

!     meanflow integration starts
      call updategrid(nlev,dt,zeta%value)
      call wequation(nlev,dt)
      call coriolis(nlev,dt)

!     update velocity
      call uequation(nlev,dt,cnpar,tx,num,gamu,ext_press_mode)
      call vequation(nlev,dt,cnpar,ty,num,gamv,ext_press_mode)
      call extpressure(ext_press_mode,nlev)
      call intpressure(nlev)
      call friction(kappa,avmolu,tx,ty)

#ifdef SEAGRASS
      if(seagrass_calc) call do_seagrass(nlev,dt)
#endif

!     update temperature and salinity
      if (sprof%method .ne. 0) then
         call salinity(nlev,dt,cnpar,nus,gams)
      endif

      if (tprof%method .ne. 0) then
         call temperature(nlev,dt,cnpar,I_0%value,heat%value,nuh,gamh,rad)
      endif

!     update shear and stratification
      call shear(nlev,cnpar)
      call stratification(nlev,buoy_method,dt,cnpar,nuh,gamh)

#ifdef SPM
      if (spm_calc) then
         call set_env_spm(nlev,rho_0,depth,u_taub,h,u,v,nuh, &
                          tx,ty,Hs,Tz,Phiw)
         call do_spm(nlev,dt)
      end if
#endif
#ifdef _FABM_
      call do_gotm_fabm(nlev,real(n,kind(_ONE_)))
#endif

!     compute turbulent mixing
      select case (turb_method)
      case (0)
!        do convective adjustment
         call convectiveadjustment(nlev,num,nuh,const_num,const_nuh,    &
                                   buoy_method,gravity,rho_0)
      case (99)
!        update KPP model
         call convert_fluxes(nlev,gravity,cp,rho_0,heat%value,precip%value+evap,    &
                             rad,T,S,tFlux,sFlux,btFlux,bsFlux,tRad,bRad)

         call do_kpp(nlev,depth,h,rho,u,v,NN,NNT,NNS,SS,                &
                     u_taus,u_taub,tFlux,btFlux,sFlux,bsFlux,           &
                     tRad,bRad,cori)

#ifdef _CVMIX_
      case (100)

!        update Langmuir number
         call langmuir_number(nlev,zi,Hs_%value,u_taus,zi(nlev)-zsbl,u10%value,v10%value)

!        use KPP via CVMix
         call convert_fluxes(nlev,gravity,cp,rho_0,heat%value,precip%value+evap,    &
                             rad,T,S,tFlux,sFlux,btFlux,bsFlux,tRad,bRad)
         select case(kpp_langmuir_method)
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
         call do_cvmix(nlev,depth,h,rho,u,v,NN,NNT,NNS,SS,              &
                       u_taus,tFlux,btFlux,sFlux,bsFlux,                &
                       tRad,bRad,cori,efactor,La)
#endif

      case default
!        update one-point models
# ifdef SEAGRASS
         call do_turbulence(nlev,dt,depth,u_taus,u_taub,z0s,z0b,h,      &
                            NN,SS,xP)
# else
         call do_turbulence(nlev,dt,depth,u_taus,u_taub,z0s,z0b,h,      &
                            NN,SS)
# endif
      end select

      call do_diagnostics(nlev)
      call output_manager_save(julianday,int(fsecondsofday),int(mod(fsecondsofday,_ONE_)*1000000),int(n))

   end do
   STDERR LINE

   return
   end subroutine time_loop
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: The run is over --- now clean up.
!
! !INTERFACE:
   subroutine clean_up()
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

   LEVEL1 'clean_up'

   call clean_airsea()

#ifdef _ICE_
   call clean_ice()
#endif

   call clean_meanflow()

   if (turb_method.eq.99) call clean_kpp()

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

   call clean_diagnostics()

   call fm%finalize()

   return
   end subroutine clean_up
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

   subroutine read_restart(restart_allow_missing_variable)
      logical                               :: restart_allow_missing_variable
#ifdef NETCDF_FMT
      type (type_field_set)                 :: field_set
      class (type_field_set_member),pointer :: member

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
#else
      FATAL 'GOTM has been compiled without NetCDF support; restart reading is not supported'
      stop 'read_restart'
#endif
   end subroutine read_restart
!-----------------------------------------------------------------------

   end module gotm

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

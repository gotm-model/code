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
   use output_manager_core, only:output_manager_host=>host, type_output_manager_host=>type_host,type_output_manager_file=>type_file,time_unit_second,type_output_category
   use output_manager
   use diagnostics
   use settings

   use meanflow
   use input
   use input_netcdf
   use observations
   use time

   use airsea,      only: init_airsea,post_init_airsea,do_airsea,clean_airsea
   use airsea,      only: set_sst,set_ssuv,integrated_fluxes
   use airsea,      only: calc_fluxes
   use airsea,      only: wind=>w,tx,ty,I_0,cloud,heat,precip,evap,airp
   use airsea,      only: bio_albedo,bio_drag_scale
   use airsea_variables, only: qa,ta

   use turbulence,  only: turb_method
   use turbulence,  only: init_turbulence,post_init_turbulence,do_turbulence
   use turbulence,  only: num,nuh,nus
   use turbulence,  only: const_num,const_nuh
   use turbulence,  only: gamu,gamv,gamh,gams
   use turbulence,  only: kappa
   use turbulence,  only: clean_turbulence

   use kpp,         only: init_kpp,do_kpp,clean_kpp

   use mtridiagonal,only: init_tridiagonal,clean_tridiagonal
   use eqstate,     only: init_eqstate

#ifdef SEAGRASS
   use seagrass
#endif
#ifdef SPM
   use spm_var, only: spm_calc
   use spm, only: init_spm, set_env_spm, do_spm, end_spm
#endif
#ifdef _FABM_
   use gotm_fabm,only:init_gotm_fabm,post_init_gotm_fabm,init_gotm_fabm_state,start_gotm_fabm,set_env_gotm_fabm,do_gotm_fabm,clean_gotm_fabm,fabm_calc
   use gotm_fabm,only:model_fabm=>model,standard_variables_fabm=>standard_variables
   use gotm_fabm_input,only:init_gotm_fabm_input
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

   character(len=1024), public :: save_yaml_path = ''
   logical, public             :: read_yaml_file = .true.
   logical, public             :: read_nml_files = .false.
   
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
   subroutine init_gotm(t1,t2)
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
   character(len=*), intent(in), optional  :: t1,t2
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   class (type_settings), pointer :: branch, twig
   integer, parameter :: rk = kind(_ONE_)

   namelist /model_setup/ title,nlev,dt,restart_offline,restart_allow_missing_variable, &
                          cnpar,buoy_method
   namelist /station/     name,latitude,longitude,depth
   namelist /time/        timefmt,MaxN,start,stop
   logical          ::    list_fields=.false.
   logical          ::    restart_online=.false.
   logical          ::    restart_offline = .false.
   logical          ::    restart_allow_missing_variable = .false.
   integer          ::    rc
   logical          ::    file_exists
   logical          ::    config_only=.false.
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm'

   if (present(t1)) then
      restart_online = .true.
   end if

   if (trim(save_yaml_path) /= '') then
      config_only = .true.
      LEVEL2 'only doing configuration'
   end if
   STDERR LINE

   read_yaml_file = .true.
   if (.not.read_nml_files) then
      inquire(file='gotm.yaml',exist=file_exists)
      if (.not. file_exists) then
         FATAL 'GOTM now reads its configuration from gotm.yaml.'
         FATAL 'If you want to read it from namelists instead, specify --read_nml.'
         FATAL 'You can generate gotm.yaml from namelists by specifying --read_nml --write_yaml gotm.yaml.'
         stop 2
      end if
      LEVEL2 'reading yaml configuration'
      call settings_store%load('gotm.yaml', namlst)
   else
      if (.not. config_only) read_yaml_file = .false.
   end if

   if (read_yaml_file) then
      branch => settings_store%get_child('model_setup')
      call branch%get(title, 'title', '', &
                      default='GOTM simulation')
      call branch%get(nlev, 'nlev', 'number of vertical layers', &
                      minimum=1, default=100)
      call branch%get(dt, 'dt', 'time step', 's', &
                      minimum=0.e-10_rk, default=3600._rk)
      call branch%get(restart_offline, 'restart_offline', &
                      'initialize simulation with state stored in restart.nc', &
                      default=.false.)
      call branch%get(restart_allow_missing_variable, 'restart_allow_missing_variable', &
                      'warning or error when variable is missing in restart file', &
                      default=.false.)
      call branch%get(cnpar, 'cnpar', &
                      'constant for the theta scheme used for time integration of diffusion-reaction components', '-', &
                      minimum=0._rk, maximum=1._rk, default=1._rk)
      call branch%get(buoy_method, 'buoy_method', 'method to compute mean buoyancy', &
         options=(/type_option(1, 'from equation of state (i.e. from potential temperature and salinity)'), &
         type_option(2, 'from prognostic equation')/), default=1)

      twig => branch%get_child('station')
      call twig%get(name, 'name', '', &
                      default='GOTM site')
      call twig%get(latitude, 'latitude', 'latitude', 'degrees North', &
                      minimum=-90._rk, maximum=90._rk, default=0._rk)
      call twig%get(longitude, 'longitude', 'longitude', 'degrees East', &
                      minimum=-360._rk, maximum=360._rk, default=0._rk)
      call twig%get(depth, 'depth', '', '-', &
                      minimum=0._rk, default=1._rk)

      twig => branch%get_child('time')
      call twig%get(timefmt, 'timefmt', 'time format', &
                      minimum=1,maximum=3,default=2)
#if 0
      call twig%get(MaxN, 'MaxN', 'max number of time steps', &
                      minimum=1,default=100)
#endif
      call twig%get(start, 'start', 'start time', &
                      default='2017-01-01 00:00:00')
      call twig%get(stop, 'stop', 'stop time', &
                      default='2018-01-01 00:00:00')
   end if

!  open the namelist file.
   if (read_nml_files) then
      LEVEL2 'reading model setup namelists..'
      open(namlst,file='gotmrun.nml',status='old',action='read',err=90)

      read(namlst,nml=model_setup,err=91)
      read(namlst,nml=station,err=92)
      read(namlst,nml=time,err=93)
   end if

   if (.not. config_only) then
      if (restart_online) then
         LEVEL3 'online restart - updating values in the time namelist ...'
         LEVEL4 'orignal: ',start,' -> ',stop
         start = t1
         stop  = t2
         LEVEL4 'updated: ',start,' -> ',stop
      end if

!  The sea surface elevation (zeta) and vertical advection method (w_adv_method)
!  will be set by init_observations.
!  However, before that happens, it is already used in updategrid.
!  therefore, we set to to a reasonable default here.
      zeta = _ZERO_
      w_adv_method = 0

      ! Initialize field manager
      call fm%register_dimension('lon',1,id=id_dim_lon)
      call fm%register_dimension('lat',1,id=id_dim_lat)
      call fm%register_dimension('z',nlev,id=id_dim_z)
      call fm%register_dimension('zi',nlev+1,id=id_dim_zi)
      call fm%register_dimension('time',id=id_dim_time)
      call fm%initialize(prepend_by_default=(/id_dim_lon,id_dim_lat/),append_by_default=(/id_dim_time/))

      allocate(type_gotm_host::output_manager_host)
      call output_manager_init(fm,title)

      inquire(file='output.yaml',exist=file_exists)
      if (.not.file_exists) then
         call deprecated_output(namlst,title,dt,list_fields)
      end if

      restart = restart_online .or. restart_offline
      if (restart_online) restart_offline = .false.

      ! initialize a few things from  namelists
      timestep   = dt
      depth0     = depth

      ! write information for this run
      LEVEL2 trim(title)
      LEVEL2 'Using ',nlev,' layers to resolve a depth of',depth
      LEVEL2 'The station ',trim(name),' is situated at (lat,long) ',      &
              latitude,longitude
      LEVEL2 trim(name)

      if (restart_offline) then
         LEVEL2 'Offline restart ....'
      end if
   end if

   if (config_only) then
      LEVEL2 'configuring modules ....'
   else
      LEVEL2 'initializing modules ....'
      call init_input(nlev)
      call init_time(MinN,MaxN)
   end if
   if (read_yaml_file) then
      call init_eqstate()
   end if
   if (read_nml_files) then
      call init_eqstate(namlst)
   end if
   close (namlst)

!  From here - each init_? is responsible for opening and closing the
!  namlst - unit.
   if (read_yaml_file) then
      branch => settings_store%get_child('meanflow')
      call init_meanflow(branch)
   end if
   if (read_nml_files) then
      call init_meanflow(namlst,'gotmmean.nml')
   end if
   if (.not. config_only) then
      call post_init_meanflow(nlev,latitude)
      call init_tridiagonal(nlev)
      call updategrid(nlev,dt,zeta)
   end if

!  initialise each of the extra features/modules
#ifdef SEAGRASS
   if (read_nml_files) then
      call init_seagrass(namlst,'seagrass.nml',unit_seagrass,nlev,h,fm)
   end if
#endif
#ifdef SPM
   if (read_nml_files) then
      call init_spm(namlst,'spm.nml',unit_spm,nlev)
   end if
#endif
   if (read_yaml_file) then
      call init_observations()
   end if
   if (read_nml_files) then
      call init_observations(namlst,'obs.nml')
   end if

   if (.not. config_only) then
!KB   call post_init_observations(julianday,secondsofday,depth,nlev,z,h,gravity,rho_0)
      call post_init_observations(depth,nlev,z,h,gravity,rho_0)
      call get_all_obs(julianday,secondsofday,nlev,z)

      ! Call do_input to make sure observed profiles are up-to-date.
      call do_input(julianday,secondsofday,nlev,z)

      ! Update the grid based on true initial zeta (possibly read from file by do_input).
      call updategrid(nlev,dt,zeta)
   end if

   if (read_yaml_file) then
      call init_turbulence()
   end if
   if (read_nml_files) then
      call init_turbulence(namlst,'gotmturb.nml')
   end if

   if (.not. config_only) then
      call post_init_turbulence(nlev)

   ! initialise mean fields
      s = sprof
      t = tprof
      u = uprof
      v = vprof
   end if

!  initialize KPP model
   if (turb_method.eq.99) then
      if (read_nml_files) then
         call init_kpp(namlst,'kpp.nml',nlev,depth,h,gravity,rho_0)
      end if
   endif

   if (read_yaml_file) then
      branch => settings_store%get_child('airsea')
      call init_airsea(branch)
   end if
   if (read_nml_files) then
      call init_airsea(namlst)
   end if
   if (.not. config_only) then
      call post_init_airsea(latitude,longitude)

      call init_diagnostics(nlev)

      call do_register_all_variables(latitude,longitude,nlev)
   end if

   !  initialize FABM module
#ifdef _FABM_

!  Initialize the GOTM-FABM coupler from its configuration file.
   if (read_yaml_file) then
      branch => settings_store%get_child('gotm_fabm')
      call init_gotm_fabm(branch)
   end if
   if (read_nml_files) then
      call init_gotm_fabm(namlst,'gotm_fabm.nml')
   end if
   if (fabm_calc .and. .not. config_only) then
      call post_init_gotm_fabm(nlev,namlst,dt,fm)
   end if

!  Link relevant GOTM data to FABM.
!  This sets pointers, rather than copying data, and therefore needs to be done only once.
   if (fabm_calc .and. .not. config_only) then
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_depth,depth)
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_depth_below_geoid,depth0)
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_roughness_length,z0b)
      if (calc_fluxes) then
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_specific_humidity,qa)
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_air_pressure,airp)
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_temperature,ta)
      end if
#if 0
   call set_env_gotm_fabm(latitude,longitude,dt,w_adv_method,w_adv_discr,t(1:nlev),s(1:nlev),rho(1:nlev), &
                          nuh,h,w,bioshade(1:nlev),I_0,cloud,taub,wind,precip,evap,z(1:nlev), &
                          A,g1,g2,yearday,secondsofday,SRelaxTau(1:nlev),sProf(1:nlev), &
                          bio_albedo,bio_drag_scale)
#endif

      ! Initialize FABM input (data files with observations)
!   if (fabm_calc .and. read_nml_files) then
      call init_gotm_fabm_input(namlst,'fabm_input.nml',nlev,h(1:nlev))
   end if
#endif

   ! Now that all inputs have been registered (FABM added some), update them all by reading from file.
   if (.not. config_only) then
      call do_input(julianday,secondsofday,nlev,z)
   end if

#ifdef _FABM_
!  Initialize FABM initial state (this is done after the first call to do_input,
!  to allow user-specified observed values to be used as initial state)
   if (fabm_calc .and. .not. config_only) call init_gotm_fabm_state(nlev)
#endif

   if (.not. config_only) then
      if (restart) then
         if (restart_offline) then
            LEVEL1 'read_restart'
            call read_restart(restart_allow_missing_variable)
            call friction(kappa,avmolu,tx,ty)
         end if
         if (restart_online) then
         end if
      end if

!   allocate(type_gotm_host::output_manager_host)
!   call output_manager_init(fm,title)
      call setup_restart()
   end if
!   call init_output(title,nlev,latitude,longitude)

   if (.not. config_only) then
      call do_airsea(julianday,secondsofday)

      ! Call stratification to make sure density has sensible value.
      ! This is needed to ensure the initial density is saved correctly, and also for FABM.
      call shear(nlev,cnpar)
      call stratification(nlev,buoy_method,dt,cnpar,nuh,gamh)

#ifdef _FABM_
!     Accept the current biogeochemical state and used it to compute derived diagnostics.
      if (fabm_calc .and. .not. config_only) call start_gotm_fabm(nlev)
#endif

      if (list_fields) call fm%list()
   end if

   LEVEL2 'done.'
   STDERR LINE

   if (config_only) then
      call settings_store%save(trim(save_yaml_path), namlst)
      LEVEL0 'Your configuration has been written to '//trim(save_yaml_path)//'.'
      stop 0
   end if
   
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

!     all observations/data
      call do_input(julianday,secondsofday,nlev,z)
      call get_all_obs(julianday,secondsofday,nlev,z)

!     external forcing
      if( calc_fluxes ) then
         call set_sst(T(nlev))
         call set_ssuv(u(nlev),v(nlev))
      end if
      call do_airsea(julianday,secondsofday)

!     reset some quantities
      tx = tx/rho_0
      ty = ty/rho_0

      call integrated_fluxes(dt)

!     meanflow integration starts
      call updategrid(nlev,dt,zeta)
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
      if (s_prof_method .ne. 0) then
         call salinity(nlev,dt,cnpar,nus,gams)
      endif

      if (t_prof_method .ne. 0) then
         call temperature(nlev,dt,cnpar,I_0,heat,nuh,gamh,rad)
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

!    compute turbulent mixing
      select case (turb_method)
      case (0)
!        do convective adjustment
         call convectiveadjustment(nlev,num,nuh,const_num,const_nuh,    &
                                   buoy_method,gravity,rho_0)
      case (99)
!        update KPP model
         call convert_fluxes(nlev,gravity,cp,rho_0,heat,precip+evap,    &
                             rad,T,S,tFlux,sFlux,btFlux,bsFlux,tRad,bRad)

         call do_kpp(nlev,depth,h,rho,u,v,NN,NNT,NNS,SS,                &
                     u_taus,u_taub,tFlux,btFlux,sFlux,bsFlux,           &
                     tRad,bRad,cori)

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

   call clean_meanflow()

   if (turb_method.eq.99) call clean_kpp()

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

      class (type_netcdf_file),     pointer :: file
      class (type_output_category), pointer :: category

      allocate(file)
      file%path = 'restart'
      file%time_unit = time_unit_day
      file%time_step = 1
      file%first_julian = jul2
      file%first_seconds = secs2
      call output_manager_add_file(fm,file)

      allocate(category)
      category%name = 'state'
      category%output_level = output_level_debug
      category%settings => file%create_settings()
      select type (settings=>category%settings)
         class is (type_netcdf_variable_settings)
            settings%xtype = NF90_DOUBLE
      end select
      call file%append_category(category)
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
         if (associated(member%field%data_0d)) then
            ! Depth-independent variable with data pointed to by child%field%data_0d
            ! Here you would read the relevant scalar (name: member%field%name) from the NetCDF file and assign it to member%field%data_0d.
            call read_restart_data(trim(member%field%name),restart_allow_missing_variable,data_0d=member%field%data_0d)
         elseif (associated(member%field%data_1d)) then
            ! Depth-dependent variable with data pointed to by child%field%data_1d
            ! Here you would read the relevant 1D variable (name: member%field%name) from the NetCDF file and assign it to member%field%data_1d.
            call read_restart_data(trim(member%field%name),restart_allow_missing_variable,data_1d=member%field%data_1d)
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

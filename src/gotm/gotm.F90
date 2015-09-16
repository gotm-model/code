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
#if defined(_FLEXIBLE_OUTPUT_)
   use output_manager_core, only:output_manager_host=>host, type_output_manager_host=>type_host
   use output_manager
#endif
   use meanflow
   use input
   use observations
   use time

   use airsea,      only: init_air_sea,do_air_sea,clean_air_sea
   use airsea,      only: set_sst,set_ssuv,integrated_fluxes
   use airsea,      only: calc_fluxes
   use airsea,      only: wind=>w,tx,ty,I_0,cloud,heat,precip,evap,airp
   use airsea,      only: bio_albedo,bio_drag_scale
   use airsea_variables, only: qa,ta

   use turbulence,  only: turb_method
   use turbulence,  only: init_turbulence,do_turbulence
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
   use gotm_fabm,only:init_gotm_fabm,init_gotm_fabm_state,set_env_gotm_fabm,do_gotm_fabm,clean_gotm_fabm,fabm_calc
   use gotm_fabm,only:model_fabm=>model,standard_variables_fabm=>standard_variables
   use gotm_fabm_input,only:init_gotm_fabm_input
#if !defined(_FLEXIBLE_OUTPUT_)
   use gotm_fabm_output,only:init_gotm_fabm_output,do_gotm_fabm_output,clean_gotm_fabm_output
#endif
#endif

#if !defined(_FLEXIBLE_OUTPUT_)
   use output
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

#if defined(_FLEXIBLE_OUTPUT_)
   type,extends(type_output_manager_host) :: type_gotm_host
   contains
      procedure :: julian_day => gotm_host_julian_day
      procedure :: calendar_date => gotm_host_calendar_date
   end type
#endif

   type (type_field_manager),target :: fm
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the model \label{initGOTM}
!
! !INTERFACE:
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
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   namelist /model_setup/ title,nlev,dt,cnpar,buoy_method
   namelist /station/     name,latitude,longitude,depth
   namelist /time/        timefmt,MaxN,start,stop
#if !defined(_FLEXIBLE_OUTPUT_)
   namelist /output/      list_fields, &
                          out_fmt,out_dir,out_fn,nfirst,nsave,sync_out, &
                          diagnostics,mld_method,diff_k,Ri_crit,rad_corr
#endif
   logical          ::    list_fields=.false.
   integer          ::    rc
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm'
   STDERR LINE

!  The sea surface elevation (zeta) and vertical advection method (w_adv_method)
!  will be set by init_observations.
!  However, before that happens, it is already used in updategrid.
!  therefore, we set to to a reasonable default here.
   zeta = _ZERO_
   w_adv_method = 0

#if !defined(_FLEXIBLE_OUTPUT_)
!  Initialize namelist parameters to reasonable defaults.
   out_fmt     = ASCII
   out_dir     = '.'
   out_fn      = 'gotm'
   nfirst      = 0
   nsave       = 1
   sync_out    = 1
   diagnostics = .false.
   mld_method  = 1
   diff_k      = 1.e-5
   Ri_crit     = 0.5
   rad_corr    = .true.
#endif

!  open the namelist file.
   LEVEL2 'reading model setup namelists..'
   open(namlst,file='gotmrun.nml',status='old',action='read',err=90)

   read(namlst,nml=model_setup,err=91)
   read(namlst,nml=station,err=92)
   read(namlst,nml=time,err=93)
#if !defined(_FLEXIBLE_OUTPUT_)
   read(namlst,nml=output,err=94)

   if (sync_out .lt. 0) then
      sync_out = 0
   end if
#endif

   LEVEL2 'done.'

!  initialize a few things from  namelists
   timestep   = dt
   depth0     = depth

!  write information for this run
   LEVEL2 trim(title)
   LEVEL2 'Using ',nlev,' layers to resolve a depth of',depth
   LEVEL2 'The station ',trim(name),' is situated at (lat,long) ',      &
           latitude,longitude
   LEVEL2 trim(name)

   LEVEL2 'initializing modules....'
   call init_input(nlev)
   call init_time(MinN,MaxN)
   call init_eqstate(namlst)
   close (namlst)

!  From here - each init_? is responsible for opening and closing the
!  namlst - unit.
   call init_meanflow(namlst,'gotmmean.nml',nlev,latitude)
   call init_tridiagonal(nlev)
   call updategrid(nlev,dt,zeta)

!  initialise each of the extra features/modules
#ifdef SEAGRASS
   call init_seagrass(namlst,'seagrass.nml',unit_seagrass,nlev,h)
#endif
#ifdef SPM
   call init_spm(namlst,'spm.nml',unit_spm,nlev)
#endif
   call init_observations(namlst,'obs.nml',julianday,secondsofday,      &
                          depth,nlev,z,h,gravity,rho_0)
   call get_all_obs(julianday,secondsofday,nlev,z)

!  Call do_input to make sure observed profiles are up-to-date.
   call do_input(julianday,secondsofday,nlev,z)

   call init_turbulence(namlst,'gotmturb.nml',nlev)

!  initialise mean fields
   s = sprof
   t = tprof
   u = uprof
   v = vprof

!  initialize KPP model
   if (turb_method.eq.99) then
      call init_kpp(namlst,'kpp.nml',nlev,depth,h,gravity,rho_0)
   endif

   call init_air_sea(namlst,latitude,longitude)

   call register_all_variables(latitude,longitude,nlev,fm)
#if defined(_FLEXIBLE_OUTPUT_)
   allocate(type_gotm_host::output_manager_host)
   call output_manager_init(fm)
#else
   call init_output(title,nlev,latitude,longitude)
#endif

!  initialize FABM module
#ifdef _FABM_

!  Initialize the GOTM-FABM coupler from its configuration file.
   call init_gotm_fabm(nlev,namlst,'gotm_fabm.nml',dt,fm)

!  Link relevant GOTM data to FABM.
!  This sets pointers, rather than copying data, and therefore needs to be done only once.
   if (fabm_calc) then
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_depth,depth)
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_depth_below_geoid,depth0)
      call model_fabm%link_horizontal_data(standard_variables_fabm%bottom_roughness_length,z0b)
      if (calc_fluxes) then
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_specific_humidity,qa)
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_air_pressure,airp)
         call model_fabm%link_horizontal_data(standard_variables_fabm%surface_temperature,ta)
      end if
   end if
   call set_env_gotm_fabm(latitude,longitude,dt,w_adv_method,w_adv_discr,t(1:nlev),s(1:nlev),rho(1:nlev), &
                          nuh,h,w,bioshade(1:nlev),I_0,cloud,taub,wind,precip,evap,z(1:nlev), &
                          A,g1,g2,yearday,secondsofday,SRelaxTau(1:nlev),sProf(1:nlev), &
                          bio_albedo,bio_drag_scale)

!  Initialize FABM input (data files with observations)
   call init_gotm_fabm_input(namlst,'fabm_input.nml',nlev,h(1:nlev))
#endif

   call do_input(julianday,secondsofday,nlev,z)

!  Call stratification to make sure density has sensible value.
!  This is needed to ensure the initial density is saved correctly by do_all_output, and also for FABM.
   call stratification(nlev,buoy_method,dt,cnpar,nuh,gamh)

#ifdef _FABM_

   if (fabm_calc) then
!     Initialize FABM initial state (this is done after the first call to do_input,
!     to allow user-specified observed values to be used as initial state)
      call init_gotm_fabm_state(nlev)

#if !defined(_FLEXIBLE_OUTPUT_)
!     Initialize FABM output (creates NetCDF variables)
!     This should be done after init_gotm_fabm_state is called, so the output module can compute
!     initial conserved quantity integrals.
      call init_gotm_fabm_output(nlev)
#endif
   end if

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
94 FATAL 'I could not read the "output" namelist'
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
   integer(kind=timestepkind):: n

   REALTYPE                  :: tFlux,btFlux,sFlux,bsFlux
   REALTYPE                  :: tRad(0:nlev),bRad(0:nlev)
!
!-----------------------------------------------------------------------
!BOC
#if !defined(_FLEXIBLE_OUTPUT_)
   LEVEL1 'saving initial conditions'
   call prepare_output(0_timestepkind)
   if (write_results) then
      call do_all_output(0_timestepkind)
   end if
#else
   call output_manager_save(julianday,secondsofday)
#endif
   LEVEL1 'time_loop'
   do n=MinN,MaxN

!     prepare time and output
      call update_time(n)
#if !defined(_FLEXIBLE_OUTPUT_)
      call prepare_output(n)
#endif

!     all observations/data
      call do_input(julianday,secondsofday,nlev,z)
      call get_all_obs(julianday,secondsofday,nlev,z)

!     external forcing
      if( calc_fluxes ) then
         call set_sst(T(nlev))
         call set_ssuv(u(nlev),v(nlev))
      end if
      call do_air_sea(julianday,secondsofday)

!     reset some quantities
      tx = tx/rho_0
      ty = ty/rho_0

!     meanflow integration starts
      call updategrid(nlev,dt,zeta)
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

#if !defined(_FLEXIBLE_OUTPUT_)
!     do the output
      if (write_results) then
         call do_all_output(n)
      end if
#else
      call output_manager_save(julianday,secondsofday)
#endif

      call integrated_fluxes(dt)

#if !defined(_FLEXIBLE_OUTPUT_)
!     diagnostic output
      if(diagnostics) then
         call do_diagnostics(n,nlev,buoy_method,dt,u_taus,u_taub,I_0,heat)
      end if
#endif

   end do
   STDERR LINE

   return
   end subroutine time_loop
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: A wrapper for doing all output.
!
! !INTERFACE:
   subroutine do_all_output(n)
!
! !DESCRIPTION:
! This function is just a wrapper for all output routines
!
! !USES:
   IMPLICIT NONE
!
 !INPUT PARAMETERS:
   integer(kind=timestepkind), intent(in)   :: n
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (turb_method .ne. 99) then
      call variances(nlev,SSU,SSV)
   endif
#if !defined(_FLEXIBLE_OUTPUT_)
   call do_output(n,nlev)
#ifdef SEAGRASS
   if (seagrass_calc) call save_seagrass()
#endif
#ifdef SPM
   if (spm_calc) call spm_save(nlev)
#endif
#ifdef _FABM_
   call do_gotm_fabm_output(nlev,initial=n==0_timestepkind)
#endif
#endif

   end subroutine do_all_output
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

#if !defined(_FLEXIBLE_OUTPUT_)
   call close_output()
#endif

   call clean_air_sea()

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
#if !defined(_FLEXIBLE_OUTPUT_)
   call clean_gotm_fabm_output()
#endif
#endif

   call close_input()

#if defined(_FLEXIBLE_OUTPUT_)
   call output_manager_clean()
#endif

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
   call print_state_bio

   end subroutine print_state
!EOC
#endif

#if defined(_FLEXIBLE_OUTPUT_)
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
#endif

!-----------------------------------------------------------------------

   end module gotm

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

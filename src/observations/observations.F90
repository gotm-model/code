#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: observations --- the 'real' world \label{sec:observations}
!
! !INTERFACE:
   module observations
!
! !DESCRIPTION:
!  This module provides the necessary subroutines for communicating
!  `observations' to GOTM.
!  The module operates according to the general philosophy used in GOTM,
!  i.e.\ it provides {\tt init\_observ\-ations()} to be called in the overall
!  initialisation routine and {\tt get\_all\_obs()} to be called in the time
!  loop to actually obtain the `observations'.
!  In addition to these subroutines the module also provides two routines
!  for reading scalar-type observations and profile-type observations.
!  Each observation has a date stamp with the format {\tt yyyy-mm-dd hh:dd:mm}.
!  The module uses the {\tt time} module (see \sect{sec:time})
!  to convert the time string to the
!  internal time representation of GOTM.
!  Profiles are interpolated to the actual GOTM model grid.
!  Free format is used for reading-in the actual data.
!
! !USES:
   use input
   IMPLICIT NONE
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_observations, post_init_observations, get_all_obs, clean_observations
#ifdef _PRINTSTATE_
   public print_state_observations
#endif

   interface init_observations
      module procedure init_observations_nml
      module procedure init_observations_yaml
   end interface
!
! !PUBLIC DATA MEMBERS:
!
!  'observed' salinity profile
   integer, public :: initial_salinity_type
   type (type_profile_input), public, target :: sprof_input

!  'observed' temperature profile
   integer, public :: initial_temperature_type
   type (type_profile_input), public, target :: tprof_input

!  'observed' oxygen profile
   type (type_profile_input), public, target :: o2_prof_input

!  'observed' horizontal salinity gradients
   type (type_profile_input), public, target :: dsdx_input,dsdy_input

!  'observed' horizontal temperature gradients
   type (type_profile_input), public, target :: dtdx_input,dtdy_input

!  internal horizontal pressure gradients
   REALTYPE, public, dimension(:), allocatable :: idpdx,idpdy

!  horizontal velocity profiles
   type (type_profile_input), public, target :: uprof_input,vprof_input

!  observed profile of turbulent dissipation rates
   type (type_profile_input), public, target :: epsprof_input

!  relaxation times for salinity and temperature
   REALTYPE, public, dimension(:), allocatable, target :: SRelaxTau
   REALTYPE, public, dimension(:), allocatable         :: TRelaxTau

!  sea surface elevation, sea surface gradients and height of velocity obs.
   type (type_scalar_input), public, target :: zeta_input,dpdx_input,dpdy_input,h_press_input

!  vertical advection velocity
   type (type_scalar_input), public, target :: w_adv_input,w_height_input

!  Parameters for water classification - default Jerlov type I
   type (type_scalar_input), public, target :: A_input, g1_input, g2_input

!------------------------------------------------------------------------------
!
! the following data are not all public,
! but have been included for clarity here
!
!------------------------------------------------------------------------------

!  Salinity profile(s)
   REALTYPE                  :: z_s1,s_1,z_s2,s_2
   REALTYPE                  :: s_obs_NN
   REALTYPE                  :: SRelaxTauM
   REALTYPE                  :: SRelaxTauS
   REALTYPE                  :: SRelaxTauB
   REALTYPE                  :: SRelaxSurf
   REALTYPE                  :: SRelaxBott

!  Temperature profile(s)
   REALTYPE                  :: z_t1,t_1,z_t2,t_2
   REALTYPE                  :: t_obs_NN
   REALTYPE                  :: TRelaxTauM
   REALTYPE                  :: TRelaxTauS
   REALTYPE                  :: TRelaxTauB
   REALTYPE                  :: TRelaxSurf
   REALTYPE                  :: TRelaxBott

!  External pressure - 'press' namelist
   integer, public           :: ext_press_mode
   REALTYPE, public          :: PeriodM
   REALTYPE, public          :: AmpMu
   REALTYPE, public          :: AmpMv
   REALTYPE, public          :: PhaseMu
   REALTYPE, public          :: PhaseMv
   REALTYPE, public          :: PeriodS
   REALTYPE, public          :: AmpSu
   REALTYPE, public          :: AmpSv
   REALTYPE, public          :: PhaseSu
   REALTYPE, public          :: PhaseSv

!  Internal pressure - 'internal_pressure' namelist
   integer, public           :: int_press_type
   logical, public           :: s_adv
   logical, public           :: t_adv

!  Plume
   integer, public           :: plume_type
   REALTYPE, public          :: plume_slope_x
   REALTYPE, public          :: plume_slope_y

!  Light extinction - the 'extinct' namelist
   integer                   :: extinct_method

!  Vertical advection velocity - 'w_advspec' namelist
   integer, public           :: w_adv_discr

!  Sea surface elevations - 'zetaspec' namelist
   REALTYPE, public          :: period_1
   REALTYPE, public          :: amp_1
   REALTYPE, public          :: phase_1
   REALTYPE, public          :: period_2
   REALTYPE, public          :: amp_2
   REALTYPE, public          :: phase_2

!  Wind waves - 'wave_nml' namelist
   type (type_scalar_input), public :: Hs_input
   type (type_scalar_input), public :: Tz_input
   type (type_scalar_input), public :: phiw_input

!  Observed velocity profile profiles - typically from ADCP
   integer                   :: vel_prof_method
   CHARACTER(LEN=PATH_MAX)   :: vel_prof_file
   REALTYPE, public          :: vel_relax_tau
   REALTYPE, public          :: vel_relax_ramp

!  Observed dissipation profiles
   integer                   :: e_prof_method
   REALTYPE                  :: e_obs_const
   CHARACTER(LEN=PATH_MAX)   :: e_prof_file

!  Buoyancy - 'bprofile' namelist
   REALTYPE, public          :: b_obs_surf,b_obs_NN
   REALTYPE, public          :: b_obs_sbf

   REALTYPE,public, parameter:: pi=3.141592654d0

! !DEFINED PARAMETERS:

!  pre-defined parameters
   integer, parameter        :: NOTHING=0
   integer, parameter        :: ANALYTICAL=1
   integer, parameter        :: CONSTANT=1
   integer, parameter        :: FROMFILE=2
   integer, parameter        :: CONST_PROF=1
   integer, parameter        :: TWO_LAYERS=2
   integer, parameter        :: CONST_NN=3
   integer, parameter        :: ANALYTICAL_OFFSET=10
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   character(len=128)         :: cbuf
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the observation module
!
! !INTERFACE:
   subroutine init_observations_nml(namlst,fn)
!
! !DESCRIPTION:
!  The {\tt init\_observations()} subroutine basically reads the {\tt obs.nml}
!  file with a number of different namelists and takes actions according
!  to the specifications in the different namelists.
!  In this routine also memory is allocated to hold the 'observations'.
!  Finally, all variables are initialised to sane values, either by
!  reading from files, by prescribing constant values, or by using analytical
!  expressions.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: namlst
   character(len=*), intent(in)        :: fn
!
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   integer, parameter :: rk = kind(_ZERO_)

   integer                   :: s_prof_method
   character(LEN=PATH_MAX)   :: s_prof_file
   integer                   :: s_analyt_method
   integer                   :: t_prof_method
   character(LEN=PATH_MAX)   :: t_prof_file
   integer                   :: t_analyt_method

   integer                 :: int_press_method
   character(LEN=PATH_MAX) :: int_press_file
   REALTYPE                :: const_dsdx
   REALTYPE                :: const_dsdy
   REALTYPE                :: const_dtdx
   REALTYPE                :: const_dtdy

   integer                 :: ext_press_method
   character(LEN=PATH_MAX) :: ext_press_file
   REALTYPE                :: PressConstU
   REALTYPE                :: PressConstV
   REALTYPE                :: PressHeight

   integer                 :: zeta_method
   character(LEN=PATH_MAX) :: zeta_file
   REALTYPE                :: zeta_scale
   REALTYPE                :: zeta_offset
   REALTYPE                :: zeta_0

   integer                 :: w_adv_method
   REALTYPE                :: w_adv0
   REALTYPE                :: w_adv_height0
   character(LEN=PATH_MAX) :: w_adv_file

   character(LEN=PATH_MAX)   :: extinct_file
   REALTYPE :: A, g1, g2

!  Oxygen profile(s)
   integer           :: o2_prof_method
   integer           :: o2_units
   character(LEN=PATH_MAX)   :: o2_prof_file
   
   REALTYPE,parameter        :: mmol_o2_per_liter = 44.661_rk
   REALTYPE,parameter        :: mmol_o2_per_gram  = 31.25_rk

   integer                   :: wave_method
   character(LEN=PATH_MAX)   :: wave_file
   REALTYPE                  :: Hs,Tz,phiw

   namelist /sprofile/                                          &
            s_prof_method,s_analyt_method,                      &
            z_s1,s_1,z_s2,s_2,s_prof_file,s_obs_NN,             &
            SRelaxTauM,SRelaxTauB,SRelaxTauS,                   &
            SRelaxBott,SRelaxSurf

   namelist /tprofile/                                          &
            t_prof_method,t_analyt_method,                      &
            z_t1,t_1,z_t2,t_2,t_prof_file,t_obs_NN,             &
            TRelaxTauM,TRelaxTauB,TRelaxTauS,                   &
            TRelaxBott,TRelaxSurf

   namelist /o2_profile/                                        &
            o2_prof_method,o2_units,o2_prof_file

   namelist /ext_pressure/                                      &
            ext_press_method,ext_press_mode,ext_press_file,     &
            PressConstU,PressConstV,PressHeight,                &
            PeriodM,AmpMu,AmpMv,PhaseMu,PhaseMv,                &
            PeriodS,AmpSu,AmpSv,PhaseSu,PhaseSv

   namelist /int_pressure/                                      &
            int_press_method,int_press_type,int_press_file,     &
            const_dsdx,const_dsdy,const_dtdx,const_dtdy,        &
            s_adv,t_adv,plume_slope_x,plume_slope_y

   namelist /extinct/ extinct_method,extinct_file,A,g1,g2

   namelist /w_advspec/                                         &
            w_adv_method,w_adv_file,w_adv_height0,w_adv0,w_adv_discr

   namelist /zetaspec/                                          &
            zeta_method,zeta_file,zeta_scale,zeta_offset,zeta_0,&
            period_1,amp_1,phase_1,period_2,amp_2,phase_2

   namelist /wave_nml/                                          &
            wave_method,wave_file,Hs,Tz,phiw

   namelist /velprofile/ vel_prof_method,vel_prof_file,         &
            vel_relax_tau,vel_relax_ramp

   namelist /eprofile/ e_prof_method,e_obs_const,e_prof_file

   namelist /bprofile/ b_obs_surf,b_obs_NN,b_obs_sbf
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_observations_nml'

!  Salinity profile(s)
   s_prof_method=0
   s_analyt_method=1
   s_prof_file='sprof.dat'
   z_s1 = _ZERO_
   s_1 = _ZERO_
   z_s2 = _ZERO_
   s_2 = _ZERO_
   s_obs_NN = _ZERO_
   SRelaxTauM=_ZERO_
   SRelaxTauS=_ZERO_
   SRelaxTauB=_ZERO_
   SRelaxSurf=_ZERO_
   SRelaxBott=_ZERO_

!  Temperature profile(s)
   t_prof_method=0
   t_analyt_method=1
   t_prof_file='tprof.dat'
   z_t1 = _ZERO_
   t_1 = _ZERO_
   z_t2 = _ZERO_
   t_2 = _ZERO_
   t_obs_NN = _ZERO_
   TRelaxTauM=_ZERO_
   TRelaxTauS=_ZERO_
   TRelaxTauB=_ZERO_
   TRelaxSurf=_ZERO_
   TRelaxBott=_ZERO_

!  Oxygen profile(s)
   o2_prof_method=0
   o2_units=1
   o2_prof_file='o2.dat'

!  External pressure - 'press' namelist
   ext_press_method=0
   ext_press_mode=0
   ext_press_file=''
   PressConstU=_ZERO_
   PressConstV=_ZERO_
   PressHeight=_ZERO_
   PeriodM=44714._rk
   AmpMu=_ZERO_
   AmpMv=_ZERO_
   PhaseMu=_ZERO_
   PhaseMv=_ZERO_
   PeriodS=43200._rk
   AmpSu=_ZERO_
   AmpSv=_ZERO_
   PhaseSu=_ZERO_
   PhaseSv=_ZERO_

!  Internal pressure - 'internal_pressure' namelist
   int_press_method=0
   int_press_type=0
   int_press_file=''
   const_dsdx=_ZERO_
   const_dsdy=_ZERO_
   const_dtdx=_ZERO_
   const_dtdy=_ZERO_
   s_adv=.false.
   t_adv=.false.

!  Light extinction - the 'extinct' namelist
   extinct_method=1
   extinct_file='extinction.dat'
   ! extinct_method=7 - user defined
   ! default values are from Lago Maggiore, Stips
   A=0.7_rk
   g1=0.40_rk
   g2=8.0_rk

!  Vertical advection velocity - 'w_advspec' namelist
   w_adv_method=0
   w_adv0=_ZERO_
   w_adv_height0=_ZERO_
   w_adv_file='w_adv.dat'
   w_adv_discr=1

!  Sea surface elevations - 'zetaspec' namelist
   zeta_method=0
   zeta_file='zeta.dat'
   zeta_scale=_ONE_
   zeta_offset=_ZERO_
   zeta_0=_ZERO_
   period_1=44714._rk
   amp_1=_ZERO_
   phase_1=_ZERO_
   period_2=43200._rk
   amp_2=_ZERO_
   phase_2=_ZERO_

!  Wind waves - 'wave_nml' namelist
   wave_method=0
   wave_file='wave.dat'
   Hs=_ZERO_
   Tz=_ZERO_
   phiw=_ZERO_

!  Observed velocity profile profiles - typically from ADCP
   vel_prof_method=0
   vel_prof_file='velprof.dat'
   vel_relax_tau=3600._rk
   vel_relax_ramp=86400._rk

!  Observed dissipation profiles
   e_prof_method=0
   e_obs_const=1.e-12_rk
   e_prof_file='eprof.dat'

!  Buoyancy - 'bprofile' namelist
   b_obs_surf=_ZERO_
   b_obs_NN=_ZERO_
   b_obs_sbf=_ZERO_

   open(namlst,file=fn,status='old',action='read',err=80)
   read(namlst,nml=sprofile,err=81)
   read(namlst,nml=tprofile,err=82)
   read(namlst,nml=ext_pressure,err=83)
   read(namlst,nml=int_pressure,err=84)
   read(namlst,nml=extinct,err=85)
   read(namlst,nml=w_advspec,err=86)
   read(namlst,nml=zetaspec,err=87)
   read(namlst,nml=wave_nml,err=88)
   read(namlst,nml=velprofile,err=89)
   read(namlst,nml=eprofile,err=90)
   read(namlst,nml=bprofile,err=91)
   read(namlst,nml=o2_profile,err=92)
   close(namlst)

   if (s_prof_method == ANALYTICAL) s_prof_method = ANALYTICAL_OFFSET + s_analyt_method
   if (t_prof_method == ANALYTICAL) t_prof_method = ANALYTICAL_OFFSET + t_analyt_method
   call sprof_input%configure(method=s_prof_method, path=s_prof_file, index=1, constant_value=s_1)
   call tprof_input%configure(method=t_prof_method, path=t_prof_file, index=1, constant_value=t_1)

   call h_press_input%configure(method=ext_press_method, path=ext_press_file, index=1, constant_value=PressHeight)
   call dpdx_input%configure(method=ext_press_method, path=ext_press_file, index=2, constant_value=PressConstU)
   call dpdy_input%configure(method=ext_press_method, path=ext_press_file, index=3, constant_value=PressConstV)

   call dsdx_input%configure(method=int_press_method, path=int_press_file, index=1, constant_value=const_dsdx)
   call dsdy_input%configure(method=int_press_method, path=int_press_file, index=2, constant_value=const_dsdy)
   call dtdx_input%configure(method=int_press_method, path=int_press_file, index=3, constant_value=const_dtdx)
   call dtdy_input%configure(method=int_press_method, path=int_press_file, index=4, constant_value=const_dtdy)

   if (extinct_method == 0) then
      extinct_method = 7
      call A_input%configure(method=2, path=extinct_file, index=1, constant_value=A)
      call g1_input%configure(method=2, path=extinct_file, index=2, constant_value=g1)
      call g2_input%configure(method=2, path=extinct_file, index=3, constant_value=g2)
   else
      call A_input%configure(method=0, path=extinct_file, index=1, constant_value=A)
      call g1_input%configure(method=0, path=extinct_file, index=2, constant_value=g1)
      call g2_input%configure(method=0, path=extinct_file, index=3, constant_value=g2)
   end if

   call w_adv_input%configure(method=w_adv_method, path=w_adv_file, index=1, constant_value=w_adv0)
   call w_height_input%configure(method=max(1, w_adv_method), path=w_adv_file, index=2, constant_value=w_adv_height0)

   call zeta_input%configure(method=zeta_method, path=zeta_file, index=1, constant_value=zeta_0, scale_factor=zeta_scale, add_offset=zeta_offset)

   call Hs_input%configure(method=wave_method, path=wave_file, index=1, constant_value=Hs)
   call Tz_input%configure(method=wave_method, path=wave_file, index=2, constant_value=Tz)
   call phiw_input%configure(method=wave_method, path=wave_file, index=3, constant_value=phiw)

   call uprof_input%configure(method=vel_prof_method, path=vel_prof_file, index=1)
   call vprof_input%configure(method=vel_prof_method, path=vel_prof_file, index=2)

   call epsprof_input%configure(method=e_prof_method, path=e_prof_file, index=1, constant_value=e_obs_const)

   call o2_prof_input%configure(method=o2_prof_method, path=o2_prof_file, index=1)
   select case (o2_units)
   case (1) ! mg/l
      o2_prof_input%scale_factor=mmol_o2_per_gram
   case (2) ! ml/l
      o2_prof_input%scale_factor=mmol_o2_per_liter
   end select

   LEVEL2 'done.'

   return

80 FATAL 'Unable to open "',fn,'" for reading'
   stop 'init_observations'
81 FATAL 'I could not read "sprofile" namelist'
   stop 'init_observations'
82 FATAL 'I could not read "tprofile" namelist'
   stop 'init_observations'
83 FATAL 'I could not read "ext_pressure" namelist'
   stop 'init_observations'
84 FATAL 'I could not read "int_pressure" namelist'
   stop 'init_observations'
85 FATAL 'I could not read "extinct" namelist'
   stop 'init_observations'
86 FATAL 'I could not read "w_advspec" namelist'
   stop 'init_observations'
87 FATAL 'I could not read "zetaspec" namelist'
   stop 'init_observations'
88 FATAL 'I could not read "wave_nml" namelist'
   stop 'init_observations'
89 FATAL 'I could not read "velprofile" namelist'
   stop 'init_observations'
90 FATAL 'I could not read "eprofile" namelist'
   stop 'init_observations'
91 FATAL 'I could not read "bprofile" namelist'
   stop 'init_observations'
92 FATAL 'I could not read "o2_profile" namelist'
   stop 'init_observations'

   end subroutine init_observations_nml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the observation module
!
! !INTERFACE:
   subroutine init_observations_yaml()
!
! !DESCRIPTION:
!  The {\tt init\_observations()} subroutine basically reads the {\tt obs.nml}
!  file with a number of different namelists and takes actions according
!  to the specifications in the different namelists.
!  In this routine also memory is allocated to hold the 'observations'.
!  Finally, all variables are initialised to sane values, either by
!  reading from files, by prescribing constant values, or by using analytical
!  expressions.
!
! !USES:
   use settings
   use util, only: UPSTREAM, P2, MUSCL, Superbee, P2_PDM
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   class (type_gotm_settings), pointer :: branch, twig, leaf, leaf2
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_observations_yaml'

   call settings_store%get(tprof_input, 'temperature', 'temperature profile used for initialization and optionally relaxation', 'Celsius', &
                   extra_options=(/option(ANALYTICAL_OFFSET + TWO_LAYERS, 'two layers with linear gradient in between', 'two_layer'), option(ANALYTICAL_OFFSET + CONST_NN, 'from salinity and buoyancy frequency', 'buoyancy')/), default=0._rk, method_off=NOTHING, method_constant=ANALYTICAL_OFFSET + CONST_PROF, pchild=branch)
   call branch%get(initial_temperature_type, 'type', 'temperature measure', &
                   options=(/option(1, 'In-situ','in-situ'), option(2, 'Potential','potential'), option(3, 'Conservative','conservative')/), default=1)
   twig => branch%get_typed_child('two_layer')
   call twig%get(z_t1, 'z_s', 'depth where upper layer ends', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(t_1, 't_s', 'upper layer temperature', 'Celsius', &
                   minimum=-2._rk,maximum=40._rk,default=0._rk)
   call twig%get(z_t2, 'z_b', 'depth where lower layer begins', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(t_2, 't_b', 'lower layer temperature', 'Celsius', &
                   minimum=-2._rk,maximum=40._rk,default=0._rk)
   call branch%get(t_obs_NN, 'NN', 'square of buoyancy frequency', 's^-2', &
                   minimum=0._rk,default=0._rk)
   twig => branch%get_typed_child('relax', 'relax model temperature to observed/prescribed value')
   call twig%get(TRelaxTauM, 'tau', 'time scale for interior layer', 's', &
                   minimum=0._rk,default=1e+15_rk)
   call twig%get(TRelaxSurf, 'h_s', 'height of surface relaxation layer', 'm', &
                   minimum=0._rk,default=0._rk, display=display_advanced)
   call twig%get(TRelaxTauS, 'tau_s', 'time scale for surface layer', 's', &
                   minimum=0._rk,default=1e+15_rk, display=display_advanced)
   call twig%get(TRelaxBott, 'h_b', 'height of bottom relaxation layer', 'm', &
                   minimum=0._rk,default=0._rk, display=display_advanced)
   call twig%get(TRelaxTauB, 'tau_b', 'time scale for bottom layer', 's', &
                   minimum=0._rk,default=1e+15_rk, display=display_advanced)

   call settings_store%get(sprof_input, 'salinity', 'salinity profile used for initialization and optionally relaxation', 'psu', minimum=0._rk, &
                   extra_options=(/option(ANALYTICAL_OFFSET + TWO_LAYERS, 'two layers with linear gradient in between', 'two_layer'), option(ANALYTICAL_OFFSET + CONST_NN, 'from temperature and buoyancy frequency', 'buoyancy')/), default=0._rk, method_off=NOTHING, method_constant=ANALYTICAL_OFFSET + CONST_PROF, pchild=branch)
   call branch%get(initial_salinity_type, 'type', 'salinity measure', &
                   options=(/option(1, 'practical','practical'), option(2, 'Absolute','absolute')/), default=1)
   twig => branch%get_typed_child('two_layer')
   call twig%get(z_s1, 'z_s', 'depth where upper layer ends', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(s_1, 's_s', 'upper layer salinity', 'psu', &
                   minimum=0._rk,maximum=40._rk,default=0._rk)
   call twig%get(z_s2, 'z_b', 'depth where lower layer begins', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(s_2, 's_b', 'lower layer salinity', 'psu', &
                   minimum=0._rk,maximum=40._rk,default=0._rk)
   call branch%get(s_obs_NN, 'NN', 'square of buoyancy frequency', 's^-2', &
                   minimum=0._rk,default=0._rk)
   twig => branch%get_typed_child('relax', 'relax model salinity to observed/prescribed value')
   call twig%get(SRelaxTauM, 'tau', 'time scale for interior layer', 's', &
                   minimum=0._rk,default=1e+15_rk)
   call twig%get(SRelaxSurf, 'h_s', 'height of surface relaxation layer', 'm', &
                   minimum=0._rk,default=0._rk, display=display_advanced)
   call twig%get(SRelaxTauS, 'tau_s', 'time scale for surface layer', 's', &
                   minimum=0._rk,default=1e+15_rk, display=display_advanced)
   call twig%get(SRelaxBott, 'h_b', 'height of bottom relaxation layer', 'm', &
                   minimum=0._rk,default=0._rk, display=display_advanced)
   call twig%get(SRelaxTauB, 'tau_b', 'time scale for bottom layer', 's', &
                   minimum=0._rk,default=1e+15_rk, display=display_advanced)

   twig => settings_store%get_typed_child('light_extinction')
   call twig%get(extinct_method, 'method', 'water type', &
                   options=(/option(1, 'Jerlov type I', 'Jerlov-I'), option(2, 'Jerlov type 1 (upper 50 m)', 'Jerlov-1-50m'), option(3, 'Jerlov type IA', 'Jerlov-IA'), &
                   option(4, 'Jerlov type IB', 'Jerlov-IB'), option(5, 'Jerlov type II', 'Jerlov-II'), option(6, 'Jerlov type III', 'Jerlov-III'), option(7, 'custom', 'custom')/), default=1)
   call twig%get(A_input, 'A', 'non-visible fraction of shortwave radiation', '1', &
                   minimum=0._rk,maximum=1._rk,default=0.7_rk)
   call twig%get(g1_input, 'g1', 'e-folding depth of non-visible shortwave radiation', 'm', &
                   minimum=0._rk,default=0.4_rk)
   call twig%get(g2_input, 'g2', 'e-folding depth of visible shortwave radiation', 'm', &
                   minimum=0._rk,default=8._rk)

   branch => settings_store%get_typed_child('mimic_3d', 'effects of horizontal gradients')

   twig => branch%get_typed_child('ext_pressure', 'external pressure')
   call twig%get(ext_press_mode, 'type', 'pressure metric', options=(/option(0, 'horizontal gradient in surface elevation', 'elevation'), option(1, 'horizontal velocities at given height above bed', 'velocity'), option(2, 'vertically averaged horizontal velocities', 'average_velocity')/), default=0)

   call twig%get(dpdx_input, 'dpdx', 'pressure in West-East direction', '', &
                   default=0._rk, extra_options=(/option(ANALYTICAL, 'from tidal constituents', 'tidal')/), pchild=leaf)
   leaf2 => leaf%get_typed_child('tidal', 'tidal constituents')
   call leaf2%get(AmpMu, 'amp_1', 'amplitude of 1st harmonic', '', &
                   default=0._rk)
   call leaf2%get(PhaseMu, 'phase_1', 'phase of 1st harmonic', 's', &
                   default=0._rk)
   call leaf2%get(AmpSu, 'amp_2', 'amplitude of 2nd harmonic', '', &
                   default=0._rk)
   call leaf2%get(PhaseSu, 'phase_2', 'phase of 2nd harmonic', 's', &
                   default=0._rk)

   call twig%get(dpdy_input, 'dpdy', 'pressure in South-North direction', '', &
                   default=0._rk, extra_options=(/option(ANALYTICAL, 'from tidal constituents', 'tidal')/), pchild=leaf)
   leaf2 => leaf%get_typed_child('tidal', 'tidal constituents')
   call leaf2%get(AmpMv, 'amp_1', 'amplitude of 1st harmonic', '', &
                   default=0._rk)
   call leaf2%get(PhaseMv, 'phase_1', 'phase of 1st harmonic', 's', &
                   default=0._rk)
   call leaf2%get(AmpSv, 'amp_2', 'amplitude of 2nd harmonic', '', &
                   default=0._rk)
   call leaf2%get(PhaseSv, 'phase_2', 'phase of 2nd harmonic', 's', &
                   default=0._rk)

   call twig%get(h_press_input, 'h', 'height above bed', 'm', &
                   minimum=0._rk,default=0._rk)
   
   call twig%get(PeriodM, 'period_1', 'period of 1st tidal harmonic (eg. M2-tide)', 's', &
                   default=44714._rk)
   call twig%get(PeriodS, 'period_2', 'period of 2nd tidal harmonic (eg. S2-tide)', 's', &
                   default=43200._rk)

   twig => branch%get_typed_child('int_pressure', 'internal pressure')
   call twig%get(int_press_type, 'type', 'method', options=(/option(0, 'None', 'none'), option(1, 'prescribed horiztonal gradients of T and S','gradients'), option(2, 'surface or bottom plume','plume')/), default=0)
   leaf => twig%get_typed_child('gradients', 'horizontal salinity and temperature gradients')
   call leaf%get(dtdx_input, 'dtdx', 'temperature gradient in West-East direction', 'Celsius/m', default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call leaf%get(dtdy_input, 'dtdy', 'temperature gradient in South-North direction', 'Celsius/m', default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call leaf%get(dsdx_input, 'dsdx', 'salinity gradient in West-East direction', 'psu/m', default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call leaf%get(dsdy_input, 'dsdy', 'salinity gradient in South-North direction', 'psu/m', default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)

   leaf => twig%get_typed_child('plume', 'surface or bottom plume')
   call leaf%get(plume_type, 'type', 'plume type', options=(/option(1, 'buoyant surface-attached','surface'), option(2, 'dense bottom-attached','bottom')/), default=2)
   call leaf%get(plume_slope_x, 'x_slope', 'plume slope in West-East direction', '-',   default=0._rk)
   call leaf%get(plume_slope_y, 'y_slope', 'plume slope in South-North direction', '-', default=0._rk)

   call twig%get(t_adv, 't_adv', 'horizontally advect temperature', default=.false.)
   call twig%get(s_adv, 's_adv', 'horizontally advect salinity', default=.false.)

   call branch%get(zeta_input, 'zeta', 'surface elevation', 'm', default=0._rk, extra_options=(/option(ANALYTICAL, 'from tidal constituents', 'tidal')/), pchild=twig)
   leaf => twig%get_typed_child('tidal', 'tidal constituents')
   call leaf%get(period_1, 'period_1', 'period of 1st harmonic (eg. M2-tide)', 's', &
                   default=44714._rk)
   call leaf%get(amp_1, 'amp_1', 'amplitude of 1st harmonic', 'm', &
                   default=0._rk)
   call leaf%get(phase_1, 'phase_1', 'phase of 1st harmonic', 's', &
                   default=0._rk)
   call leaf%get(period_2, 'period_2', 'period of 2nd harmonic (eg. S2-tide)', 's', &
                   default=43200._rk)
   call leaf%get(amp_2, 'amp_2', 'amplitude of 2nd harmonic', 'm', &
                   default=0._rk)
   call leaf%get(phase_2, 'phase_2', 'phase of 2nd harmonic', 's', &
                   default=0._rk)

   twig => settings_store%get_typed_child('velocities', 'observed/prescribed horizontal velocities', display=display_advanced)
   call twig%get(uprof_input, 'u', 'velocity in West-East direction', 'm/s', default=0._rk, &
                   method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(vprof_input, 'v', 'velocity in South-North direction', 'm/s', default=0._rk, &
                   method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   leaf => twig%get_typed_child('relax', 'relax model velocities towards observed/prescribed value')
   call leaf%get(vel_relax_tau, 'tau', 'time scale', 's', &
                   minimum=0._rk,default=1.e15_rk)
   call leaf%get(vel_relax_ramp, 'ramp', 'duration of initial relaxation period', 's', &
                   minimum=0._rk,default=1.e15_rk)

   twig => branch%get_typed_child('w', 'vertical velocity')
   call twig%get(w_adv_input, 'max', 'maximum velocity', 'm/s', &
      default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(w_height_input, 'height', 'height of maximum velocity', 'm', &
      default=0._rk, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(w_adv_discr, 'adv_discr', 'vertical advection scheme', options=&
             (/ option(UPSTREAM, 'first-order upstream', 'upstream'), option(P2, 'third-order upstream-biased polynomial', 'P2'), &
                option(Superbee, 'third-order TVD with Superbee limiter', 'Superbee'), option(MUSCL, 'third-order TVD with MUSCL limiter', 'MUSCL'), &
                option(P2_PDM, 'third-order TVD with ULTIMATE QUICKEST limiter', 'P2_PDM') /), default=P2_PDM)

   twig => settings_store%get_typed_child('waves')
   call twig%get(Hs_input, 'Hs', 'significant wave-height', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(Tz_input, 'Tz', 'mean zero-crossing period', 's', &
                   minimum=0._rk,default=0._rk)
   call twig%get(phiw_input, 'phiw', 'mean direction', '-', &
                   minimum=0._rk,maximum=360._rk,default=0._rk)

   branch => settings_store%get_typed_child('turbulence')

   call branch%get(epsprof_input, 'epsprof', 'observed dissipation rate', 'W/kg', &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE, display=display_advanced, order=999)

   call settings_store%get(o2_prof_input, 'o2', 'oxygen', '', &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE, display=display_hidden)

   return

   end subroutine init_observations_yaml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the observation module
!
! !INTERFACE:
   subroutine post_init_observations(depth,nlev,z,zi,h,gravity)
!
! !DESCRIPTION:
!  The {\tt init\_observations()} subroutine basically reads the {\tt obs.nml}
!  file with a number of different namelists and takes actions according
!  to the specifications in the different namelists.
!  In this routine also memory is allocated to hold the 'observations'.
!  Finally, all variables are initialised to sane values, either by
!  reading from files, by prescribing constant values, or by using analytical
!  expressions.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: depth
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev),zi(0:nlev),h(0:nlev)
   REALTYPE, intent(in)                :: gravity
!
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   integer                   :: rc,i
   REALTYPE                  :: ds,db
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'post_init_observations'

   LEVEL2 'allocation observation memory..'

   allocate(idpdx(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (idpdx)'
   idpdx = _ZERO_

   allocate(idpdy(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (idpdy)'
   idpdy = _ZERO_

   allocate(SRelaxTau(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (SRelaxTau)'
   SRelaxTau = _ZERO_

   allocate(TRelaxTau(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (TRelaxTau)'
   TRelaxTau = _ZERO_

   db=_ZERO_
   ds=depth
   SRelaxTau(0)=SRelaxTauB
   TRelaxTau(0)=TRelaxTauB
   do i=1,nlev
      TRelaxTau(i)=TRelaxTauM
      SRelaxTau(i)=SRelaxTauM
      db=db+0.5*h(i)
      ds=ds-0.5*h(i)
      if (db.le.SRelaxBott) SRelaxTau(i)=SRelaxTauB
      if (ds.le.SRelaxSurf) SRelaxTau(i)=SRelaxTauS
      if (db.le.TRelaxBott) TRelaxTau(i)=TRelaxTauB
      if (ds.le.TRelaxSurf) TRelaxTau(i)=TRelaxTauS
      db=db+0.5*h(i)
      ds=ds-0.5*h(i)
      if ((sprof_input%method.ne.0).and.(SRelaxTau(i).le._ZERO_)) then
         LEVEL2 ''
         LEVEL2 '***************************************************'
         LEVEL2 'SRelaxTau at i=',i,' is not a positive value.'
         LEVEL2 'Please correct obs.nml and rerun.'
         LEVEL2 'Program aborted.'
         LEVEL2 '***************************************************'
         stop 'init_observations'
      end if
      if ((tprof_input%method.ne.0).and.(TRelaxTau(i).le._ZERO_)) then
         LEVEL2 ''
         LEVEL2 '***************************************************'
         LEVEL2 'TRelaxTau at i=',i,' is not a positive value.'
         LEVEL2 'Please correct your configuration and rerun.'
         LEVEL2 'Program aborted.'
         LEVEL2 '***************************************************'
         stop 'init_observations'
      end if
   end do

!  The salinity profile
   call register_input(sprof_input)
   sprof_input%data=sprof_input%constant_value
   select case (sprof_input%method)
!KB      case (ANALYTICAL_OFFSET + CONST_PROF)
!KB          sprof_input%data=sprof_input%constant_value
      case (ANALYTICAL_OFFSET + TWO_LAYERS)
         call analytical_profile(nlev,z,z_s1,s_1,z_s2,s_2,sprof_input%data)
      case (ANALYTICAL_OFFSET + CONST_NN)

         if (.not. (tprof_input%method .eq. ANALYTICAL_OFFSET + CONST_PROF))  then
            LEVEL2 ''
            LEVEL2 '***************************************************'
            LEVEL2 'For salinity profiles with NN=const. you have to   '
            LEVEL2 'prescribe constant temperature.                    '
            LEVEL2 'Please correct your configuration and re-run.      '
            LEVEL2 'Program aborted.                                   '
            LEVEL2 '***************************************************'
            stop 'init_observations'
         endif

         call const_NNS(nlev,z,z,s_1,t_1,s_obs_NN,gravity,sprof_input%data)
   end select

!  The temperature profile
   call register_input(tprof_input)
   tprof_input%data=tprof_input%constant_value
   select case (tprof_input%method)
!KB      case (ANALYTICAL_OFFSET + CONST_PROF)
!KB          tprof_input%data=tprof_input%constant_value
      case (ANALYTICAL_OFFSET + TWO_LAYERS)
         call analytical_profile(nlev,z,z_t1,t_1,z_t2,t_2,tprof_input%data)
      case (ANALYTICAL_OFFSET + CONST_NN)

         if (.not. (sprof_input%method .eq. ANALYTICAL_OFFSET + CONST_PROF))  then

            LEVEL2 ''
            LEVEL2 '***************************************************'
            LEVEL2 'For temperature profiles with NN=const you have to '
            LEVEL2 'prescribe constant salinity.                       '
            LEVEL2 'Please correct your configuration and re-run.      '
            LEVEL2 'Program aborted.                                   '
            LEVEL2 '***************************************************'
            stop 'init_observations'
         endif

         call const_NNT(nlev,z,z,t_1,s_1,t_obs_NN,gravity,tprof_input%data)
   end select

!  The external pressure
   call register_input(h_press_input)
   call register_input(dpdx_input)
   call register_input(dpdy_input)

!  The internal pressure
   call register_input(dsdx_input)
   call register_input(dsdy_input)
   call register_input(dtdx_input)
   call register_input(dtdy_input)

!  The light extinction profiles
   select case (extinct_method)
      case (1)
         A_input%value=0.58;g1_input%value=0.35;g2_input%value=23.0
      case (2)
         A_input%value=0.68;g1_input%value=1.20;g2_input%value=28.0
      case (3)
         A_input%value=0.62;g1_input%value=0.60;g2_input%value=20.0
      case (4)
         A_input%value=0.67;g1_input%value=1.00;g2_input%value=17.0
      case (5)
         A_input%value=0.77;g1_input%value=1.50;g2_input%value=14.0
      case (6)
         A_input%value=0.78;g1_input%value=1.40;g2_input%value=7.9
      case default
         call register_input(A_input)
         call register_input(g1_input)
         call register_input(g2_input)
   end select

!  The vertical advection velocity
   call register_input(w_height_input)
   call register_input(w_adv_input)

!  The sea surface elevation
   call register_input(zeta_input)

!  Wind waves
   call register_input(Hs_input)
   call register_input(Tz_input)
   call register_input(phiw_input)

!  The observed velocity profile
   call register_input(uprof_input)
   call register_input(vprof_input)

!  The observed dissipation profile
   call register_input(epsprof_input)

!  The oxygen profile
   call register_input(o2_prof_input)
   LEVEL2 'done.'
   return

   end subroutine post_init_observations
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_all_obs
!
! !INTERFACE:
   subroutine get_all_obs(julday,secs,nlev,z)
!
! !DESCRIPTION:
!  During the time integration this subroutine is called each time step
!  to update all 'observation'. The routine is basically a wrapper
!  routine which calls the variable specific routines.
!  The only input to this routine is the time (in internal GOTM
!  representation) and the vertical grid. It is up to each of the individual
!  routines to use this information and to provide updated 'observations'.
!
! !USES:
   use time, only: fsecs

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: julday,secs
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(:)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (dpdx_input%method==ANALYTICAL) then
!     Analytical prescription of tides
      dpdx_input%value = AmpMu*sin(2*pi*(fsecs-PhaseMu)/PeriodM)    &
             + AmpSu*sin(2*pi*(fsecs-PhaseSu)/PeriodS)    &
             + dpdx_input%constant_value
   end if

   if (dpdy_input%method==ANALYTICAL) then
!     Analytical prescription of tides
      dpdy_input%value = AmpMv*sin(2*pi*(fsecs-PhaseMv)/PeriodM)    &
             + AmpSv*sin(2*pi*(fsecs-PhaseSv)/PeriodS)    &
             + dpdy_input%constant_value
   end if

   if (zeta_input%method==ANALYTICAL) then
!     Analytical prescription of tides
      zeta_input%value = amp_1*sin(2*pi*(fsecs-phase_1)/period_1) &
            +amp_2*sin(2*pi*(fsecs-phase_2)/period_2) &
            +zeta_input%constant_value
   end if

   end subroutine get_all_obs
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Clean up the observation module
!
! !INTERFACE:
   subroutine clean_observations()
!
! !DESCRIPTION:
!  De-allocates memory allocated in init\_observations().
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
   LEVEL1 'clean_observations'

   LEVEL2 'de-allocate observation memory ...'
   if (allocated(idpdx)) deallocate(idpdx)
   if (allocated(idpdy)) deallocate(idpdy)
   if (allocated(SRelaxTau)) deallocate(SRelaxTau)
   if (allocated(TRelaxTau)) deallocate(TRelaxTau)
   LEVEL2 'done.'

   end subroutine clean_observations
!EOC

#ifdef _PRINTSTATE_
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Print the current state of the observations module.
!
! !INTERFACE:
   subroutine print_state_observations()
!
! !DESCRIPTION:
!  This routine writes the value of all module-level variables to screen.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'State of observations module:'
   if (allocated(sprof  )) LEVEL2 'sprof',sprof
   if (allocated(tprof  )) LEVEL2 'tprof',tprof
   if (allocated(o2_prof)) LEVEL2 'o2_prof',o2_prof
   if (allocated(dsdx   )) LEVEL2 'dsdx',dsdx
   if (allocated(dsdy   )) LEVEL2 'dsdy',dsdy
   if (allocated(dtdx   )) LEVEL2 'dtdx',dtdx
   if (allocated(dtdy   )) LEVEL2 'dtdy',dtdy
   if (allocated(idpdx  )) LEVEL2 'idpdx',idpdx
   if (allocated(idpdy  )) LEVEL2 'idpdy',idpdy
   if (allocated(uprof  )) LEVEL2 'uprof',uprof
   if (allocated(vprof  )) LEVEL2 'vprof',vprof
   if (allocated(epsprof)) LEVEL2 'epsprof',epsprof
   if (allocated(SRelaxTau)) LEVEL2 'SRelaxTau',SRelaxTau
   if (allocated(TRelaxTau)) LEVEL2 'TRelaxTau',TRelaxTau
   LEVEL2 'zeta,dpdx,dpdy,h_press',zeta,dpdx,dpdy,h_press
   LEVEL2 'w_adv,w_height',w_adv,w_height
   LEVEL2 'A,g1,g2',A,g1,g2

   LEVEL2 'salinity namelist',                                  &
            s_prof_method,s_analyt_method,                      &
            s_prof_file,z_s1,s_1,z_s2,s_2,s_obs_NN,             &
            SRelaxTauM,SRelaxTauS,SRelaxTauB,                   &
            SRelaxSurf,SRelaxBott

   LEVEL2 'temperature namelist',                               &
            t_prof_method,t_analyt_method,                      &
            t_prof_file,z_t1,t_1,z_t2,t_2,t_obs_NN,             &
            TRelaxTauM,TRelaxTauS,TRelaxTauB,                   &
            TRelaxSurf,TRelaxBott

   LEVEL2 'oxygen namelist',                                    &
            o2_prof_method,o2_units,o2_prof_file

   LEVEL2 'external pressure namelist',                         &
            ext_press_method,ext_press_mode,ext_press_file,     &
            PressConstU,PressConstV,PressHeight,                &
            PeriodM,AmpMu,AmpMv,PhaseMu,PhaseMv,                &
            PeriodS,AmpSu,AmpSv,PhaseSu,PhaseSv

   LEVEL2 'internal_pressure namelist',                         &
            int_press_method,int_press_type,int_press_file,     &
            const_dsdx,const_dsdy,const_dtdx,const_dtdy,        &
            s_adv,t_adv,plume_slope_x,plume_slope_y

   LEVEL2 'extinct namelist',extinct_method,extinct_file

   LEVEL2 'w_advspec namelist',                                 &
            w_adv_method,w_adv0,w_adv_height0,w_adv_file,w_adv_discr

   LEVEL2 'zetaspec namelist',                                  &
            zeta_method,zeta_file,zeta_0,                       &
            period_1,amp_1,phase_1,period_2,amp_2,phase_2

   LEVEL2 'wave_nml namelist',                                  &
            wave_method,wave_file,Hs,Tz,phiw

   LEVEL2 'observed velocity profiles namelist',                &
            vel_prof_method,vel_prof_file,                      &
            vel_relax_tau,vel_relax_ramp

   LEVEL2 'observed dissipation profiles namelist',             &
            e_prof_method,e_obs_const,e_prof_file

   LEVEL2 'bprofile namelist',b_obs_surf,b_obs_NN,b_obs_sbf

   end subroutine print_state_observations
!EOC
#endif

!-----------------------------------------------------------------------

   end module observations

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

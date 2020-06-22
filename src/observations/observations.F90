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
   type (type_profile_input), public, target :: sprof

!  'observed' temperature profile
   type (type_profile_input), public, target :: tprof

!  'observed' oxygen profile
   type (type_profile_input), public, target :: o2_prof

!  'observed' horizontal salinity gradients
   type (type_profile_input), public, target :: dsdx,dsdy

!  'observed' horizontal temperature gradients
   type (type_profile_input), public, target :: dtdx,dtdy

!  internal horizontal pressure gradients
   REALTYPE, public, dimension(:), allocatable :: idpdx,idpdy

!  horizontal velocity profiles
   type (type_profile_input), public, target :: uprof,vprof

!  observed profile of turbulent dissipation rates
   type (type_profile_input), public, target :: epsprof

!  relaxation times for salinity and temperature
   REALTYPE, public, dimension(:), allocatable, target :: SRelaxTau
   REALTYPE, public, dimension(:), allocatable         :: TRelaxTau

!  sea surface elevation, sea surface gradients and height of velocity obs.
   type (type_scalar_input), public, target :: zeta,dpdx,dpdy,h_press

!  vertical advection velocity
   type (type_scalar_input), public, target :: w_adv,w_height

!  Parameters for water classification - default Jerlov type I
   type (type_scalar_input), public, target :: A_, g1_, g2_

!------------------------------------------------------------------------------
!
! the following data are not all public,
! but have been included for clarity here
!
!------------------------------------------------------------------------------

!  Salinity profile(s)
   integer, public           :: s_analyt_method
   REALTYPE                  :: z_s1,s_1,z_s2,s_2
   REALTYPE                  :: s_obs_NN
   REALTYPE                  :: SRelaxTauM
   REALTYPE                  :: SRelaxTauS
   REALTYPE                  :: SRelaxTauB
   REALTYPE                  :: SRelaxSurf
   REALTYPE                  :: SRelaxBott

!  Temperature profile(s)
   integer, public           :: t_analyt_method
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
   logical, public           :: s_adv
   logical, public           :: t_adv

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
   type (type_scalar_input), public :: Hs_
   type (type_scalar_input), public :: Tz_
   type (type_scalar_input), public :: phiw_

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
   integer                   :: t_prof_method
   character(LEN=PATH_MAX)   :: t_prof_file

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
            int_press_method,int_press_file,                    &
            const_dsdx,const_dsdy,const_dtdx,const_dtdy,        &
            s_adv,t_adv

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

   open(10,file='obs.nml',status='old',action='read',err=80)
   read(10,nml=sprofile,err=81)
   read(10,nml=tprofile,err=82)
   read(10,nml=ext_pressure,err=83)
   read(10,nml=int_pressure,err=84)
   read(10,nml=extinct,err=85)
   read(10,nml=w_advspec,err=86)
   read(10,nml=zetaspec,err=87)
   read(10,nml=wave_nml,err=88)
   read(10,nml=velprofile,err=89)
   read(10,nml=eprofile,err=90)
   read(10,nml=bprofile,err=91)
   read(10,nml=o2_profile,err=92)
   close(10)

   call sprof%configure(method=s_prof_method, path=s_prof_file, index=1)
   call tprof%configure(method=t_prof_method, path=t_prof_file, index=1)

   call h_press%configure(method=ext_press_method, path=ext_press_file, index=1, constant_value=PressHeight)
   call dpdx%configure(method=ext_press_method, path=ext_press_file, index=2, constant_value=PressConstU)
   call dpdy%configure(method=ext_press_method, path=ext_press_file, index=3, constant_value=PressConstV)

   call dsdx%configure(method=int_press_method, path=int_press_file, index=1, constant_value=const_dsdx)
   call dsdy%configure(method=int_press_method, path=int_press_file, index=2, constant_value=const_dsdy)
   call dtdx%configure(method=int_press_method, path=int_press_file, index=3, constant_value=const_dtdx)
   call dtdy%configure(method=int_press_method, path=int_press_file, index=4, constant_value=const_dtdy)

   if (extinct_method == 0) then
      extinct_method = 7
      call A_%configure(method=2, path=extinct_file, index=1, constant_value=A)
      call g1_%configure(method=2, path=extinct_file, index=2, constant_value=g1)
      call g2_%configure(method=2, path=extinct_file, index=3, constant_value=g2)
   else
      call A_%configure(method=0, path=extinct_file, index=1, constant_value=A)
      call g1_%configure(method=0, path=extinct_file, index=2, constant_value=g1)
      call g2_%configure(method=0, path=extinct_file, index=3, constant_value=g2)
   end if

   call w_adv%configure(method=w_adv_method, path=w_adv_file, index=1, constant_value=w_adv0)
   call w_height%configure(method=max(1, w_adv_method), path=w_adv_file, index=2, constant_value=w_adv_height0)

   call zeta%configure(method=zeta_method, path=zeta_file, index=1, constant_value=zeta_0, scale_factor=zeta_scale, add_offset=zeta_offset)

   call Hs_%configure(method=wave_method, path=wave_file, index=1, constant_value=Hs)
   call Tz_%configure(method=wave_method, path=wave_file, index=2, constant_value=Tz)
   call phiw_%configure(method=wave_method, path=wave_file, index=3, constant_value=phiw)

   call uprof%configure(method=vel_prof_method, path=vel_prof_file, index=1)
   call vprof%configure(method=vel_prof_method, path=vel_prof_file, index=2)

   call epsprof%configure(method=e_prof_method, path=e_prof_file, index=1, constant_value=e_obs_const)

   call o2_prof%configure(method=o2_prof_method, path=o2_prof_file, index=1)
   select case (o2_units)
   case (1) ! mg/l
      o2_prof%scale_factor=mmol_o2_per_gram
   case (2) ! ml/l
      o2_prof%scale_factor=mmol_o2_per_liter
   end select

   LEVEL2 'done.'

   return

80 FATAL 'Unable to open "',trim('obs.nml'),'" for reading'
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
   class (type_gotm_settings), pointer :: branch, twig, leaf
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_observations_yaml'

   call settings_store%get(tprof, 'temperature', 'temperature profile used for initialization and optionally relaxation', 'Celsius', &
                   extra_options=(/option(ANALYTICAL, 'analytical')/), method_off=NOTHING, method_constant=method_unsupported, pchild=branch)
   twig => branch%get_typed_child('analytical')
   call twig%get(t_analyt_method, 'method', 'type of analytical initial temperature profile', &
                   options=(/option(CONST_PROF, 'constant'), option(TWO_LAYERS, 'two layers'), option(CONST_NN, 'from salinity and buoyancy frequency')/),default=CONST_PROF)
   call twig%get(z_t1, 'z_t1', 'upper layer thickness', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(t_1, 't_1', 'upper layer temperature', 'Celsius', &
                   minimum=0._rk,maximum=40._rk,default=0._rk)
   call twig%get(z_t2, 'z_t2', 'lower layer thickness', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(t_2, 't_2', 'lower layer temperature', 'Celsius', &
                   minimum=0._rk,maximum=40._rk,default=0._rk)
   call twig%get(t_obs_NN, 'obs_NN', 'constant buoyancy frequency', 's^-2', &
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

   call settings_store%get(sprof, 'salinity', 'salinity profile used for initialization and optionally relaxation', 'psu', &
                   extra_options=(/option(ANALYTICAL, 'analytical')/), method_off=NOTHING, method_constant=method_unsupported, pchild=branch)
   twig => branch%get_typed_child('analytical')
   call twig%get(s_analyt_method, 'method', 'type of analytical initial salinity profile', &
                   options=(/option(CONST_PROF, 'constant'), option(TWO_LAYERS, 'two layers'), option(CONST_NN, 'from temperature and buoyancy frequency')/),default=CONST_PROF)
   call twig%get(z_s1, 'z_s1', 'upper layer thickness', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(s_1, 's_1', 'upper layer salinity', 'psu', &
                   minimum=0._rk,maximum=40._rk,default=0._rk)
   call twig%get(z_s2, 'z_s2', 'lower layer thickness', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(s_2, 's_2', 'lower layer salinity', 'psu', &
                   minimum=0._rk,maximum=40._rk,default=0._rk)
   call twig%get(s_obs_NN, 'obs_NN', 'constant buoyancy frequency', 's^-2', &
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
                   options=(/option(1, 'Jerlov type I'), option(2, 'Jerlov type 1 (upper 50 m)'), option(3, 'Jerlov type IA'), &
                   option(4, 'Jerlov type IB'), option(5, 'Jerlov type II'), option(6, 'Jerlov type III'), option(7, 'custom')/), default=1)
   call twig%get(A_, 'A', 'non-visible fraction of shortwave radiation', '1', &
                   minimum=0._rk,maximum=1._rk,default=0.7_rk)
   call twig%get(g1_, 'g1', 'e-folding depth of non-visible shortwave radiation', 'm', &
                   minimum=0._rk,default=0.4_rk)
   call twig%get(g2_, 'g2', 'e-folding depth of visible shortwave radiation', 'm', &
                   minimum=0._rk,default=8._rk)

   branch => settings_store%get_typed_child('mimic_3d', 'effects of horizontal gradients')

   twig => branch%get_typed_child('ext_pressure', 'external pressure')
   call twig%get(ext_press_mode, 'mode', 'formulation', options=(/option(0, 'horizontal gradient in surface elevation'), option(1, 'horizontal velocities at given height above bed'), option(2, 'vertically averaged horizontal velocities')/), default=0)

   call twig%get(dpdx, 'dpdx', 'pressure in West-East direction', '', &
                   default=0._rk, extra_options=(/option(ANALYTICAL, 'from tidal constituents')/), pchild=leaf)
   call leaf%get(AmpMu, 'AmpM', 'amplitude of 1st harmonic', '-', &
                   default=0._rk)
   call leaf%get(PhaseMu, 'PhaseM', 'phase of 1st harmonic', 's', &
                   default=0._rk)
   call leaf%get(AmpSu, 'AmpS', 'amplitude of 2nd harmonic', '-', &
                   default=0._rk)
   call leaf%get(PhaseSu, 'PhaseS', 'phase of 2nd harmonic', 's', &
                   default=0._rk)

   call twig%get(dpdy, 'dpdy', 'pressure in South-North direction', '', &
                   default=0._rk, extra_options=(/option(ANALYTICAL, 'from tidal constituents')/), pchild=leaf)
   call leaf%get(AmpMv, 'AmpM', 'amplitude of 1st harmonic', '-', &
                   default=0._rk)
   call leaf%get(PhaseMv, 'PhaseM', 'phase of 1st harmonic', 's', &
                   default=0._rk)
   call leaf%get(AmpSv, 'AmpS', 'amplitude of 2nd harmonic', '-', &
                   default=0._rk)
   call leaf%get(PhaseSv, 'PhaseS', 'phase of 2nd harmonic', 's', &
                   default=0._rk)

   call twig%get(h_press, 'h', 'height above bed', 'm', &
                   minimum=0._rk,default=0._rk)
   
   call twig%get(PeriodM, 'PeriodM', 'period of 1st tidal harmonic (eg. M2-tide)', 's', &
                   default=44714._rk)
   call twig%get(PeriodS, 'PeriodS', 'period of 2nd tidal harmonic (eg. S2-tide)', 's', &
                   default=43200._rk)

   twig => branch%get_typed_child('int_press', 'internal pressure')
   call twig%get(dtdx, 'dtdx', 'temperature gradient in West-East direction', 'Celsius/m', &
      default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(dtdy, 'dtdy', 'temperature gradient in South-North direction', 'Celsius/m', &
      default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(dsdx, 'dsdx', 'salinity gradient in West-East direction', 'psu/m', &
      default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(dsdy, 'dsdy', 'salinity gradient in South-North direction', 'psu/m', &
      default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(t_adv, 't_adv', 'horizontally advect temperature', default=.false.)
   call twig%get(s_adv, 's_adv', 'horizontally advect salinity', default=.false.)

   call branch%get(zeta, 'zeta', 'surface elevation', 'm', default=0._rk, extra_options=(/option(ANALYTICAL, 'from tidal constituents')/), pchild=twig)
   call twig%get(period_1, 'period_1', 'period of 1st harmonic (eg. M2-tide)', 's', &
                   default=44714._rk)
   call twig%get(amp_1, 'amp_1', 'amplitude of 1st harmonic', 'm', &
                   default=0._rk)
   call twig%get(phase_1, 'phase_1', 'phase of 1st harmonic', 's', &
                   default=0._rk)
   call twig%get(period_2, 'period_2', 'period of 2nd harmonic (eg. S2-tide)', 's', &
                   default=43200._rk)
   call twig%get(amp_2, 'amp_2', 'amplitude of 2nd harmonic', 'm', &
                   default=0._rk)
   call twig%get(phase_2, 'phase_2', 'phase of 2nd harmonic', 's', &
                   default=0._rk)

   twig => settings_store%get_typed_child('velocities', 'observed/prescribed horizontal velocities')
   call twig%get(uprof, 'u', 'velocity in West-East direction', 'm/s', default=0._rk, &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE)   
   call twig%get(vprof, 'v', 'velocity in South-North direction', 'm/s', default=0._rk, &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE)   
   leaf => twig%get_typed_child('relax', 'relax model velocities towards observed/prescribed value')
   call leaf%get(vel_relax_tau, 'tau', 'time scale', 's', &
                   minimum=0._rk,default=1.e15_rk)
   call leaf%get(vel_relax_ramp, 'ramp', 'duration of initial relaxation period', 's', &
                   minimum=0._rk,default=1.e15_rk)

   twig => branch%get_typed_child('w', 'vertical velocity')
   call twig%get(w_adv, 'max', 'maximum velocity', 'm/s', &
      default=0._rk, method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(w_height, 'height', 'height of maximum velocity', 'm', &
      default=0._rk, method_constant=CONSTANT, method_file=FROMFILE)
   call twig%get(w_adv_discr, 'adv_discr', 'vertical advection scheme', options=&
             (/ option(UPSTREAM, 'first-order upstream'), option(P2, 'third-order upstream-biased polynomial'), &
                option(Superbee, 'third-order TVD with Superbee limiter'), option(MUSCL, 'third-order TVD with MUSCL limiter'), &
                option(P2_PDM, 'third-order TVD with ULTIMATE QUICKEST limiter') /), default=P2_PDM)

   twig => settings_store%get_typed_child('surface/wave', 'wind waves', display=display_advanced)
   call twig%get(Hs_, 'Hs', 'significant wave-height', 'm', &
                   minimum=0._rk,default=0._rk)
   call twig%get(Tz_, 'Tz', 'mean zero-crossing period', 's', &
                   minimum=0._rk,default=0._rk)
   call twig%get(phiw_, 'phiw', 'mean direction', '-', &
                   minimum=0._rk,maximum=360._rk,default=0._rk)

   branch => settings_store%get_typed_child('turbulence')

   call branch%get(epsprof, 'epsprof', 'observed dissipation rate', 'W/kg', &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE, display=display_advanced, order=999)

   call settings_store%get(o2_prof, 'o2', 'oxygen', '', &
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
!KB   subroutine post_init_observations(julday,secs,depth,nlev,z,h,gravity,rho_0)
   subroutine post_init_observations(depth,nlev,z,h,gravity,rho_0)
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
!KB   integer, intent(in)                 :: julday,secs
   REALTYPE, intent(in)                :: depth
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev),h(0:nlev)
   REALTYPE, intent(in)                :: gravity,rho_0
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
      if ((sprof%method.ne.0).and.(SRelaxTau(i).le._ZERO_)) then
         LEVEL2 ''
         LEVEL2 '***************************************************'
         LEVEL2 'SRelaxTau at i=',i,' is not a positive value.'
         LEVEL2 'Please correct obs.nml and rerun.'
         LEVEL2 'Program aborted.'
         LEVEL2 '***************************************************'
         stop 'init_observations'
      end if
      if ((tprof%method.ne.0).and.(TRelaxTau(i).le._ZERO_)) then
         LEVEL2 ''
         LEVEL2 '***************************************************'
         LEVEL2 'TRelaxTau at i=',i,' is not a positive value.'
         LEVEL2 'Please correct obs.nml and rerun.'
         LEVEL2 'Program aborted.'
         LEVEL2 '***************************************************'
         stop 'init_observations'
      end if
   end do

!  The salinity profile
   call register_input(sprof)
   select case (sprof%method)
      case (ANALYTICAL)
         ! different ways to prescribe profiles analytically
         select case (s_analyt_method)
            case (CONST_PROF)
               sprof%data = s_1
            case (TWO_LAYERS)
               call analytical_profile(nlev,z,z_s1,s_1,z_s2,s_2,sprof%data)
            case (CONST_NN)

               if (.not.((tprof%method    .eq. ANALYTICAL) .and.      &
                         (t_analyt_method .eq. CONST_PROF))   )  then
                  LEVEL2 ''
                  LEVEL2 '***************************************************'
                  LEVEL2 'For salinity profiles with NN=const. you have to   '
                  LEVEL2 'prescribe constant temperature.                    '
                  LEVEL2 'Please correct obs.nml and re-run.                 '
                  LEVEL2 'Program aborted.                                   '
                  LEVEL2 '***************************************************'
                  stop 'init_observations'
               endif

               call const_NNS(nlev,z,s_1,t_1,s_obs_NN,gravity,rho_0,sprof%data)
            case default
               LEVEL1 'A non-valid s_analyt_method has been given ',s_analyt_method
               stop 'init_observations()'
         end select
   end select

!  The temperature profile
   call register_input(tprof)
   select case (tprof%method)
      case (ANALYTICAL)

        ! different ways to prescribe profiles analytically
         select case (t_analyt_method)
         case (CONST_PROF)
               tprof%data = t_1
            case (TWO_LAYERS)
               call analytical_profile(nlev,z,z_t1,t_1,z_t2,t_2,tprof%data)
            case (CONST_NN)

               if (.not.((sprof%method    .eq. ANALYTICAL) .and.      &
                         (s_analyt_method .eq. CONST_PROF))   )  then

                  LEVEL2 ''
                  LEVEL2 '***************************************************'
                  LEVEL2 'For temperature profiles with NN=const you have to '
                  LEVEL2 'prescribe constant salinity.                       '
                  LEVEL2 'Please correct obs.nml and re-run.                 '
                  LEVEL2 'Program aborted.                                   '
                  LEVEL2 '***************************************************'
                  stop 'init_observations'
               endif

               call const_NNT(nlev,z,t_1,s_1,t_obs_NN,gravity,rho_0,tprof%data)
            case default
               LEVEL1 'A non-valid t_analyt_method has been given ',t_analyt_method
               stop 'init_observations()'
         end select
   end select

!  The external pressure
   call register_input(h_press)
   call register_input(dpdx)
   call register_input(dpdy)

!  The internal pressure
   call register_input(dsdx)
   call register_input(dsdy)
   call register_input(dtdx)
   call register_input(dtdy)

!  The light extinction profiles
   select case (extinct_method)
      case (1)
         A_%value=0.58;g1_%value=0.35;g2_%value=23.0
      case (2)
         A_%value=0.68;g1_%value=1.20;g2_%value=28.0
      case (3)
         A_%value=0.62;g1_%value=0.60;g2_%value=20.0
      case (4)
         A_%value=0.67;g1_%value=1.00;g2_%value=17.0
      case (5)
         A_%value=0.77;g1_%value=1.50;g2_%value=14.0
      case (6)
         A_%value=0.78;g1_%value=1.40;g2_%value=7.9
      case default
         call register_input(A_)
         call register_input(g1_)
         call register_input(g2_)
   end select

!  The vertical advection velocity
   call register_input(w_height)
   call register_input(w_adv)

!  The sea surface elevation
   call register_input(zeta)

!  Wind waves
   call register_input(Hs_)
   call register_input(Tz_)
   call register_input(phiw_)

!  The observed velocity profile
   call register_input(uprof)
   call register_input(vprof)

!  The observed dissipation profile
   call register_input(epsprof)

!  The oxygen profile
   call register_input(o2_prof)
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
   if (dpdx%method==ANALYTICAL) then
!     Analytical prescription of tides
      dpdx%value = AmpMu*sin(2*pi*(fsecs-PhaseMu)/PeriodM)    &
             + AmpSu*sin(2*pi*(fsecs-PhaseSu)/PeriodS)    &
             + dpdx%constant_value
   end if

   if (dpdy%method==ANALYTICAL) then
!     Analytical prescription of tides
      dpdy%value = AmpMv*sin(2*pi*(fsecs-PhaseMv)/PeriodM)    &
             + AmpSv*sin(2*pi*(fsecs-PhaseSv)/PeriodS)    &
             + dpdy%constant_value
   end if

   if (zeta%method==ANALYTICAL) then
!     Analytical prescription of tides
      Zeta%value = amp_1*sin(2*pi*(fsecs-phase_1)/period_1) &
            +amp_2*sin(2*pi*(fsecs-phase_2)/period_2) &
            +zeta%constant_value
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
            int_press_method,int_press_file,                    &
            const_dsdx,const_dsdy,const_dtdx,const_dtdy,        &
            s_adv,t_adv

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

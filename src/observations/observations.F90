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
   public init_observations, get_all_obs, clean_observations
#ifdef _PRINTSTATE_
   public print_state_observations
#endif
!
! !PUBLIC DATA MEMBERS:
!
!  'observed' salinity profile
   REALTYPE, public, dimension(:), allocatable, target :: sprof

!  'observed' temperature profile
   REALTYPE, public, dimension(:), allocatable, target :: tprof

!  'observed' oxygen profile
   REALTYPE, public, dimension(:), allocatable, target :: o2_prof

!  'observed' horizontal salinity  gradients
   REALTYPE, public, dimension(:), allocatable, target :: dsdx,dsdy

!  'observed' horizontal temperarure  gradients
   REALTYPE, public, dimension(:), allocatable, target :: dtdx,dtdy

!  internal horizontal pressure gradients
   REALTYPE, public, dimension(:), allocatable         :: idpdx,idpdy

!  horizontal velocity profiles
   REALTYPE, public, dimension(:), allocatable, target :: uprof,vprof

!  observed profile of turbulent dissipation rates
   REALTYPE, public, dimension(:), allocatable, target :: epsprof

!  ralaxation times for salinity and temperature
   REALTYPE, public, dimension(:), allocatable, target :: SRelaxTau
   REALTYPE, public, dimension(:), allocatable         :: TRelaxTau

!  sea surface elevation, sea surface gradients and height of velocity obs.
   REALTYPE, public, target                            :: zeta,dpdx,dpdy,h_press

!  vertical advection velocity
   REALTYPE, public, target                            :: w_adv,w_height

!  Parameters for water classification - default Jerlov type I
   REALTYPE, public, target                            :: A,g1,g2

!------------------------------------------------------------------------------
!
! the following data are not all public,
! but have been included for clarity here
!
!------------------------------------------------------------------------------

!  Salinity profile(s)
   integer, public           :: s_prof_method
   integer, public           :: s_analyt_method
   character(LEN=PATH_MAX)   :: s_prof_file
   REALTYPE                  :: z_s1,s_1,z_s2,s_2
   REALTYPE                  :: s_obs_NN
   REALTYPE                  :: SRelaxTauM
   REALTYPE                  :: SRelaxTauS
   REALTYPE                  :: SRelaxTauB
   REALTYPE                  :: SRelaxSurf
   REALTYPE                  :: SRelaxBott

!  Temperature profile(s)
   integer, public           :: t_prof_method
   integer, public           :: t_analyt_method
   character(LEN=PATH_MAX)   :: t_prof_file
   REALTYPE                  :: z_t1,t_1,z_t2,t_2
   REALTYPE                  :: t_obs_NN
   REALTYPE                  :: TRelaxTauM
   REALTYPE                  :: TRelaxTauS
   REALTYPE                  :: TRelaxTauB
   REALTYPE                  :: TRelaxSurf
   REALTYPE                  :: TRelaxBott

!  Oxygen profile(s)
   integer, public           :: o2_prof_method
   integer, public           :: o2_units
   character(LEN=PATH_MAX)   :: o2_prof_file

!  External pressure - 'press' namelist
   integer, public           :: ext_press_method,ext_press_mode
   character(LEN=PATH_MAX)   :: ext_press_file
   REALTYPE, public          :: PressConstU
   REALTYPE, public          :: PressConstV
   REALTYPE, public          :: PressHeight
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
   integer, public           :: int_press_method
   character(LEN=PATH_MAX)   :: int_press_file
   REALTYPE, public          :: const_dsdx
   REALTYPE, public          :: const_dsdy
   REALTYPE, public          :: const_dtdx
   REALTYPE, public          :: const_dtdy
   logical, public           :: s_adv
   logical, public           :: t_adv

!  Light extinction - the 'extinct' namelist
   integer                   :: extinct_method
   character(LEN=PATH_MAX)   :: extinct_file

!  Vertical advection velocity - 'w_advspec' namelist
   integer, public           :: w_adv_method
   REALTYPE, public          :: w_adv0
   REALTYPE, public          :: w_adv_height0
   character(LEN=PATH_MAX)   :: w_adv_file
   integer, public           :: w_adv_discr

!  Sea surface elevations - 'zetaspec' namelist
   integer,public            :: zeta_method
   character(LEN=PATH_MAX)   :: zeta_file
   REALTYPE, public          :: zeta_0
   REALTYPE, public          :: period_1
   REALTYPE, public          :: amp_1
   REALTYPE, public          :: phase_1
   REALTYPE, public          :: period_2
   REALTYPE, public          :: amp_2
   REALTYPE, public          :: phase_2

!  Wind waves - 'wave_nml' namelist
   integer,public            :: wave_method
   character(LEN=PATH_MAX)   :: wave_file
   REALTYPE, public          :: Hs
   REALTYPE, public          :: Tz
   REALTYPE, public          :: phiw

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
   subroutine init_observations(namlst,fn,julday,secs,                 &
                                depth,nlev,z,h,gravity,rho_0)
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
   integer, intent(in)                 :: julday,secs
   REALTYPE, intent(in)                :: depth
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev),h(0:nlev)
   REALTYPE, intent(in)                :: gravity,rho_0
!
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
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

   namelist /extinct/ extinct_method,extinct_file

   namelist /w_advspec/                                         &
            w_adv_method,w_adv_file,w_adv_height0,w_adv0,w_adv_discr

   namelist /zetaspec/                                          &
            zeta_method,zeta_file,zeta_0,                       &
            period_1,amp_1,phase_1,period_2,amp_2,phase_2

   namelist /wave_nml/                                          &
            wave_method,wave_file,Hs,Tz,phiw

   namelist /velprofile/ vel_prof_method,vel_prof_file,         &
            vel_relax_tau,vel_relax_ramp

   namelist /eprofile/ e_prof_method,e_obs_const,e_prof_file

   namelist /bprofile/ b_obs_surf,b_obs_NN,b_obs_sbf

   integer                   :: rc,i
   REALTYPE                  :: ds,db

   REALTYPE,parameter        :: mmol_o2_per_liter = 44.661
   REALTYPE,parameter        :: mmol_o2_per_gram  = 31.25
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_observations'

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
   PeriodM=44714.
   AmpMu=_ZERO_
   AmpMv=_ZERO_
   PhaseMu=_ZERO_
   PhaseMv=_ZERO_
   PeriodS=43200.
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

!  Vertical advection velocity - 'w_advspec' namelist
   w_adv_method=0
   w_adv0=_ZERO_
   w_adv_height0=_ZERO_
   w_adv_file='w_adv.dat'
   w_adv_discr=1

!  Sea surface elevations - 'zetaspec' namelist
   zeta_method=0
   zeta_file='zeta.dat'
   zeta_0=_ZERO_
   period_1=44714.
   amp_1=_ZERO_
   phase_1=_ZERO_
   period_2=43200.
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
   vel_relax_tau=3600.
   vel_relax_ramp=86400.

!  Observed dissipation profiles
   e_prof_method=0
   e_obs_const=1.e-12
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

   allocate(sprof(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (sprof)'
   sprof = _ZERO_

   allocate(tprof(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (tprof)'
   tprof = _ZERO_

   allocate(dsdx(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (dsdx)'
   dsdx = _ZERO_

   allocate(dsdy(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (dsdy)'
   dsdy = _ZERO_

   allocate(dtdx(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (dtdx)'
   dtdx = _ZERO_

   allocate(dtdy(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (dtdy)'
   dsdy = _ZERO_

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

   allocate(uprof(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_observations: Error allocating (uprof)'
   uprof = _ZERO_

   allocate(vprof(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_observations: Error allocating (vprof)'
   vprof = _ZERO_

   allocate(epsprof(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_observations: Error allocating (epsprof)'
   epsprof = _ZERO_

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
      if ((s_prof_method.ne.0).and.(SRelaxTau(i).le._ZERO_)) then
         LEVEL2 ''
         LEVEL2 '***************************************************'
         LEVEL2 'SRelaxTau at i=',i,' is not a positive value.'
         LEVEL2 'Please correct obs.nml and rerun.'
         LEVEL2 'Program aborted.'
         LEVEL2 '***************************************************'
         stop 'init_observations'
      end if
      if ((t_prof_method.ne.0).and.(TRelaxTau(i).le._ZERO_)) then
         LEVEL2 ''
         LEVEL2 '***************************************************'
         LEVEL2 'TRelaxTau at i=',i,' is not a positive value.'
         LEVEL2 'Please correct obs.nml and rerun.'
         LEVEL2 'Program aborted.'
         LEVEL2 '***************************************************'
         stop 'init_observations'
      end if
   end do

   allocate(o2_prof(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_observations: Error allocating (o2_prof)'
   o2_prof = _ZERO_

!  The salinity profile
   select case (s_prof_method)
      case (NOTHING)
         sprof = _ZERO_
      case (ANALYTICAL)

         ! different ways to prescribe profiles analytically
         select case (s_analyt_method)
            case (CONST_PROF)
               sprof = s_1
            case (TWO_LAYERS)
               call analytical_profile(nlev,z,z_s1,s_1,z_s2,s_2,sprof)
            case (CONST_NN)

               if (.not.((t_prof_method       .eq. ANALYTICAL) .and.      &
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

               call const_NNS(nlev,z,s_1,t_1,s_obs_NN,gravity,rho_0,sprof)
            case default
               LEVEL1 'A non-valid s_analyt_method has been given ',s_analyt_method
               stop 'init_observations()'
         end select

      case (FROMFILE)
         call register_input_1d(s_prof_file,1,sprof,'observed salinity')
         LEVEL2 'Reading salinity profiles from:'
         LEVEL3 trim(s_prof_file)
      case default
         LEVEL1 'A non-valid s_prof_method has been given ',s_prof_method
         stop 'init_observations()'
   end select

!  The temperature profile
   select case (t_prof_method)
      case (NOTHING)
         tprof = _ZERO_
      case (ANALYTICAL)

        ! different ways to prescribe profiles analytically
         select case (t_analyt_method)
         case (CONST_PROF)
               tprof = t_1
            case (TWO_LAYERS)
               call analytical_profile(nlev,z,z_t1,t_1,z_t2,t_2,tprof)
            case (CONST_NN)

               if (.not.((s_prof_method       .eq. ANALYTICAL) .and.      &
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

               call const_NNT(nlev,z,t_1,s_1,t_obs_NN,gravity,rho_0,tprof)
            case default
               LEVEL1 'A non-valid t_analyt_method has been given ',t_analyt_method
               stop 'init_observations()'
         end select
      case (FROMFILE)
         call register_input_1d(t_prof_file,1,tprof,'observed temperature')
         LEVEL2 'Reading temperature profiles from:'
         LEVEL3 trim(t_prof_file)
      case default
         LEVEL1 'A non-valid t_prof_method has been given ',t_prof_method
         stop 'init_observations()'
   end select

!  The external pressure
   select case (ext_press_method)
      case (NOTHING)
         h_press = PressHeight
         dpdx    = PressConstU
         dpdy    = PressConstV
      case (ANALYTICAL)
!        The analytical prescription of external pressure is time-dependent
!        and is therefore handled in get_all_obs.
      case (FROMFILE)
         call register_input_0d(ext_press_file,1,h_press,'observed external pressure: height above bed')
         call register_input_0d(ext_press_file,2,dpdx,'observed external pressure: x-direction')
         call register_input_0d(ext_press_file,3,dpdy,'observed external pressure: y-direction')
         LEVEL2 'Reading external pressure from:'
         LEVEL3 trim(ext_press_file)
      case default
         LEVEL1 'A non-valid ext_press_method has been given ',ext_press_method
         stop 'init_observations()'
   end select

!  The internal pressure
   select case (int_press_method)
      case (NOTHING)
         dsdx = _ZERO_
         dsdy = _ZERO_
         dtdx = _ZERO_
         dtdy = _ZERO_
      case (CONSTANT)
         dsdx = const_dsdx
         dsdy = const_dsdy
         dtdx = const_dtdx
         dtdy = const_dtdy
      case (FROMFILE)
         call register_input_1d(int_press_file,1,dsdx,'observed internal pressure: salinity gradient in x-direction')
         call register_input_1d(int_press_file,2,dsdy,'observed internal pressure: salinity gradient in y-direction')
         call register_input_1d(int_press_file,3,dtdx,'observed internal pressure: temperature gradient in x-direction')
         call register_input_1d(int_press_file,4,dtdy,'observed internal pressure: temperature gradient in y-direction')
         LEVEL2 'Reading internal pressure from:'
         LEVEL3 trim(int_press_file)
      case default
         LEVEL1 'A non-valid int_press_method has been given ',int_press_method
         stop 'init_observations()'
   end select

!  The light extinction profiles
   select case (extinct_method)
      case (0)
         call register_input_0d(extinct_file,1,A,'observed light extinction: non-visible fraction')
         call register_input_0d(extinct_file,2,g1,'observed light extinction: e-folding depth of non-visible fraction')
         call register_input_0d(extinct_file,3,g2,'observed light extinction: e-folding depth of visible fraction')
         LEVEL2 'Reading extinction data from:'
         LEVEL3 trim(extinct_file)
      case (1)
         A=0.58;g1=0.35;g2=23.0
      case (2)
         A=0.68;g1=1.20;g2=28.0
      case (3)
         A=0.62;g1=0.60;g2=20.0
      case (4)
         A=0.67;g1=1.00;g2=17.0
      case (5)
         A=0.77;g1=1.50;g2=14.0
      case (6)
         A=0.78;g1=1.40;g2=7.9
      case (7)
         A=0.7;g1=0.40;g2=8.0 ! Adolf Stips - Lago Maggiore
      case default
         LEVEL1 'A non-valid extinct_method has been given ',extinct_method
         stop 'init_observations()'
   end select

!  The vertical advection velocity
   select case (w_adv_method)
      case(NOTHING)                               ! no vertical advection
         w_height = _ZERO_
         w_adv    = _ZERO_
      case(CONSTANT)
         w_height = w_adv_height0
         w_adv    = w_adv0
      case (FROMFILE)
         call register_input_0d(w_adv_file,1,w_height,'observed vertical velocity: depth')
         call register_input_0d(w_adv_file,2,w_adv,'observed vertical velocity: value')
         LEVEL2 'Reading vertical velocity observations from:'
         LEVEL3 trim(w_adv_file)
      case default
         LEVEL1 'A non-valid w_adv_method has been given ',w_adv_method
         stop 'init_observations()'
   end select

!  The sea surface elevation
   select case (zeta_method)
      case(0)
         zeta = zeta_0
      case(ANALYTICAL)
!        The analytical prescription of surface elevation is time-dependent
!        and is therefore handled in get_all_obs.
      case (FROMFILE)
         call register_input_0d(zeta_file,1,zeta,'observed sea surface elevation')
         LEVEL2 'Reading sea surface elevations from:'
         LEVEL3 trim(zeta_file)
      case default
         LEVEL1 'A non-valid zeta_method has been given ',zeta_method
         stop 'init_observations()'
   end select

!  Wind waves
   select case (wave_method)
      case (NOTHING)
         Hs   = _ZERO_
         Tz   = _ZERO_
         phiw = _ZERO_
      case (CONSTANT)
!        Constant Hs, Tz and phiw have been already been set via the wave namelist.
      case (FROMFILE)
         call register_input_0d(wave_file,1,Hs,'observed wind waves: significant wave height')
         call register_input_0d(wave_file,2,Tz,'observed wind waves: mean zero-crossing period')
         call register_input_0d(wave_file,3,phiw,'observed wind waves: mean direction')
         LEVEL2 'Reading wind wave data from:'
         LEVEL3 trim(wave_file)
      case default
         LEVEL1 'A non-valid wave_method has been given ',wave_method
         stop 'init_observations()'
   end select

!  The observed velocity profile
   select case (vel_prof_method)
      case (NOTHING)
         uprof = _ZERO_
         vprof = _ZERO_
      case (FROMFILE)
         call register_input_1d(vel_prof_file,1,uprof,'observed horizontal velocity: x-direction')
         call register_input_1d(vel_prof_file,2,vprof,'observed horizontal velocity: y-direction')
         LEVEL2 'Reading velocity profiles from:'
         LEVEL3 trim(vel_prof_file)
      case default
         LEVEL1 'A non-valid vel_prof_method has been given ',vel_prof_method
         stop 'init_observations()'
   end select

!  The observed dissipation profile
   select case (e_prof_method)
      case (NOTHING)
         epsprof = _ZERO_
      case (FROMFILE)
         call register_input_1d(e_prof_file,1,epsprof,'observed turbulence dissipation')
         LEVEL2 'Reading dissipation profiles from:'
         LEVEL3 trim(e_prof_file)
      case default
         LEVEL1 'A non-valid e_prof_method has been given ',e_prof_method
         stop 'init_observations()'
   end select

!  The oxygen profile
   select case (o2_prof_method)
      case (NOTHING)
         o2_prof = _ZERO_
      case (ANALYTICAL)
      case (FROMFILE)
         select case (o2_units)
            case (1) ! mg/l
               call register_input_1d(o2_prof_file,1,o2_prof,'observed dissolved oxygen',scale_factor=mmol_o2_per_gram)
            case (2) ! ml/l
               call register_input_1d(o2_prof_file,1,o2_prof,'observed dissolved oxygen',scale_factor=mmol_o2_per_liter)
            case (3) ! mmol/m^3
               call register_input_1d(o2_prof_file,1,o2_prof,'observed dissolved oxygen')
            case default
               STDERR "Invalid choice for oxygen unit conversion given in"
               STDERR "namelist o2_profile of obs.nml. program aborted in"
               stop 'init_observations()'
         end select
         LEVEL2 'Reading oxygen profiles from:'
         LEVEL3 trim(o2_prof_file)
      case default
         LEVEL1 'A non-valid o2_prof_method has been given ',o2_prof_method
         stop 'init_observations()'
   end select

   return

80 FATAL 'Unable to open "',trim(fn),'" for reading'
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

   end subroutine init_observations
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
   if (ext_press_method==ANALYTICAL) then
!     Analytical prescription of tides
      h_press = PressHeight
      dpdx = AmpMu*sin(2*pi*(fsecs-PhaseMu)/PeriodM)    &
             + AmpSu*sin(2*pi*(fsecs-PhaseSu)/PeriodS)    &
             + PressConstU
      dpdy = AmpMv*sin(2*pi*(fsecs-PhaseMv)/PeriodM)    &
             + AmpSv*sin(2*pi*(fsecs-PhaseSv)/PeriodS)    &
             + PressConstV
   end if

   if (zeta_method==ANALYTICAL) then
!     Analytical prescription of tides
      Zeta = amp_1*sin(2*pi*(fsecs-phase_1)/period_1) &
            +amp_2*sin(2*pi*(fsecs-phase_2)/period_2) &
            +zeta_0
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
   if (allocated(sprof)) deallocate(sprof)
   if (allocated(tprof)) deallocate(tprof)
   if (allocated(dsdx)) deallocate(dsdx)
   if (allocated(dsdy)) deallocate(dsdy)
   if (allocated(dtdx)) deallocate(dtdx)
   if (allocated(dtdy)) deallocate(dtdy)
   if (allocated(idpdx)) deallocate(idpdx)
   if (allocated(idpdy)) deallocate(idpdy)
   if (allocated(SRelaxTau)) deallocate(SRelaxTau)
   if (allocated(TRelaxTau)) deallocate(TRelaxTau)
   if (allocated(uprof)) deallocate(uprof)
   if (allocated(vprof)) deallocate(vprof)
   if (allocated(epsprof)) deallocate(epsprof)
   if (allocated(o2_prof)) deallocate(o2_prof)
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

#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: Mean Flow
!
! !INTERFACE:
   module meanflow
!
! !DESCRIPTION:
!  This module provides all variables necessary for the meanflow
!  calculation and also makes the proper initialisations.
!
! !USES:
   IMPLICIT NONE
!  Default all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_meanflow, post_init_meanflow, clean_meanflow
#ifdef _PRINTSTATE_
   public print_state_meanflow
#endif

   interface init_meanflow
      module procedure init_meanflow_yaml
   end interface
!
! !PUBLIC DATA MEMBERS:
   logical, public                              :: grid_ready

!  coordinate z, layer thicknesses
   REALTYPE, public, dimension(:), allocatable, target  :: ga,z,zi,h,ho

!  the sea surface elevation
   REALTYPE, public, target  :: zeta=_ZERO_

!  the velocity components
   REALTYPE, public, dimension(:), allocatable, target  :: u,v,w

!  velocity at old time step
   REALTYPE, public, dimension(:), allocatable  :: uo,vo

!  potential temperature, salinity
!  T -> Tc - conservative temperature, S -> Sa - absolute salinity
   REALTYPE, public, dimension(:), allocatable, target  :: T,S
!  Tp - potential temperature, Sp - practical salinity
   REALTYPE, public, dimension(:), allocatable, target  :: Tp,Sp
!  Ti - in-situ temperature
   REALTYPE, public, dimension(:), allocatable, target  :: Ti
!  Tobs, Sobs - observed profiles as conservative and absolute values
   REALTYPE, public, dimension(:), allocatable, target  :: Tobs, Sobs

!  boyancy frequency squared
!  (total, from temperature only, from salinity only)
   REALTYPE, public, dimension(:), allocatable  :: NN,NNT,NNS

!  shear-frequency squared
!  (total, from u only, from v only)
   REALTYPE, public, dimension(:), allocatable  :: SS,SSU,SSV

!  Stokes-Eulerian cross-shear and Stokes shear squared
   REALTYPE, public, dimension(:), allocatable  :: SSCSTK, SSSTK

!  buoyancy, short-wave radiation,
!  extra production of tke by see-grass etc
   REALTYPE, public, dimension(:), allocatable  :: buoy,rad,xP

!  a dummy array
!  (most often used for diffusivities)
   REALTYPE, public, dimension(:), allocatable  :: avh

!  extra friction terms due to e.g. seagrass
   REALTYPE, public, dimension(:), allocatable  :: fric,drag

!  shading in the water column
   REALTYPE, public, dimension(:), allocatable, target  :: bioshade

#ifndef _ICE_
!  fake ice thickness - switch between 0 and 1 - see temperature.F90
   REALTYPE, public  :: Hice
#endif

# ifdef EXTRA_OUTPUT

!  dummies for testing
   REALTYPE, public, dimension(:), allocatable   :: mean1,mean2,mean3,mean4,mean5

# endif

!  the 'meanflow' configuration
   REALTYPE, public                    :: h0b
   logical,  public                    :: bottom_stress
   REALTYPE, public                    :: z0s_min
   logical,  public                    :: charnock
   REALTYPE, public                    :: charnock_val
   REALTYPE, public                    :: ddu
   REALTYPE, public                    :: ddl
   integer,  public                    :: grid_method
   REALTYPE, public                    :: c1ad
   REALTYPE, public                    :: c2ad
   REALTYPE, public                    :: c3ad
   REALTYPE, public                    :: c4ad
   REALTYPE, public                    :: Tgrid
   REALTYPE, public                    :: NNnorm
   REALTYPE, public                    :: SSnorm
   REALTYPE, public                    :: dsurf
   REALTYPE, public                    :: dtgrid
   character(LEN=PATH_MAX), public     :: grid_file
   REALTYPE, public                    :: gravity
   REALTYPE, public                    :: rotation_period
   REALTYPE, public                    :: avmolu
   REALTYPE, public                    :: avmolT
   REALTYPE, public                    :: avmolS
   integer,  public                    :: MaxItz0b
   logical,  public                    :: no_shear

!  the roughness lengths
   REALTYPE, public                    :: z0b,z0s,za

!  the coriolis parameter
   REALTYPE, public                    :: cori

!  the friction velocities
   REALTYPE, public                    :: u_taub,u_taubo,u_taus

!  bottom stress
   REALTYPE, public, target            :: taub

!  other stuff
   REALTYPE, public, target            :: depth0
   REALTYPE, public, target            :: depth
   REALTYPE, public                    :: runtimeu, runtimev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialisation of the mean flow variables
!
! !INTERFACE:
   subroutine init_meanflow_yaml()
!
! !DESCRIPTION:
!  Allocates memory and initialises everything related
!  to the `meanflow' component of GOTM.
!
! !USES:
   use settings
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See log for the meanflow module
!
! !LOCAL VARIABLES:
   type (type_gotm_settings), pointer :: branch
   integer                   :: rc
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_meanflow_yaml'

   branch => settings_store%get_typed_child('bottom')
   call branch%get(bottom_stress, 'bottom_stress', 'compute bottom stress (default: true)', &
                   default=.true.)
   call branch%get(h0b, 'h0b', 'physical bottom roughness', 'm', &
                minimum=0._rk,default=0.05_rk, description='physical bottom roughness or bed roughness. This variable, h0b, relates to the hydrodynamic bottom roughness z0b as z0b = 0.03*h0b + 0.1*nu/ustar.')
   call branch%get(MaxItz0b, 'max_it_z0b', 'number of iterations for hydrodynamic bottom roughness', &
                minimum=1,default=1, display=display_advanced, description='number of iterations for calculating the hydrodynamic bottom roughness from the bottom friction velocity and the physical bottom roughness.')

   branch => settings_store%get_typed_child('surface/roughness')
   call branch%get(charnock, 'charnock', 'use Charnock (1955) roughness adaptation', &
                default=.false.)
   call branch%get(charnock_val, 'charnock_val', 'empirical constant for roughness adaptation', '-', &
                minimum=0._rk,default=1400._rk)
   call branch%get(z0s_min, 'z0s_min', 'hydrodynamic roughness (minimum value if Charnock adaptation is used)', 'm', &
                minimum=0.0_rk,default=0.02_rk)

   branch => settings_store%get_typed_child('physical_constants', display=display_advanced)
   call branch%get(gravity, 'gravity', 'gravitational acceleration', 'm/s^2', &
                minimum=0._rk,default=9.81_rk)
   call branch%get(rotation_period, 'rotation_period', 'Rotation period', 's', &
                minimum=1.e-9_rk,default=86164.0_rk, description='rotation_period for Coriolis parameter, default is one sidereal day (86164 s)')
   call branch%get(avmolu, 'avmolu', 'molecular viscosity for momentum', 'm^2/s', &
                minimum=0._rk,default=1.3e-6_rk)
   call branch%get(avmolt, 'avmolt', 'molecular viscosity for temperature', 'm^2/s', &
                minimum=0._rk,default=1.4e-7_rk)
   call branch%get(avmols, 'avmols', 'molecular viscosity for salinity', 'm^2/s', &
                minimum=0._rk,default=1.1e-9_rk)
   !call branch%get(no_shear, 'no_shear', 'set shear production term to zero', &
   !             default=.false.)
   LEVEL2 'done'

   end subroutine init_meanflow_yaml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialisation of the mean flow variables
!
! !INTERFACE:
   subroutine post_init_meanflow(nlev,latitude)
!
! !DESCRIPTION:
!  Allocates memory and initialises everything related
!  to the `meanflow' component of GOTM.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                      :: nlev
   REALTYPE, intent(in)                     :: latitude
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See log for the meanflow module
!
! !DEFINED PARAMETERS:
   REALTYPE, parameter       :: pi=3.141592654
!
! !LOCAL VARIABLES:
   integer                   :: rc
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'post_init_meanflow'

!  Important: we do not initialize "depth" here, because it has already been initialized by gotm.F90.

!  Initialize bottom and surface stress to zero
!  They will be set in friction, but also used as input in the same routine.
   u_taub = _ZERO_
   u_taubo = _ZERO_
   u_taus = _ZERO_
   taub = _ZERO_

!  Store initial depth (actual depth will e a function of surface elevation)
   depth0=depth

!  Initialize surface and bottom roughness
   z0b=0.03*h0b
   z0s=z0s_min    ! lu (otherwise z0s is not initialized)
   za=_ZERO_      ! roughness caused by suspended sediment

!  Calculate Coriolis parameter
   cori=2*2*pi/rotation_period * sin(2*pi*latitude/360.)

!  Specify that the buoyance profile and grid still need to be calculated.
!  Note that the former is used only if a prognostic equation for buoyancy is used.
   grid_ready = .false.

!  Initialize cumulative run time used to detect u and v ramp.
   runtimeu = _ZERO_
   runtimev = _ZERO_

   LEVEL2 'allocation meanflow memory..'

   allocate(ga(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (ga)'
   ga = _ZERO_

   allocate(z(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (z)'
   z = _ZERO_

   allocate(zi(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (zi)'
   zi = _ZERO_

   allocate(h(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (h)'
   h = _ZERO_

   allocate(ho(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (ho)'
   ho = _ZERO_

   allocate(u(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (u)'
   u = _ZERO_

   allocate(uo(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (uo)'
   uo = _ZERO_

   allocate(v(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (v)'
   v = _ZERO_

   allocate(vo(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (vo)'
   vo = _ZERO_

   allocate(w(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (w)'
   w = _ZERO_

   allocate(fric(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (fric)'
   fric = _ZERO_

   allocate(drag(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (drag)'
   drag = _ZERO_

   allocate(T(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (T)'
   T = _ZERO_

   allocate(Tp(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (Tp)'
   Tp = _ZERO_

   allocate(Ti(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (Ti)'
   Ti = _ZERO_

   allocate(Tobs(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (Tobs)'
   Tobs = _ZERO_

   allocate(S(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (S)'
   S = _ZERO_

   allocate(Sp(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (Sp)'
   Sp = _ZERO_

   allocate(Sobs(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (Sobs)'
   Sobs = _ZERO_

   allocate(NN(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (NN)'
   NN = _ZERO_

   allocate(NNT(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (NNT)'
   NNT = _ZERO_

   allocate(NNS(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (NNS)'
   NNS = _ZERO_

   allocate(SS(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (SS)'
   SS = _ZERO_

   allocate(SSU(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (SSU)'
   SSU = _ZERO_

   allocate(SSV(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (SSV)'
   SSV = _ZERO_

   allocate(SSCSTK(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (SSCSTK)'
   SSCSTK = _ZERO_

   allocate(SSSTK(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (SSSTK)'
   SSSTK = _ZERO_

   allocate(xP(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (xP)'
   xP = _ZERO_

   allocate(buoy(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (buoy)'
   buoy = _ZERO_

   allocate(rad(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (rad)'
   rad = _ZERO_

   allocate(avh(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (avh)'
   avh = _ZERO_

   allocate(bioshade(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (bioshade)'
   bioshade = _ONE_

# ifdef EXTRA_OUTPUT

   allocate(mean1(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_meanflow: Error allocating (mean1)'
   mean1 = _ZERO_

   allocate(mean2(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_meanflow: Error allocating (mean2)'
   mean2 = _ZERO_

   allocate(mean3(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_meanflow: Error allocating (mean3)'
   mean3 = _ZERO_

   allocate(mean4(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_meanflow: Error allocating (mean4)'
   mean4 = _ZERO_

   allocate(mean5(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_meanflow: Error allocating (mean5)'
   mean5 = _ZERO_

# endif

   LEVEL2 'done.'
   return
   end subroutine post_init_meanflow
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleaning up the mean flow variables
!
! !INTERFACE:
   subroutine clean_meanflow()
!
! !DESCRIPTION:
!  De-allocates all memory allocated via init\_meanflow()
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See log for the meanflow module
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'clean_meanflow'

   LEVEL2 'de-allocation meanflow memory ...'
   if (allocated(ga)) deallocate(ga)
   if (allocated(z)) deallocate(z)
   if (allocated(zi)) deallocate(zi)
   if (allocated(h)) deallocate(h)
   if (allocated(ho)) deallocate(ho)
   if (allocated(u)) deallocate(u)
   if (allocated(uo)) deallocate(uo)
   if (allocated(v)) deallocate(v)
   if (allocated(vo)) deallocate(vo)
   if (allocated(w)) deallocate(w)
   if (allocated(fric)) deallocate(fric)
   if (allocated(drag)) deallocate(drag)
   if (allocated(T)) deallocate(T)
   if (allocated(Tp)) deallocate(Tp)
   if (allocated(Ti)) deallocate(Ti)
   if (allocated(S)) deallocate(S)
   if (allocated(Sp)) deallocate(Sp)
   if (allocated(NN)) deallocate(NN)
   if (allocated(NNT)) deallocate(NNT)
   if (allocated(NNS)) deallocate(NNS)
   if (allocated(SS)) deallocate(SS)
   if (allocated(SSU)) deallocate(SSU)
   if (allocated(SSV)) deallocate(SSV)
   if (allocated(SSCSTK)) deallocate(SSCSTK)
   if (allocated(SSSTK)) deallocate(SSSTK)
   if (allocated(xP)) deallocate(xP)
   if (allocated(buoy)) deallocate(buoy)
   if (allocated(rad)) deallocate(rad)
   if (allocated(avh)) deallocate(avh)
   if (allocated(bioshade)) deallocate(bioshade)
# ifdef EXTRA_OUTPUT
   if (allocated(mean1)) deallocate(mean1)
   if (allocated(mean2)) deallocate(mean2)
   if (allocated(mean3)) deallocate(mean3)
   if (allocated(mean4)) deallocate(mean4)
   if (allocated(mean5)) deallocate(mean5)
# endif
   LEVEL2 'done.'

   return
   end subroutine clean_meanflow
!EOC

#ifdef _PRINTSTATE_
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Print the current state of the meanflow module.
!
! !INTERFACE:
   subroutine print_state_meanflow()
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
   LEVEL1 'State of meanflow module:'
   LEVEL2 'grid_ready',grid_ready
   if (allocated(ga))  LEVEL2 'ga',ga
   if (allocated(z))   LEVEL2 'z',z
   if (allocated(h))   LEVEL2 'h',h
   if (allocated(ho))  LEVEL2 'ho',ho
   if (allocated(u))   LEVEL2 'u',u
   if (allocated(v))   LEVEL2 'v',v
   if (allocated(w))   LEVEL2 'w',w
   if (allocated(uo))  LEVEL2 'uo',uo
   if (allocated(vo))  LEVEL2 'vo',vo
   if (allocated(T))   LEVEL2 'T',t
   if (allocated(S))   LEVEL2 'S',s
   if (allocated(NN))  LEVEL2 'NN',NN
   if (allocated(NNT)) LEVEL2 'NNT',NNT
   if (allocated(NNS)) LEVEL2 'NNS',NNS
   if (allocated(SS))  LEVEL2 'SS',SS
   if (allocated(SSU)) LEVEL2 'SSU',SSU
   if (allocated(SSV)) LEVEL2 'SSV',SSV
   if (allocated(SSCSTK)) LEVEL2 'SSCSTK',SSCSTK
   if (allocated(SSSTK)) LEVEL2 'SSSTK',SSSTK
   if (allocated(buoy)) LEVEL2 'buoy',buoy
   if (allocated(rad)) LEVEL2 'rad',rad
   if (allocated(xp))  LEVEL2 'xP',xP
   if (allocated(avh)) LEVEL2 'avh',avh
   if (allocated(fric)) LEVEL2 'fric',fric
   if (allocated(drag)) LEVEL2 'drag',drag
   if (allocated(bioshade)) LEVEL2 'bioshade',bioshade
# ifdef EXTRA_OUTPUT
   if (allocated(mean1)) LEVEL2 'mean1',mean1
   if (allocated(mean2)) LEVEL2 'mean2',mean2
   if (allocated(mean3)) LEVEL2 'mean3',mean3
   if (allocated(mean4)) LEVEL2 'mean4',mean4
   if (allocated(mean5)) LEVEL2 'mean5',mean5
# endif

   LEVEL2 'meanflow configuration',                 &
      h0b,z0s_min,charnock,charnock_val,ddu,ddl,    &
      grid_method,c1ad,c2ad,c3ad,c4ad,Tgrid,NNnorm, &
      SSnorm,dsurf,dtgrid,grid_file,gravity,        &
      rotation_period,rho_0,avmolu,avmolT, avmolS,  &
      MaxItz0b,no_shear,bottom_stress

   LEVEL2 'z0b,z0s,za',z0b,z0s,za
   LEVEL2 'cori',cori
   LEVEL2 'u_taub,u_taus,taub',u_taub,u_taus,taub
   LEVEL2 'depth0, depth',depth0, depth
   LEVEL2 'runtimeu, runtimev',runtimeu, runtimev

   end subroutine print_state_meanflow
!EOC
#endif

!-----------------------------------------------------------------------

   end module meanflow

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

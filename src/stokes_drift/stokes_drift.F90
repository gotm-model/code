#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: stokes_drift --- Stokes drift \label{sec:stokes_drift}
!
! !INTERFACE:
   module stokes_drift
!
! !DESCRIPTION:
!  This module provides subroutines to compute Stokes drift from
!  various input.
!
! !USES:
   use input

   IMPLICIT NONE

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_stokes_drift, post_init_stokes_drift, do_stokes_drift, clean_stokes_drift

   interface init_stokes_drift
      module procedure init_stokes_drift_nml
      module procedure init_stokes_drift_yaml
   end interface
!
!
! !PUBLIC DATA MEMBERS:
!
!  Stokes drift profile - x component
   type (type_profile_input), public, target :: usprof

!  Stokes drift profile - y component
   type (type_profile_input), public, target :: vsprof

!  Stokes drift shear - x component
   type (type_profile_input), public, target :: dusdz

!  Stokes drift shear - y component
   type (type_profile_input), public, target :: dvsdz

!  Surface Stokes drift x and y components, Stokes depth
   type (type_scalar_input), public, target :: us0, vs0, ds

!  Surface wind for computing Stokes drift
   type (type_scalar_input), public, target :: uwnd, vwnd

! !DEFINED PARAMETERS:

!  pre-defined parameters
   integer, parameter        :: NOTHING=0
   integer, parameter        :: CONSTANT=1
   integer, parameter        :: FROMFILE=2
   integer, parameter        :: EXPONENTIAL=3
   integer, parameter        :: THEORYWAVE=4

   REALTYPE, parameter, public    :: pi = 4.*atan(_ONE_)

!
! !REVISION HISTORY:
!  Original author(s): Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the Stokes drift module
!
! !INTERFACE:
   subroutine init_stokes_drift_nml(namlst,fn)
!
! !DESCRIPTION:
!  The {\tt init\_stokes_drift_nml()} subroutine reads the {\tt stokes_drift.nml}
!  file with a number of different Stokes drift input options.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: namlst
   character(len=*), intent(in)        :: fn
!
! !REVISION HISTORY:
!  Original author(s): Qing Li
!
! !LOCAL VARIABLES:
!  Stokes drift - 'stokes_drift' namelist
   integer                   :: us_prof_method
   character(LEN=PATH_MAX)   :: us_prof_file

   integer                   :: dusdz_prof_method
   character(LEN=PATH_MAX)   :: dusdz_prof_file

   integer                   :: us0_method
   character(LEN=PATH_MAX)   :: us0_file
   REALTYPE                  :: const_us0
   REALTYPE                  :: const_vs0
   REALTYPE                  :: const_ds

   integer                   :: wnd_method
   character(LEN=PATH_MAX)   :: wnd_file
   REALTYPE                  :: const_uwnd, const_vwnd

!  Stokes drift namelist
   namelist /stokes_drift/                                      &
            us_prof_method,us_prof_file,                        &
            dusdz_prof_method,dusdz_prof_file,                  &
            us0_method,us0_file,const_us0,const_vs0,const_ds,   &
            wnd_method,wnd_file,const_uwnd,const_vwnd
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_stokes_drift_nml'

!  Stokes drift - 'stokes_drift' namelist
   us_prof_method=0
   us_prof_file='us_prof_file.dat'
   dusdz_prof_method=0
   dusdz_prof_file='dusdz_prof_file.dat'
   us0_method=0
   us0_file='us0_file.dat'
   const_us0=0.068
   const_vs0=_ZERO_
   const_ds=4.76
   wnd_method=0
   wnd_file='wnd_file.dat'
   const_uwnd=5.0
   const_vwnd=_ZERO_

   open(namlst,file=fn,status='old',action='read',err=91)
   read(namlst,nml=stokes_drift,err=92)
   close(namlst)

   call usprof%configure(method=us_prof_method, path=us_prof_file, index=1)
   call vsprof%configure(method=us_prof_method, path=us_prof_file, index=2)

   call dusdz%configure(method=dusdz_prof_method, path=dusdz_prof_file, index=1)
   call dvsdz%configure(method=dusdz_prof_method, path=dusdz_prof_file, index=2)

   call us0%configure(method=us0_method, path=us0_file, index=1, constant_value=const_us0)
   call vs0%configure(method=us0_method, path=us0_file, index=2, constant_value=const_vs0)
   call  ds%configure(method=us0_method, path=us0_file, index=3, constant_value=const_ds)

   call uwnd%configure(method=wnd_method, path=wnd_file, index=1, constant_value=const_uwnd)
   call vwnd%configure(method=wnd_method, path=wnd_file, index=2, constant_value=const_vwnd)

   LEVEL2 'done'

   return

91 FATAL 'I could not open "stokes_drift.nml"'
   stop 'init_stokes_drift_nml'
92 FATAL 'I could not read "stokes_drift" namelist'
   stop 'init_stokes_drift_nml'

   end subroutine init_stokes_drift_nml

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the stokes_drift module
!
! !INTERFACE:
   subroutine init_stokes_drift_yaml()
!
! !DESCRIPTION:
!  The {\tt init\_stokes_drift_yaml()} subroutine reads the {\tt gotm.yaml}
!  file with a number of different Stokes drift input options.
!
! !USES:
   use settings
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Qing Li
!
! !LOCAL VARIABLES:
   class (type_gotm_settings), pointer :: branch, twig
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_stokes_drift_yaml'

   branch => settings_store%get_typed_child('stokes_drift', 'observed/prescribed Stokes drift')
   call branch%get(usprof, 'us', 'Stokes drift in West-East direction', 'm/s', default=0._rk,    &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE, &
                   extra_options=(/option(EXPONENTIAL, 'exponential profile'), &
                                   option(THEORYWAVE, 'theory-wave of Li et al., 2017')/))
   call branch%get(vsprof, 'vs', 'Stokes drift in South-North direction', 'm/s', default=0._rk,  &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE, &
                   extra_options=(/option(EXPONENTIAL, 'exponential profile'), &
                                   option(THEORYWAVE, 'theory-wave of Li et al., 2017')/))
   call branch%get(dusdz, 'dusdz', 'Stokes drift shear in West-East direction', '1/s', default=0._rk,    &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE)
   call branch%get(dvsdz, 'dvsdz', 'Stokes drift shear in South-North direction', '1/s', default=0._rk,  &
                   method_off=NOTHING, method_constant=method_unsupported, method_file=FROMFILE)
   twig => branch%get_typed_child('exponential', 'exponential Stokes drift profile defined by surface value and decay depth')
   call twig%get(us0, 'us0', 'Surface Stokes drift in West-East direction', 'm/s',   &
                 method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE, &
                 minimum=0._rk, default=0.068_rk)
   call twig%get(vs0, 'vs0', 'Surface Stokes drift in South-North direction', 'm/s', &
                 method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE, &
                 minimum=0._rk, default=0._rk)
   call twig%get(ds, 'ds', 'Stokes drift decay depth', 'm',                          &
                 method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE, &
                 minimum=0._rk, default=4.76_rk)
   twig => branch%get_typed_child('empirical', 'approximate Stokes drift from empirical wave spectrum')
   call twig%get(uwnd, 'uwnd', 'Surface wind for Stokes drift in West-East direction', 'm/s',     &
                 method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE,              &
                 minimum=0._rk, default=5._rk)
   call twig%get(vwnd, 'vwnd', 'Surface wind for Stokes drift in South-North direction', 'm/s',   &
                 method_off=NOTHING, method_constant=CONSTANT, method_file=FROMFILE,              &
                 minimum=0._rk, default=0._rk)

   LEVEL2 'done'

   return

   end subroutine init_stokes_drift_yaml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialisation of the Stokes drift variables
!
! !INTERFACE:
   subroutine post_init_stokes_drift(nlev)
!
! !DESCRIPTION:
!  Allocates memory and initialises everything related
!  to the `stokes_drift' component of GOTM.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                      :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Qing Li
!
! !LOCAL VARIABLES:
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'post_init_stokes_drift'

   LEVEL2 'allocation stokes_drift memory..'

!  Stokes drift profile
   call register_input(usprof)
   call register_input(vsprof)

   select case (usprof%method)
      case (NOTHING)
         LEVEL2 'Stokes drift off.'
      case (FROMFILE)
      case (EXPONENTIAL)
         LEVEL2 'Using exponential Stokes drift profile.'
         call register_input(us0)
         call register_input(vs0)
         call register_input(ds)
      case (THEORYWAVE)
         LEVEL2 'Using Stokes drift estimated from the theory-wave of Li et al. (2017)'
         call register_input(uwnd)
         call register_input(vwnd)
      case (CONSTANT)
         LEVEL1 'The following us_prof_method has yet been supported: ', usprof%method
         stop 'init_stokes_drift()'
      case default
         LEVEL1 'A non-valid us_prof_method has been given ', usprof%method
         stop 'init_stokes_drift()'
   end select

!  Stokes drift shear profile
   call register_input(dusdz)
   call register_input(dvsdz)

   LEVEL2 'done.'

   return

   end subroutine post_init_stokes_drift
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: do_stokes_drift
!
! !INTERFACE:
   subroutine do_stokes_drift(nlev,z,zi,gravity,u10,v10)
!
! !DESCRIPTION:
!  A wrapper for all the subroutines to calculate the Stokes drift profile.
!
! !USES:

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: gravity
   REALTYPE, intent(in)                :: u10, v10
   REALTYPE, intent(in)                :: z(0:nlev), zi(0:nlev)
!
! !OUTPUT PARAMETERS:

! !REVISION HISTORY:
!  Original author(s): Qing Li
!
!EOP
! !LOCAL VARIABLES:
   integer                   :: k
   REALTYPE                  :: ustran
!-----------------------------------------------------------------------
!BOC

   select case (usprof%method)
      case (FROMFILE)
         ! usprof and vsprof already read from file
         us0%value = usprof%data(nlev)
         vs0%value = vsprof%data(nlev)
         ! Stokes transport
         ustran = _ZERO_
         do k=1,nlev
            ustran = ustran + sqrt(usprof%data(k)**2+vsprof%data(k)**2)*(zi(k)-zi(k-1))
         end do
         ds%value = ustran/max(SMALL, sqrt(us0%value**2.+us0%value**2.))
      case (EXPONENTIAL)
         call stokes_drift_exp(nlev,z,zi)
      case (THEORYWAVE)
         if (uwnd%method .eq. NOTHING) then
            call stokes_drift_theory(nlev,z,zi,u10,v10,gravity)
         else
            call stokes_drift_theory(nlev,z,zi,uwnd%value,vwnd%value,gravity)
         endif
   end select

   ! Stokes shear
   if (dusdz%method .eq. NOTHING) then
      do k=1,nlev-1
         dusdz%data(k) = (usprof%data(k+1)-usprof%data(k))/(z(k+1)-z(k))
         dvsdz%data(k) = (vsprof%data(k+1)-vsprof%data(k))/(z(k+1)-z(k))
      end do
      dusdz%data(0   ) = dusdz%data(1     )
      dusdz%data(nlev) = dusdz%data(nlev-1)
      dvsdz%data(0   ) = dvsdz%data(1     )
      dvsdz%data(nlev) = dvsdz%data(nlev-1)
   endif

   return

   end subroutine do_stokes_drift
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Clean up the stokes drift module
!
! !INTERFACE:
   subroutine clean_stokes_drift()
!
! !DESCRIPTION:
!  De-allocates memory allocated in init\_stokes_drift().
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Qing Li
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'clean_stokes_drift'

   LEVEL2 'de-allocate stokes drift memory ...'

   LEVEL2 'done.'

   end subroutine clean_stokes_drift
!EOC

!-----------------------------------------------------------------------

   end module stokes_drift

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

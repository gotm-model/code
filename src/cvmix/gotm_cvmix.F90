#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: gotm_cvmix: interface to the Community Ocean Vertical Mixing Project (CVMix, http://cvmix.github.io) \label{sec:cvmix}
!
! !INTERFACE:
   module gotm_cvmix
!
! !DESCRIPTION:
!  This module provides an interface to use the Community Ocean Vertical Mixing
!  Project (CVMix) in the General Ocean Turbulence Model.
!
! !USES:
   use cvmix_background
   use cvmix_convection
   use cvmix_ddiff
   use cvmix_kinds_and_types
   use cvmix_kpp
   use cvmix_math
   use cvmix_put_get
   use cvmix_shear
   use cvmix_tidal

   use turbulence, only: cvmix_num=>num,   &
                         cvmix_nuh=>nuh,   &
                         cvmix_nus=>nus,   &
                         cvmix_gamh=>gamh, &
                         cvmix_gams=>gams, &
                         cvmix_Rig=>Rig

   IMPLICIT NONE

!  default: all is private.
   private

! !PUBLIC MEMBER FUNCTIONS:
   public init_cvmix, do_cvmix, clean_cvmix, post_init_cvmix
!
   interface init_cvmix
      module procedure init_cvmix_nml
      module procedure init_cvmix_yaml
   end interface
!
! !PUBLIC DATA MEMBERS:
!
! z-position of surface boundary layer depth
  REALTYPE, public                       ::    zsbl

! !DEFINED PARAMETERS:
!
!  method of Langmuir turbulence parameterization
   integer, parameter                    ::    CVMIX_LT_NOLANGMUIR = 0
   integer, parameter                    ::    CVMIX_LT_LWF16 = 1
   integer, parameter                    ::    CVMIX_LT_LF17 = 2
   integer, parameter                    ::    CVMIX_LT_RWH16 = 3
   integer, parameter                    ::    CVMIX_INTERP_LINEAR = 1
   integer, parameter                    ::    CVMIX_INTERP_QUADRATIC = 2
   integer, parameter                    ::    CVMIX_INTERP_CUBIC = 3
   integer, parameter                    ::    CVMIX_INTERP_LMD94 = 4
   integer, parameter                    ::    CVMIX_MATCH_SIMPLE = 1
   integer, parameter                    ::    CVMIX_MATCH_GRADIENT = 2
   integer, parameter                    ::    CVMIX_MATCH_BOTH = 3
   integer, parameter                    ::    CVMIX_MATCH_PARABOLIC = 4
   integer, parameter                    ::    CVMIX_SHEAR_PP = 1
   integer, parameter                    ::    CVMIX_SHEAR_KPP = 2
   REALTYPE, parameter                   ::    eps = 1.0e-14

! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
! !LOCAL VARIABLES:
!
!  acceleration of gravity
   REALTYPE                              ::    cvmix_g

!  reference density
   REALTYPE                              ::    cvmix_rho0

!  g/rho0
   REALTYPE                              ::    cvmix_gorho0

!  compute surface boundary layer mixing and internal mixing
   logical                               ::    use_surface_layer,      &
                                               use_interior

!  flags for different components of CVMix
   logical                               ::    use_kpp,                &
                                               use_background,         &
                                               use_shear,              &
                                               use_convection,         &
                                               use_tidal_mixing,       &
                                               use_double_diffusion
!
!  G'(1) = 0 (shape function) if true, otherwise compute G'(1) as in LMD94
   logical                               ::    kpp_use_noDGat1

!  limit the OBL by the Ekman depth / Monin-Obukhov length if true
   logical                               ::    kpp_check_Ekman_length, &
                                               kpp_check_MonOb_length
!  enhance diffusivity at OBL
   logical                               ::    kpp_use_enhanced_diff

!  method to parameterize the effects of Langmuir turbulence
!  options are
!  (0)   no Langmuir turbulence parameterization
!  (1)   Langmuir mixing following Li et al., 2016
!  (2)   Langmuir enhanced entrainment following Li & Fox-Kemper, 2017
!  (3)   Langmuir turbulence in hurricanes following Reichl et al., 2016
   integer, public                       ::    kpp_langmuir_method

!  interpolation type used to interpolate bulk Richardson number
!  options are
!  (1) linear
!  (2) quadratic
!  (3) cubic
   integer                               ::    kpp_bulk_Ri_interp_type

!  interpolation type used to interpolate diff and visc at OBL_depth
!  options are
!  (1) linear
!  (2) quadratic
!  (3) cubic
!  (4) LMD94
   integer                               ::    kpp_OBL_interp_type

!  matching technique between the boundary layer and the ocean interior
!  options are
!  (1) SimpleShapes      => Shape functions for both the gradient and nonlocal
!                           terms vanish at interface
!  (2) MatchGradient     => Shape function for nonlocal term vanishes at
!                           interface, but gradient term matches interior
!                           values.
!  (3) MatchBoth         => Shape functions for both the gradient and nonlocal
!                           term match interior values at interface
!  (4) ParabolicNonLocal => Shape function for the nonlocal term is
!                           (1-sigma)^2, gradient term is sigma*(1-sigma)^2
   integer                               ::    kpp_match_technique

!  surface layer extent
   REALTYPE                              ::    kpp_surface_layer_extent

!  critical Richardson number
   REALTYPE                              ::    kpp_Ri_c

!  background diffusivity and viscosity
   REALTYPE                              ::    background_diffusivity, &
                                               background_viscosity

!  shear mixing scheme
!  options are
!  (1) PP   => Pacanowski-Philander, 1981
!  (2) KPP  => KPP, Large et al, 1994
   integer                               ::    shear_mix_scheme

!  number of loops to smooth the gradient Richardson number
   integer                               ::    shear_num_smooth_Ri

!  parameters in PP:
!   - numerator in viscosity term
!   - coefficient of Richardson number in denominator of visc / diff terms
!   - exponent of denominator in viscosity term
   REALTYPE                              ::    shear_PP_nu_zero,       &
                                               shear_PP_alpha,         &
                                               shear_PP_exp
!
!  parameters in KPP
!   - leading coefficient of shear mixing formula
!   - critical Richardson number value
!   - exponent of unitless factor of diffusivity
   REALTYPE                              ::    shear_KPP_nu_zero,      &
                                               shear_KPP_Ri_zero,      &
                                               shear_KPP_exp
!
!  convective diffusivity and viscosity
   REALTYPE                              ::    convection_diffusivity, &
                                               convection_viscosity
!  triger convection based on the squared Brunt-Vaisala frequency and the
!  threshold in s^-2
   logical                               ::    convection_basedOnBVF
   REALTYPE                              ::    convection_triggerBVF

!  positions of grid faces and centers
   REALTYPE, dimension(:), allocatable   ::    z_w, z_r

!  distance between centers
   REALTYPE, dimension(:), allocatable   ::    h_r

!  CVMix datatypes
   type(cvmix_data_type)                 ::    CVmix_vars

!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the CVMix module
!
! !INTERFACE:
   subroutine init_cvmix_nml(namlst, fn, nlev, h0, g, rho0)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
!  namelist reference
   integer, intent(in)                 :: namlst

!  filename containing namelist
   character(len=*), intent(in)        :: fn

!  optional arguments, if present, call post_init_cvmix() after reading the namelist
!   - number of levels
!   - water depth (m)
!   - acceleration of gravity (m/s^2)
!   - reference density (kg/m^3)
   integer, optional, intent(in)       :: nlev
   REALTYPE, optional, intent(in)      :: h0, g, rho0
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   namelist /cvmix/     use_surface_layer,           &
                        use_interior,                &
                        use_kpp,                     &
                        kpp_langmuir_method,         &
                        kpp_Ri_c,                    &
                        kpp_surface_layer_extent,    &
                        kpp_check_Ekman_length,      &
                        kpp_check_MonOb_length,      &
                        kpp_use_enhanced_diff,       &
                        kpp_use_noDGat1,             &
                        kpp_match_technique,         &
                        kpp_bulk_Ri_interp_type,     &
                        kpp_OBL_interp_type,         &
                        use_background,              &
                        background_diffusivity,      &
                        background_viscosity,        &
                        use_shear,                   &
                        shear_num_smooth_Ri,         &
                        shear_mix_scheme,            &
                        shear_PP_nu_zero,            &
                        shear_PP_alpha,              &
                        shear_PP_exp,                &
                        shear_KPP_nu_zero,           &
                        shear_KPP_Ri_zero,           &
                        shear_KPP_exp,               &
                        use_convection,              &
                        convection_diffusivity,      &
                        convection_viscosity,        &
                        convection_basedOnBVF,       &
                        convection_triggerBVF,       &
                        use_tidal_mixing,            &
                        use_double_diffusion
!
!-----------------------------------------------------------------------
!BOC

   LEVEL1 'init_cvmix_nml'

!  CVMix - 'cvmix' namelist
   use_surface_layer = .true.
   use_interior = .false.
   ! kpp
   use_kpp = .true.
   kpp_langmuir_method = 0
   kpp_surface_layer_extent = 0.1
   kpp_Ri_c = 0.3
   kpp_check_Ekman_length = .false.
   kpp_check_MonOb_length = .false.
   kpp_use_enhanced_diff = .true.
   kpp_use_noDGat1 = .true.
   kpp_match_technique = 1
   kpp_bulk_Ri_interp_type = 2
   kpp_OBL_interp_type = 4
   ! background
   use_background = .false.
   background_diffusivity = 1.0e-5
   background_viscosity = 1.0e-4
   ! shear
   use_shear = .false.
   shear_num_smooth_Ri = 1
   shear_mix_scheme = 2
   shear_PP_nu_zero = 0.005
   shear_PP_alpha = 5.0
   shear_PP_exp = 2.0
   shear_KPP_nu_zero = 0.005
   shear_KPP_Ri_zero = 0.7
   shear_KPP_exp = 3.0
   ! convection
   use_convection = .false.
   convection_diffusivity = 1.0
   convection_viscosity = 1.0
   convection_basedOnBVF = .true.
   convection_triggerBVF = 0.0
   ! tidal mixing
   use_tidal_mixing = .false.
   ! double diffusion
   use_double_diffusion = .false.

   ! read the variables from the namelist file
   open(namlst,file=fn,status='old',action='read',err=80)
   read(namlst,nml=cvmix,err=81)
   close (namlst)

   LEVEL2 'done.'

   ! provide an interface to use init_cvmix() for all initialization steps
   if (present(nlev) .and. present(h0)  &
      .and. present(g) .and. present(rho0)) then
      call post_init_cvmix(nlev, h0, g, rho0)
   endif

   return

80 FATAL 'I could not open "cvmix.nml"'
   stop 'init_cvmix_nml'
81 FATAL 'I could not read "cvmix" namelist'
   stop 'init_cvmix_nml'

 end subroutine init_cvmix_nml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the cvmix module
!
! !INTERFACE:
   subroutine init_cvmix_yaml(branch)
!
! !DESCRIPTION:
!
! !USES:
   use yaml_settings
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   class (type_settings), intent(inout) :: branch
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard,
!                      Manuel Ruiz Villarreal,
!                      Lars Umlauf
!   Adapted for CVMix: Qing Li
!
! !LOCAL VARIABLES:
   class (type_settings), pointer      :: twig, leaf
   integer, parameter                  :: rk = kind(_ONE_)
   integer                             :: rc
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_cvmix_yaml'

   twig => branch%get_child('surface_layer', 'surface layer mixing')
   call twig%get(use_surface_layer, 'use',                             &
      'compute surface layer mixing coefficients', default=.true.)
   leaf => twig%get_child('kpp', 'K-Profile Parameterization')
   call leaf%get(use_kpp, 'use', 'use the K-Profile Parameterization', &
      default=.true.)
   call leaf%get(kpp_langmuir_method, 'langmuir_method',               &
      'method of Langmuir turbulence pararmeterization', default=0,    &
      options=(/option(0, 'none'), option(1, 'Li et al. (2016)'),      &
      option(2, 'Li and Fox-Kemper (2017)'),                           &
      option(3, 'Reichl et al. (2016)')/))
   call leaf%get(kpp_surface_layer_extent, 'surface_layer_extent',     &
      'extent of surface layer in fraction of the boundary layer', '-',&
      minimum=0._rk, maximum=1._rk, default=0.1_rk)
   call leaf%get(kpp_Ri_c, 'Ri_c', 'critical Richardson number', '-',  &
      minimum=0._rk, default=0.3_rk)
   call leaf%get(kpp_check_Ekman_length, 'check_Ekman_length',         &
      'limit the OBL by the Ekman depth', default=.false.)
   call leaf%get(kpp_check_MonOb_length, 'check_MonOb_length',         &
      'limit the OBL by the Monin-Obukhov depth', default=.false.)
   call leaf%get(kpp_use_enhanced_diff, 'use_enhanced_diff',           &
      'enhance diffusivity at OBL', default=.true.)
   call leaf%get(kpp_use_noDGat1, 'use_noDGat1',                       &
      'zero gradient of the shape function at OBL', default=.true.)
   call leaf%get(kpp_match_technique, 'match_technique',               &
      'matching technique with the ocean interior',                    &
      default=1, options=(/                                            &
      option(1, 'SimpleShapes'),                                       &
      option(2, 'MatchGradient'),                                      &
      option(3, 'MatchBoth'),                                          &
      option(4, 'ParabolicNonLocal')/))
   call leaf%get(kpp_bulk_Ri_interp_type, 'bulk_Ri_interp_type',       &
      'interpolation type for the bulk Richardson number',             &
      default=2, options=(/                                            &
      option(1, 'linear'), option(2, 'quadratic'), option(3, 'cubic')/))
   call leaf%get(kpp_OBL_interp_type, 'OBL_interp_type',               &
      'interpolation type for diffusivity and viscosity at OBL',       &
      default=4, options=(/                                            &
      option(1, 'linear'), option(2, 'quadratic'),                     &
      option(3, 'cubic'), option(4, 'LMD94')/))

   twig => branch%get_child('interior', 'interior mixing')
   call twig%get(use_interior, 'use',                                  &
      'compute interior mixing coefficients', default=.false.)
   leaf => twig%get_child('background')
   call leaf%get(use_background, 'use',                                &
      'use interior background mixing coefficients', default=.false.)
   call leaf%get(background_diffusivity, 'diffusivity',                &
      'background diffusivity', 'm^2/s',                               &
      default=1.e-5_rk)
   call leaf%get(background_viscosity, 'viscosity',                    &
      'background viscosity', 'm^2/s',                                 &
      default=1.e-4_rk)
   leaf => twig%get_child('shear')
   call leaf%get(use_shear, 'use',                                     &
      'compute interior shear mixing coefficients', default=.false.)
   call leaf%get(shear_num_smooth_Ri, 'num_smooth_Ri',                 &
      'number of iterations to smooth the gradient Richardson number', &
      default=1)
   call leaf%get(shear_mix_scheme, 'mix_scheme', 'shear mixing scheme',&
      default=2, options=(/                                            &
      option(1, 'PP, Pacanowski and Philander (1981)'),                &
      option(2, 'KPP, Large et al. (1994)')/))
   call leaf%get(shear_PP_nu_zero, 'PP_nu_zero',                       &
      'numerator in viscosity term in PP', 'm^2/s',                    &
      default=0.005_rk)
   call leaf%get(shear_PP_alpha, 'PP_alpha',                           &
      'coefficient of Ri in denominator of visc / diff terms', '-',    &
      default=5.0_rk)
   call leaf%get(shear_PP_exp, 'PP_exp',                               &
      'exponent of denominator in viscosity term', '-',                &
      default=2.0_rk)
   call leaf%get(shear_KPP_nu_zero, 'KPP_nu_zero',                     &
      'leading coefficient of the KPP shear mixing formula', 'm^2/s',  &
      default=0.005_rk)
   call leaf%get(shear_KPP_Ri_zero, 'KPP_Ri_zero',                     &
      'critical Richardson number for KPP shear mixing', '-',          &
      default=0.7_rk)
   call leaf%get(shear_KPP_exp, 'KPP_exp',                             &
      'exponent of unitless factor of diffusivity', '-',               &
      default=3.0_rk)
   leaf => twig%get_child('convection')
   call leaf%get(use_convection, 'use',                                &
      'compute interior convective mixing coefficients',               &
      default=.false.)
   call leaf%get(convection_diffusivity, 'diffusivity',                &
      'convective diffusivity', 'm^2/s',                               &
      default=1.0_rk)
   call leaf%get(convection_viscosity, 'viscosity',                    &
      'convective viscosity', 'm^2/s',                                 &
      default=1.0_rk)
   call leaf%get(convection_basedOnBVF, 'basedOnBVF',                  &
      'triger convection based on the squared Brunt-Vaisala frequency',&
      default=.true.)
   call leaf%get(convection_triggerBVF, 'triggerBVF',                  &
      'threshold of squared Brunt-Vaisala frequency', '1/s^2',         &
      default=0.0_rk)
   leaf => twig%get_child('tidal_mixing')
   call leaf%get(use_tidal_mixing, 'use',                              &
      'compute interior tidal mixing coefficients',                    &
      default=.false.)
   leaf => twig%get_child('double_diffusion')
   call leaf%get(use_double_diffusion, 'use',                          &
      'compute interior double diffusion mixing coefficients',         &
      default=.false.)

   LEVEL2 'done.'

   return

 end subroutine init_cvmix_yaml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialisation of the CVMix variables
!
! !INTERFACE:
   subroutine post_init_cvmix(nlev, h0, g, rho0)
!
! !DESCRIPTION:
!  Allocates memory and initialises everything related
!  to the `cvmix' component of GOTM.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev

!  bathymetry (m)
   REALTYPE, intent(in)                :: h0

!  acceleration of gravity (m/s^2)
   REALTYPE, intent(in)                :: g

!  reference density (kg/m^3)
   REALTYPE, intent(in)                :: rho0
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
! !LOCAL VARIABLES:
   integer, parameter                  :: rk = kind(_ONE_)
   integer                             :: k
   integer                             :: rc

!  Langmuir mixing method: scheme to enhance the velocity scale used
!  in turbulent mixing coefficient calculation
!  'NONE'    - No enhancement
!  'LWF16'   - Enhancement based on Van Roekel et al. 2012 and Li et al. 2016
!  'RWHGK16' - Enhancement based on Reichl et al. 2016
   character(len=32)                   :: langmuir_mixing_method

!  Langmuiri entrainment method: scheme to enhance the entrainment used
!  in the mixing layer depth calculation
!  'NONE'    - No enhancement
!  'LWF16'   - Enhancement based on Li et al. 2016
!  'LF17'    - Enhancement based on Li and Fox-Kemper 2017
!  'RWHGK16' - Enhancement based on Reichl et al. 2016
   character(len=32)                   :: langmuir_entrainment_method

!  interpolation type used to interpolate bulk Richardson number
!  'linear'
!  'quadratic'
!  'cubic'
   character(len=32)                   ::  bulk_Ri_interp_method

!  interpolation type used to interpolate diff and visc at OBL_depth
!  'linear'
!  'quadratic'
!  'cubic'
!  'LMD94'
   character(len=32)                   ::  OBL_interp_method

!  matching technique between the boundary layer and the ocean interior
!  'SimpleShapes'      - Shape functions for both the gradient and nonlocal
!                        terms vanish at interface
!  'MatchGradient'     - Shape function for nonlocal term vanishes at
!                        interface, but gradient term matches interior
!                        values.
!  'MatchBoth'         - Shape functions for both the gradient and nonlocal
!                        term match interior values at interface
!  'ParabolicNonLocal' - Shape function for the nonlocal term is
!                        (1-sigma)^2, gradient term is sigma*(1-sigma)^2
   character(len=32)                   ::  match_technique
!
!  shear mixing scheme
!  'PP'   - Pacanowski-Philander, 1981
!  'KPP'  - KPP, Large et al, 1994
   character(len=32)                   ::  shear_mix_scheme_name

!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'post_init_cvmix'

!  allocate memory for variables defined in other modules
!
   LEVEL2 'allocation cvmix memory..'

   allocate(z_w(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (z_w)'
   z_w = _ZERO_

   allocate(z_r(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (z_r)'
   z_r = _ZERO_

   allocate(h_r(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (h_r)'
   h_r = _ZERO_

   LEVEL2 'done.'

!  report model parameters

   LEVEL2 '--------------------------------------------------------'
   LEVEL3 'You are using the CVMix turbulence model'
   LEVEL3 'with the following specifications:'
   LEVEL3 ''
   ! interior
   if (use_interior) then
      LEVEL3 'Interior mixing algorithm                  - active -'
      if (use_background) then
         LEVEL4 'backgroud mixing                       - active -'
         LEVEL4 ' - diffusivity: ', background_diffusivity
         LEVEL4 ' - viscosity: ', background_viscosity
      else
         LEVEL4 'background mixing                  - not active -'
      endif
      if (use_shear) then
         LEVEL4 'shear instability mixing               - active -'
         select case (shear_mix_scheme)
         case (CVMIX_SHEAR_PP)
            shear_mix_scheme_name = 'PP'
         case (CVMIX_SHEAR_KPP)
            shear_mix_scheme_name = 'KPP'
         case default
            STDERR 'Unsupported shear_mix_scheme: ', shear_mix_scheme
            stop 'init_cvmix'
         end select
         LEVEL4 ' - mixing scheme: ', trim(shear_mix_scheme_name)
         if (.not. use_background .and.                                &
            shear_mix_scheme==CVMIX_SHEAR_PP) then
            STDERR 'Pacanowski-Philander shear mixing scheme requires'
            STDERR 'use_background = .true.'
            stop 'init_cvmix'
         endif
      else
         LEVEL4 'shear instability mixing           - not active -'
      endif
      if (use_convection) then
         LEVEL4 'convective instability mixing          - active -'
         if (.not. convection_basedOnBVF) then
            STDERR 'Convection based on density is not yet supported'
            STDERR 'Please set convection_basedOnBVF = .true.'
            stop 'init_cvmix'
         endif
      else
         LEVEL4 'convective instability mixing      - not active -'
      endif
      if (use_double_diffusion) then
         LEVEL4 'double diffusive mixing                - active -'
      else
         LEVEL4 'double diffusive mixing            - not active -'
      endif
      if (use_tidal_mixing) then
         ! LEVEL4 'tidal mixing                           - active -'
         STDERR 'tidal mixing with CVMix is not yet supported'
         STDERR 'Please set use_tital_mixing = .false.'
         stop 'init_cvmix'
      else
         LEVEL4 'tidal mixing                       - not active -'
      endif
   else
      LEVEL3 'Interior mixing algorithm              - not active -'
      use_background = .false.
      use_shear = .false.
      use_convection = .false.
      use_double_diffusion = .false.
      use_tidal_mixing = .false.
   endif

   if (use_surface_layer) then
      LEVEL3 'Surface layer mixing algorithm             - active -'
      if (kpp_check_Ekman_length) then
         LEVEL4 'Clipping at Ekman scale                - active -'
      else
         LEVEL4 'Clipping at Ekman scale            - not active -'
      endif
      if (kpp_check_MonOb_length) then
         LEVEL4 'Clipping at Monin-Obukhov scale        - active -'
      else
         LEVEL4 'Clipping at Monin-Obukhov scale    - not active -'
      endif
      if (kpp_use_noDGat1) then
         LEVEL4 "Set shape function G'(1) = 0           - active -"
      else
         LEVEL4 "Set shape function G'(1) = 0       - not active -"
      endif
      LEVEL4 'Ri_c: ', kpp_Ri_c

      select case (kpp_match_technique)
      case (CVMIX_MATCH_SIMPLE)
         match_technique = 'SimpleShapes'
      case (CVMIX_MATCH_GRADIENT)
         match_technique = 'MatchGradient'
      case (CVMIX_MATCH_BOTH)
         match_technique = 'MatchBoth'
      case (CVMIX_MATCH_PARABOLIC)
         match_technique = 'ParabolicNonLocal'
      case default
         STDERR 'Unsupported kpp_match_technique: ', kpp_match_technique
         stop 'init_cvmix'
      end select

      LEVEL4 'Matching technique: ', trim(match_technique)
      select case (kpp_bulk_Ri_interp_type)
      case (CVMIX_INTERP_LINEAR)
         bulk_Ri_interp_method = 'linear'
      case (CVMIX_INTERP_QUADRATIC)
         bulk_Ri_interp_method = 'quadratic'
      case (CVMIX_INTERP_CUBIC)
         bulk_Ri_interp_method = 'cubic'
      case default
         STDERR 'Unsupported kpp_bulk_Ri_interp_type: ', kpp_bulk_Ri_interp_type
         stop 'init_cvmix'
      end select
      LEVEL4 'Interpolation type for Ri: ', trim(bulk_Ri_interp_method)

      select case (kpp_OBL_interp_type)
      case (CVMIX_INTERP_LINEAR)
         OBL_interp_method = 'linear'
      case (CVMIX_INTERP_QUADRATIC)
         OBL_interp_method = 'quadratic'
      case (CVMIX_INTERP_CUBIC)
         OBL_interp_method = 'cubic'
      case (CVMIX_INTERP_LMD94)
         OBL_interp_method = 'LMD94'
      case default
         STDERR 'Unsupported kpp_OBL_interp_type: ', kpp_OBL_interp_type
         stop 'init_cvmix'
      end select
      LEVEL4 'Interpolation type for diff and visc: ', trim(OBL_interp_method)

      select case (kpp_langmuir_method)
      case (CVMIX_LT_NOLANGMUIR)
         Langmuir_mixing_method = 'NONE'
         Langmuir_entrainment_method = 'NONE'
         LEVEL4 'Langmuir turbulence                - not active -'
      case (CVMIX_LT_LWF16)
         Langmuir_mixing_method = 'LWF16'
         Langmuir_entrainment_method = 'LWF16'
         LEVEL4 'Langmuir turbulence                    - active -'
         LEVEL4 ' - Langmuir mixing (Li et al., 2016)'
      case (CVMIX_LT_LF17)
         Langmuir_mixing_method = 'LWF16'
         Langmuir_entrainment_method = 'LF17'
         LEVEL4 'Langmuir turbulence                    - active -'
         LEVEL4 ' - Langmuir enhanced entrainment (Li and Fox-Kemper, 2017)'
      case (CVMIX_LT_RWH16)
         Langmuir_mixing_method = 'RWHGK16'
         Langmuir_entrainment_method = 'RWHGK16'
         LEVEL3 'Langmuir turbulence                    - active -'
         LEVEL4 ' - Langmuir mixing in hurricanes (Reichl et al. 2016)'
      case default
         STDERR 'Unsupported kpp_langmuir_method: ', kpp_langmuir_method
         stop 'init_cvmix'
      end select
   else
      LEVEL3 'Surface layer mixing algorithm         - not active -'
   endif

   LEVEL2 '--------------------------------------------------------'
   LEVEL2 ' '

   ! initialize background mixing
   if (use_background) then
      call cvmix_init_bkgnd(bkgnd_Tdiff = background_diffusivity,             &
                           bkgnd_Mdiff = background_viscosity)
   endif

   ! initialize shear mixing
   if (use_shear) then
      call cvmix_init_shear(mix_scheme=trim(shear_mix_scheme_name),           &
                            PP_nu_zero=shear_PP_nu_zero,                      &
                            PP_alpha=shear_PP_alpha,                          &
                            PP_exp=shear_PP_exp,                              &
                            KPP_nu_zero=shear_KPP_nu_zero,                    &
                            KPP_Ri_zero=shear_KPP_Ri_zero,                    &
                            KPP_exp=shear_KPP_exp)
   endif

   ! initialize convective mixing
   if (use_convection) then
      call cvmix_init_conv(convect_diff=convection_diffusivity,               &
                           convect_visc=convection_viscosity,                 &
                           lBruntVaisala=convection_basedOnBVF,               &
                           BVsqr_convect=convection_triggerBVF)
   endif

   ! initialize tidal mixing
   if (use_tidal_mixing) then
      ! use the default setting in CVMix
      call cvmix_init_tidal()
   endif

   ! initialize double diffusion
   if (use_double_diffusion) then
      ! use the default setting in CVMix
      call cvmix_init_ddiff(strat_param_max=2.55_rk,                          &
                            kappa_ddiff_s=1e-4_rk,                            &
                            ddiff_exp1=1.0_rk,                                &
                            ddiff_exp2=3.0_rk,                                &
                            mol_diff=1.5e-6_rk,                               &
                            kappa_ddiff_param1=0.909_rk,                      &
                            kappa_ddiff_param2=4.6_rk,                        &
                            kappa_ddiff_param3=-0.54_rk,                      &
                            diff_conv_type="MC76")
   endif

   ! initialize KPP
   if (use_kpp) then
      call cvmix_init_kpp(Ri_crit=kpp_Ri_c,                                   &
                          interp_type=trim(bulk_Ri_interp_method),            &
                          interp_type2=trim(OBL_interp_method),               &
                          MatchTechnique=trim(match_technique),               &
                          minVtsqr=_ZERO_,                                    &
                          lEkman=kpp_check_Ekman_length,                      &
                          lMonOb=kpp_check_MonOb_length,                      &
                          lnoDGat1=kpp_use_noDGat1,                           &
                          lenhanced_diff=kpp_use_enhanced_diff,               &
                          surf_layer_ext=kpp_surface_layer_extent,            &
                          langmuir_mixing_str=trim(langmuir_mixing_method),   &
                          langmuir_entrainment_str=                           &
                                    trim(langmuir_entrainment_method))
   endif

   ! initialize constants
   cvmix_g = g
   cvmix_rho0 = rho0
   cvmix_gorho0 = g/rho0

   ! initialize boundary layer
   zsbl = _ZERO_

   ! initialize CVMix variables
   call cvmix_put(CVmix_vars, 'nlev', nlev)
   call cvmix_put(CVmix_vars, 'max_nlev', nlev)
   call cvmix_put(CVmix_vars, 'OceanDepth', h0)
   call cvmix_put(CVmix_vars, 'zw_iface', z_w(nlev:0:-1))
   call cvmix_put(CVmix_vars, 'zt_cntr',  z_r(nlev:1:-1))
   call cvmix_put(CVmix_vars, 'Mdiff', _ZERO_)
   call cvmix_put(CVmix_vars, 'Tdiff', _ZERO_)
   call cvmix_put(CVmix_vars, 'Sdiff', _ZERO_)
   call cvmix_put(CVmix_vars, 'ShearRichardson_iface', _ZERO_)
   call cvmix_put(CVmix_vars, 'BulkRichardson_cntr', _ZERO_)
   call cvmix_put(CVmix_vars, 'SqrBuoyancyFreq_iface', _ZERO_)
   call cvmix_put(CVmix_vars, 'strat_param_num', _ZERO_)
   call cvmix_put(CVmix_vars, 'strat_param_denom', _ZERO_)

   ! TODO: the following variables need to be initialized if convection based on density is enabled <20200522, Qing Li> !
   ! call cvmix_put(CVmix_vars, 'WaterDensity_cntr', _ZERO_)
   ! call cvmix_put(CVmix_vars, 'AdiabWaterDensity_cntr', _ZERO_)

   return

 end subroutine post_init_cvmix
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Do KPP with CVMix
!
! !INTERFACE:
   subroutine do_cvmix(nlev,h0,h,rho,u,v,NN,NNT,NNS,SS,u_taus,         &
                       tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f,          &
                       EFactor,LaSL)
!
! !DESCRIPTION:
!  Do KPP with CVMix
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!  number of grid cells
   integer, intent(in)                           :: nlev

!  bathymetry (m)
   REALTYPE, intent(in)                          :: h0

!  thickness of grid cells (m)
   REALTYPE, intent(in)                          :: h(0:nlev)

!  potential density at grid centers (kg/m^3)
   REALTYPE, intent(in)                          :: rho(0:nlev)

!  velocity components at grid centers (m/s)
   REALTYPE, intent(in)                          :: u(0:nlev),v(0:nlev)

!  square of buoyancy frequency (1/s^2)
   REALTYPE, intent(in)                          :: NN(0:nlev)

!  square of buoyancy frequency caused by
!  temperature and salinity stratification
   REALTYPE, intent(in)                          :: NNT(0:nlev),NNS(0:nlev)

!  square of shear frequency (1/s^2)
   REALTYPE, intent(in)                          :: SS(0:nlev)

!  surface friction velocities (m/s)
   REALTYPE, intent(in)                          :: u_taus

!  surface temperature flux (K m/s) and
!  salinity flux (psu m/s) (negative for loss)
   REALTYPE, intent(in)                          :: tFlux,sFlux

!  surface buoyancy fluxes (m^2/s^3) due to
!  heat and salinity fluxes
   REALTYPE, intent(in)                          :: btFlux,bsFlux

!  radiative flux [ I(z)/(rho Cp) ] (K m/s)
!  and associated buoyancy flux (m^2/s^3)
   REALTYPE, intent(in)                          :: tRad(0:nlev),bRad(0:nlev)

!  Coriolis parameter (rad/s)
   REALTYPE, intent(in)                          :: f

!  Langmuir enhancement factor
   REALTYPE, intent(in)                          :: EFactor

!  Surface layer averaged Langmuir number to be passed in CVMix
!  for Langmuir enhanced entrainment
   REALTYPE, intent(in)                          :: LaSL

! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   integer                             :: k

!
!-----------------------------------------------------------------------
!BOC
!
!-----------------------------------------------------------------------
! Update model grid
!-----------------------------------------------------------------------

!  Compute distance between centers (between rho-points)
!  Note that h is the distance between faces (between w-points)
   do k=1,nlev-1
      h_r(k) = 0.5*(h(k)+ h(k+1))
   enddo

!  Compute position of interfaces (w-points)
   z_w(0) = - h0
   do k=1,nlev
      z_w(k) = z_w(k-1) + h(k)
   enddo

!  Compute position of centers (rho-points)
   z_r(1) = - h0 + 0.5*h(1)
   do k=2,nlev
      z_r(k) = z_r(k-1) + h_r(k-1)
   enddo

!-----------------------------------------------------------------------
! compute interior non-convective mixing
!-----------------------------------------------------------------------

   if (use_interior) then
      call interior_nonconv(nlev,NN,NNT,NNS,SS)
   else
      cvmix_num = _ZERO_
      cvmix_nuh = _ZERO_
      cvmix_nus = _ZERO_
   endif

!-----------------------------------------------------------------------
! compute surface boundary layer mixing
!-----------------------------------------------------------------------

   if (use_surface_layer) then
      call surface_layer(nlev,h,rho,u,v,NN,u_taus,                   &
                         tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f,      &
                         EFactor, LaSL)
   endif

!-----------------------------------------------------------------------
! compute interior convective mixing
!-----------------------------------------------------------------------

   if (use_interior) then
      call interior_conv(nlev,NN)
   endif

   return

 end subroutine do_cvmix
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Interior non-convective mixing
!
! !INTERFACE:
   subroutine interior_nonconv(nlev,NN,NNT,NNS,SS)
!
! !DESCRIPTION:
!  All interior mixing coefficients excluding convective mixing
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of grid cells
   integer, intent(in)                           :: nlev

!  square of buoyancy frequency (1/s^2)
   REALTYPE, intent(in)                          :: NN(0:nlev)

!  square of buoyancy frequencies caused by
!  temperature and salinity stratification
   REALTYPE, intent(in)                          :: NNT(0:nlev),NNS(0:nlev)

!  square of shear frequency (1/s^2)
   REALTYPE, intent(in)                          :: SS(0:nlev)

!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
!EOP
!
! !LOCAL VARIABLES:

   integer                      :: i, k
   REALTYPE                     :: cff,shear2
   REALTYPE                     :: nu_sx,nu_sxc
   REALTYPE                     :: iwm,iws
   REALTYPE                     :: drhoT,drhoS,Rrho,nu_dds,nu_ddt
   REALTYPE, dimension(0:nlev)  :: work

!
!-----------------------------------------------------------------------
!BOC
!
!  initialize turbulent viscosity and diffusivity
   CVmix_vars%Mdiff_iface = _ZERO_
   CVmix_vars%Tdiff_iface = _ZERO_

!-----------------------------------------------------------------------
! Background mixing
!-----------------------------------------------------------------------

   if (use_background) then
      ! CVMix subroutine for background mixing
      call cvmix_coeffs_bkgnd(CVmix_vars)

      ! update turbulent viscosity and diffusivity
      cvmix_num(0:nlev) = CVmix_vars%Mdiff_iface(nlev+1:1:-1)
      cvmix_nuh(0:nlev) = CVmix_vars%Tdiff_iface(nlev+1:1:-1)

      ! CVMix background only update Tdiff, so same background diffusivity
      ! for salinity
      cvmix_nus(:) = cvmix_nuh(:)
   endif

!-----------------------------------------------------------------------
! Compute gradient Richardson number
!-----------------------------------------------------------------------

!  Save Ri in a temporary array
   do k=1,nlev-1
      work(k) = NN(k)/(SS(k) + eps)
   enddo

   work(0)    = _ZERO_
   work(nlev) = _ZERO_

!  Smooth gradient Richardson number
   do i=1,shear_num_smooth_Ri
      do k=1,nlev-1
         cvmix_Rig(k) = 0.25*work(k-1) + 0.50*work(k) + 0.25*work(k+1)
      enddo
      work(1:nlev-1) = cvmix_Rig(1:nlev-1)
   enddo
   cvmix_Rig(0) = _ZERO_
   cvmix_Rig(nlev) = _ZERO_

!-----------------------------------------------------------------------
! Shear mixing
!-----------------------------------------------------------------------

   if (use_shear) then
      ! Fill the gradient Richardson number
      ! Note that arrays at the cell interface in CVMix have indices (1:nlev+1)
      ! from the surface to the bottom, whereas those in GOTM have indices (nlev:0)
      CVmix_vars%ShearRichardson_iface(1:nlev+1) = cvmix_Rig(nlev:0:-1)

      ! CVMix subroutine for shear mixing
      call cvmix_coeffs_shear(CVmix_vars)

      ! update turbulent viscosity and diffusivity
      cvmix_num(0:nlev) = cvmix_num(0:nlev) + CVmix_vars%Mdiff_iface(nlev+1:1:-1)
      cvmix_nuh(0:nlev) = cvmix_nuh(0:nlev) + CVmix_vars%Tdiff_iface(nlev+1:1:-1)
      ! CVMix shear only update Tdiff, so same shear diffusivity
      ! for salinity
      cvmix_nus(:) = cvmix_nuh(:)
   endif

!-----------------------------------------------------------------------
! Tidal mixing
!-----------------------------------------------------------------------

! TODO: implement tidal mixing <20200510, Qing Li> !

!-----------------------------------------------------------------------
! Double diffusion
!-----------------------------------------------------------------------

   if (use_double_diffusion) then
      ! fill the numerator and denominator of the stratification parameter
      CVmix_vars%strat_param_num(2:nlev)   =  NNT(nlev-1:1:-1)
      CVmix_vars%strat_param_denom(2:nlev) = -NNS(nlev-1:1:-1)

      ! CVMix subroutine for double diffusion
      call cvmix_coeffs_ddiff(CVmix_vars)

      ! update turbulent diffusivity for temperature and salinity
      cvmix_nuh(0:nlev) = cvmix_nuh(0:nlev) + CVmix_vars%Tdiff_iface(nlev+1:1:-1)
      cvmix_nus(0:nlev) = cvmix_nus(0:nlev) + CVmix_vars%Sdiff_iface(nlev+1:1:-1)
   endif

   return

 end subroutine interior_nonconv
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Interior convective mixing
!
! !INTERFACE:
   subroutine interior_conv(nlev,NN)
!
! !DESCRIPTION:
!  Interior mixing due to convection
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of grid cells
   integer, intent(in)                           :: nlev

!  square of buoyancy frequency (1/s^2)
   REALTYPE, intent(in)                          :: NN(0:nlev)

!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
!EOP
!
! !LOCAL VARIABLES:

   integer                      :: i, k
   REALTYPE, dimension(0:nlev)  :: work

!
!-----------------------------------------------------------------------
!BOC
!
!  return if not use convection
   if (.not. use_convection) return

!-----------------------------------------------------------------------
! convective mixing
!-----------------------------------------------------------------------

   ! fill the squared buoyancy frequency
   CVmix_vars%SqrBuoyancyFreq_iface(1:nlev+1) = NN(nlev:0:-1)

   ! fill the water density and the water density after adiabatic
   ! displacement to the level below where the water actually is
   ! TODO: convection based on density is not yet supported, which requires the displaced density at cell centers <20200522, Qing Li> !
   ! CVmix_vars%WaterDensity_cntr(1:nlev) = rho(nlev:1:-1)
   ! CVmix_vars%AdiabWaterDensity_cntr(1:nlev) =

   ! CVMix subroutine for convective mixing
   call cvmix_coeffs_conv(CVmix_vars)

   ! update turbulent viscosity and diffusivity
   cvmix_num(0:nlev) = cvmix_num(0:nlev) + CVmix_vars%Mdiff_iface(nlev+1:1:-1)
   cvmix_nuh(0:nlev) = cvmix_nuh(0:nlev) + CVmix_vars%Tdiff_iface(nlev+1:1:-1)
   ! CVMix shear only update Tdiff, so same shear diffusivity
   ! for salinity
   cvmix_nus(:) = cvmix_nuh(:)

   return

 end subroutine interior_conv
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute turbulence in the surface layer with CVMix
!
! !INTERFACE:
   subroutine surface_layer(nlev,h,rho,u,v,NN,u_taus,                  &
                            tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f,     &
                            EFactor,LaSL)
!
! !DESCRIPTION:
! In this routine all computations related to turbulence in the surface layer
! are performed. CVMix library is used.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!  number of grid cells
   integer, intent(in)                           :: nlev

!  thickness of grid cells (m)
   REALTYPE, intent(in)                          :: h(0:nlev)

!  potential density at grid centers (kg/m^3)
   REALTYPE, intent(in)                          :: rho(0:nlev)

!  velocity components at grid centers (m/s)
   REALTYPE, intent(in)                          :: u(0:nlev),v(0:nlev)

!  square of buoyancy frequency (1/s^2)
   REALTYPE, intent(in)                          :: NN(0:nlev)

!  surface friction velocities (m/s)
   REALTYPE, intent(in)                          :: u_taus

!  surface temperature flux (K m/s) and
!  salinity flux (sal m/s) (negative for loss)
   REALTYPE, intent(in)                          :: tFlux,sFlux

!  surface buoyancy fluxes (m^2/s^3) due to
!  heat and salinity fluxes
   REALTYPE, intent(in)                          :: btFlux,bsFlux

!  radiative flux [ I(z)/(rho Cp) ] (K m/s)
!  and associated buoyancy flux (m^2/s^3)
   REALTYPE, intent(in)                          :: tRad(0:nlev),bRad(0:nlev)

!  Coriolis parameter (rad/s)
   REALTYPE, intent(in)                          :: f

!  Langmuir enhancement factor
   REALTYPE, intent(in)                          :: EFactor

!  Surface layer averaged Langmuir number to be passed in CVMix
!  for Langmuir enhanced entrainment
   REALTYPE, intent(in)                          :: LaSL


!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   integer                      :: k,ksbl
   integer                      :: kk,kref,kp1

   REALTYPE                     :: Bo,Bfsfc
   REALTYPE                     :: tRadSrf
   REALTYPE                     :: bRadSrf,bRadSbl
   REALTYPE                     :: wm, ws
   REALTYPE                     :: depth
   REALTYPE                     :: Rk, Rref
   REALTYPE                     :: Uk, Uref, Vk, Vref
   REALTYPE                     :: bRad_cntr
   REALTYPE                     :: NN_max

   REALTYPE, dimension (0:nlev) :: Bflux
   REALTYPE, dimension (0:nlev) :: RiBulk

!  Thickness of surface layer
   REALTYPE                     :: surfthick

!-----------------------------------------------------------------------
!BOC
!
!-----------------------------------------------------------------------
!  Compute total buoyancy flux at W-points.
!  Bo is negative, if heat is lost or salinity is gained (convection).
!  It does not include the short wave radiative flux at the surface.
!-----------------------------------------------------------------------
!
    tRadSrf   =   tRad(nlev)
    bRadSrf   =   bRad(nlev)

!  surface buoyancy flux (negative for buoyancy loss)
   Bo         = btFlux + bsFlux

!  include effect of short wave radiation
!  prepare non-local fluxes
!  Bflux(k) is the total buoyancy flux above the level z_r(k)

   do k = 1,nlev
      bRad_cntr = 0.5*(bRad(k)+bRad(k-1))
      Bflux(k)  = Bo  + ( bRadSrf - bRad_cntr )
   enddo

!-----------------------------------------------------------------------
!  Update grid in CVMix
!-----------------------------------------------------------------------

!  CVMix assumes that z indices increase with depth (surface to bottom)
   CVmix_vars%zw_iface = z_w(nlev:0:-1)
   CVmix_vars%zt_cntr = z_r(nlev:1:-1)

!-----------------------------------------------------------------------
!  Compute potential density and velocity components surface reference
!  values.
!-----------------------------------------------------------------------

!  initialize the reference potential density and velocity
   Rref = rho(nlev)
   Uref =   u(nlev)
   Vref =   v(nlev)

!-----------------------------------------------------------------------
!  Compute bulk Richardson number at grid cell center
!-----------------------------------------------------------------------

   RiBulk = _ZERO_

   do k=nlev-1,2,-1
      ! do the calculation at grid cell center
      kp1 = k+1
      depth = z_w(nlev)-z_r(kp1)
      call cvmix_kpp_compute_turbulent_scales(_ONE_,       &
          depth,Bflux(kp1),u_taus,                         &
          w_s=ws,w_m=wm)

      ! update potential density and velocity components surface
      ! reference values with the surface layer averaged values
      ! determine which layer contains surface layer
      surfthick = kpp_surface_layer_extent*depth
      do kk = nlev,k,-1
         if (z_w(nlev)-z_w(kk-1) .ge. surfthick) then
            kref = kk
            exit
         end if
      end do
      if (kref < nlev) then
         Rref = rho(kref)*(surfthick+z_w(kref))
         Uref =   u(kref)*(surfthick+z_w(kref))
         Vref =   v(kref)*(surfthick+z_w(kref))
         do kk = nlev,kref+1,-1
            Rref = Rref + rho(kk)*h(kk)
            Uref = Uref +   u(kk)*h(kk)
            Vref = Vref +   v(kk)*h(kk)
         end do
         Rref = Rref/surfthick
         Uref = Uref/surfthick
         Vref = Vref/surfthick
      end if
      ! use the values at grid centers
      Rk = rho(kp1)
      Uk =   u(kp1)
      Vk =   v(kp1)
      ! compute the Bulk Richardson number
      ! use the max of NN at the upper and lower interface following Van Roekel et al., 2019
      NN_max = max(NN(kp1), NN(k))
      RiBulk(kp1:kp1) = cvmix_kpp_compute_bulk_Richardson(           &
                zt_cntr = (/-depth/),                                &
                delta_buoy_cntr = (/-cvmix_gorho0*(Rref-Rk)/),       &
                delta_Vsqr_cntr = (/(Uref-Uk)**2+(Vref-Vk)**2/),     &
                ws_cntr = (/ws/),                                    &
                Nsqr_iface = (/NN_max, NN_max/),                     &
                EFactor = EFactor,                                   &
                LaSL = LaSL,                                         &
                bfsfc = Bflux(kp1),                                  &
                ustar = u_taus)

   enddo  ! inner grid faces

!-----------------------------------------------------------------------
!  Compute total buoyancy flux at surface boundary layer depth
!-----------------------------------------------------------------------
!  This calculation is based on the boundary layer depth in the previous
!  time step

!  first find old boundary layer index "ksbl".
   ksbl=1
   do k=nlev,2,-1
      if ((ksbl.eq.1).and.(z_w(k-1).lt.zsbl)) then
         ksbl = k
      endif
   enddo

   bRadSbl = ( bRad(ksbl-1)*(z_w(ksbl)-zsbl) +                          &
               bRad(ksbl  )*(zsbl-z_w(ksbl-1) ) )/ h(ksbl)

   Bfsfc   = Bo + (bRadSrf - bRadSbl)

!-----------------------------------------------------------------------
! Find the boundary layer depth
!-----------------------------------------------------------------------

   CVmix_vars%BulkRichardson_cntr = RiBulk(nlev:1:-1)
   CVmix_vars%SurfaceFriction = u_taus
   CVmix_vars%SurfaceBuoyancyForcing = Bfsfc
   CVmix_vars%Coriolis = f

   call cvmix_kpp_compute_OBL_depth(CVmix_vars)

   ! CVMix returns a BoundaryLayerDepth > 0
   zsbl = -CVmix_vars%BoundaryLayerDepth

!-----------------------------------------------------------------------
!  Update surface buoyancy flux in the new surface boundary layer
!-----------------------------------------------------------------------

   ksbl=1
   do k=nlev,2,-1
      if ((ksbl.eq.1).and.(z_w(k-1).lt.zsbl)) then
         ksbl = k
      endif
   enddo

   bRadSbl = ( bRad(ksbl-1)*(z_w(ksbl)-zsbl) +                          &
               bRad(ksbl  )*(zsbl-z_w(ksbl-1) ) )/ h(ksbl)

   Bfsfc   = Bo + (bRadSrf - bRadSbl)

   CVmix_vars%SurfaceBuoyancyForcing = Bfsfc

!-----------------------------------------------------------------------
!  Compute the mixing coefficients within the surface boundary layer
!-----------------------------------------------------------------------

   ! set Langmuir enhancement factor
   CVmix_vars%LangmuirEnhancementFactor = EFactor

   ! Note that arrays at the cell interface in CVMix have indices (1:nlev+1)
   ! from the surface to the bottom, whereas those in GOTM have indices (nlev:0)
   CVmix_vars%Mdiff_iface(1:nlev+1) = cvmix_num(nlev:0:-1)
   CVmix_vars%Tdiff_iface(1:nlev+1) = cvmix_nuh(nlev:0:-1)
   CVmix_vars%Sdiff_iface(1:nlev+1) = cvmix_nus(nlev:0:-1)

   call cvmix_coeffs_kpp(CVmix_vars)

   cvmix_num(0:nlev) = CVmix_vars%Mdiff_iface(nlev+1:1:-1)
   cvmix_nuh(0:nlev) = CVmix_vars%Tdiff_iface(nlev+1:1:-1)
   cvmix_nus(0:nlev) = CVmix_vars%Sdiff_iface(nlev+1:1:-1)
   cvmix_gamh(0:nlev) = CVmix_vars%kpp_Tnonlocal_iface(nlev+1:1:-1)
   cvmix_gams(0:nlev) = CVmix_vars%kpp_Snonlocal_iface(nlev+1:1:-1)

   ! Note that kpp_transport_iface in CVMix is the value of K_x*gamma_x/flux_x,
   ! in other words, the user must multiply this value by either the freshwater
   ! flux or the penetrative shortwave heat flux to get the nonlocal fluxes

   ! include the effect of penetrating solar radiation
   tRadSrf   =   tRad(nlev)
   do k = 0,nlev
      cvmix_gamh(k)   = -cvmix_gamh(k)*(tFlux+tRadSrf-tRad(k))
      cvmix_gams(k)   = -cvmix_gams(k)*sFlux
   enddo

!  no non-local fluxes at top and bottom
   cvmix_gamh(0   ) = _ZERO_
   cvmix_gams(0   ) = _ZERO_
   cvmix_gamh(nlev) = _ZERO_
   cvmix_gams(nlev) = _ZERO_

   return

 end subroutine surface_layer
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Clean up the gotm_cvmix module
!
! !INTERFACE:
   subroutine clean_cvmix()
!
! !DESCRIPTION:
!  De-allocate all memory allocated in init\_cvmix().
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!   Adapted for CVMix: Qing Li
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'clean_cvmix'

   LEVEL2 'de-allocating CVMix memory ...'
   if (allocated(z_w)) deallocate(z_w)
   if (allocated(z_r)) deallocate(z_r)
   if (allocated(h_r)) deallocate(h_r)

   LEVEL2 'done.'

   return

 end subroutine clean_cvmix
!EOC

 end module gotm_cvmix

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

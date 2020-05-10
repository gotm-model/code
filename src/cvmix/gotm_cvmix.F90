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

   IMPLICIT NONE

!  default: all is private.
   private

! !PUBLIC MEMBER FUNCTIONS:
   public init_cvmix, do_cvmix, clean_cvmix
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

! z-position of bottom boundary layer depth
  REALTYPE, public                       ::    zbbl

! !DEFINED PARAMETERS:
!
!  method of Langmuir turbulence parameterization
   integer, parameter                    ::    CVMIX_LT_NOLANGMUIR = 0
   integer, parameter                    ::    CVMIX_LT_LWF16 = 1
   integer, parameter                    ::    CVMIX_LT_LF17 = 2
   integer, parameter                    ::    CVMIX_LT_RWHGK16 = 3
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

!  compute surface and bottom boundary layer mixing and internal mixing
   logical                               ::    use_surface_layer,      &
                                               use_bottom_layer,       &
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
!  (1)  0  => no Langmuir turbulence parameterization
!  (2)  1  => Langmuir mixing following Li et al., 2016
!  (3)  2  => Langmuir enhanced entrainment following Li & Fox-Kemper, 2017
!  (4)  3  => Langmuir turbulence in hurricanes following Reichl et al., 2016
   integer                               ::    kpp_langmuir_method

!  interpolation type used to interpolate bulk Richardson number
!  options are
!  (1)   linear
!  (2)   quadratic
!  (3)   cubic
   character(len=32)                     ::    kpp_bulk_Ri_interp_type

!  interpolation type used to interpolate diff and visc at OBL_depth
!  options are
!  (1)   linear
!  (2)   quadratic
!  (3)   cubic
!  (4)   LMD94
   character(len=32)                     ::    kpp_OBL_interp_type

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
   character(len=32)                     ::    kpp_match_technique

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
   character(len=32)                     ::    shear_mix_scheme

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
!   - exponent of unitless factor of diffusities
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

!  turbulent diffusivities
!  of momentum, temperature, salinity
   REALTYPE, dimension(:), allocatable   ::    cvmix_num
   REALTYPE, dimension(:), allocatable   ::    cvmix_nuh
   REALTYPE, dimension(:), allocatable   ::    cvmix_nus

!  non-local fluxes of momentum
   REALTYPE, dimension(:), allocatable   ::    cvmix_gamu,             &
                                               cvmix_gamv

!  non-local fluxes of buoyancy, temperature, salinity
   REALTYPE, dimension(:), allocatable   ::    cvmix_gamb,             &
                                               cvmix_gamh,             &
                                               cvmix_gams

!  gradient Richardson number
   REALTYPE, dimension(:), allocatable   ::    cvmix_Rig

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
                        use_bottom_layer,            &
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
   use_bottom_layer = .false.
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
   kpp_match_technique = 'SimpleShapes'
   kpp_bulk_Ri_interp_type = 'quadratic'
   kpp_OBL_interp_type = 'LMD94'
   ! background
   use_background = .false.
   background_diffusivity = 1.0e-5
   background_viscosity = 1.0e-4
   ! shear
   use_shear = .false.
   shear_num_smooth_Ri = 1
   shear_mix_scheme = 'KPP'
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
   class (type_settings), pointer      :: twig
   integer, parameter                  :: rk = kind(_ONE_)
   integer                             :: rc
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_cvmix_yaml'

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

!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'post_init_cvmix'

!  allocate memory for variables defined in other modules
!
   LEVEL2 'allocation cvmix memory..'
   allocate(cvmix_num(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (cvmix_num)'
   cvmix_num = _ZERO_

   allocate(cvmix_nuh(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (cvmix_num)'
   cvmix_nuh = _ZERO_

   allocate(cvmix_nus(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (cvmix_nus)'
   cvmix_nus = _ZERO_

   allocate(cvmix_gamu(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (cvmix_gamu)'
   cvmix_gamu = _ZERO_

   allocate(cvmix_gamv(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (cvmix_gamv)'
   cvmix_gamv = _ZERO_

   allocate(cvmix_gamh(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (cvmix_gamh)'
   cvmix_gamh = _ZERO_

   allocate(cvmix_gams(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (cvmix_gams)'
   cvmix_gams = _ZERO_

   allocate(cvmix_Rig(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (cvmix_Rig)'
   cvmix_Rig = _ZERO_

   allocate(z_w(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (z_w)'
   z_w = _ZERO_

   allocate(z_r(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (z_r)'
   z_r = _ZERO_

   allocate(h_r(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_cvmix: Error allocating (h_r)'
   h_r = _ZERO_

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
         LEVEL4 ' - mixing scheme: ', trim(shear_mix_scheme)
         if (.not. use_background .and. trim(shear_mix_scheme)=='PP') then
            STDERR 'Pacanowski-Philander shear mixing scheme requires'
            STDERR 'use_background = .true.'
            stop 'init_cvmix'
         endif
      else
         LEVEL4 'shear instability mixing           - not active -'
      endif
      if (use_convection) then
         LEVEL4 'convective instability mixing          - active -'
      else
         LEVEL4 'convective instability mixing      - not active -'
      endif
      if (use_double_diffusion) then
         LEVEL4 'double diffusive mixing                - active -'
      else
         LEVEL4 'double diffusive mixing            - not active -'
      endif
      if (use_tidal_mixing) then
         LEVEL4 'tidal mixing                           - active -'
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
      LEVEL4 'Matching technique: ', trim(kpp_match_technique)
      LEVEL4 'Interpolation type for Ri: ', trim(kpp_bulk_Ri_interp_type)
      LEVEL4 'Interpolation type for diff and visc: ', trim(kpp_OBL_interp_type)
      LEVEL4 'Ri_c: ', kpp_Ri_c

   !  message of Langmuir turbulence parameterization
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
      case (CVMIX_LT_RWHGK16)
         Langmuir_mixing_method = 'RWHGK16'
         Langmuir_entrainment_method = 'RWHGK16'
         LEVEL3 'Langmuir turbulence                    - active -'
         LEVEL4 ' - Langmuir mixing in hurricanes (Reichl et al. 2016)'
      case default
         STDERR 'Unsupported langmuir_method: ', kpp_langmuir_method
         stop 'init_cvmix'
      end select
   else
      LEVEL3 'Surface layer mixing algorithm         - not active -'
   endif

   if (use_bottom_layer) then
      LEVEL3 'Bottom layer mixing algorithm              - active -'
      LEVEL4 '(Same parameters as surface layer mixing)'
   else
      LEVEL3 'Bottom layer mixing algorithm          - not active -'
   endif

   LEVEL2 '--------------------------------------------------------'

   ! initialize background mixing
   if (use_background) then
      call cvmix_init_bkgnd(bkgnd_Tdiff = background_diffusivity,             &
                           bkgnd_Mdiff = background_viscosity)
   endif

   ! initialize shear mixing
   if (use_shear) then
      call cvmix_init_shear(mix_scheme=trim(shear_mix_scheme),                &
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
      call cvmix_init_ddiff()
   endif

   ! initialize KPP
   if (use_kpp) then
      call cvmix_init_kpp(Ri_crit=kpp_Ri_c,                                   &
                          interp_type=trim(kpp_bulk_Ri_interp_type),          &
                          interp_type2=trim(kpp_OBL_interp_type),             &
                          MatchTechnique=trim(kpp_match_technique),           &
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

   ! initialize CVMix variables
   call cvmix_put(CVmix_vars, 'nlev', nlev)
   call cvmix_put(CVmix_vars, 'max_nlev', nlev)
   call cvmix_put(CVmix_vars, 'OceanDepth', h0)
   call cvmix_put(CVmix_vars, 'Mdiff', cvmix_num)
   call cvmix_put(CVmix_vars, 'Tdiff', cvmix_nuh)
   call cvmix_put(CVmix_vars, 'Sdiff', cvmix_nus)
   call cvmix_put(CVmix_vars, 'Gravity', cvmix_g)

   LEVEL1 'done.'

   return

 end subroutine post_init_cvmix
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Do KPP with CVMix
!
! !INTERFACE:
   subroutine do_cvmix(nlev,h0,h,rho,u,v,NN,NNT,NNS,SS,u_taus,u_taub,  &
                     tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f,EFactor,LaSL)
!
! !DESCRIPTION:
!  TODO
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!  number of grid cells
   integer                                       :: nlev

!  bathymetry (m)
   REALTYPE                                      :: h0

!  thickness of grid cells (m)
   REALTYPE                                      :: h(0:nlev)

!  potential density at grid centers (kg/m^3)
   REALTYPE                                      :: rho(0:nlev)

!  velocity components at grid centers (m/s)
   REALTYPE                                      :: u(0:nlev),v(0:nlev)

!  square of buoyancy frequency (1/s^2)
   REALTYPE                                      :: NN(0:nlev)

!  square of buoyancy frequency caused by
!  temperature and salinity stratification
   REALTYPE                                      :: NNT(0:nlev),NNS(0:nlev)

!  square of shear frequency (1/s^2)
   REALTYPE                                      :: SS(0:nlev)

!  surface and bottom friction velocities (m/s)
   REALTYPE                                      :: u_taus,u_taub

!  surface temperature flux (K m/s) and
!  salinity flux (psu m/s) (negative for loss)
   REALTYPE                                      :: tFlux,sFlux

!  surface buoyancy fluxes (m^2/s^3) due to
!  heat and salinity fluxes
   REALTYPE                                      :: btFlux,bsFlux

!  radiative flux [ I(z)/(rho Cp) ] (K m/s)
!  and associated buoyancy flux (m^2/s^3)
   REALTYPE                                      :: tRad(0:nlev),bRad(0:nlev)

!  Coriolis parameter (rad/s)
   REALTYPE                                      :: f

!  Langmuir enhancement factor
   REALTYPE                                      :: EFactor

!  Surface layer averaged Langmuir number to be passed in CVMix
!  for Langmuir enhanced entrainment
   REALTYPE                                      :: LaSL

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
! compute interior mixing
!-----------------------------------------------------------------------

   if (use_interior) then
      call interior(nlev,NN,NNT,NNS,SS)
   else
      cvmix_num = _ZERO_
      cvmix_nuh = _ZERO_
      cvmix_nus = _ZERO_
   endif

!-----------------------------------------------------------------------
! compute surface boundary layer mixing
!-----------------------------------------------------------------------

   if (use_surface_layer) then
      call surface_layer(nlev,h,rho,u,v,NN,u_taus,u_taub,            &
                         tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f,      &
                         EFactor, LaSL)
   endif

!-----------------------------------------------------------------------
! compute bottom boundary layer mixing
!-----------------------------------------------------------------------

   if (use_bottom_layer) then
      call bottom_layer(nlev,h,rho,u,v,NN,u_taus,u_taub,             &
                        _ZERO_,_ZERO_,_ZERO_,_ZERO_,tRad,bRad,f)
   endif

   return

 end subroutine do_cvmix
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute interior fluxes
!
! !INTERFACE:
   subroutine interior(nlev,NN,NNT,NNS,SS)
!
! !DESCRIPTION:
!  TODO
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of grid cells
   integer                                       :: nlev

!  square of buoyancy frequency (1/s^2)
   REALTYPE                                      :: NN(0:nlev)

!  square of buoyancy frequencies caused by
!  temperature and salinity stratification
   REALTYPE                                      :: NNT(0:nlev),NNS(0:nlev)

!  square of shear frequency (1/s^2)
   REALTYPE                                      :: SS(0:nlev)

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

      ! same background diffusivity for salinity
      cvmix_nus = cvmix_nuh
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
      cvmix_nus(0:nlev) = cvmix_nus(0:nlev) + CVmix_vars%Tdiff_iface(nlev+1:1:-1)
   endif

!-----------------------------------------------------------------------
! convective mixing
!-----------------------------------------------------------------------

   ! call cvmix_coeffs_conv(CVmix_vars)


!-----------------------------------------------------------------------
! Double diffusion
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
! Tidal mixing
!-----------------------------------------------------------------------

   return

 end subroutine interior
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute turbulence in the surface layer with CVMix
!
! !INTERFACE:
   subroutine surface_layer(nlev,h,rho,u,v,NN,u_taus,u_taub,           &
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
   integer                                       :: nlev

!  thickness of grid cells (m)
   REALTYPE                                      :: h(0:nlev)

!  potential density at grid centers (kg/m^3)
   REALTYPE                                      :: rho(0:nlev)

!  velocity components at grid centers (m/s)
   REALTYPE                                      :: u(0:nlev),v(0:nlev)

!  square of buoyancy frequency (1/s^2)
   REALTYPE                                      :: NN(0:nlev)

!  surface and bottom friction velocities (m/s)
   REALTYPE                                      :: u_taus,u_taub

!  surface temperature flux (K m/s) and
!  salinity flux (sal m/s) (negative for loss)
   REALTYPE                                      :: tFlux,sFlux

!  surface buoyancy fluxes (m^2/s^3) due to
!  heat and salinity fluxes
   REALTYPE                                      :: btFlux,bsFlux

!  radiative flux [ I(z)/(rho Cp) ] (K m/s)
!  and associated buoyancy flux (m^2/s^3)
   REALTYPE                                      :: tRad(0:nlev),bRad(0:nlev)

!  Coriolis parameter (rad/s)
   REALTYPE                                      :: f

!  Langmuir enhancement factor
   REALTYPE                                      :: EFactor

!  Surface layer averaged Langmuir number to be passed in CVMix
!  for Langmuir enhanced entrainment
   REALTYPE                                      :: LaSL


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
   call cvmix_put(CVmix_vars, 'zw_iface', z_w(nlev:0:-1))
   call cvmix_put(CVmix_vars, 'zt_cntr',  z_r(nlev:1:-1))

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
      ! TODO: make sure this choice of NN following Van Roekel et al., 2019 is documented.<20200509, Qing Li> !
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

   call cvmix_put(CVmix_vars, 'BulkRichardson_cntr', RiBulk(nlev:1:-1))
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
! !IROUTINE: Compute turbulence in the bottom layer \label{sec:kppBottom}
!
! !INTERFACE:
   subroutine bottom_layer(nlev,h,rho,u,v,NN,u_taus,u_taub, &
                            tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f)
!
! !DESCRIPTION:
! TODO

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of grid cells
   integer                                       :: nlev

!  thickness of grid cells (m)
   REALTYPE                                      :: h(0:nlev)

!  potential density at grid centers (kg/m^3)
   REALTYPE                                      :: rho(0:nlev)

!  velocity components at grid centers (m/s)
   REALTYPE                                      :: u(0:nlev),v(0:nlev)

!  square of buoyancy frequency (1/s^2)
   REALTYPE                                      :: NN(0:nlev)

!  surface and bottom friction velocities (m/s)
   REALTYPE                                      :: u_taus,u_taub

!  bottom temperature flux (K m/s) and
!  salinity flux (sal m/s) (negative for loss)
   REALTYPE                                      :: tFlux,sFlux

!  bottom buoyancy fluxes (m^2/s^3) due to
!  heat and salinity fluxes
   REALTYPE                                      :: btFlux,bsFlux

!  radiative flux [ I(z)/(rho Cp) ] (K m/s)
!  and associated buoyancy flux (m^2/s^3)
   REALTYPE                                      :: tRad(0:nlev),bRad(0:nlev)

!  Coriolis parameter (rad/s)
   REALTYPE                                      :: f

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

!-----------------------------------------------------------------------
!BOC
!

   return

 end subroutine bottom_layer
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
   if (allocated(cvmix_num))  deallocate(cvmix_num)
   if (allocated(cvmix_nuh))  deallocate(cvmix_nuh)
   if (allocated(cvmix_nus))  deallocate(cvmix_nus)
   if (allocated(cvmix_gamu)) deallocate(cvmix_gamu)
   if (allocated(cvmix_gamv)) deallocate(cvmix_gamv)
   if (allocated(cvmix_gamh)) deallocate(cvmix_gamh)
   if (allocated(cvmix_gams)) deallocate(cvmix_gams)
   if (allocated(cvmix_Rig))  deallocate(cvmix_Rig)
   if (allocated(z_w)) deallocate(z_w)
   if (allocated(z_r)) deallocate(z_r)
   if (allocated(h_r)) deallocate(h_r)

   return

 end subroutine clean_cvmix
!EOC

 end module gotm_cvmix

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

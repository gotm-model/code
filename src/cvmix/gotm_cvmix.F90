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
! !DEFINED PARAMETERS:
!
!  method of Langmuir turbulence parameterization
   integer, parameter                    ::  CVMIX_LT_NOLANGMUIR = 0
   integer, parameter                    ::  CVMIX_LT_LWF16 = 1
   integer, parameter                    ::  CVMIX_LT_LF17 = 2
   integer, parameter                    ::  CVMIX_LT_RWHGK16 = 3
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
! !LOCAL VARIABLES:
!
!  compute surface and bottom boundary layer mixing and internal mixing
   logical                               ::    use_surface_layer,      &
                                               use_bottom_layer,       &
                                               use_interior
!
!  flags for different components of CVMix
   logical                               ::    use_kpp,                &
                                               use_background,         &
                                               use_shear,              &
                                               use_convection,         &
                                               use_tidal_mixing,       &
                                               use_double_diffusion
!
!  G'(1) = 0 (shape function) if true, otherwise compute G'(1) as in LMD94
   logical                               ::    use_noDGat1
!
!  limit the OBL by the Ekman depth / Monin-Obukhov length if true
   logical                               ::    check_Ekman_length,     &
                                               check_MonOb_length
!  enhance diffusivity at OBL
   logical                               ::    use_enhanced_diff
!
!  method to parameterize the effects of Langmuir turbulence
!  options are
!  (1)  0  => no Langmuir turbulence parameterization
!  (2)  1  => Langmuir mixing following Li et al., 2016
!  (3)  2  => Langmuir enhanced entrainment following Li & Fox-Kemper, 2017
!  (4)  3  => Langmuir turbulence in hurricanes following Reichl et al., 2016
   integer                               ::    langmuir_method
!
!  interpolation type used to interpolate bulk Richardson number
!  options are
!  (1)   linear
!  (2)   quadratic
!  (3)   cubic
   character(len=32)                     ::    bulk_Ri_interp_type
!
!  interpolation type used to interpolate diff and visc at OBL_depth
!  options are
!  (1)   linear
!  (2)   quadratic
!  (3)   cubic
!  (4)   LMD94
   character(len=32)                     ::    OBL_interp_type
!
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
   character(len=32)                     ::    match_technique
!
!  surface layer extent
   REALTYPE                              ::    surface_layer_extent
!
!  critical Richardson number
   REALTYPE                              ::    Ri_c
!
!  background diffusivity and viscosity
   REALTYPE                              ::    background_diffusivity, &
                                               background_viscosity
!
!  shear mixing scheme
!  options are
!  (1) PP   => Pacanowski-Philander, 1981
!  (2) KPP  => KPP, Large et al, 1994
   character(len=32)                     ::    shear_mix_scheme
!
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
!
!  turbulent diffusivities
!  of momentum, temperature, salinity
   REALTYPE, dimension(:), allocatable   ::    cvmix_num
   REALTYPE, dimension(:), allocatable   ::    cvmix_nuh
   REALTYPE, dimension(:), allocatable   ::    cvmix_nus
!
!  non-local fluxes of momentum
   REALTYPE, dimension(:), allocatable   ::    cvmix_gamu,             &
                                               cvmix_gamv
!
!  non-local fluxes of buoyancy, temperature, salinity
   REALTYPE, dimension(:), allocatable   ::    cvmix_gamb,             &
                                               cvmix_gamh,             &
                                               cvmix_gams
!
!  positions of grid faces and centers
   REALTYPE, dimension(:), allocatable   ::    z_w, z_r

!  distance between centers
   REALTYPE, dimension(:), allocatable   ::    h_r
!
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
   subroutine init_cvmix_nml(namlst, fn, nlev, h0)
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
!
!  filename containing namelist
   character(len=*), intent(in)        :: fn
!  number of levels and water depth, optional
!  if present, call post_init_cvmix() after reading the namelist
   integer, optional, intent(in)       :: nlev
   REALTYPE, optional, intent(in)      :: h0
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
                        langmuir_method,             &
                        Ri_c,                        &
                        surface_layer_extent,        &
                        check_Ekman_length,          &
                        check_MonOb_length,          &
                        use_enhanced_diff,           &
                        use_noDGat1,                 &
                        match_technique,             &
                        bulk_Ri_interp_type,         &
                        OBL_interp_type,             &
                        use_background,              &
                        background_diffusivity,      &
                        background_viscosity,        &
                        use_shear,                   &
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
   langmuir_method = 0
   surface_layer_extent = 0.1
   Ri_c = 0.3
   check_Ekman_length = .false.
   check_MonOb_length = .false.
   use_enhanced_diff = .true.
   use_noDGat1 = .true.
   match_technique = 'SimpleShapes'
   bulk_Ri_interp_type = 'quadratic'
   OBL_interp_type = 'LMD94'
   ! background
   use_background = .false.
   background_diffusivity = 1.0e-5
   background_viscosity = 1.0e-4
   ! shear
   use_shear = .false.
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
   if (present(nlev) .and. present(h0)) call post_init_cvmix(nlev, h0)

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
   subroutine post_init_cvmix(nlev, h0)
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
!
!  bathymetry (m)
   REALTYPE, intent(in)                :: h0
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
! !LOCAL VARIABLES:
   integer                             :: k
   integer                             :: rc
!
!  Langmuir mixing method: scheme to enhance the velocity scale used
!  in turbulent mixing coefficient calculation
!  'NONE'    - No enhancement
!  'LWF16'   - Enhancement based on Van Roekel et al. 2012 and Li et al. 2016
!  'RWHGK16' - Enhancement based on Reichl et al. 2016
   character(len=32)                   :: langmuir_mixing_method
!
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
      if (check_Ekman_length) then
         LEVEL4 'Clipping at Ekman scale                - active -'
      else
         LEVEL4 'Clipping at Ekman scale            - not active -'
      endif
      if (check_MonOb_length) then
         LEVEL4 'Clipping at Monin-Obukhov scale        - active -'
      else
         LEVEL4 'Clipping at Monin-Obukhov scale    - not active -'
      endif
      if (use_noDGat1) then
         LEVEL4 "Set shape function G'(1) = 0           - active -"
      else
         LEVEL4 "Set shape function G'(1) = 0       - not active -"
      endif
      LEVEL4 'Matching technique: ', trim(match_technique)
      LEVEL4 'Interpolation type for Ri: ', trim(bulk_Ri_interp_type)
      LEVEL4 'Interpolation type for diff and visc: ', trim(OBL_interp_type)
      LEVEL4 'Ri_c: ', Ri_c

   !  message of Langmuir turbulence parameterization
      select case (langmuir_method)
      case (KPP_LT_NOLANGMUIR)
         Langmuir_mixing_method = 'NONE'
         Langmuir_entrainment_method = 'NONE'
         LEVEL4 'Langmuir turbulence                - not active -'
      case (KPP_LT_LWF16)
         Langmuir_mixing_method = 'LWF16'
         Langmuir_entrainment_method = 'LWF16'
         LEVEL4 'Langmuir turbulence                    - active -'
         LEVEL4 ' - Langmuir mixing (Li et al., 2016)'
      case (KPP_LT_LF17)
         Langmuir_mixing_method = 'LWF16'
         Langmuir_entrainment_method = 'LF17'
         LEVEL4 'Langmuir turbulence                    - active -'
         LEVEL4 ' - Langmuir enhanced entrainment (Li and Fox-Kemper, 2017)'
      case (KPP_LT_RWHGK16)
         Langmuir_mixing_method = 'RWHGK16'
         Langmuir_entrainment_method = 'RWHGK16'
         LEVEL3 'Langmuir turbulence                    - active -'
         LEVEL4 ' - Langmuir mixing in hurricanes (Reichl et al. 2016)'
      case default
         STDERR 'Unsupported langmuir_method: ', langmuir_method
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
      call cvmix_init_kpp(Ri_crit=Ri_c,                                       &
                          interp_type=trim(bulk_Ri_interp_type),              &
                          interp_type2=trim(OBL_interp_type),                 &
                          MatchTechnique=trim(match_technique),               &
                          minVtsqr=_ZERO_,                                    &
                          lEkman=check_Ekman_length,                          &
                          lMonOb=check_MonOb_length,                          &
                          lnoDGat1=use_noDGat1,                               &
                          lenhenced_diff=use_enhanced_diff,                   &
                          surf_layer_ext=surface_layer_extent,                &
                          langmuir_mixing_str=trim(langmuir_mixing_method),   &
                          langmuir_entrainment_str=                           &
                                    trim(langmuir_entrainment_method))
   endif

   ! initialize CVMix variabels
   call cvmix_put(CVmix_vars, 'nlev', nlev)
   call cvmix_put(CVmix_vars, 'max_nlev', nlev)
   call cvmix_put(CVmix_vars, 'ocn_depth', h0)
   call cvmix_put(CVmix_vars, 'Mdiff', cvmix_num)
   call cvmix_put(CVmix_vars, 'Tdiff', cvmix_nuh)
   call cvmix_put(CVmix_vars, 'Sdiff', cvmix_nus)

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
                     tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f)
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

! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                            :: cff
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
      num = _ZERO_
      nuh = _ZERO_
      nus = _ZERO_
   endif

!-----------------------------------------------------------------------
! compute surface boundary layer mixing
!-----------------------------------------------------------------------

   if (use_surface_layer) then
      call surface_layer(nlev,h,rho,u,v,NN,u_taus,u_taub,   &
                         tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f)
   endif

!-----------------------------------------------------------------------
! compute bottom boundary layer mixing
!-----------------------------------------------------------------------

   if (use_bottom_layer) then
      call bottom_layer(nlev,h,rho,u,v,NN,u_taus,u_taub,             &
                        _ZERO_,_ZERO_,_ZERO_,_ZERO_,tRad,bRad,f)
   endif

 end subroutine do_cvmix
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute interior fluxes \label{sec:kppInterior}
!
! !INTERFACE:
   subroutine interior(nlev,NN,NNT,NNS,SS)
!
! !DESCRIPTION:
! Here, the interior diffusivities (defined as the diffusivities outside the
! surface and bottom boundary layers) are computed. The algorithms are identical
! to those suggested by \cite{Largeetal94}. For numerical efficiency, the
! algorithms for different physical processes are active only if certain
! pre-processor macros are defined in {\tt cppdefs.h}.
! \begin{itemize}
!  \item The shear instability algorithm is active if the macro
!        {\tt KPP\_SHEAR} is defined.
!  \item The internal wave algorithm is active if the macro
!        {\tt KPP\_INTERNAL\_WAVE} is defined.
!  \item The convective instability algorithm is active if the macro
!        {\tt KPP\_CONVEC} is defined.
!  \item The double-diffusion algorithm is active if the macro
!        {\tt KPP\_DDMIX} is defined. Note that in this case, the
!        macro {\tt SALINITY} has to be defined as well.
! \end{itemize}

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


! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE , parameter       :: eps=1.0E-14

   integer                    :: i
   REALTYPE                   :: cff,shear2
   REALTYPE                   :: nu_sx,nu_sxc
   REALTYPE                   :: iwm,iws
   REALTYPE                   :: drhoT,drhoS,Rrho,nu_dds,nu_ddt

!
!-----------------------------------------------------------------------
!BOC
!
!-----------------------------------------------------------------------
! Compute gradient Richardson number
!-----------------------------------------------------------------------
!
   do i=1,nlev-1
      Rig(i) = NN(i)/(SS(i) + eps)
   enddo

   Rig(0)    = _ZERO_
   Rig(nlev) = _ZERO_
!
!-----------------------------------------------------------------------
!  Compute "interior" viscosities and diffusivities everywhere as
!  the superposition of three processes: local Richardson number
!  instability due to resolved vertical shear, internal wave
!  breaking, and double diffusion.
!-----------------------------------------------------------------------
!
   do i=1,nlev-1
!
!     Smooth gradient Richardson number
      Rig(i)=0.25*Rig(i-1) + 0.50*Rig(i) + 0.25*Rig(i+1)
!
!     Compute interior diffusivity due to shear instability mixing.
# ifdef KPP_SHEAR
      cff=min(_ONE_,max(_ZERO_,Rig(i))/Ri0)
      nu_sx  = _ONE_-cff*cff
      nu_sx  = nu_sx*nu_sx*nu_sx
!
!     The shear mixing should be also a function of the actual magnitude
!     of the shear, see Polzin (1996, JPO, 1409-1425).
      shear2 = SS(i)
      cff    = shear2*shear2/(shear2*shear2+16.0E-10)
      nu_sx  = cff*nu_sx
# else
      nu_sx=_ZERO_
# endif

#ifdef KPP_INTERNAL_WAVE
!
!      Compute interior diffusivity due to wave breaking
!
!      Version A, see Gargett and Holloway (1984)
!      cff  =  _ONE_/sqrt(max(NN(i),1.0d-7))
!      iwm  =  1.0E-6*cff
!      iws  =  1.0E-7*cff

!     Version B, see Large et al. (1994)
      iwm  =  nuwm
      iws  =  nuws
#else
      iwm  =  _ZERO_
      iws  =  _ZERO_
#endif


# ifdef KPP_CONVEC
!     Compute interior convective diffusivity due to static instability
!     mixing
      cff    =  max(NN(i),bvfcon)
      cff    =  min(_ONE_,(bvfcon-cff)/bvfcon)
      nu_sxc =  _ONE_-cff*cff
      nu_sxc =  nu_sxc*nu_sxc*nu_sxc
# else
      nu_sxc =  _ZERO_
# endif
!
!     Sum contributions due to internal wave breaking, shear instability
!     and convective diffusivity due to shear instability.
      num(i)=iwm+nu0m*nu_sx+nu0c*nu_sxc
      nuh(i)=iws+nu0s*nu_sx+nu0c*nu_sxc
      nus(i)=nuh(i)
!
# ifdef KPP_DDMIX
!
!-----------------------------------------------------------------------
!  Compute double-diffusive mixing.  It can occur when vertical
!  gradient of density is stable but the vertical gradient of
!  salinity (salt figering) or temperature (diffusive convection)
!  is unstable.
!-----------------------------------------------------------------------
!
!     Compute double-diffusive density ratio, Rrho.
      drhoT =  NNT(i)
      drhoS = -NNS(i)
      Rrho  = drhoT/drhoS
!
!
!     Salt fingering case.
      if ((Rrho.gt._ONE_).and.(drhoS.gt._ZERO_)) then
!
!        Compute interior diffusivity for double diffusive mixing of
!        salinity.  Upper bound "Rrho" by "Rrho0"; (Rrho0=1.9, nuf=0.001).
         Rrho=min(Rrho,Rrho0)
         nu_dds=_ONE_-((Rrho-_ONE_)/(Rrho0-_ONE_))**2.0
         nu_dds=nuf*nu_dds*nu_dds*nu_dds
!
!        Compute interior diffusivity for double diffusive mixing
!        of temperature (fdd=0.7).
         nu_ddt=fdd*nu_dds
!
!
!     Diffusive convection case.
      elseif ((Rrho.gt._ZERO_).and.(Rrho.lt._ONE_).and.(drhoS.lt._ZERO_)) then
!
!        Compute interior diffusivity for double diffusive mixing of
!        temperature (Marmorino and Caldwell, 1976); (nu=1.5e-6,
!        tdd1=0.909, tdd2=4.6, tdd3=0.54).
         nu_ddt=nu*tdd1*exp(tdd2*exp(-tdd3*((_ONE_/Rrho)-_ONE_)))

!        Compute interior diffusivity for double diffusive mixing
!        of salinity (sdd1=0.15, sdd2=1.85, sdd3=0.85).
         if (Rrho.lt.0.5) then
            nu_dds=nu_ddt*sdd1*Rrho
         else
            nu_dds=nu_ddt*(sdd2*Rrho-sdd3)
         endif
      else
         nu_ddt=_ZERO_
         nu_dds=_ZERO_
      endif
!
!     Add double diffusion contribution to temperature and salinity
!     mixing coefficients.
      nuh(i)=nuh(i)  + nu_ddt
      nus(i)=nuh(i)  + nu_dds

# endif

   enddo ! loop over interior points

 end subroutine interior
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute turbulence in the surface layer with CVMix
!            \label{sec:kppSurface}
!
! !INTERFACE:
   subroutine surface_layer(nlev,h,rho,u,v,NN,u_taus,u_taub,    &
                            tFlux,btFlux,sFlux,bsFlux,tRad,bRad,f)
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

!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!   Adapted for CVMix: Qing Li
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE, parameter          :: eps      = 1.0E-10

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

!  Langmuir enhancement factor
   REALTYPE                     :: EFactor
!  Surface layer averaged Langmuir number to be passed in CVMix
!  for Langmuir enhanced entrainment
!  Projected if Langmuir_entrainment_method = 'RWHGK16'
!  Not projected if Langmuir_entrainment_method = 'L17'
   REALTYPE                     :: LaSL

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
!  Get Langmuir enhancement factor and Langmuir number
!-----------------------------------------------------------------------

   ! update Langmuir number
   call langmuir_number(nlev, u_taus, z_w(nlev)-zsbl)
   ! get Langmuir number for Langmuir-enhanced entrainment
   call kpp_langmuir_number(LaSL)
   ! get Langmuir enhancement factor
   call kpp_enhancement_factor(EFactor)

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
      surfthick = epsilon*depth
      do kk = nlev,k,-1
         if (z_w(nlev)-z_w(kk-1) .ge. surfthick) then
            kref = kk
            exit
         end if
      end do
      ! update Rref, Uref and Vref
      if (langmuir_method == KPP_LT_RWHGK16) then
         u=u+ustokes
         v=v+vstokes
      endif
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
      if (langmuir_method == KPP_LT_RWHGK16) then
         u=u-ustokes
         v=v-vstokes
      endif
      ! compute the Bulk Richardson number
      NN_max = max(NN(kp1), NN(k))
      RiBulk(kp1:kp1) = cvmix_kpp_compute_bulk_Richardson(           &
                zt_cntr = (/-depth/),                                &
                delta_buoy_cntr = (/-gorho0*(Rref-Rk)/),             &
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
!  Update Langmuir enhancement factor and Langmuir number
!-----------------------------------------------------------------------

   ! update Langmuir number
   call langmuir_number(nlev, u_taus, z_w(nlev)-zsbl)
   ! update Langmuir enhancement factor
   call kpp_enhancement_factor(EFactor)

!-----------------------------------------------------------------------
!  Compute the mixing coefficients within the surface boundary layer
!-----------------------------------------------------------------------

   ! set Langmuir enhancement factor
   CVmix_vars%LangmuirEnhancementFactor = EFactor

   ! Note that arrays at the cell interface in CVMix have indices (1:nlev)
   ! from the surface to the bottom, whereas those in GOTM have indices (nlev:0)
   CVmix_vars%Mdiff_iface(1:nlev+1) = num(nlev:0:-1)
   CVmix_vars%Tdiff_iface(1:nlev+1) = nuh(nlev:0:-1)
   CVmix_vars%Sdiff_iface(1:nlev+1) = nus(nlev:0:-1)

   call cvmix_coeffs_kpp(CVmix_vars)

   num(0:nlev) = CVmix_vars%Mdiff_iface(nlev+1:1:-1)
   nuh(0:nlev) = CVmix_vars%Tdiff_iface(nlev+1:1:-1)
   nus(0:nlev) = CVmix_vars%Sdiff_iface(nlev+1:1:-1)
   gamh(0:nlev) = CVmix_vars%kpp_Tnonlocal_iface(nlev+1:1:-1)
   gams(0:nlev) = CVmix_vars%kpp_Snonlocal_iface(nlev+1:1:-1)

   ! Note that kpp_transport_iface in CVMix is the value of K_x*gamma_x/flux_x,
   ! in other words, the user must multiply this value by either the freshwater
   ! flux or the penetrative shortwave heat flux to get the nonlocal fluxes

   ! include the effect of penetrating solar radiation
   tRadSrf   =   tRad(nlev)
   do k = 0,nlev
      gamh(k)   = -gamh(k)*(tFlux+tRadSrf-tRad(k))
      gams(k)   = -gams(k)*sFlux
   enddo

!  no non-local fluxes at top and bottom
   gamh(0   ) = _ZERO_
   gams(0   ) = _ZERO_
   gamh(nlev) = _ZERO_
   gams(nlev) = _ZERO_

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
! In this routine all computations related to turbulence in the bottom layer
! are performed. The algorithms are described in \sect{sec:kpp}. Note that these
! algorithms are affected by some pre-processor macros defined in {\tt cppdefs.inp},
! and by the parameters set in {\tt kpp.nml}, see \sect{sec:kpp}.

! The computation of the bulk Richardson number is slightly different from the
! surface boundary layer, since for the bottom boundary layer this quantity
! is defined as,
! \begin{equation}
!   \label{RibBottom}
!   Ri_b = \dfrac{(B(z_{bl})-B_r) d}
!                {\magn{\V U(z_{bl})-\V U_r}^2 + V_t^2(z_{bl})}
! \comma
! \end{equation}
! where $z_{bl}$ denotes the position of the edge of the bottom boundary layer.
!
! Also different from the surface layer computations is the absence of non-local
! fluxes.

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
   REALTYPE, parameter          :: eps      = 1.0E-10

   integer                      :: k,kbbl

   REALTYPE                     :: Bo
   REALTYPE                     :: Bfbot
   REALTYPE                     :: tRadBot,tRadBbl
   REALTYPE                     :: bRadBot,bRadBbl
   REALTYPE                     :: Gm1
   REALTYPE                     :: Gt1
   REALTYPE                     :: Gs1
   REALTYPE                     :: dGm1dS
   REALTYPE                     :: dGt1dS
   REALTYPE                     :: dGs1dS
   REALTYPE                     :: f1
   REALTYPE                     :: bl_dpth,bl_z
   REALTYPE                     :: wm
   REALTYPE                     :: ws
   REALTYPE                     :: depth

   REALTYPE                     :: Gm, Gt, Gs, K_bl, Ribot, Ritop, Rk, Rref
   REALTYPE                     :: Uk, Uref, Ustarb, Vk, Vref
   REALTYPE                     :: dR,dU,dV
   REALTYPE                     :: a1, a2, a3
   REALTYPE                     :: cff,cff_up, cff_dn
   REALTYPE                     :: dK_bl, hekman, hmonob, sigma, zbl

   REALTYPE, dimension (0:nlev) :: Bflux
   REALTYPE, dimension (0:nlev) :: FC


!-----------------------------------------------------------------------
!BOC
!
!
!-----------------------------------------------------------------------
!  Get approximation of bottom layer depth using "epsilon" and
!  boundary layer depth from previous time step.
!-----------------------------------------------------------------------
!
   bl_dpth = epsilon*(zbbl-z_w(0))
   bl_z    = epsilon*zbbl

!-----------------------------------------------------------------------
!  Compute total buoyancy flux (m2/s3) at W-points.
!-----------------------------------------------------------------------
!
    tRadBot   =   tRad(0)
    bRadBot   =   bRad(0)

!  bottom buoyancy flux
!  (negative for buoyancy gain)
   Bo         = - (btFlux + bsFlux)

!  include effect of short-wave radiation
   do k = 0,nlev
      Bflux(k)  = Bo + ( bRad(k) - bRadBot )
   enddo


!-----------------------------------------------------------------------
!  Compute potential density and velocity components bottom reference
!  values.
!-----------------------------------------------------------------------


!  simply use lowest value
   Rref = rho(1)
   Uref =   u(1)
   Vref =   v(1)

!-----------------------------------------------------------------------
!  Compute critical function, FC, for bulk Richardson number
!-----------------------------------------------------------------------

   FC(0   ) = _ZERO_
   FC(nlev) = _ZERO_

   do k=1,nlev-1

      depth=z_w(k)-z_w(0)

      if (Bflux(k).lt._ZERO_) then
         sigma=min(bl_dpth,depth)
      else
         sigma=depth
      endif

      call wscale (Bflux(k),u_taub,sigma,wm,ws)



#ifdef KPP_TWOPOINT_REF

!     interpolate reference value at grid face "k"
!     from values at grid centers

      cff = _ONE_/h_r(k)

      dR  = cff*( rho(k+1) - rho(k) )
      dU  = cff*( u  (k+1) - u  (k) )
      dV  = cff*( v  (k+1) - v  (k) )

      cff = _ONE_/2.0

      Rk  = rho(k) + h(k)*cff*dR
      Uk  =   u(k) + h(k)*cff*dU
      Vk  =   v(k) + h(k)*cff*dV

#else
!     identify reference value at grid face "k"
!     with value at center below
      Rk  = rho(k)
      Uk  =   u(k)
      Vk  =   v(k)
#endif

!     compute numerator and denominator of Ri_b
      Ritop = -gorho0*(Rk-Rref)*depth
      Ribot = (Uk-Uref)**2+(Vk-Vref)**2 +                              &
              Vtc*depth*ws*sqrt(abs(NN(k)) )

# ifdef KPP_IP_FC
      FC(k)=Ritop-Ric*Ribot
# else
      FC(k)=Ritop/(Ribot+eps)
# endif

   enddo ! inner grid faces



!-----------------------------------------------------------------------
! Linearly interpolate to find "zbbl" where Rib/Ric=1.
!-----------------------------------------------------------------------

   kbbl = nlev           ! kbbl is  index of cell containing zbbl
   zbbl = z_w(nlev)

# ifdef KPP_IP_FC
!  look for position of vanishing F_crit
   do k=1,nlev-1
      if ((kbbl.eq.nlev).and.(FC(k).gt._ZERO_)) then
         zbbl = (z_w(k)*FC(k-1)-z_w(k-1)*FC(k))/(FC(k-1)-FC(k))
         kbbl = k
      endif
   enddo
# else
!  look for position of vanishing Ri_b
   do k=1,nlev-1
      if ((kbbl.eq.nlev).and.((FC(k-1).lt.Ric).and.(FC(k).ge.Ric))) then
         zbbl = ( (Ric  -  FC(k-1) )*z_w(k  ) +                         &
                  (FC(k) - Ric     )*z_w(k-1) )/(FC(k)-FC(k-1))
         kbbl = k
      endif
   enddo
# endif


!-----------------------------------------------------------------------
!  Limit boundary layer thickness by Ekman scale
!-----------------------------------------------------------------------

   if (clip_mld) then
      if (u_taub.gt._ZERO_) then
         hekman = cekman*u_taub/max(abs(f),eps)
         zbbl   = min(z_w(0)+hekman,zbbl)
      endif
   endif

   zbbl = min(zbbl,z_w(nlev))
   zbbl = max(zbbl,z_w(0   ))

!  find new boundary layer index "kbbl".
   kbbl=nlev
   do k=1,nlev-1
      if ((kbbl.eq.nlev).and.(z_w(k).gt.zbbl)) then
         kbbl = k
      endif
   enddo

!-----------------------------------------------------------------------
!  Compute total buoyancy flux at bottom boundary layer depth
!-----------------------------------------------------------------------

   bRadBbl = ( bRad(kbbl-1)*(z_w(kbbl)-zbbl)  +                         &
               bRad(kbbl  )*(zbbl-z_w(kbbl-1) ) )/ h(kbbl)

   Bfbot   = Bo + (bRadBbl-bRadBot)


!-----------------------------------------------------------------------
!  Compute tubulent velocity scales (wm,ws) at "zbbl".
!-----------------------------------------------------------------------

   zbl     = zbbl-z_w(0)
   bl_dpth = epsilon*zbl

   if (Bfbot.gt._ZERO_) then
      cff  = _ONE_
   else
      cff  = epsilon
   endif

   sigma=cff*zbl

   call wscale (Bfbot,u_taub,sigma,wm,ws)


!-----------------------------------------------------------------------
!  Compute nondimensional shape function Gx(sigma) in terms of the
!  interior diffusivities at sigma=1 (Gm1, Gs1, Gt1) and its vertical
!  derivative evaluated "zbbl" via interpolation.
!-----------------------------------------------------------------------

!  original code with kappa-bug
!   f1 = 5.0*max(_ZERO_,Bfbot)*kappa/(u_taub*u_taub*u_taub*u_taub+eps)

!   new code without kappa-bug
   f1 = 5.0*max(_ZERO_,Bfbot)/(u_taub*u_taub*u_taub*u_taub+eps)

   k      = kbbl
   cff    = _ONE_/h(k)
   cff_dn = cff*(zbbl-z_w(k-1))
   cff_up = cff*(z_w(k)-zbbl  )

!  Compute nondimensional shape function for viscosity "Gm1" and its
!  vertical derivative "dGm1dS" evaluated at "zbbl".

   K_bl   =  cff_dn*num(k)+cff_up*num(k-1)
   dK_bl  =  cff*(num(k)-num(k-1))
   Gm1    =  K_bl/(zbl*wm+eps)

# ifdef KPP_CLIP_GS
   dGm1dS = min(_ZERO_,dK_bl/(wm+eps)+K_bl*f1)
# else
   dGm1dS = dK_bl/(wm+eps) + K_bl*f1
# endif

!
!  Compute nondimensional shape function for diffusion of temperature
!  "Gt1" and its vertical derivative "dGt1dS" evaluated at "zbbl".
!
   K_bl   =  cff_dn*nuh(k)+cff_up*nuh(k-1)
   dK_bl  =  cff*(nuh(k)-nuh(k-1))
   Gt1    =   K_bl/(zbl*ws+eps)


# ifdef KPP_CLIP_GS
   dGt1dS = min(_ZERO_,dK_bl/(ws+eps)+K_bl*f1)
# else
   dGt1dS = dK_bl/(ws+eps) + K_bl*f1
# endif

# ifdef KPP_SALINITY
!
!  Compute nondimensional shape function for diffusion of salinity
!  "Gs1" and its vertical derivative "dGs1dS" evaluated at "zbbl".
!
   K_bl   =  cff_dn*nus(k)+cff_up*nus(k-1)
   dK_bl  =  cff*(nus(k)-nus(k-1))
   Gs1    =   K_bl/(zbl*ws+eps)

# ifdef KPP_CLIP_GS
   dGs1dS = min(_ZERO_,dK_bl/(ws+eps)+K_bl*f1)
# else
   dGs1dS = dK_bl/(ws+eps) + K_bl*f1
# endif

# endif

!
!-----------------------------------------------------------------------
!  Compute bottom boundary layer mixing coefficients
!-----------------------------------------------------------------------
!
!  loop over the inner interfaces
!  (the outermost are not needed)
   do k=1,nlev-1

      if (k.lt.kbbl) then

!        Compute turbulent velocity scales at vertical W-points.
         depth=z_w(k)-z_w(0)
         if (Bflux(k).lt._ZERO_) then
            sigma=min(bl_dpth,depth)
         else
            sigma=depth
         endif

         call wscale(Bflux(k),u_taub,sigma,wm, ws)
!
!        Set polynomial coefficients for shape function.
         sigma = depth/(zbl+eps)

         a1    = sigma-2.0
         a2    = 3.0-2.0*sigma
         a3    = sigma-1.0
!
!        Compute nondimesional shape functions.

         Gm = a1+a2*Gm1+a3*dGm1dS
         Gt = a1+a2*Gt1+a3*dGt1dS
# ifdef KPP_SALINITY
         Gs = a1+a2*Gs1+a3*dGs1dS
# endif
!
!        Compute boundary layer mixing coefficients, combine them
!        with interior mixing coefficients.
         num(k) = depth*wm*(_ONE_+sigma*Gm)
         nuh(k) = depth*ws*(_ONE_+sigma*Gt)
# ifdef KPP_SALINITY
         nus(k) = depth*ws*(_ONE_+sigma*Gs)
# endif

      endif
   enddo



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
   if (allocated(num)) deallocate(num)
   if (allocated(nuh)) deallocate(nuh)
   if (allocated(nus)) deallocate(nus)
   if (allocated(nucl)) deallocate(nucl)
   if (allocated(gamu)) deallocate(gamu)
   if (allocated(gamv)) deallocate(gamv)
   if (allocated(gamh)) deallocate(gamh)
   if (allocated(gams)) deallocate(gams)
   if (allocated(Rig)) deallocate(Rig)
   if (allocated(tke)) deallocate(tke)
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

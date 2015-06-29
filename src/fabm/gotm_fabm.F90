#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: gotm_fabm --- Interface to Framework for Aquatic Biogeochemical Models (FABM)
!
! !INTERFACE:
   module gotm_fabm
!
! !DESCRIPTION:
!  This module provides the link between the General Ocean Turbulence Model and
!  the Framework for Aquatic Biogeochemical Models.
!
! !USES:
   use fabm
   use fabm_types
   use fabm_expressions
   use fabm_config
   use fabm_driver

   implicit none
!
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_fabm, init_gotm_fabm_state
   public set_env_gotm_fabm,do_gotm_fabm
   public clean_gotm_fabm
   public fabm_calc
   public register_observation

   ! Passed through from fabm_types, used by hosts to provide additional inputs:
   public standard_variables

   ! Variables below must be accessible for gotm_fabm_output
   public dt,h,save_inputs

   ! Variables below must be accessible for getm_fabm
   public cc_transport
!
! !PUBLIC DATA MEMBERS:
!  The one and only model
   class (type_model), pointer, public :: model

   type,extends(type_base_driver) :: type_gotm_driver
   contains
      procedure :: fatal_error => gotm_driver_fatal_error
      procedure :: log_message => gotm_driver_log_message
   end type

!  Arrays for state and diagnostic variables
   REALTYPE,allocatable,dimension(:,:),public,target :: cc
   REALTYPE,allocatable,dimension(:,:),public        :: cc_diag
   REALTYPE,allocatable,dimension(:),  public        :: cc_diag_hz

!  Arrays for observations, relaxation times and FABM variable identifiers associated with the observations.
   type type_forced_1d_state
      REALTYPE, pointer,dimension(:) :: data      => null()
      REALTYPE, pointer,dimension(:) :: relax_tau => null()
   end type

   type type_forced_0d_state
      REALTYPE, pointer :: data      => null()
      REALTYPE, pointer :: relax_tau => null()
   end type

   REALTYPE,allocatable,dimension(:),target :: horizontal_expression_data

!  Observation indices (from obs_0d, obs_1d) for pelagic and benthic state variables.
   type (type_forced_1d_state),allocatable :: cc_obs(:)
   type (type_forced_0d_state),allocatable :: cc_ben_obs(:)

   interface register_observation
      module procedure register_bulk_observation
      module procedure register_horizontal_observation
      module procedure register_scalar_observation
   end interface

   type (type_bulk_variable_id),      save :: temp_id,salt_id,rho_id,h_id,swr_id,par_id,pres_id
   type (type_horizontal_variable_id),save :: lon_id,lat_id,windspeed_id,par_sf_id,cloud_id,taub_id,swr_sf_id

!  Variables to hold time spent on advection, diffusion, sink/source terms.
   integer(8) :: clock_adv,clock_diff,clock_source
!
! !PRIVATE DATA MEMBERS:
   ! Namelist variables
   REALTYPE                  :: cnpar
   integer                   :: w_adv_method,w_adv_discr,ode_method,split_factor,configuration_method
   logical                   :: fabm_calc,repair_state, &
                                bioshade_feedback,bioalbedo_feedback,biodrag_feedback, &
                                no_precipitation_dilution,salinity_relaxation_to_freshwater_flux, &
                                save_inputs, no_surface

   ! Arrays for work, vertical movement, and cross-boundary fluxes
   REALTYPE,allocatable,dimension(:,:) :: ws
   REALTYPE,allocatable,dimension(:)   :: sfl,bfl,Qsour,Lsour,DefaultRelaxTau,curh,curnuh,iweights
   logical,allocatable, dimension(:)   :: cc_transport
   integer,allocatable, dimension(:)   :: posconc

   ! Arrays for environmental variables not supplied externally.
   REALTYPE,allocatable,dimension(:),target :: par,pres,swr,k_par,z

   ! External variables
   REALTYPE :: dt,dt_eff   ! External and internal time steps
   integer  :: w_adv_ctr   ! Scheme for vertical advection (0 if not used)
   REALTYPE,pointer,dimension(:) :: nuh,h,bioshade,w,rho
   REALTYPE,pointer,dimension(:) :: SRelaxTau,sProf,salt
   REALTYPE,pointer              :: precip,evap,bio_drag_scale,bio_albedo

   REALTYPE,pointer :: I_0,A,g1,g2
   integer,pointer  :: yearday,secondsofday
   REALTYPE, target :: decimal_yearday
   logical          :: fabm_ready

   contains

#include "../src/util/ode_solvers_template.F90"

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the FABM driver
!
! !INTERFACE:
   subroutine init_gotm_fabm(nlev,namlst,fname,dt)
!
! !DESCRIPTION:
! Initializes the GOTM-FABM driver module by reading settings from fabm.nml.
!
! !INPUT PARAMETERS:
   integer,          intent(in)        :: nlev,namlst
   character(len=*), intent(in)        :: fname
   REALTYPE,optional,intent(in)        :: dt
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
!  local variables
   integer :: i
   logical :: file_exists
   namelist /gotm_fabm_nml/ fabm_calc,                                               &
                            cnpar,w_adv_discr,ode_method,split_factor,               &
                            bioshade_feedback,bioalbedo_feedback,biodrag_feedback,   &
                            repair_state,no_precipitation_dilution,                  &
                            salinity_relaxation_to_freshwater_flux,save_inputs, &
                            no_surface,configuration_method
!
!-----------------------------------------------------------------------
!BOC

   LEVEL1 'init_gotm_fabm'

   nullify(model)

   ! Initialize all namelist variables to reasonable default values.
   fabm_calc         = .false.
   cnpar             = _ONE_
   w_adv_discr       = 6
   ode_method        = 1
   split_factor      = 1
   bioshade_feedback = .true.
   bioalbedo_feedback = .true.
   biodrag_feedback  = .true.
   repair_state      = .false.
   salinity_relaxation_to_freshwater_flux = .false.
   no_precipitation_dilution = .false.              ! useful to check mass conservation
   no_surface = .false.                             ! disables surface exchange; useful to check mass conservation
   save_inputs = .false.
   configuration_method = -1                        ! -1: auto-detect, 0: namelists, 1: YAML

   ! Open the namelist file and read the namelist.
   ! Note that the namelist file is left open until the routine terminates,
   ! so FABM can read more namelists from it during initialization.
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=gotm_fabm_nml,err=99)
   close(namlst)

   if (fabm_calc) then
      ! Provide FABM with an object for communication with host
      allocate(type_gotm_driver::driver)

      clock_adv    = 0
      clock_diff   = 0
      clock_source = 0

      fabm_ready = .false.

      ! Create model tree
      if (configuration_method==-1) then
         configuration_method = 1
         inquire(file='fabm.yaml',exist=file_exists)
         if (.not.file_exists) then
            inquire(file='fabm.nml',exist=file_exists)
            if (file_exists) configuration_method = 0
         end if
      end if
      select case (configuration_method)
         case (0)
            ! From namelists in fabm.nml
            model => fabm_create_model_from_file(namlst)
         case (1)
            ! From YAML file fabm.yaml
            allocate(model)
            call fabm_create_model_from_yaml_file(model)
      end select

      ! Initialize model tree (creates metadata and assigns variable identifiers)
      call fabm_set_domain(model,nlev,dt)

      ! Report prognostic variable descriptions
      LEVEL2 'FABM pelagic state variables:'
      do i=1,size(model%state_variables)
         LEVEL3 trim(model%state_variables(i)%name), '  ', &
                trim(model%state_variables(i)%units),'  ',&
                trim(model%state_variables(i)%long_name)
      end do

      LEVEL2 'FABM bottom-bound state variables:'
      do i=1,size(model%bottom_state_variables)
         LEVEL3 trim(model%bottom_state_variables(i)%name), '  ', &
                trim(model%bottom_state_variables(i)%units),'  ',&
                trim(model%bottom_state_variables(i)%long_name)
      end do

      LEVEL2 'FABM surface-bound state variables:'
      do i=1,size(model%surface_state_variables)
         LEVEL3 trim(model%surface_state_variables(i)%name), '  ', &
                trim(model%surface_state_variables(i)%units),'  ',&
                trim(model%surface_state_variables(i)%long_name)
      end do

      ! Report diagnostic variable descriptions
      LEVEL2 'FABM diagnostic variables defined on the full model domain:'
      do i=1,size(model%diagnostic_variables)
         LEVEL3 trim(model%diagnostic_variables(i)%name), '  ', &
                trim(model%diagnostic_variables(i)%units),'  ',&
                trim(model%diagnostic_variables(i)%long_name)
      end do

      LEVEL2 'FABM diagnostic variables defined on a horizontal slice of the model domain:'
      do i=1,size(model%horizontal_diagnostic_variables)
         LEVEL3 trim(model%horizontal_diagnostic_variables(i)%name), '  ', &
                trim(model%horizontal_diagnostic_variables(i)%units),'  ',&
                trim(model%horizontal_diagnostic_variables(i)%long_name)
      end do

      ! Report type of solver
      LEVEL2 "Using Eulerian solver"
      select case (ode_method)
         case (1)
            LEVEL2 'Using euler_forward()'
         case (2)
            LEVEL2 'Using runge_kutta_2()'
         case (3)
            LEVEL2 'Using runge_kutta_4()'
         case (4)
            LEVEL2 'Using patankar()'
         case (5)
            LEVEL2 'Using patankar_runge_kutta_2()'
         case (6)
            LEVEL2 'Using patankar_runge_kutta_4()'
         case (7)
            LEVEL2 'Using modified_patankar()'
         case (8)
            LEVEL2 'Using modified_patankar_2()'
         case (9)
            LEVEL2 'Using modified_patankar_4()'
         case (10)
            LEVEL2 'Using emp_1()'
         case (11)
            LEVEL2 'Using emp_2()'
         case default
            stop "init_gotm_fabm: no valid ode_method specified in fabm.nml!"
      end select

      ! Get ids for standard variables, to be used later to send data to FABM.
      temp_id = model%get_bulk_variable_id(standard_variables%temperature)
      salt_id = model%get_bulk_variable_id(standard_variables%practical_salinity)
      rho_id  = model%get_bulk_variable_id(standard_variables%density)
      h_id    = model%get_bulk_variable_id(standard_variables%cell_thickness)
      par_id  = model%get_bulk_variable_id(standard_variables%downwelling_photosynthetic_radiative_flux)
      swr_id  = model%get_bulk_variable_id(standard_variables%downwelling_shortwave_flux)
      pres_id = model%get_bulk_variable_id(standard_variables%pressure)
      lon_id       = model%get_horizontal_variable_id(standard_variables%longitude)
      lat_id       = model%get_horizontal_variable_id(standard_variables%latitude)
      windspeed_id = model%get_horizontal_variable_id(standard_variables%wind_speed)
      par_sf_id    = model%get_horizontal_variable_id(standard_variables%surface_downwelling_photosynthetic_radiative_flux)
      swr_sf_id    = model%get_horizontal_variable_id(standard_variables%surface_downwelling_shortwave_flux)
      cloud_id     = model%get_horizontal_variable_id(standard_variables%cloud_area_fraction)
      taub_id      = model%get_horizontal_variable_id(standard_variables%bottom_stress)

      ! Initialize spatially explicit variables
      call init_var_gotm_fabm(nlev)

      ! Enumerate expressions needed by FABM and allocate arrays to hold the associated data.
      call check_fabm_expressions()

   end if

   return

98 LEVEL2 'I could not open '//trim(fname)
   LEVEL2 'If thats not what you want you have to supply '//trim(fname)
   LEVEL2 'See the bio example on www.gotm.net for a working '//trim(fname)
   fabm_calc = .false.
   return

99 FATAL 'I could not read '//trim(fname)
   stop 'init_gotm_fabm'
   return

   end subroutine init_gotm_fabm
!EOC



!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise FABM variables
!
! !INTERFACE:
   subroutine init_var_gotm_fabm(nlev)
!
! !DESCRIPTION:
! This routine allocates memory for all FABM variables.
!
   integer,intent(in) :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i,rc
!
!-----------------------------------------------------------------------
!BOC
   ! Allocate state variable array for pelagic and benthos combined and provide initial values.
   ! In terms of memory use, it is a waste to allocate storage for benthic variables across the entire
   ! column (the bottom layer should suffice). However, it is important that all values at a given point
   ! in time are integrated simultaneously in multi-step algorithms. This currently can only be arranged
   ! by storing benthic values together with the pelagic, in a fully depth-explicit array.
   allocate(cc(0:nlev,1:size(model%state_variables)+size(model%bottom_state_variables)+size(model%surface_state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (cc)'
   cc = _ZERO_
   do i=1,size(model%state_variables)
      cc(:,i) = model%state_variables(i)%initial_value
      call fabm_link_bulk_state_data(model,i,cc(1:,i))
   end do
   do i=1,size(model%bottom_state_variables)
      cc(1,size(model%state_variables)+i) = model%bottom_state_variables(i)%initial_value
      call fabm_link_bottom_state_data(model,i,cc(1,size(model%state_variables)+i))
   end do
   do i=1,size(model%surface_state_variables)
      cc(1,size(model%state_variables)+size(model%bottom_state_variables)+i) = model%surface_state_variables(i)%initial_value
      call fabm_link_surface_state_data(model,i,cc(nlev,size(model%state_variables)+size(model%bottom_state_variables)+i))
   end do

   ! Allocate arrays that contain observation indices of pelagic and benthic state variables.
   ! Initialize observation indices to -1 (no external observations provided)
   allocate(cc_obs(1:size(model%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (cc_obs)'
   allocate(cc_ben_obs(1:size(model%bottom_state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (cc_ben_obs)'

   ! Allocate array for pelagic diagnostic variables; set all values to zero.
   ! (zeroing is needed because time-integrated/averaged variables will increment rather than set the array)
   allocate(cc_diag(1:nlev,1:size(model%diagnostic_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (cc_diag)'
   cc_diag = _ZERO_

   ! Allocate array for diagnostic variables on horizontal surfaces; set all values to zero.
   ! (zeroing is needed because time-integrated/averaged variables will increment rather than set the array)
   allocate(cc_diag_hz(1:size(model%horizontal_diagnostic_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (cc_diag_hz)'
   cc_diag_hz = _ZERO_

   ! Allocate array for vertical movement rates (m/s, positive for upwards),
   ! and set these to the values provided by the model.
   allocate(ws(0:nlev,1:size(model%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (ws)'
   do i=1,size(model%state_variables)
      ws(:,i) = model%state_variables(i)%vertical_movement
   end do

   ! Allocate array for surface fluxes and initialize these to zero (no flux).
   allocate(sfl(1:size(model%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (sfl)'
   sfl = _ZERO_

   ! Allocate array for bottom fluxes and initialize these to zero (no flux).
   allocate(bfl(1:size(model%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (bfl)'
   bfl = _ZERO_

   ! Allocate array for surface fluxes and initialize these to zero (no flux).
   allocate(cc_transport(1:size(model%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (cc_transport)'
   cc_transport = .true.
   do i=1,size(model%state_variables)
      cc_transport(i) = .not.model%state_variables(i)%properties%get_logical('disable_transport',default=.false.)
   end do

   ! Allocate array for photosynthetically active radiation (PAR).
   ! This will be calculated internally during each time step.
   allocate(par(1:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (par)'
   par = _ZERO_
   if (fabm_variable_needs_values(model,par_id)) call fabm_link_bulk_data(model,par_id,par)

   ! Allocate array for attenuation coefficient pf photosynthetically active radiation (PAR).
   ! This will be calculated internally during each time step.
   allocate(k_par(1:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (k_par)'
   k_par = _ZERO_
   call fabm_link_bulk_data(model,standard_variables%attenuation_coefficient_of_photosynthetic_radiative_flux,k_par)

   ! Allocate array for shortwave radiation (swr).
   ! This will be calculated internally during each time step.
   allocate(swr(1:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (swr)'
   swr = _ZERO_
   if (fabm_variable_needs_values(model,swr_id)) call fabm_link_bulk_data(model,swr_id,swr)

   ! Allocate array for local pressure.
   ! This will be calculated from layer depths and density internally during each time step.
   if (fabm_variable_needs_values(model,pres_id)) then
      allocate(pres(1:nlev),stat=rc)
      if (rc /= 0) stop 'allocate_memory(): Error allocating (pres)'
      pres = _ZERO_
      call fabm_link_bulk_data(model,pres_id,pres)
   end if

   ! Allocate array for local depth (below water surface).
   ! This will be calculated from layer depths.
   allocate(z(1:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (z)'
   z = _ZERO_
   call fabm_link_bulk_data(model,standard_variables%depth,z)

   ! Initialize scalar to hold day of the year (floating point value),
   ! and link it to FABM.
   decimal_yearday = _ZERO_
   call fabm_link_scalar_data(model,standard_variables%number_of_days_since_start_of_the_year,decimal_yearday)

   allocate(Qsour(0:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (Qsour)'
   Qsour = _ZERO_

   allocate(Lsour(0:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (Lsour)'
   Lsour = _ZERO_

   allocate(DefaultRelaxTau(0:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (DefaultRelaxTau)'
   DefaultRelaxTau = 1.d15

   allocate(curh(0:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (curh)'
   curh = _ZERO_

   allocate(curnuh(0:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (curnuh)'
   curnuh = _ZERO_

   allocate(iweights(0:nlev),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (iweights)'
   iweights = _ZERO_

   allocate(posconc(1:size(model%state_variables)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (posconc)'
   posconc = 0
   do i=1,size(model%state_variables)
      if (model%state_variables(i)%minimum>=_ZERO_) posconc(i) = 1
   end do

   end subroutine init_var_gotm_fabm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set environment for FABM
!
! !INTERFACE:
   subroutine set_env_gotm_fabm(latitude,longitude,dt_,w_adv_method_,w_adv_ctr_,temp,salt_,rho_,nuh_,h_,w_, &
                                bioshade_,I_0_,cloud,taub,wnd,precip_,evap_,z_,A_,g1_,g2_, &
                                yearday_,secondsofday_,SRelaxTau_,sProf_,bio_albedo_,bio_drag_scale_)
!
! !DESCRIPTION:
! This routine is called once from GOTM to provide pointers to the arrays that describe
! the physical environment relevant for biogeochemical processes (temprature, salinity, etc.)
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in),target :: latitude,longitude
   REALTYPE, intent(in) :: dt_
   integer,  intent(in) :: w_adv_method_,w_adv_ctr_
   REALTYPE, intent(in),target,dimension(:) :: temp,salt_,rho_,nuh_,h_,w_,bioshade_,z_
   REALTYPE, intent(in),target :: I_0_,cloud,wnd,precip_,evap_,taub
   REALTYPE, intent(in),target :: A_,g1_,g2_
   integer,  intent(in),target :: yearday_,secondsofday_
   REALTYPE, intent(in),optional,target,dimension(:) :: SRelaxTau_,sProf_
   REALTYPE, intent(in),optional,target :: bio_albedo_,bio_drag_scale_
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------!
!BOC
   if (.not. fabm_calc) return

   ! Provide pointers to arrays with environmental variables to FABM.
   call fabm_link_bulk_data      (model,temp_id,     temp)
   call fabm_link_bulk_data      (model,salt_id,     salt_)
   call fabm_link_bulk_data      (model,rho_id,      rho_)
   call fabm_link_bulk_data      (model,h_id,        h_(2:))
   call fabm_link_horizontal_data(model,lon_id,      longitude)
   call fabm_link_horizontal_data(model,lat_id,      latitude)
   call fabm_link_horizontal_data(model,windspeed_id,wnd)
   call fabm_link_horizontal_data(model,par_sf_id,   I_0_)
   call fabm_link_horizontal_data(model,swr_sf_id,   I_0_)
   call fabm_link_horizontal_data(model,cloud_id,    cloud)
   call fabm_link_horizontal_data(model,taub_id,     taub)

   ! Save pointers to external dynamic variables that we need later (in do_gotm_fabm)
   nuh      => nuh_        ! turbulent heat diffusivity [1d array] used to diffuse biogeochemical state variables
   h        => h_          ! layer heights [1d array] needed for advection, diffusion
   w        => w_          ! vertical medium velocity [1d array] needed for advection of biogeochemical state variables
   bioshade => bioshade_   ! biogeochemical light attenuation coefficients [1d array], output of biogeochemistry, input for physics
   precip   => precip_     ! precipitation [scalar] - used to calculate dilution due to increased water volume
   evap     => evap_       ! evaporation [scalar] - used to calculate concentration due to decreased water volume
   salt     => salt_       ! salinity [1d array] - used to calculate virtual freshening due to salinity relaxation
   rho      => rho_        ! density [1d array] - used to calculate pressure.

   if (biodrag_feedback.and.present(bio_drag_scale_)) then
      bio_drag_scale => bio_drag_scale_
   else
      nullify(bio_drag_scale)
   end if
   if (bioalbedo_feedback.and.present(bio_albedo_)) then
      bio_albedo => bio_albedo_
   else
      nullify(bio_albedo)
   end if

   if (present(SRelaxTau_) .and. present(sProf_)) then
      SRelaxTau => SRelaxTau_ ! salinity relaxation times  [1d array] - used to calculate virtual freshening due to salinity relaxation
      sProf     => sProf_     ! salinity relaxation values [1d array] - used to calculate virtual freshening due to salinity relaxation
   else
      if (salinity_relaxation_to_freshwater_flux) &
         stop 'gotm_fabm:set_env_gotm_fabm: salinity_relaxation_to_freshwater_flux is set, &
              &but salinity relaxation arrays are not provided.'
      nullify(SRelaxTau)
      nullify(sProf)
   end if

   ! Copy scalars that will not change during simulation, and are needed in do_gotm_fabm)
   dt = dt_
   w_adv_method = w_adv_method_
   w_adv_ctr = w_adv_ctr_

   ! Calculate and save internal time step.
   dt_eff = dt/split_factor

   I_0 => I_0_
   A => A_
   g1 => g1_
   g2 => g2_

   yearday => yearday_
   secondsofday => secondsofday_

   end subroutine set_env_gotm_fabm
!EOC

   subroutine calculate_derived_input(nlev,itime)

   integer, intent(in)          :: nlev
   REALTYPE,intent(in),optional :: itime

   integer :: i
   REALTYPE,parameter :: gravity = 9.81d0

   ! Update contiguous arrays with layer heights and diffusivity, needed in calls to adv_center and diff_center.
   curh   = h
   curnuh = nuh

   if (allocated(pres)) then
      ! Calculate local pressure in dbar (10 kPa) from layer height and density
      pres(nlev) = rho(nlev)*curh(nlev)/2
      do i=nlev-1,1,-1
         pres(i) = pres(i+1) + (rho(i)*curh(i)+rho(i+1)*curh(i+1))/2
      end do
      pres(1:nlev) = pres(1:nlev)*gravity/10000
   end if

   ! Calculate local depth below surface from layer height
   ! Used internally to compute light field, and may be used by
   ! biogeochemical models as well.
   z(nlev) = curh(nlev)/2
   do i=nlev-1,1,-1
      z(i) = z(i+1) + (curh(i)+curh(i+1))/2
   end do

!  Calculate decimal day of the year (1 jan 00:00 = 0.)
   decimal_yearday = yearday-1 + dble(secondsofday)/86400

   call fabm_update_time(model,itime)

   end subroutine

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the FABM model
!
! !INTERFACE:
   subroutine do_gotm_fabm(nlev,itime)
!
! !DESCRIPTION:
! TODO
!
! !USES:
   use util,only: flux,Neumann
!
   integer, intent(in)          :: nlev
   REALTYPE,intent(in),optional :: itime
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer, parameter        :: adv_mode_0=0
   integer, parameter        :: adv_mode_1=1
   REALTYPE,dimension(0:nlev):: ws1d
   REALTYPE                  :: dilution,virtual_dilution
   integer                   :: i
   integer                   :: split
   integer(8)                :: clock_start,clock_end
!
!-----------------------------------------------------------------------
!BOC

   if (.not. fabm_calc) return

   ! At this stage, FABM has been provided with arrays for all state variables, any variables
   ! read in from file (gotm_fabm_input), and all variables exposed by GOTM. If FABM is still
   ! lacking variable references, this should now trigger an error.
   if (.not.fabm_ready) then
      call fabm_check_ready(model)
      fabm_ready = .true.
   end if

   call calculate_derived_input(nlev,itime)

   ! Get updated vertical movement (m/s, positive for upwards) for biological state variables.
   call fabm_get_vertical_movement(model,1,nlev,ws(1:nlev,:))

   ! Start building surface flux (dilution due to precipitation/concentration due to evaporation only,
   ! as biogeochemical processes that cause surface fluxes are handled as part of the sink/source terms.
   sfl = _ZERO_

   ! Calculate dilution due to surface freshwater flux (m/s)
   dilution = precip+evap

   ! If salinity is relaxed to observations, the change in column-integrated salinity can converted into a
   ! a virtual freshwater flux. Optionally, this freshwater flux can be imposed at the surface on biogeochemical
   ! variables, effectively mimicking precipitation or evaporation. This makes sense only if the salinity change
   ! is primarily due to surface fluxes - not if it is meant to represent lateral input of other water masses.
   virtual_dilution = _ZERO_
   if (salinity_relaxation_to_freshwater_flux) then
      ! NB unit of virtual_dilution is relative dilution across column, i.e., fraction/s
      if (any(SRelaxTau(1:nlev)<1.e10) .and. any(salt>0.)) &
         virtual_dilution = sum((salt(1:nlev)-sProf(1:nlev))/SRelaxTau(1:nlev)*curh(1:nlev))/sum(salt(1:nlev)*curh(1:nlev))
   end if

   ! Add surface flux due to evaporation/precipitation, unless the model explicitly says otherwise.
   do i=1,size(model%state_variables)
      if (.not. (model%state_variables(i)%no_precipitation_dilution .or. no_precipitation_dilution)) then
         sfl(i) = sfl(i)-cc(nlev,i)*dilution
         if (virtual_dilution/=_ZERO_) sfl(i) = sfl(i)-sum(cc(1:nlev,i)*curh(1:nlev))*virtual_dilution
      end if
   end do

   ! Vertical advection and residual movement (sinking/floating)
   ws1d(0   ) = _ZERO_
   ws1d(nlev) = _ZERO_
   iweights(1:nlev-1) = curh(2:nlev) / ( curh(1:nlev-1) + curh(2:nlev) )
   call system_clock(clock_start)
   do i=1,size(model%state_variables)
      if (cc_transport(i)) then
         ! Do advection step due to settling or rising
         ws1d(1:nlev-1) = iweights(1:nlev-1)*ws(1:nlev-1,i) + (_ONE_-iweights(1:nlev-1))*ws(2:nlev,i)
         call adv_center(nlev,dt,curh,curh,ws1d,flux,flux,_ZERO_,_ZERO_,w_adv_discr,adv_mode_1,cc(:,i))

         ! Do advection step due to vertical velocity
         if (w_adv_method/=0) call adv_center(nlev,dt,curh,curh,w,flux,flux,_ZERO_,_ZERO_,w_adv_ctr,adv_mode_0,cc(:,i))
      end if
   end do
   call system_clock(clock_end)
   clock_adv = clock_adv + clock_end-clock_start

   ! Vertical diffusion
   clock_start = clock_end
   do i=1,size(model%state_variables)
      if (cc_transport(i)) then
         ! Do diffusion step
         if (associated(cc_obs(i)%data)) then
   !        Observations on this variable are available.
            call diff_center(nlev,dt,cnpar,posconc(i),curh,Neumann,Neumann,&
               sfl(i),bfl(i),curnuh,Lsour,Qsour,cc_obs(i)%relax_tau,cc_obs(i)%data,cc(:,i))
         else
   !        Observations on this variable are not available.
            call diff_center(nlev,dt,cnpar,posconc(i),curh,Neumann,Neumann,&
               sfl(i),bfl(i),curnuh,Lsour,Qsour,DefaultRelaxTau,cc(:,i),cc(:,i))
         end if
      end if
   end do
   call system_clock(clock_end)
   clock_diff = clock_diff + clock_end-clock_start

   clock_start = clock_end

   ! Repair state before calling FABM
   call do_repair_state(nlev,'gotm_fabm::do_gotm_fabm, after advection/diffusion')

   do split=1,split_factor
      ! Update local light field (self-shading may have changed through changes in biological state variables)
      call light(nlev,bioshade_feedback)

      ! Time-integrate one biological time step
      call ode_solver(ode_method,size(cc,2),nlev,dt_eff,cc,right_hand_side_rhs,right_hand_side_ppdd)

      ! Provide FABM with (pointers to) updated state variables.
      ! (integration scheme has redirected FABM to a temporary biogeochemical state)
      do i=1,size(model%state_variables)
         call fabm_link_bulk_state_data(model,i,cc(1:nlev,i))
      end do
      do i=1,size(model%bottom_state_variables)
         call fabm_link_bottom_state_data(model,i,cc(1,size(model%state_variables)+i))
      end do
      do i=1,size(model%surface_state_variables)
         call fabm_link_surface_state_data(model,i,cc(nlev,size(model%state_variables)+size(model%bottom_state_variables)+i))
      end do

      ! Repair state
      call do_repair_state(nlev,'gotm_fabm::do_gotm_fabm, after time integration')
   end do

   call system_clock(clock_end)
   clock_source = clock_source + clock_end-clock_start

   if (associated(bio_albedo))     call fabm_get_albedo(model,nlev,bio_albedo)
   if (associated(bio_drag_scale)) call fabm_get_drag(model,nlev,bio_drag_scale)

   end subroutine do_gotm_fabm
!EOC

   subroutine check_fabm_expressions()
      class (type_expression),pointer :: expression
      integer :: n

      n = 0
      expression => model%root%first_expression
      do while (associated(expression))
         select type (expression)
            class is (type_vertical_integral)
               n = n + 1
         end select
         expression => expression%next
      end do

      allocate(horizontal_expression_data(n))
      horizontal_expression_data = _ZERO_

      n = 0
      expression => model%root%first_expression
      do while (associated(expression))
         select type (expression)
            class is (type_vertical_integral)
               n = n + 1
               call fabm_link_horizontal_data(model,trim(expression%output_name),horizontal_expression_data(n))
         end select
         expression => expression%next
      end do
   end subroutine

   subroutine update_fabm_expressions(nlev)
      integer,intent(in)             :: nlev

      class (type_expression),pointer :: expression
      real(rk)                       :: depth,weights(nlev)
      logical                        :: started
      integer                        :: n,k

      n = 0
      expression => model%root%first_expression
      do while (associated(expression))
         select type (expression)
            class is (type_vertical_integral)
               n = n + 1
               horizontal_expression_data(n) = calculate_vertical_mean(expression)
         end select
         expression => expression%next
      end do

   contains

      REALTYPE function calculate_vertical_mean(expression)
         class (type_vertical_integral),intent(in) :: expression

         ! Loop over all levels, surface to bottom, and compute vertical mean.
         depth = _ZERO_
         weights = _ZERO_
         started = .false.
         do k=nlev,1,-1
            depth = depth + curh(k)
            if (.not.started) then
               ! Not yet at minimum depth before
               if (depth>=expression%minimum_depth) then
                  ! Now crossing minimum depth interface
                  started = .true.
                  weights(k) = depth-expression%minimum_depth
               end if
            else
               ! Beyond minimum depth, not yet at maximum depth before
               weights(k) = curh(k)
            end if
            if (depth>expression%maximum_depth) then
               ! Now crossing maximum depth interface; subtract part of layer height that is not included
               weights(k) = weights(k) - (depth-expression%maximum_depth)
               exit
            end if
         end do
         if (expression%average) weights = weights/(min(expression%maximum_depth,depth)-expression%minimum_depth)
         calculate_vertical_mean = sum(model%data(expression%in)%p(1:nlev)*weights)
      end function

   end subroutine

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Check the current values of all state variables
!
! !INTERFACE:
   subroutine do_repair_state(nlev,location)
!
! !DESCRIPTION:
! Checks the current values of all state variables and repairs these
! if allowed and possible. If the state is invalid and repair is not
! allowed, the model is brought down.
!
! !INPUT PARAMETERS:
   integer,         intent(in) :: nlev
   character(len=*),intent(in) :: location
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   logical :: valid,tmpvalid
!
!-----------------------------------------------------------------------
!BOC
   call fabm_check_state(model,1,nlev,repair_state,valid)
   if (valid .or. repair_state) then
      call fabm_check_surface_state(model,nlev,repair_state,tmpvalid)
      valid = valid.and.tmpvalid
   end if
   if (valid .or. repair_state) then
      call fabm_check_bottom_state(model,1,repair_state,tmpvalid)
      valid = valid.and.tmpvalid
   end if
   if (.not. (valid .or. repair_state)) then
      FATAL 'State variable values are invalid and repair is not allowed.'
      FATAL location
      stop 'gotm_fabm::do_repair_state'
   end if

   end subroutine do_repair_state
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate sink/source terms as temporal derivative vector
!
! !INTERFACE:
   subroutine right_hand_side_rhs(first,numc,nlev,cc,rhs)
!
! !DESCRIPTION:
! Calculates the sink/source terms of the FABM variables and returns
! these as a vector with temporal derivatives.
!
! !INPUT PARAMETERS:
   logical, intent(in)                  :: first
   integer, intent(in)                  :: numc,nlev
   REALTYPE, intent(in)                 :: cc(0:nlev,1:numc)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)                :: rhs(0:nlev,1:numc)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: i,n
!
!-----------------------------------------------------------------------
!BOC
   ! Shortcut to the number of pelagic state variables.
   n = size(model%state_variables)

   ! Provide FABM with (pointers to) the current state.
   do i=1,size(model%state_variables)
      call fabm_link_bulk_state_data(model,i,cc(1:nlev,i))
   end do
   do i=1,size(model%bottom_state_variables)
      call fabm_link_bottom_state_data(model,i,cc(1,n+i))
   end do
   do i=1,size(model%surface_state_variables)
      call fabm_link_surface_state_data(model,i,cc(nlev,n+size(model%bottom_state_variables)+i))
   end do
   call update_fabm_expressions(nlev)

   ! If this is not the first step in the (multi-step) integration scheme,
   ! then first make sure that the intermediate state variable values are valid.
   if (.not. first) call do_repair_state(nlev,'gotm_fabm::right_hand_side_rhs')

   ! Initialization is needed because biogeochemical models increment or decrement
   ! the temporal derivatives, rather than setting them directly.
   rhs = _ZERO_

   ! Calculate temporal derivatives due to bottom processes (e.g. sedimentation, benthic biota).
   call fabm_do_bottom(model,1,rhs(1,1:n),rhs(1,n+1:n+size(model%bottom_state_variables)))

   ! Distribute bottom flux into pelagic over bottom box (i.e., divide by layer height).
   rhs(1,1:n) = rhs(1,1:n)/curh(1)

   if (.not.no_surface) then
      ! Calculate temporal derivatives due to surface processes (e.g. gas exchange, ice algae).
      call fabm_do_surface(model,nlev,rhs(nlev,1:n),rhs(nlev,n+size(model%bottom_state_variables)+1:))

      ! Distribute surface flux into pelagic over surface box (i.e., divide by layer height).
      rhs(nlev,1:n) = rhs(nlev,1:n)/curh(nlev)
   end if

   ! Add pelagic sink and source terms for all depth levels.
   call fabm_do(model,1,nlev,rhs(1:nlev,1:n))

   if (first) call save_diagnostics()

   end subroutine right_hand_side_rhs
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate sink/source terms as production/destruction matrices
!
! !INTERFACE:
   subroutine right_hand_side_ppdd(first,numc,nlev,cc,pp,dd)
!
! !DESCRIPTION:
! Calculates the sink/source terms of the FABM variables and returns
! these as production and destruction matrices \cite{Burchardetal2003b}.
!
! !INPUT PARAMETERS:
   logical,  intent(in)                 :: first
   integer,  intent(in)                 :: numc,nlev
   REALTYPE, intent(in)                 :: cc(0:nlev,1:numc)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out),dimension(0:nlev,1:numc,1:numc) :: pp,dd
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: i,n
!
!-----------------------------------------------------------------------
!BOC
   ! Shortcut to the number of pelagic state variables.
   n = size(model%state_variables)

   ! Provide FABM with (pointers to) the current state.
   do i=1,size(model%state_variables)
      call fabm_link_bulk_state_data(model,i,cc(1:nlev,i))
   end do
   do i=1,size(model%bottom_state_variables)
      call fabm_link_bottom_state_data(model,i,cc(1,n+i))
   end do
   do i=1,size(model%surface_state_variables)
      call fabm_link_surface_state_data(model,i,cc(nlev,n+size(model%bottom_state_variables)+i))
   end do
   call update_fabm_expressions(nlev)

   ! If this is not the first step in the (multi-step) integration scheme,
   ! then first make sure that the intermediate state variable values are valid.
   if (.not. first) call do_repair_state(nlev,'gotm_fabm::right_hand_side_ppdd')

   ! Initialize production and destruction matrices to zero because FABM
   ! biogeochemical models increment these, rather than set these.
   pp = _ZERO_
   dd = _ZERO_

   ! Calculate temporal derivatives due to benthic processes.
   call fabm_do_benthos(model,1,pp(1,:,:),dd(1,:,:),n)

   ! Distribute bottom flux into pelagic over bottom box (i.e., divide by layer height).
   pp(1,1:n,:) = pp(1,1:n,:)/curh(1)
   dd(1,1:n,:) = dd(1,1:n,:)/curh(1)

   ! Add pelagic sink and source terms for all depth levels.
   call fabm_do(model,1,nlev,pp(1:nlev,1:n,1:n),dd(1:nlev,1:n,1:n))

   if (first) call save_diagnostics()

   end subroutine right_hand_side_ppdd
!EOC

   subroutine save_diagnostics()
      integer :: i

      ! Time-integrate diagnostic variables defined on horizontal slices, where needed.
      do i=1,size(model%horizontal_diagnostic_variables)
         if (model%horizontal_diagnostic_variables(i)%output==output_instantaneous) then
            ! Simply use last value
            cc_diag_hz(i) = fabm_get_horizontal_diagnostic_data(model,i)
         elseif (model%horizontal_diagnostic_variables(i)%output/=output_none) then
            ! Integration or averaging in time needed: for now do simple Forward Euler integration.
            ! If averaging is required, this will be done upon output by dividing by the elapsed period.
            cc_diag_hz(i) = cc_diag_hz(i) + fabm_get_horizontal_diagnostic_data(model,i)*dt_eff
         end if
      end do

      ! Time-integrate diagnostic variables defined on the full domain, where needed.
      do i=1,size(model%diagnostic_variables)
         if (model%diagnostic_variables(i)%output==output_instantaneous) then
            ! Simply use last value
            cc_diag(:,i) = fabm_get_bulk_diagnostic_data(model,i)
         elseif (model%diagnostic_variables(i)%output/=output_none) then
            ! Integration or averaging in time needed: for now do simple Forward Euler integration.
            ! If averaging is required, this will be done upon output by dividing by the elapsed period.
            cc_diag(:,i) = cc_diag(:,i) + fabm_get_bulk_diagnostic_data(model,i)*dt_eff
         end if
      end do
   end subroutine

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish biogeochemical model
!
! !INTERFACE:
   subroutine clean_gotm_fabm
!
! !DESCRIPTION:
!  Report timing results and deallocate memory.
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP

   integer(8) :: clock,ticks_per_sec
   REALTYPE :: tick_rate
!-----------------------------------------------------------------------
!BOC
   if (.not. fabm_calc) return

   LEVEL1 'clean_gotm_fabm'

   call system_clock( count=clock, count_rate=ticks_per_sec)
   tick_rate = _ONE_/ticks_per_sec

   LEVEL1 'Time spent on advection of FABM variables:',clock_adv*tick_rate
   LEVEL1 'Time spent on diffusion of FABM variables:',clock_diff*tick_rate
   LEVEL1 'Time spent on sink/source terms of FABM variables:',clock_source*tick_rate

   ! Deallocate internal arrays
   if (allocated(cc))         deallocate(cc)
   if (allocated(cc_obs))     deallocate(cc_obs)
   if (allocated(cc_ben_obs)) deallocate(cc_ben_obs)
   if (allocated(cc_diag))    deallocate(cc_diag)
   if (allocated(cc_diag_hz)) deallocate(cc_diag_hz)
   if (allocated(horizontal_expression_data)) deallocate(horizontal_expression_data)

   ! Deallocate work arrays used from do_gotm_fabm.
   if (allocated(ws))              deallocate(ws)
   if (allocated(sfl))             deallocate(sfl)
   if (allocated(bfl))             deallocate(bfl)
   if (allocated(Qsour))           deallocate(Qsour)
   if (allocated(Lsour))           deallocate(Lsour)
   if (allocated(DefaultRelaxTau)) deallocate(DefaultRelaxTau)
   if (allocated(curh))            deallocate(curh)
   if (allocated(curnuh))          deallocate(curnuh)
   if (allocated(cc_transport))    deallocate(cc_transport)
   if (allocated(posconc))         deallocate(posconc)

   ! Deallocate arrays with internally computed environmental variables.
   if (allocated(par))   deallocate(par)
   if (allocated(k_par)) deallocate(k_par)
   if (allocated(swr))   deallocate(swr)
   if (allocated(pres))  deallocate(pres)
   if (allocated(z))     deallocate(z)

   ! Reset all module-level pointers
   nullify(nuh)
   nullify(h)
   nullify(bioshade)
   nullify(w)
   nullify(rho)
   nullify(SRelaxTau)
   nullify(sProf)
   nullify(salt)
   nullify(precip)
   nullify(evap)
   nullify(bio_drag_scale)
   nullify(bio_albedo)
   nullify(I_0)
   nullify(A)
   nullify(g1)
   nullify(g2)
   nullify(yearday)
   nullify(secondsofday)

   LEVEL1 'done.'

   end subroutine clean_gotm_fabm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate light over entire column
!
! !INTERFACE:
   subroutine light(nlev,bioshade_feedback)
!
! !DESCRIPTION:
! Calculate photosynthetically active radiation (PAR) and short wave
! radiation (SWR) over entire column, using surface short wave radiation,
! and background and biotic extinction.
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   logical, intent(in)                 :: bioshade_feedback
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: i
   REALTYPE :: bioext
   REALTYPE :: localexts(1:nlev)
!
!-----------------------------------------------------------------------
!BOC
   bioext = _ZERO_

   call fabm_get_light_extinction(model,1,nlev,k_par(1:nlev))
   call fabm_get_light(model,1,nlev)

   call fabm_get_light_extinction(model,1,nlev,localexts(1:nlev))
   do i=nlev,1,-1

      ! Add the extinction of the first half of the grid box.
      bioext = bioext+localexts(i)*curh(i)/2

      ! Calculate photosynthetically active radiation (PAR), shortwave radiation, and PAR attenuation.
      par(i) = I_0*(1-A)*exp(-z(i)/g2-bioext)
      swr(i) = par(i)+I_0*A*exp(-z(i)/g1)
      k_par(i) = 1/g2+localexts(i)

      ! Add the extinction of the second half of the grid box.
      bioext = bioext+localexts(i)*curh(i)/2

      if (bioshade_feedback) bioshade(i)=exp(-bioext)
   end do

   end subroutine light
!EOC

   subroutine register_bulk_observation(id,data,relax_tau)
      type(type_bulk_variable_id),intent(inout) :: id
      REALTYPE,target,dimension(0:) :: data,relax_tau

      integer                         :: i
      character(len=attribute_length) :: varname

      varname = fabm_get_variable_name(model,id)
      do i=1,size(model%state_variables)
         if (varname==model%state_variables(i)%name) then
            ! This is a state variable. Register the link between the state variable and the observations.
            cc_obs(i)%data => data
            cc_obs(i)%relax_tau => relax_tau
            return
         end if
      end do
      call fabm_link_bulk_data(model,id,data(1:))
   end subroutine register_bulk_observation

   subroutine register_horizontal_observation(horizontal_id,data,relax_tau)
      type(type_horizontal_variable_id),intent(inout) :: horizontal_id
      REALTYPE,target                        :: data,relax_tau
      integer                         :: i
      character(len=attribute_length) :: varname

      varname = fabm_get_variable_name(model,horizontal_id)
      do i=1,size(model%bottom_state_variables)
         if (varname==model%bottom_state_variables(i)%name) then
            ! This is a state variable. Register the link between the state variable and the observations.
            cc_ben_obs(i)%data => data
            cc_ben_obs(i)%relax_tau => relax_tau
            return
         end if
      end do
      call fabm_link_horizontal_data(model,horizontal_id,data)
   end subroutine register_horizontal_observation

   subroutine register_scalar_observation(scalar_id,data)
      type(type_scalar_variable_id),intent(inout) :: scalar_id
      REALTYPE,target                    :: data

      call fabm_link_scalar_data(model,scalar_id,data)
   end subroutine register_scalar_observation

   subroutine init_gotm_fabm_state(nlev)
      integer,intent(in) :: nlev

      integer :: i
      REALTYPE :: rhs(1:nlev,1:size(model%state_variables)),bottom_flux(size(model%bottom_state_variables)),surface_flux(size(model%surface_state_variables))

      call fabm_check_ready(model)
      fabm_ready = .true.

      ! Allow individual biogeochemical models to provide a custom initial state.
      call fabm_initialize_state(model,1,nlev)
      call fabm_initialize_surface_state(model,nlev)
      call fabm_initialize_bottom_state(model,1)

      ! If custom initial state have been provided through fabm_input.nml, use these to override the current initial state.
      do i=1,size(model%state_variables)
         if (associated(cc_obs(i)%data)) cc(:,i) = cc_obs(i)%data
      end do
      do i=1,size(model%bottom_state_variables)
         if (associated(cc_ben_obs(i)%data)) cc(1,size(model%state_variables,1)+i) = cc_ben_obs(i)%data
      end do

      ! Compute pressure, depth, day of the year
      call calculate_derived_input(nlev,_ZERO_)

      ! Update derived expressions (vertical means, etc.)
      call update_fabm_expressions(nlev)

      ! Call fabm_do here to make sure diagnostic variables all have an initial value.
      ! Note that rhs (biogeochemical source-sink terms) is a dummy variable that remains unused.
      rhs = _ZERO_
      call fabm_do(model,1,nlev,rhs)
      call fabm_do_surface(model,nlev,rhs(nlev,:))
      call fabm_do_bottom(model,1,rhs(1,:),bottom_flux)

      ! Obtain current values of diagnostic variables from FABM.
      do i=1,size(model%horizontal_diagnostic_variables)
         if (model%horizontal_diagnostic_variables(i)%output/=output_time_integrated &
             .and.model%horizontal_diagnostic_variables(i)%output/=output_none) &
            cc_diag_hz(i) = fabm_get_horizontal_diagnostic_data(model,i)
      end do
      do i=1,size(model%diagnostic_variables)
         if (model%diagnostic_variables(i)%output/=output_time_integrated.and.model%diagnostic_variables(i)%output/=output_none) &
            cc_diag(:,i) = fabm_get_bulk_diagnostic_data(model,i)
      end do

   end subroutine init_gotm_fabm_state

   subroutine gotm_driver_fatal_error(self,location,message)
      class (type_gotm_driver), intent(inout) :: self
      character(len=*),  intent(in)    :: location,message

      FATAL trim(location)//': '//trim(message)
      stop 1
   end subroutine

   subroutine gotm_driver_log_message(self,message)
      class (type_gotm_driver), intent(inout) :: self
      character(len=*),  intent(in)    :: message

      write (*,*) trim(message)
   end subroutine

   end module gotm_fabm

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


#include "cppdefs.h"
#include "fabm_driver.h"
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

   implicit none
!
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_fabm
   public set_env_gotm_fabm,do_gotm_fabm
   public clean_gotm_fabm
   public fabm_calc

   ! Variables below must be accessible for gotm_fabm_output
   public local,total,dt,h
!
! !PUBLIC DATA MEMBERS:
!  The one and only model
   type (type_model), pointer, public :: model

!  Arrays for state and diagnostic variables
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_,:),public,target :: cc
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_,:),public        :: cc_diag
   REALTYPE,allocatable,dimension(:),                      public        :: cc_diag_hz

!  Arrays for observations, relaxation times and FABM variable identifiers associated with the observations.
   REALTYPE, allocatable, target, public :: obs_0d(:),obs_1d(:,:),relax_tau_0d(:),relax_tau_1d(:,:)
   integer,  allocatable,         public :: obs_0d_ids(:), obs_1d_ids(:)

!  Observation indices (from obs_0d, obs_1d) for pelagic and benthic state variables.
   integer,allocatable,dimension(:),public :: cc_ben_obs_indices,cc_obs_indices

!  Variables to hold time spent on advection, diffusion, sink/source terms.
   integer(8) :: clock_adv,clock_diff,clock_source
!
! !REVISION HISTORY:!
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!
! !PRIVATE DATA MEMBERS:
   ! Namelist variables
   REALTYPE                  :: cnpar
   integer                   :: w_adv_method,w_adv_discr,ode_method,split_factor
   logical                   :: fabm_calc,bioshade_feedback,repair_state, &
                                no_precipitation_dilution,salinity_relaxation_to_freshwater_flux

   ! Arrays for work, vertical movement, and cross-boundary fluxes
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_,:) :: ws
   REALTYPE,allocatable,dimension(:)                       :: sfl,bfl,total
   REALTYPE,allocatable _ATTR_DIMENSIONS_1_                :: local

   ! Arrays for environmental variables not supplied externally.
   REALTYPE,allocatable,dimension(_LOCATION_DIMENSIONS_)   :: par,pres,swr

   ! External variables
   REALTYPE :: dt,dt_eff   ! External and internal time steps
   integer  :: w_adv_ctr   ! Scheme for vertical advection (0 if not used)
   REALTYPE,pointer,dimension(_LOCATION_DIMENSIONS_) :: nuh,h,bioshade,w,z,rho
   REALTYPE,pointer,dimension(_LOCATION_DIMENSIONS_) :: SRelaxTau,sProf,salt
   REALTYPE,pointer _ATTR_LOCATION_DIMENSIONS_HZ_  :: precip,evap

   REALTYPE,pointer :: I_0,A,g1,g2

!  Explicit interface for ode solver.
   interface
      subroutine ode_solver(solver,numc,nlev,dt,cc,right_hand_side_rhs,right_hand_side_ppdd)
         integer,  intent(in)                :: solver,nlev,numc
         REALTYPE, intent(in)                :: dt
         REALTYPE, intent(inout)             :: cc(1:numc,0:nlev)

         interface
            subroutine right_hand_side_ppdd(first,numc,nlev,cc,pp,dd)
               logical,  intent(in)                 :: first
               integer,  intent(in)                 :: numc,nlev
               REALTYPE, intent(in)                 :: cc(1:numc,0:nlev)

               REALTYPE, intent(out)                :: pp(1:numc,1:numc,0:nlev)
               REALTYPE, intent(out)                :: dd(1:numc,1:numc,0:nlev)
            end
         end interface

         interface
            subroutine right_hand_side_rhs(first,numc,nlev,cc,rhs)
               logical,  intent(in)                 :: first
               integer,  intent(in)                 :: numc,nlev
               REALTYPE, intent(in)                 :: cc(1:numc,0:nlev)
               REALTYPE, intent(out)                :: rhs(1:numc,0:nlev)
            end
         end interface
      end subroutine
   end interface

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the FABM driver
!
! !INTERFACE:
   subroutine init_gotm_fabm(_LOCATION_,namlst,fname)
!
! !DESCRIPTION:
! Initializes the GOTM-FABM driver module by reading settings from fabm.nml.
!
! !INPUT PARAMETERS:
   integer,          intent(in)        :: _LOCATION_,namlst
   character(len=*), intent(in)        :: fname
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
!  local variables
   integer                   :: i,j,variablecount,ivariables(256)
   character(len=64)         :: variables(256),file
   type (type_model),pointer :: childmodel
   namelist /gotm_fabm_nml/ fabm_calc,                                 &
                            cnpar,w_adv_discr,ode_method,split_factor,        &
                            bioshade_feedback,repair_state,no_precipitation_dilution, &
                            salinity_relaxation_to_freshwater_flux
!
!-----------------------------------------------------------------------
!BOC

   LEVEL1 'init_gotm_fabm'

   ! Initialize FABM model identifiers to invalid id.
   fabm_calc         = .false.
   cnpar             = _ONE_
   w_adv_discr       = 6
   ode_method        = 1
   split_factor      = 1
   bioshade_feedback = .true.
   repair_state      = .false.
   salinity_relaxation_to_freshwater_flux = .false.
   no_precipitation_dilution = .false. ! useful to check mass conservation

   ! Open the namelist file and read the namelist.
   ! Note that the namelist file is left open until the routine terminates,
   ! so FABM can read more namelists from it during initialization.
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=gotm_fabm_nml,err=99)
   close(namlst)

   if (fabm_calc) then

      clock_adv    = 0
      clock_diff   = 0
      clock_source = 0

      ! Create model tree
      model => fabm_create_model_from_file(namlst)

      ! Initialize model tree (creates metadata and assigns variable identifiers)
      call fabm_set_domain(model,_LOCATION_)

      ! Report prognostic variable descriptions
      LEVEL2 'FABM pelagic state variables:'
      do i=1,ubound(model%info%state_variables,1)
         LEVEL3 trim(model%info%state_variables(i)%name), '  ', &
                trim(model%info%state_variables(i)%units),'  ',&
                trim(model%info%state_variables(i)%longname)
      end do

      LEVEL2 'FABM benthic state variables:'
      do i=1,ubound(model%info%state_variables_ben,1)
         LEVEL3 trim(model%info%state_variables_ben(i)%name), '  ', &
                trim(model%info%state_variables_ben(i)%units),'  ',&
                trim(model%info%state_variables_ben(i)%longname)
      end do

      ! Report diagnostic variable descriptions
      LEVEL2 'FABM diagnostic variables defined on the full model domain:'
      do i=1,ubound(model%info%diagnostic_variables,1)
         LEVEL3 trim(model%info%diagnostic_variables(i)%name), '  ', &
                trim(model%info%diagnostic_variables(i)%units),'  ',&
                trim(model%info%diagnostic_variables(i)%longname)
      end do

      LEVEL2 'FABM diagnostic variables defined on a horizontal slice of the model domain:'
      do i=1,ubound(model%info%diagnostic_variables_hz,1)
         LEVEL3 trim(model%info%diagnostic_variables_hz(i)%name), '  ', &
                trim(model%info%diagnostic_variables_hz(i)%units),'  ',&
                trim(model%info%diagnostic_variables_hz(i)%longname)
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

      ! Initialize spatially explicit variables
      call init_var_gotm_fabm(_LOCATION_)

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
   subroutine init_var_gotm_fabm(_LOCATION_)
!
! !DESCRIPTION:
! This routine allocates memory for all variables needed to hold the values of
! FABM variables.
!
   _DECLARE_LOCATION_ARG_
!
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
   allocate(cc(1:ubound(model%info%state_variables,1)+ubound(model%info%state_variables_ben,1),_LOCATION_RANGE_),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (cc)'
   cc = _ZERO_
   do i=1,ubound(model%info%state_variables,1)
      cc(i,:) = model%info%state_variables(i)%initial_value
      call fabm_link_state_data(model,i,cc(i,1:))
   end do
   do i=1,ubound(model%info%state_variables_ben,1)
      cc(ubound(model%info%state_variables,1)+i,1) = model%info%state_variables_ben(i)%initial_value
      call fabm_link_benthos_state_data(model,i,cc(ubound(model%info%state_variables,1)+i,1))
   end do

   ! Allocate arrays that contain observation indices of pelagic and benthic state variables.
   ! Initialize observation indices to -1 (no external observations provided)
   allocate(cc_obs_indices    (1:ubound(model%info%state_variables,1)),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (cc_obs_indices)'
   cc_obs_indices = -1
   allocate(cc_ben_obs_indices(1:ubound(model%info%state_variables_ben,1)),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (cc_ben_obs_indices)'
   cc_ben_obs_indices = -1

   ! Allocate array for pelagic diagnostic variables; set all values to zero.
   ! (zeroing is needed because time-integrated/averaged variables will increment rather than set the array)
   allocate(cc_diag(1:ubound(model%info%diagnostic_variables,1),_LOCATION_RANGE_),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (cc_diag)'
   cc_diag = _ZERO_

   ! Allocate array for diagnostic variables on horizontal surfaces; set all values to zero.
   ! (zeroing is needed because time-integrated/averaged variables will increment rather than set the array)
   allocate(cc_diag_hz(1:ubound(model%info%diagnostic_variables_hz,1)),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (cc_diag_hz)'
   cc_diag_hz = _ZERO_

   ! Allocate array for vertical movement rates (m/s, positive for upwards),
   ! and set these to the values provided by the model.
   allocate(ws(_LOCATION_RANGE_,1:ubound(model%info%state_variables,1)),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (ws)'
   do i=1,ubound(model%info%state_variables,1)
      ws(:,i) = model%info%state_variables(i)%vertical_movement
   end do

   ! Allocate array for surface fluxes and initialize these to zero (no flux).
   allocate(sfl(1:ubound(model%info%state_variables,1)),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (sfl)'
   sfl = _ZERO_

   ! Allocate array for bottom fluxes and initialize these to zero (no flux).
   allocate(bfl(1:ubound(model%info%state_variables,1)),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (bfl)'
   bfl = _ZERO_

   ! Allocate array for photosynthetically active radiation (PAR).
   ! This will be calculated internally during each time step.
   allocate(par(_LOCATION_RANGE_),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (par)'
   call fabm_link_data(model,varname_par,par(1:_LOCATION_))

   ! Allocate array for shortwave radiation (swr).
   ! This will be calculated internally during each time step.
   allocate(swr(_LOCATION_RANGE_),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (swr)'
   call fabm_link_data(model,varname_swr,swr(1:_LOCATION_))

   ! Allocate array for local pressure.
   ! This will be calculated from layer depths and density internally during each time step.
   allocate(pres(_LOCATION_RANGE_),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (pres)'
   call fabm_link_data(model,varname_pres,pres(1:_LOCATION_))

   ! Allocate arrays for storing local and column-integrated values of conserved quantities.
   ! These are used during each save.
   allocate(total(1:ubound(model%info%conserved_quantities,1)),stat=rc)
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (total)'
#ifdef _FABM_USE_1D_LOOP_
   allocate(local(1:_LOCATION_,1:ubound(model%info%conserved_quantities,1)),stat=rc)
#else
   allocate(local(1:ubound(model%info%conserved_quantities,1)),stat=rc)
#endif
   if (rc /= 0) STOP 'allocate_memory(): Error allocating (local)'

   end subroutine init_var_gotm_fabm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set environment for FABM
!
! !INTERFACE:
   subroutine set_env_gotm_fabm(dt_,w_adv_method_,w_adv_ctr_,temp,salt_,rho_,nuh_,h_,w_, &
                                bioshade_,I_0_,taub,wnd,precip_,evap_,z_,A_,g1_,g2_,SRelaxTau_,sProf_)
!
! !DESCRIPTION:
! This routine is called once from GOTM to provide pointers to the arrays that describe
! the physical environment relevant for biogeochemical processes (temprature, salinity, etc.)
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in) :: dt_
   integer,  intent(in) :: w_adv_method_,w_adv_ctr_
   REALTYPE, intent(in),target _ATTR_LOCATION_DIMENSIONS_    :: temp,salt_,rho_,nuh_,h_,w_,bioshade_,z_
   REALTYPE, intent(in),optional,target _ATTR_LOCATION_DIMENSIONS_ :: SRelaxTau_,sProf_
   REALTYPE, intent(in),target _ATTR_LOCATION_DIMENSIONS_HZ_ :: I_0_,wnd,precip_,evap_,taub
   REALTYPE, intent(in),target :: A_,g1_,g2_
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i,j
   logical                   :: isstatevariable
!-----------------------------------------------------------------------!
!BOC
   if (.not. fabm_calc) return

   ! Provide pointers to arrays with environmental variables to FABM.
   call fabm_link_data   (model,varname_temp,   temp)
   call fabm_link_data   (model,varname_salt,   salt_)
   call fabm_link_data   (model,varname_dens,   rho_)
   call fabm_link_data_hz(model,varname_wind_sf,wnd)
   call fabm_link_data_hz(model,varname_par_sf, I_0_)
   call fabm_link_data_hz(model,varname_taub,   taub)

   ! Save pointers to external dynamic variables that we need later (in do_gotm_fabm)
   nuh      => nuh_        ! turbulent heat diffusivity [1d array] used to diffuse biogeochemical state variables
   h        => h_          ! layer heights [1d array] needed for advection, diffusion
   w        => w_          ! vertical medium velocity [1d array] needed for advection of biogeochemical state variables
   bioshade => bioshade_   ! biogeochemical light attenuation coefficients [1d array], output of biogeochemistry, input for physics
   z        => z_          ! depth [1d array], used to calculate local pressure
   precip   => precip_     ! precipitation [scalar] - used to calculate dilution due to increased water volume
   evap     => evap_       ! evaporation [scalar] - used to calculate concentration due to decreased water volume
   salt     => salt_       ! salinity [1d array] - used to calculate virtual freshening due to salinity relaxation
   rho      => rho_        ! density [1d array] - used to calculate bottom stress from bottom friction velocity.

   if (present(SRelaxTau_) .and. present(sProf_)) then
      SRelaxTau => SRelaxTau_ ! salinity relaxation times  [1d array] - used to calculate virtual freshening due to salinity relaxation
      sProf     => sProf_     ! salinity relaxation values [1d array] - used to calculate virtual freshening due to salinity relaxation
   else
      if (salinity_relaxation_to_freshwater_flux) &
         stop 'gotm_fabm:set_env_gotm_fabm: salinity_relaxation_to_freshwater_flux is set, but salinity relaxation arrays are not provided.'
      nullify(SRelaxTau)
      nullify(sProf)
   end if

   ! Copy scalars that will not change during simulation, and are needed in do_gotm_fabm)
   dt = dt_
   w_adv_method = w_adv_method_
   w_adv_ctr = w_adv_ctr_

   ! Calculate and save internal time step.
   dt_eff = dt/float(split_factor)

   I_0 => I_0_
   A => A_
   g1 => g1_
   g2 => g2_

   ! Handle externally provided 0d observations.
   if (allocated(obs_0d_ids)) then
      do i=1,ubound(obs_0d_ids,1)
         if (obs_0d_ids(i).ne.-1) then
            ! Check whether this is a state variable.
            ! If so, observations will be handled through relaxation, not here.
            isstatevariable = .false.
            do j=1,ubound(cc_ben_obs_indices,1)
               if (obs_0d_ids(i).eq.cc_ben_obs_indices(j)) isstatevariable = .true.
            end do

            ! If not a state variable, handle the observations by providing FABM with a pointer to the observed data.
            if (.not. isstatevariable) call fabm_link_data_hz(model,obs_0d_ids(i),obs_0d(i))
         end if
      end do
   end if

   ! Handle externally provided 1d observations.
   if (allocated(obs_1d_ids)) then
      do i=1,ubound(obs_1d_ids,1)
         if (obs_1d_ids(i).ne.-1) then
            ! Check whether this is a state variable.
            ! If so, observations will be handled through relaxation, not here.
            isstatevariable = .false.
            do j=1,ubound(cc_obs_indices,1)
               if (obs_1d_ids(i).eq.cc_obs_indices(j)) isstatevariable = .true.
            end do

            ! If not a state variable, handle the observations by providing FABM with a pointer to the observed data.
            if (.not. isstatevariable) call fabm_link_data(model,obs_1d_ids(i),obs_1d(1:,i))
         end if
      end do
   end if

   ! At this stage, FABM has been provided with arrays for all state variables, any variables
   ! read in from file (gotm_fabm_input), and all variables exposed by GOTM. If FABM is still
   ! lacking variable references, this should now trigger an error.
   call fabm_check_ready(model)

   end subroutine set_env_gotm_fabm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the FABM model
!
! !INTERFACE:
   subroutine do_gotm_fabm(nlev)
!
! !DESCRIPTION:
! TODO
!
! !USES:
   use util,only: flux,Neumann
!
   integer, intent(in) :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer, parameter        :: adv_mode_0=0
   integer, parameter        :: adv_mode_1=1
   REALTYPE                  :: Qsour(0:nlev),Lsour(0:nlev)
   REALTYPE                  :: RelaxTau(0:nlev)
   REALTYPE                  :: dilution,virtual_dilution
   integer                   :: i
   integer                   :: split,posconc
   integer(8)                :: clock_start,clock_end
!
!-----------------------------------------------------------------------
!BOC

   if (.not. fabm_calc) return

   ! Set local source terms for diffusion scheme to zero,
   ! because source terms are integrated independently later on.
   Qsour    = _ZERO_
   Lsour    = _ZERO_

!  Default: no relaxation
   RelaxTau = 1.d15

   ! Calculate local pressure from layer height and density
   pres(nlev) = rho(nlev)*h(nlev+1)*0.5d0
   do i=nlev-1,1,-1
      pres(i) = pres(i+1) + (rho(i)*h(i+1)+rho(i+1)*h(i+2))*0.5d0
   end do
   pres(1:nlev) = pres(1:nlev)*9.81d-4

!  Transfer current state to FABM.
   do i=1,ubound(model%info%state_variables,1)
      call fabm_link_state_data(model,i,cc(i,1:nlev))
   end do
   do i=1,ubound(model%info%state_variables_ben,1)
      call fabm_link_benthos_state_data(model,i,cc(ubound(model%info%state_variables,1)+i,1))
   end do

   ! Get updated vertical movement (m/s, positive for upwards) for biological state variables.
#ifdef _FABM_USE_1D_LOOP_
   call fabm_get_vertical_movement(model,1,nlev,ws(1:nlev,:))
#else
   do i=1,nlev
      call fabm_get_vertical_movement(model,i,ws(i,:))
   end do
#endif

   ! Get updated air-sea fluxes for biological state variables.
   call fabm_get_surface_exchange(model,nlev,sfl)

   ! Calculate dilution due to surface freshwater flux (m/s)
   dilution = precip+evap

   ! If salinity is relaxed to observations, the change in column-integrated salintiy can converted into a
   ! a virtual freshwater flux. Optionally, this freshwater flux can be imposed at the surface on biogoeochemical
   ! variables, effectively mimicking precipitation or evaporation. This makes sense only if the salinity change
   ! is primarily due to surface fluxes - not if it is meant to represent lateral input of other water masses.
   virtual_dilution = _ZERO_
   if (salinity_relaxation_to_freshwater_flux) then
      ! NB unit of virtual_dilution is relative dilution across column, i.e., fraction/s
      if (any(SRelaxTau(1:nlev)<1.e10)) &
         virtual_dilution = sum((salt(1:nlev)-sProf(1:nlev))/SRelaxTau(1:nlev)*h(2:nlev+1))/sum(salt(1:nlev)*h(2:nlev+1))
   end if

   do i=1,ubound(model%info%state_variables,1)
      ! Add surface flux due to evaporation/precipitation, unless the model explicitly says otherwise.
      if (.not. (model%info%state_variables(i)%no_precipitation_dilution .or. no_precipitation_dilution)) then
         sfl(i) = sfl(i)-cc(i,nlev)*dilution
         if (virtual_dilution.ne._ZERO_) sfl(i) = sfl(i)-sum(cc(i,1:nlev)*h(2:nlev+1))*virtual_dilution
      end if

      ! Determine whether the variable is positive-definite based on its lower allowed bound.
      posconc = 0
      if (model%info%state_variables(i)%minimum.ge._ZERO_) posconc = 1

      call system_clock(clock_start)

      ! Do advection step due to settling or rising
      call adv_center(nlev,dt,h,h,ws(:,i),flux,                   &
           flux,_ZERO_,_ZERO_,w_adv_discr,adv_mode_1,cc(i,:))

      ! Do advection step due to vertical velocity
      if (w_adv_method .ne. 0) &
         call adv_center(nlev,dt,h,h,w,flux,                   &
              flux,_ZERO_,_ZERO_,w_adv_ctr,adv_mode_0,cc(i,:))

      call system_clock(clock_end)
      clock_adv = clock_adv + clock_end-clock_start

      call system_clock(clock_start)

      ! Do diffusion step
      if (cc_obs_indices(i).ne.-1) then
!        Observations on this variable are available.
         call diff_center(nlev,dt,cnpar,posconc,h,Neumann,Neumann,&
            sfl(i),bfl(i),nuh,Lsour,Qsour,relax_tau_1d(:,cc_obs_indices(i)),obs_1d(:,cc_obs_indices(i)),cc(i,:))
      else
!        Observations on this variable are not available.
         call diff_center(nlev,dt,cnpar,posconc,h,Neumann,Neumann,&
            sfl(i),bfl(i),nuh,Lsour,Qsour,RelaxTau,cc(i,:),cc(i,:))
      end if

      call system_clock(clock_end)
      clock_diff = clock_diff + clock_end-clock_start

   end do

   ! Repair state before calling FABM
   call do_repair_state(nlev,'gotm_fabm::do_gotm_fabm, after advection/diffusion')

   call system_clock(clock_start)

   do split=1,split_factor
      ! Update local light field (self-shading may have changed through changes in biological state variables)
      call light(nlev,bioshade_feedback)

      ! Time-integrate one biological time step
      call ode_solver(ode_method,ubound(cc,1),nlev,dt_eff,cc(:,0:nlev),right_hand_side_rhs,right_hand_side_ppdd)

      ! Provide FABM with (pointers to) updated state variables.
      do i=1,ubound(model%info%state_variables,1)
         call fabm_link_state_data(model,i,cc(i,1:nlev))
      end do
      do i=1,ubound(model%info%state_variables_ben,1)
         call fabm_link_benthos_state_data(model,i,cc(ubound(model%info%state_variables,1)+i,1))
      end do

      ! Repair state
      call do_repair_state(nlev,'gotm_fabm::do_gotm_fabm, after time integration')

      ! Time-integrate diagnostic variables defined on horizontal slices, where needed.
      do i=1,ubound(model%info%diagnostic_variables_hz,1)
         if (model%info%diagnostic_variables_hz(i)%time_treatment.eq.time_treatment_last) then
            ! Simply use last value
            cc_diag_hz(i) = fabm_get_diagnostic_data_hz(model,i)
         else
            ! Integration or averaging in time needed: for now do simple Forward Euler integration.
            ! If averaging is required, this will be done upon output by diving by the elapsed period.
            cc_diag_hz(i) = cc_diag_hz(i) + fabm_get_diagnostic_data_hz(model,i)*dt_eff
         end if
      end do

      ! Time-integrate diagnostic variables defined on the full domain, where needed.
      do i=1,ubound(model%info%diagnostic_variables,1)
         if (model%info%diagnostic_variables(i)%time_treatment.eq.time_treatment_last) then
            ! Simply use last value
            cc_diag(i,1:nlev) = fabm_get_diagnostic_data(model,i)
         else
            ! Integration or averaging in time needed: for now do simple Forward Euler integration.
            ! If averaging is required, this will be done upon output by diving by the elapsed period.
            cc_diag(i,1:nlev) = cc_diag(i,1:nlev) + fabm_get_diagnostic_data(model,i)*dt_eff
         end if
      end do
   end do

   call system_clock(clock_end)
   clock_source = clock_source + clock_end-clock_start

   end subroutine do_gotm_fabm
!EOC

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
   logical :: valid
#ifndef _FABM_USE_1D_LOOP_
   integer :: ci
#endif
!
!-----------------------------------------------------------------------
!BOC
#ifdef _FABM_USE_1D_LOOP_
   call fabm_check_state(model,1,nlev,repair_state,valid)
#else
   do ci=1,nlev
      call fabm_check_state(model,ci,repair_state,valid)
      if (.not.(valid.or.repair_state)) exit
   end do
#endif
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
   REALTYPE, intent(in)                 :: cc(1:numc,0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)                :: rhs(1:numc,0:nlev)
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
   n = ubound(model%info%state_variables,1)

   ! Provide FABM with (pointers to) the current state.
   do i=1,ubound(model%info%state_variables,1)
      call fabm_link_state_data(model,i,cc(i,1:nlev))
   end do
   do i=1,ubound(model%info%state_variables_ben,1)
      call fabm_link_benthos_state_data(model,i,cc(n+i,1))
   end do

   ! If this is not the first step in the (multi-step) integration scheme,
   ! then first make sure that the intermediate state variable values are valid.
   if (.not. first) call do_repair_state(nlev,'gotm_fabm::right_hand_side_rhs')

   ! Initialization is needed because the different biogeochemical models increment or decrement
   ! the temporal derivatives, rather than setting them directly. This is needed for the simultaenous
   ! running of different coupled BGC models.
   rhs = _ZERO_

   ! Calculate temporal derivatives due to benthic processes.
   call fabm_do_benthos(model,1,rhs(1:n,1),rhs(n+1:,1))

   ! Distribute bottom flux into pelagic over bottom box (i.e., divide by layer height).
   rhs(1:n,1) = rhs(1:n,1)/h(2)

   ! Add pelagic sink and source terms for all depth levels.
#ifdef _FABM_USE_1D_LOOP_
   call fabm_do(model,1,nlev,rhs(1:n,1:nlev))
#else
   do i=1,nlev
      call fabm_do(model,i,rhs(1:n,i))
   end do
#endif

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
   REALTYPE, intent(in)                 :: cc(1:numc,0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)                :: pp(1:numc,1:numc,0:nlev)
   REALTYPE, intent(out)                :: dd(1:numc,1:numc,0:nlev)
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
   n = ubound(model%info%state_variables,1)

   ! Provide FABM with (pointers to) the current state.
   do i=1,ubound(model%info%state_variables,1)
      call fabm_link_state_data(model,i,cc(i,1:nlev))
   end do
   do i=1,ubound(model%info%state_variables_ben,1)
      call fabm_link_benthos_state_data(model,i,cc(n+i,1))
   end do

   ! If this is not the first step in the (multi-step) integration scheme,
   ! then first make sure that the intermediate state variable values are valid.
   if (.not. first) call do_repair_state(nlev,'gotm_fabm::right_hand_side_ppdd')

   ! Initialiaze production and destruction matrices to zero because FABM
   ! biogeochemical models increment these, rather than set these.
   pp = _ZERO_
   dd = _ZERO_

   ! Calculate temporal derivatives due to benthic processes.
   call fabm_do_benthos(model,1,pp(:,:,1),dd(:,:,1),n)

   ! Distribute bottom flux into pelagic over bottom box (i.e., divide by layer height).
   pp(1:n,:,1) = pp(1:n,:,1)/h(2)
   dd(1:n,:,1) = dd(1:n,:,1)/h(2)

   ! Add pelagic sink and source terms for all depth levels.
#ifdef _FABM_USE_1D_LOOP_
   call fabm_do(model,1,nlev,pp(1:n,1:n,1:nlev),dd(1:n,1:n,1:nlev))
#else
   do i=1,nlev
      call fabm_do(model,i,pp(1:n,1:n,i),dd(1:n,1:n,i))
   end do
#endif

   end subroutine right_hand_side_ppdd
!EOC

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

   call system_clock( count=clock, count_rate=ticks_per_sec)
   tick_rate = 1./ticks_per_sec

   LEVEL1 'Time spent on advection of FABM variables:',clock_adv*tick_rate
   LEVEL1 'Time spent on diffusion of FABM variables:',clock_diff*tick_rate
   LEVEL1 'Time spent on sink/source terms of FABM variables:',clock_source*tick_rate

   LEVEL1 'clean_gotm_fabm'

   ! Deallocate internal arrays
   if (allocated(cc))                 deallocate(cc)
   if (allocated(cc_diag))            deallocate(cc_diag)
   if (allocated(cc_diag_hz))         deallocate(cc_diag_hz)
   if (allocated(ws))                 deallocate(ws)
   if (allocated(sfl))                deallocate(sfl)
   if (allocated(bfl))                deallocate(bfl)
   if (allocated(par))                deallocate(par)
   if (allocated(swr))                deallocate(swr)
   if (allocated(pres))               deallocate(pres)
   if (allocated(total))              deallocate(total)
   if (allocated(local))              deallocate(local)
   if (allocated(cc_obs_indices))     deallocate(cc_obs_indices)
   if (allocated(cc_ben_obs_indices)) deallocate(cc_ben_obs_indices)
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
   REALTYPE :: bioext,localext
#ifdef _FABM_USE_1D_LOOP_
   REALTYPE :: localexts(1:nlev)
#endif
!
!-----------------------------------------------------------------------
!BOC
   bioext = _ZERO_

#ifdef _FABM_USE_1D_LOOP_
   call fabm_get_light_extinction(model,1,nlev,localexts)
#endif
   do i=nlev,1,-1
#ifdef _FABM_USE_1D_LOOP_
      localext = localexts(i)
#else
      call fabm_get_light_extinction(model,i,localext)
#endif

      ! Add the extinction of the first half of the grid box.
      bioext = bioext+localext*0.5*h(i+1)

      par(i) = I_0*(_ONE_-A)*exp(z(i)/g2-bioext)
      swr(i) = par(i)+I_0*A*exp(z(i)/g1)

      ! Add the extinction of the second half of the grid box.
      bioext = bioext+localext*0.5*h(i+1)

      if (bioshade_feedback) bioshade(i)=exp(-bioext)
   end do

   end subroutine light
!EOC

   end module gotm_fabm

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


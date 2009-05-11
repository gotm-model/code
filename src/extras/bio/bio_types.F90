!$Id: bio_types.F90,v 1.6 2009-05-11 13:41:41 jorn Exp $
#include"cppdefs.h"

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_types --- Derived types used by 0D biogeochemical modules
!
! !INTERFACE:
   module bio_types
!
! !DESCRIPTION:
! This module contains the derived types that are used for communication between
! the 0D biogeochemical models and a hosting physical environment (e.g. GOTM).
! Types are used to describe the 0d model, its state variables, and the local
! environment.
!
! Subroutines for intialization of the derived types are also provided. It is
! recommended that you always call these subroutines to initialize such types
! before use. This ensures that if new members are added to the types, they will
! be set to a reasonable default even if your program is not aware the member
! has been added.
!
! !USES:
!  default: all is private.
   private
   public type_model_info
   public type_state_variable_info,type_diagnostic_variable_info,type_conserved_quantity_info
   public type_environment
   public create_model_info, init_environment
!
! !PUBLIC DERIVED TYPES:
!
   ! Properties of a single state variable
   type type_state_variable_info
      character(len=64) :: name, longname, unit

      REALTYPE :: initial_value              ! Initial state variable value
      REALTYPE :: sinking_rate               ! Sinking rate (m/s)
      REALTYPE :: specific_light_extinction  ! Specific light extinction (/m/state variable unit)
#if 0
      REALTYPE :: mussels_inhale
#endif
      logical  :: positive_definite          ! Whether this variable is positive definite (negative values are invalid)
   end type type_state_variable_info

   ! Properties of a diagnostic variable
   type type_diagnostic_variable_info
      character(len=64) :: name, longname, unit
      integer           :: id
      
      ! Time treatment:
      ! 0: last value
      ! 1: time-integrated
      ! 2: time step-averaged
      ! 3: time step-integrated
      integer           :: time_treatment
   end type type_diagnostic_variable_info

   ! Properties of a conserved quantity
   type type_conserved_quantity_info
      character(len=64) :: name, longname, unit
      integer           :: id
   end type type_conserved_quantity_info

   ! Global 0D model properties
   type type_model_info
      ! Number of state variables
      integer  :: state_variable_count, diagnostic_variable_count, conserved_quantity_count

      ! Sinking rate type
      ! 0: variable-specific sinking rates are constant in time and space
      ! 2: variable-specific sinking rates depend on time and space
      !    (optionally including the current state and local enviroment)
      integer  :: dynamic_sinking_rates
      
      type (type_state_variable_info),     allocatable :: variables(:)
      type (type_diagnostic_variable_info),allocatable :: diagnostic_variables(:)
      type (type_conserved_quantity_info), allocatable :: conserved_quantities(:)
   end type type_model_info

   ! Properties of the abiotic environment
   type type_environment
      ! Local variables
      REALTYPE :: par    ! Photosynthetically Active Radiation (W/m2)
      REALTYPE :: t      ! Temperature (degrees Celsius)
      REALTYPE :: s      ! Salinity (g/kg)
      REALTYPE :: rho    ! Density (kg/m3)
      REALTYPE :: nuh    ! Turbulent diffusivity (m2/s)
      REALTYPE :: z      ! Depth (m)
      
      ! Surface variables. Note: Ideally a biogeochemical model should
      ! depend on local conditions only, and therefore not need any of
      ! the below variables. This is particularly true if the model is
      ! supposed to work ok in a 0D shell, where surface properties are
      ! not [well] defined.
      REALTYPE :: I_0    ! Incoming short-wave radiation (W/m2)
      REALTYPE :: wind   ! Wind speed (m/s)
   end type type_environment

!-----------------------------------------------------------------------

   contains
   
   function create_model_info(variable_count,diagnostic_variable_count,conserved_quantity_count) result(modelinfo)
      integer, intent(in) :: variable_count,diagnostic_variable_count,conserved_quantity_count
      type (type_model_info) :: modelinfo
   
      modelinfo%state_variable_count      = variable_count
      modelinfo%diagnostic_variable_count = diagnostic_variable_count
      modelinfo%conserved_quantity_count  = conserved_quantity_count
      
      modelinfo%dynamic_sinking_rates = 0
      
      ! Allocate and initialize memory for state variable information
      allocate(modelinfo%variables(1:modelinfo%state_variable_count))
      do i=1,modelinfo%state_variable_count
         call init_state_variable_info(modelinfo%variables(i))
      end do

      ! Allocate and initialize memory for diagnostic variable information
      allocate(modelinfo%diagnostic_variables(1:modelinfo%diagnostic_variable_count))
      do i=1,modelinfo%diagnostic_variable_count
         call init_diagnostic_variable_info(modelinfo%diagnostic_variables(i))
      end do

      ! Allocate and initialize memory for conserved quantity information
      allocate(modelinfo%conserved_quantities(1:modelinfo%conserved_quantity_count))
      do i=1,modelinfo%conserved_quantity_count
         call init_conserved_quantity_info(modelinfo%conserved_quantities(i))
      end do
   end function create_model_info

   subroutine init_state_variable_info(varinfo)
      type (type_state_variable_info), intent(inout) :: varinfo
   
      varinfo%name = ''
      varinfo%unit = ''
      varinfo%longname = ''
      varinfo%initial_value = _ZERO_
      varinfo%sinking_rate = _ZERO_
      varinfo%specific_light_extinction = _ZERO_
      varinfo%positive_definite = .false.
#if 0
      varinfo%mussels_inhale = .false.
#endif
   end subroutine init_state_variable_info

   subroutine init_diagnostic_variable_info(varinfo)
      type (type_diagnostic_variable_info), intent(inout) :: varinfo
   
      varinfo%name = ''
      varinfo%unit = ''
      varinfo%longname = ''
      varinfo%id = -1
      varinfo%time_treatment = 0
   end subroutine init_diagnostic_variable_info

   subroutine init_conserved_quantity_info(conservedinfo)
      type (type_conserved_quantity_info), intent(inout) :: conservedinfo
   
      conservedinfo%name = ''
      conservedinfo%unit = ''
      conservedinfo%longname = ''
      conservedinfo%id = -1
   end subroutine init_conserved_quantity_info

   subroutine init_environment(env)
      type (type_environment), intent(inout) :: env
      
      ! Local properties
      env%par  = _ZERO_
      env%t    = _ZERO_
      env%s    = _ZERO_
      env%nuh  = _ZERO_
      env%rho  = _ZERO_
      env%z    = _ZERO_

      ! Surface properties
      env%I_0  = _ZERO_
      env%wind = _ZERO_
   end subroutine init_environment

   end module bio_types

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

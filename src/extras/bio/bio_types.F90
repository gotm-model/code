!$Id: bio_types.F90,v 1.1 2008-11-06 15:04:32 jorn Exp $
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
   public type_model_info,type_variable_info,type_environment
   public init_model_info, init_variable_info, init_environment
!
! !PUBLIC DERIVED TYPES:
!
   ! Global 0D model properties
   type type_model_info
      ! Number of state variables
      integer  :: numc

      ! Photosynthetically Active Radiation (PAR):
      ! par_fraction:              fraction of incoming short-wave radiation that is PAR (-)
      ! par_background_extinction: PAR extinction coefficient for water (1/m)
      REALTYPE :: par_fraction
      REALTYPE :: par_background_extinction

      ! Sinking rate type
      ! 0: variable-specific sinking rates are constant in time and space
      ! 2: variable-specific sinking rates depend on time and space
      !    (optionally including the current state and local enviroment)
      integer  :: dynamic_sinking_rates
   end type type_model_info

   ! Properties of a single state variable
   type type_variable_info
      character(len=64) :: name, longname, unit

      REALTYPE :: initial_value
      REALTYPE :: sinking_rate
#if 0
      REALTYPE :: mussels_inhale
#endif
      logical  :: positive_definite
   end type type_variable_info

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
   
   subroutine init_model_info(modelinfo)
      type (type_model_info), intent(inout) :: modelinfo
   
      modelinfo%numc = 0
      modelinfo%par_fraction = _ONE_
      modelinfo%par_background_extinction = _ZERO_
      
      modelinfo%dynamic_sinking_rates = 0
   end subroutine init_model_info

   subroutine init_variable_info(varinfo)
      type (type_variable_info), intent(inout) :: varinfo
   
      varinfo%name = ''
      varinfo%unit = ''
      varinfo%longname = ''
      varinfo%initial_value = _ZERO_
      varinfo%sinking_rate = _ZERO_
      varinfo%positive_definite = .false.
#if 0
      varinfo%mussels_inhale = .false.
#endif
   end subroutine init_variable_info

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

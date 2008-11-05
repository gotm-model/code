!$Id: bio_0d_base.F90,v 1.3 2008-11-05 12:51:38 jorn Exp $
#include"cppdefs.h"

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_0d_base --- Base functionality needed by 0D biogeochemical modules
!
! !INTERFACE:
   module bio_0d_base
!
! !DESCRIPTION:
! TODO
!
! !USES:
!  default: all is private.
   private
   public type_model_info,type_variable_info,type_environment
   public init_model_info, init_variable_info, init_environment
!
! !PUBLIC DERIVED TYPES:

    ! Global 0D model properties
    type type_model_info
      integer  :: numc
      
      ! For calculation of Photosynthetically Active Radiation (PAR) from the incoming surfcae radiation:
      REALTYPE :: par_fraction                  ! Fraction of incoming short-wave radiation that is PAR (-)
      REALTYPE :: par_background_extinction     ! PAR extinction coefficient for water (1/m)
      REALTYPE :: par_bio_background_extinction ! PAR extinction coefficient for background biological components (1/m)
    end type type_model_info

    ! Properties of a single state variable
    type type_variable_info
      character(len=64) :: name, longname, unit

      REALTYPE :: initial_value
      REALTYPE :: light_extinction
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
      modelinfo%par_bio_background_extinction = _ZERO_
   end subroutine init_model_info

   subroutine init_variable_info(varinfo)
      type (type_variable_info), intent(inout) :: varinfo
   
      varinfo%name = ''
      varinfo%unit = ''
      varinfo%longname = ''
      varinfo%initial_value = _ZERO_
      varinfo%light_extinction = _ZERO_
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

   end module bio_0d_base

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

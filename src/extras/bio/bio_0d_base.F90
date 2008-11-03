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
   public init_model_info, init_variable_info
!
! !PUBLIC DERIVED TYPES:

    ! Global 0D model properties
    type type_model_info
      integer  :: numc
      REALTYPE :: par_fraction, par_background_extinction, par_bio_background_extinction
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
      REALTYPE :: par,t,s,nuh,rho
      
      ! Surface variables. Note: Ideally a biogeochemical model should
      ! depend on local conditions only, and therefore not need any of
      ! the below variables. This is particularly true if the model is
      ! supposed to work ok in a 0D shell, where surface properties are
      ! not [well] defined.
      REALTYPE :: I_0,wind
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

   end module bio_0d_base

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

!$Id: bio_0d_gen.F90,v 1.2 2008-11-06 15:04:32 jorn Exp $
#include"cppdefs.h"

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_0d --- Generic 0D biogeochemical model
!
! !INTERFACE:
   module bio_0d_gen
!
! !DESCRIPTION:
! This module encapsulates all specific 0d biogeochemical models. Each
! subroutine and function is called with an identifier [bio_model] specifying
! the desired specific biogeochemical model; based on this identifier
! the correct subroutine or function of the specific model will be called.
!
! A new 0D biogeochemical models only need to be registered in this file
! in order to be usable by GOTM, GETM and the independent 0D driver.
!
! Registering a new 0D biogeochemical model is done by:
!
! 1) Adding a use statement that references the new model (cf. "use bio_npzd_0d" below)
! 2) Defining an integer identifier for the new model (cf. npzd_0d_id below)
! 3) Adding the model as option to the "select" statements in the following subroutines:
!    init_bio_0d_generic, get_var_info_bio_0d_generic, do_bio_0d_generic.
!
! The following is optional:
!
! - If the sinking rate of any of the model variables varies in time and/or space, a subroutine
!   that provides the sinking rates (m/s) must be added as option to the "select" statement in
!   get_sinking_rates_bio_0d_generic.
!   Otherwise sinking rates are assumed to be constant in time and space; they will be taken from
!   the sinking_rate member of the respective type_variable_info derived types (see bio_types.F90).
!
! - If any of the model variables attentuate light, a function that provides the light
!   extinction coefficient due to biota (/m) must be added as option to the "select" statement
!   in get_bio_extinction_bio_0d_generic.
!   Otherwise the biogeochemical state variables are assumed not to have any effect on light
!   attenuation (but background attentuation due to water still occurs)
!
! !USES:
   use bio_types
!   
!  Reference specific biogeochemical models:
   use bio_npzd_0d
!   
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio_0d_generic,get_var_info_bio_0d_generic,do_bio_0d_generic, &
          get_sinking_rates_bio_0d_generic, get_bio_extinction_bio_0d_generic
!
! !PRIVATE DATA MEMBERS:

!  Identifiers for specific biogeochemical models:
   integer, parameter :: npzd_0d_id = 1001

!
! !REVISION HISTORY:!
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
   
   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the selected 0d biogeochemical model
!
! !INTERFACE:
   subroutine init_bio_0d_generic(bio_model,namlst,modelinfo)
!
! !INPUT PARAMETERS:
   integer,               intent(in)    :: bio_model,namlst
   type (type_model_info),intent(inout) :: modelinfo
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (bio_model)
      case (npzd_0d_id)
         call init_bio_npzd_0d(namlst,'bio_npzd.nml',modelinfo)
      case default
         stop "bio_0d_gen::init_bio_0d_generic: no valid 0d biogeochemical model specified!"
   end select
   
   end subroutine init_bio_0d_generic
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get variable information for the selected 0d biogeochemical model
!
! !INTERFACE:
   subroutine get_var_info_bio_0d_generic(bio_model,numc,varinfo)
!
! !INPUT PARAMETERS:
   integer,                  intent(in)    :: bio_model,numc
   type (type_variable_info),intent(inout) :: varinfo(1:numc)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (bio_model)
      case (npzd_0d_id)
         call get_var_info_npzd_0d(numc,varinfo)
      case default
         stop "bio_0d_gen::get_var_info_bio_0d_generic: no valid 0d biogeochemical model specified!"
   end select

   end subroutine get_var_info_bio_0d_generic
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the local temporal derivatives for the selected 0d
! biogeochemical model
!
! !INTERFACE:
   subroutine do_bio_0d_generic(bio_model,first,numc,cc,env,pp,dd)
!
! !INPUT PARAMETERS:
   integer,                intent(in)    :: bio_model,numc
   logical,                intent(in)    :: first
   REALTYPE,               intent(in)    :: cc(1:numc)
   type (type_environment),intent(in)    :: env
   REALTYPE,               intent(inout) :: pp(1:numc,1:numc),dd(1:numc,1:numc)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (bio_model)
      case (npzd_0d_id)
         call do_bio_npzd_0d(first,numc,cc,env,pp,dd)
      case default
         stop "bio_0d_gen::do_bio_0d_generic: no valid 0d biogeochemical model specified!"
   end select

   end subroutine do_bio_0d_generic
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the sinking rates for the state variables of the
! selected 0d biogeochemical model
!
! !INTERFACE:
   subroutine get_sinking_rates_bio_0d_generic(bio_model,modelinfo,varinfo,env,cc,sinking_rate)
!
! !INPUT PARAMETERS:
   integer,                  intent(in)  :: bio_model
   type (type_model_info),   intent(in)  :: modelinfo
   type (type_variable_info),intent(in)  :: varinfo(1:modelinfo%numc)
   type (type_environment),  intent(in)  :: env
   REALTYPE,                 intent(in)  :: cc(1:modelinfo%numc)
   REALTYPE,                 intent(out) :: sinking_rate(1:modelinfo%numc)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (bio_model)
      case default
         if (modelinfo%dynamic_sinking_rates.ne.0) &
            stop 'get_sinking_rates_bio_0d_generic: the 0d model specifies that sinking rates are time- and/or space-dependent, but a function that provides these sinking rates has not been specified.'
         sinking_rate = varinfo%sinking_rate
   end select

   end subroutine get_sinking_rates_bio_0d_generic
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the light extinction coefficient due to biogeochemical
! variables
!
! !INTERFACE:
   function get_bio_extinction_bio_0d_generic(bio_model,numc,cc) result(extinction)
!
! !INPUT PARAMETERS:
   integer,  intent(in)  :: bio_model, numc
   REALTYPE, intent(in)  :: cc(1:numc)
   REALTYPE              :: extinction
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (bio_model)
      case (npzd_0d_id)
         extinction = get_bio_extinction_npzd_0d(numc,cc)
      case default
         ! By default no light extinction due to biota (but still background extinction due to water)
         extinction = _ZERO_
   end select

   end function get_bio_extinction_bio_0d_generic
!EOC

   end module bio_0d_gen

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

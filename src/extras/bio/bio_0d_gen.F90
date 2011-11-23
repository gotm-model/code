#include"cppdefs.h"

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_0d_gen --- Generic 0D biogeochemical model
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
! How to register a new biogeochemical model:
!
! 1) Add a use statement that references your new model (cf. "use bio_npzd_0d" below)
!
! 2) Define an integer identifier > 1000 for your new model (cf. "npzd_0d_id" below)
!
! 3) If your model uses model-specific data (e.g. parameter values) that are grouped in a
!    (model-specific) derived type, add an instance of this type as member to "type_model"
!    defined below. Note: if you want to be able to run multiple instances of your model
!    side-by-side, grouping the model parameters in a derived type is a *requirement*.
!
! 4) Add the model as option to the "select" statements in the following subroutines:
!    "get_model_name", "init_bio_single", "do_bio_single".
!
! The following steps are optional:
!
! 5) If the sinking rate of any of the model variables varies in time and/or space, a subroutine
!    that provides the sinking rates (m/s) must be added as option to the "select" statement in
!    "get_sinking_rates_single". Note that in that case, your model must also set member
!    "dynamic_sinking_rates" in the model information to a value of 2 (otherwise your function will
!    not be called).
!    If a function is not provided (dynamic_sinking_rates=0), sinking rates are assumed to be constant
!    in time and space; they will be taken from the sinking_rate member of the respective
!    type_state_variable_info derived type (see bio_types.F90).
!
! 6) If any of the model variables attentuate light, a function that calculate the light
!    extinction coefficient (/m) from the current model state may be added as option to the "select"
!    statement in get_bio_extinction_single. This allows for complete customization of the bio
!    extinction.
!    If this function is not provided, the bio extinction will be calculated from the specific
!    extinction coefficients for each state variable as specified in the model information,
!    specifically member "specific_light_extinction" of the state variable information.
!    (note: specific extinction coefficients default to 0: no attenuation due to biogeochemical components) 
!
! 7) If (part of) the model state variable are composed of conserved quantities (energy and/or
!    chemical elements), a function that provides the sum of these quantities given the model
!    state must be added as option to the "select" statement in get_conserved_quantities_single.
!
! How to use this library of biogeochemical models:
!
! 1) Add a use statement that references this module.
!
! 2) Define an instance of the derived type "type_model" if you want to use one single model, or
!    of type "type_model_collection" if you want to use multiple models side-by-side (N.B. these
!    models will not be aware of each other).
!    In both cases this instance will hold all information on the selected biogeochemical model,
!    including descriptive strings, state variable information, and parameter values.
!
! 3) Initialize the derived type instance by calling "init_bio_0d_generic"
!
! 4) Access the model by the following subroutines:
!    - do_bio_0d_generic: to get local temporal derivatives
!    - get_sinking_rates_bio_0d_generic: to get current sinking rates for the state variables
!    - get_bio_extinction_bio_0d_generic: to get the combined light extinction coefficient due to
!      biogeochemical components
!    - get_conserved_quantities_bio_0d_generic: to get the sums of the conserved quantities described
!      by the model
!    Additional information on the model (e.g. descriptive string for its variables) is present in the
!    "info" member of the derived type, after the model (collection) has been initialized.
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
   public type_model, type_model_collection, &
          get_model_name, init_bio_0d_generic,do_bio_0d_generic, &
          get_sinking_rates_bio_0d_generic, get_bio_extinction_bio_0d_generic, get_conserved_quantities_bio_0d_generic
!
! !PRIVATE DATA MEMBERS:

!  Identifiers for specific biogeochemical models:
   integer, parameter :: npzd_0d_id = 1001

! !PUBLIC TYPES:
!
!  Single generic biogeochemical model
   type type_model
      integer :: id
      type (type_model_info) :: info

      ! Derived types that below to specific biogeochemical models.
      type (type_npzd) :: npzd
   end type type_model
!
!  Collection of generic biogeochemical models.
   type type_model_collection
      integer :: count
      type (type_model_info) :: info
      type (type_model),allocatable :: models(:)
   end type type_model_collection
!
! !PUBLIC INTERFACES:
!
!  Model initialization
   interface init_bio_0d_generic
      module procedure init_bio_single
      module procedure init_bio_collection
   end interface init_bio_0d_generic
!
!  Access to temporal derivatives
   interface do_bio_0d_generic
      module procedure do_bio_single
      module procedure do_bio_collection
   end interface do_bio_0d_generic
!
!  Access to sinking rates
   interface get_sinking_rates_bio_0d_generic
      module procedure get_sinking_rates_single
      module procedure get_sinking_rates_collection
   end interface get_sinking_rates_bio_0d_generic
!
!  Access to light extinction coefficient
   interface get_bio_extinction_bio_0d_generic
      module procedure get_bio_extinction_single
      module procedure get_bio_extinction_collection
   end interface get_bio_extinction_bio_0d_generic
!
!  Access to sums of conserved quantities
   interface get_conserved_quantities_bio_0d_generic
      module procedure get_conserved_quantities_single
      module procedure get_conserved_quantities_collection
   end interface get_conserved_quantities_bio_0d_generic
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
! !IROUTINE:Get short model name (letters, numbers and underscores only)
!
! !INTERFACE:
   function get_model_name(bio_model) result(name)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in) :: bio_model
   character(len=64)   :: name
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (bio_model)
      case (npzd_0d_id)
         name = 'npzd'
      case default
         stop 'bio_0d_gen::get_model_name: no valid biogeochemical model specified!'
   end select

   end function get_model_name
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the selected 0d biogeochemical model
!
! !INTERFACE:
   function init_bio_single(bio_model,nmlunit,nmlfilename,nameprefix,longnameprefix) result(model)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,                    intent(in) :: bio_model,nmlunit
   character(len=*),           intent(in) :: nmlfilename
   character(len=*), optional, intent(in) :: nameprefix,longnameprefix
   type (type_model)                      :: model
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
  integer           :: i
  character(len=64) :: modelname
!EOP
!-----------------------------------------------------------------------
!BOC
   modelname = get_model_name(bio_model)
   LEVEL2 'Initializing biogeochemical model '//trim(modelname)

   ! Allow the selected model to initialize
   open(nmlunit,file=nmlfilename,action='read',status='old',err=98)
   select case (bio_model)
      case (npzd_0d_id)
         model%info = init_bio_npzd_0d(model%npzd,nmlunit)
      case default
         stop 'bio_0d_gen::init_bio_single: no valid biogeochemical model specified!'
   end select
   close(nmlunit)
   LEVEL3 'model '//trim(modelname)//' initialized successfully from '//trim(nmlfilename)

   ! Apply prefix to variable names
   if (present(nameprefix)) then
      do i=1,model%info%state_variable_count
         model%info%variables(i)%name = trim(nameprefix)//trim(model%info%variables(i)%name)
      end do
      do i=1,model%info%diagnostic_variable_count
         model%info%diagnostic_variables(i)%name = trim(nameprefix)//trim(model%info%diagnostic_variables(i)%name)
      end do
      do i=1,model%info%conserved_quantity_count
         model%info%conserved_quantities(i)%name = trim(nameprefix)//trim(model%info%conserved_quantities(i)%name)
      end do
   end if

   ! Apply prefix to long variable names
   if (present(longnameprefix)) then
      do i=1,model%info%state_variable_count
         model%info%variables(i)%longname = trim(longnameprefix)//' '//trim(model%info%variables(i)%longname)
      end do
      do i=1,model%info%diagnostic_variable_count
         model%info%diagnostic_variables(i)%longname = trim(longnameprefix)//' '//trim(model%info%diagnostic_variables(i)%longname)
      end do
      do i=1,model%info%conserved_quantity_count
         model%info%conserved_quantities(i)%longname = trim(longnameprefix)//' '//trim(model%info%conserved_quantities(i)%longname)
      end do
   end if

   ! Store the identifier for the selected model.
   model%id = bio_model

   return

98 LEVEL2 'I could not open '//trim(nmlfilename)
   LEVEL2 'If thats not what you want you have to supply '//trim(nmlfilename)
   LEVEL2 'See the bio example on www.gotm.net for a working '//trim(nmlfilename)
   return

   end function init_bio_single
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the local temporal derivatives for the selected 0d
! biogeochemical model
!
! !INTERFACE:
   subroutine do_bio_single(model,first,cc,env,pp,dd,diag)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_model),      intent(in)    :: model
   logical,                intent(in)    :: first
   REALTYPE,               intent(in)    :: cc(1:model%info%state_variable_count)
   type (type_environment),intent(in)    :: env
   REALTYPE,               intent(inout) :: pp(1:model%info%state_variable_count,1:model%info%state_variable_count)
   REALTYPE,               intent(inout) :: dd(1:model%info%state_variable_count,1:model%info%state_variable_count)
   REALTYPE,               intent(out)   :: diag(1:model%info%diagnostic_variable_count)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (model%id)
      case (npzd_0d_id)
         call do_bio_npzd_0d(model%npzd,first,model%info%state_variable_count,cc,model%info%diagnostic_variable_count, &
                             env,pp,dd,diag)
      case default
         stop 'bio_0d_gen::do_bio_single: the selected biogeochemical model does not yet provide &
              &a function that returns the local temporal derivatives.'
   end select

   end subroutine do_bio_single
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the sinking rates for the state variables of the
! selected 0d biogeochemical model
!
! !INTERFACE:
   subroutine get_sinking_rates_single(model,env,cc,sinking_rate)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_model),        intent(in)  :: model
   type (type_environment),  intent(in)  :: env
   REALTYPE,                 intent(in)  :: cc(1:model%info%state_variable_count)
   REALTYPE,                 intent(out) :: sinking_rate(1:model%info%state_variable_count)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (model%id)
      case default
         ! Default: use the constant sinking rates specified in state variable properties.
         if (model%info%dynamic_sinking_rates.ne.0) &
            stop 'get_sinking_rates_single: the 0d model specifies that sinking rates are time- and/or &
                 &space-dependent, but a function that provides these sinking rates has not been specified.'
         sinking_rate = model%info%variables%sinking_rate
   end select

   end subroutine get_sinking_rates_single
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the light extinction coefficient due to biogeochemical
! variables
!
! !INTERFACE:
   function get_bio_extinction_single(model,cc) result(extinction)
!
! !INPUT PARAMETERS:
   type (type_model), intent(in) :: model
   REALTYPE, intent(in)          :: cc(1:model%info%state_variable_count)
   REALTYPE                      :: extinction
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (model%id)
      case (npzd_0d_id)
         extinction = get_bio_extinction_npzd_0d(model%npzd,numc,cc)
      case default
         ! Default: use constant specific light extinction values specified in the state variable properties
         extinction = sum(model%info%variables%specific_light_extinction*cc)
   end select

   end function get_bio_extinction_single
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the total of all conserved quantities
!
! !INTERFACE:
   function get_conserved_quantities_single(model,cc) result(sums)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_model), intent(in)      :: model
   REALTYPE, intent(in)               :: cc(1:model%info%state_variable_count)
   REALTYPE                           :: sums(1:model%info%conserved_quantity_count)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (model%id)
      case (npzd_0d_id)
         sums = get_conserved_quantities_npzd_0d(model%npzd,model%info% &
                state_variable_count,cc,model%info%conserved_quantity_count)
      case default
         ! Default: the model does not describe any conserved quantities.
         if (model%info%conserved_quantity_count.gt.0) &
            stop 'get_conserved_quantities_single: the model specifies that it describes one or more conserved &
                 &quantities, but a function that provides sums of these quantities has not been specified.'
   end select

   end function get_conserved_quantities_single
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise a collection of 0d biogeochemical models
!
! !INTERFACE:
   function init_bio_collection(count,bio_model,nmlunit,nmlfilename,nameprefixes,longnameprefixes) result(collection)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,                     intent(in) :: count,bio_model(count),nmlunit
   character(len=*),            intent(in) :: nmlfilename(count)
   character(len=*),optional,   intent(in) :: nameprefixes(count),longnameprefixes(count)
   type (type_model_collection)            :: collection
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
  integer :: i,nstate,ndiag,ncons
  character(len=64) :: nameprefix,longnameprefix,modelname
!EOP
!-----------------------------------------------------------------------
!BOC
   ! Allocate the memory for the set of biogeochemical models in the collection.
   collection%count = count
   allocate(collection%models(1:count))

   ! Initial number of state variables and conserved quantities.
   nstate = 0
   ndiag = 0
   ncons = 0

   ! Allow each biogeochemical model to initialize.
   do i = 1,count
      ! Generate prefix strings if not specified
      modelname = get_model_name(bio_model(i))
      if (present(nameprefixes)) then
         nameprefix = nameprefixes(i)
      else
         write (unit=nameprefix, fmt='(a,i2.2,a)') trim(modelname),i,'_'
      end if
      if (present(longnameprefixes)) then
         longnameprefix = longnameprefixes(i)
      else
         write (unit=longnameprefix, fmt='(a,a,i2.2)') trim(modelname),' ',i
      end if

      ! Initialize the current model
      collection%models(i) = init_bio_0d_generic(bio_model(i),nmlunit,nmlfilename(i),nameprefix,longnameprefix)

      ! Update the total number of state variables and conserved quantities.
      nstate = nstate + collection%models(i)%info%state_variable_count
      ndiag  = ndiag  + collection%models(i)%info%diagnostic_variable_count
      ncons  = ncons  + collection%models(i)%info%conserved_quantity_count
   end do

   ! Create table for information on the entire collection of models.
   collection%info = create_model_info(nstate,ndiag,ncons)
   collection%info%dynamic_sinking_rates = maxval(collection%models%info%dynamic_sinking_rates)

   ! Copy the information on all individual biogeochemical models to the global table with
   ! information on the entire model collection.
   nstate = 1
   ndiag = 1
   ncons = 1
   do i = 1,count
      collection%info%variables(nstate:nstate+collection%models(i)%info%state_variable_count-1)&
                        = collection%models(i)%info%variables
      collection%info%diagnostic_variables(ndiag:ndiag+collection%models(i)%info%diagnostic_variable_count-1)&
                        = collection%models(i)%info%diagnostic_variables
      collection%info%conserved_quantities(ncons:ncons+collection%models(i)%info%conserved_quantity_count-1)&
                        = collection%models(i)%info%conserved_quantities
      nstate = nstate + collection%models(i)%info%state_variable_count
      ndiag  = ndiag  + collection%models(i)%info%diagnostic_variable_count
      ncons  = ncons  + collection%models(i)%info%conserved_quantity_count
   end do

   end function init_bio_collection
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the local temporal derivatives for the collection of biogeochemical models.
!
! !INTERFACE:
   subroutine do_bio_collection(collection,first,cc,env,pp,dd,diag)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_model_collection), intent(in)    :: collection
   logical,                      intent(in)    :: first
   REALTYPE,                     intent(in)    :: cc(1:collection%info%state_variable_count)
   type (type_environment),      intent(in)    :: env
   REALTYPE,                     intent(inout) :: &
                                 pp(1:collection%info%state_variable_count, &
                                 1:collection%info%state_variable_count)
   REALTYPE,                     intent(inout) :: &
                                 dd(1:collection%info%state_variable_count, &
                                 1:collection%info%state_variable_count)
   REALTYPE,                     intent(out) :: diag(1:collection%info%diagnostic_variable_count)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
  integer :: i,ifirst,ilast,ifirst_diag,ilast_diag
!EOP
!-----------------------------------------------------------------------
!BOC
   ifirst = 1
   ifirst_diag = 1
   do i = 1,collection%count
      ilast = ifirst + collection%models(i)%info%state_variable_count - 1
      ilast_diag = ifirst_diag + collection%models(i)%info%diagnostic_variable_count - 1
      call do_bio_single(collection%models(i),first,cc(ifirst:ilast),env,&
               pp(ifirst:ilast,ifirst:ilast),dd(ifirst:ilast,ifirst:ilast),diag(ifirst_diag:ilast_diag))
      ifirst = ilast+1
      ifirst_diag = ilast_diag+1
   end do

   end subroutine do_bio_collection
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the sinking rates for the state variables of the
! collection of biogeochemical models
!
! !INTERFACE:
   subroutine get_sinking_rates_collection(collection,env,cc,sinking_rate)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_model_collection), intent(in)  :: collection
   type (type_environment),      intent(in)  :: env
   REALTYPE,                     intent(in)  :: cc(1:collection%info%state_variable_count)
   REALTYPE,                     intent(out) :: sinking_rate(1:collection%info%state_variable_count)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
  integer :: i,ifirst,ilast
!EOP
!-----------------------------------------------------------------------
!BOC
   ifirst = 1
   do i = 1,collection%count
      ilast = ifirst + collection%models(i)%info%state_variable_count - 1
      call get_sinking_rates_single(collection%models(i),env,cc(ifirst:ilast),sinking_rate(ifirst:ilast))
      ifirst = ilast+1
   end do

   end subroutine get_sinking_rates_collection
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the light extinction coefficient due to biogeochemical
! variables
!
! !INTERFACE:
   function get_bio_extinction_collection(collection,cc) result(extinction)
!
! !INPUT PARAMETERS:
   type (type_model_collection), intent(in) :: collection
   REALTYPE,                     intent(in) :: cc(1:collection%info%state_variable_count)
   REALTYPE                                 :: extinction
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   ! Self-shading is currently not supported if models are running side-by-side
   extinction = _ZERO_

   end function get_bio_extinction_collection
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the total of all conserved quantities for a collection
! of models.
!
! !INTERFACE:
   function get_conserved_quantities_collection(collection,cc) result(sums)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_model_collection), intent(in) :: collection
   REALTYPE,                     intent(in) :: cc(1:collection%info%state_variable_count)
   REALTYPE                                 :: sums(1:collection%info%conserved_quantity_count)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
  integer :: i,ifirst,ilast,ifirstcons,ilastcons
!EOP
!-----------------------------------------------------------------------
!BOC
   ifirst     = 1
   ifirstcons = 1
   do i = 1,collection%count
      ilast     = ifirst     + collection%models(i)%info%state_variable_count     - 1
      ilastcons = ifirstcons + collection%models(i)%info%conserved_quantity_count - 1
      sums(ifirstcons:ilastcons) = get_conserved_quantities_single(collection%models(i),cc(ifirst:ilast))
      ifirst     = ilast    +1
      ifirstcons = ilastcons+1
   end do

   end function get_conserved_quantities_collection
!EOC

   end module bio_0d_gen

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

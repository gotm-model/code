#ifdef _FABM_

#include "cppdefs.h"
#include "fabm_version.h"

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: gotm_fabm_input
!
! !INTERFACE:
   module gotm_fabm_input
!
! !DESCRIPTION:
!  This module contains routines for initializing and reading one or more
!  data files containing observed profiles for (a subset of) FABM state
!  variables.
!
! !USES:
   use fabm
   use fabm_types, only: attribute_length
#if _FABM_API_VERSION_ > 0
   use fabm_v0_compatibility
#endif
   use gotm_fabm,only:fabm_calc,model,cc,register_observation
   use input,only: register_input, type_scalar_input, type_profile_input
   use settings
   use yaml_settings

   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public configure_gotm_fabm_input, configure_gotm_fabm_input_from_nml, init_gotm_fabm_input
   public type_input_variable,first_input_variable
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!
!  PRIVATE TYPES

   integer,parameter :: maxpathlen=256

!  Information on an observed variable
   type type_input_variable
      type (type_scalar_input)                :: scalar_input
      type (type_profile_input)               :: profile_input
      type (type_bulk_variable_id)            :: interior_id          ! FABM identifier of pelagic variable (not associated if variable is not pelagic)
      type (type_horizontal_variable_id)      :: horizontal_id        ! FABM identifier of horizontal variable (not associated if variable is not horizontal)
      type (type_scalar_variable_id)          :: scalar_id            ! FABM identifier of scalar variable (not associated if variable is not scalar)
      integer                                 :: ncid = -1            ! NetCDF id in output file (only used if this variable is included in output)
      REALTYPE                                :: relax_tau            ! Relaxation times
      REALTYPE                                :: relax_tau_bot        ! Relaxation times for bottom layer (depth-dependent variables only)
      REALTYPE                                :: relax_tau_surf       ! Relaxation times for surface layer (depth-dependent variables only)
      REALTYPE                                :: h_bot, h_surf        ! Thickness of bottom and surface layers (for relaxation rates that vary per layer)
      REALTYPE, allocatable,dimension(:)      :: relax_tau_1d         ! Relaxation times for profiles (depth-dependent variables)
      type (type_input_variable),pointer      :: next => null()       ! Next variable in current input file
   end type

!  PRIVATE DATA MEMBERS
   type (type_input_variable), pointer, save :: first_input_variable => null()
   type (type_input_variable), pointer, save :: last_input_variable  => null()
!
!  PRIVATE PARAMETERS
   integer,parameter :: max_variable_count_per_file = 256

   type, extends(type_dictionary_populator) :: type_fabm_input_populator
   contains
      procedure :: create => fabm_input_create
   end type
   class (type_fabm_input_populator), pointer :: fabm_input_populator => null()

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize input
!
! !INTERFACE:
   subroutine configure_gotm_fabm_input()

      class (type_settings), pointer :: cfg

      allocate(fabm_input_populator)
      cfg => settings_store%get_child('fabm/input', populator=fabm_input_populator)
   end subroutine configure_gotm_fabm_input
!EOC

   subroutine append_input(input_variable)
      type (type_input_variable), target :: input_variable

      if (.not. associated(first_input_variable)) then
         first_input_variable => input_variable
      else
         last_input_variable%next => input_variable
      end if
      last_input_variable => input_variable
   end subroutine

   subroutine fabm_input_create(self, pair)
      class (type_fabm_input_populator), intent(inout) :: self
      type (type_key_value_pair),        intent(inout) :: pair

      type (type_input_variable), pointer :: input_variable
      class (type_gotm_settings), pointer :: branch
      character(len=attribute_length)     :: fabm_name
      integer :: i

      allocate(input_variable)

!     First search in interior variables
      input_variable%interior_id = model%get_bulk_variable_id(pair%name)

      if (fabm_is_variable_used(input_variable%interior_id)) then
         fabm_name = fabm_get_variable_name(model, input_variable%interior_id)
         call type_input_create(pair, input_variable%profile_input, trim(input_variable%interior_id%variable%long_name), trim(input_variable%interior_id%variable%units), default=0._rk, pchild=branch)
         do i = 1, size(model%state_variables)
            if (fabm_name == model%state_variables(i)%name) then
               call branch%get(input_variable%relax_tau, 'relax_tau', 'relaxation time scale', 's', minimum=0._rk, default=1.e15_rk)
               call branch%get(input_variable%relax_tau_bot, 'relax_tau_bot', 'relaxation time scale for bottom layer', 's', minimum=0._rk, default=1.e15_rk)
               call branch%get(input_variable%relax_tau_surf, 'relax_tau_surf', 'relaxation time scale for surface layer', 's', minimum=0._rk, default=1.e15_rk)
               call branch%get(input_variable%h_bot, 'thickness_bot', 'thickness of bottom relaxation layer', 'm', minimum=0._rk, default=0._rk)
               call branch%get(input_variable%h_surf, 'thickness_surf', 'thickness of surface relaxation layer', 'm', minimum=0._rk, default=0._rk)
               exit
            end if
         end do
      else
!        Variable was not found among interior variables. Try variables defined on horizontal slice of model domain (e.g., benthos)
         input_variable%horizontal_id = model%get_horizontal_variable_id(pair%name)
         if (fabm_is_variable_used(input_variable%horizontal_id)) then
            fabm_name = fabm_get_variable_name(model, input_variable%horizontal_id)
            call type_input_create(pair, input_variable%scalar_input, trim(input_variable%horizontal_id%variable%long_name), trim(input_variable%horizontal_id%variable%units), default=0._rk, pchild=branch)
            do i = 1, size(model%bottom_state_variables)
               if (fabm_name == model%bottom_state_variables(i)%name) then
                  call branch%get(input_variable%relax_tau, 'relax_tau', 'relaxation time scale', 's', minimum=0._rk, default=1.e15_rk)
                  exit
               end if
            end do
         else
!           Variable was not found among interior or horizontal variables. Try global scalars.
            input_variable%scalar_id = model%get_scalar_variable_id(pair%name)
            if (.not. fabm_is_variable_used(input_variable%scalar_id)) then
               FATAL 'Variable '//pair%name//', referenced among FABM inputs was not found in model.'
               stop 'gotm_fabm_input:init_gotm_fabm_input'
            end if
            call type_input_create(pair, input_variable%scalar_input, trim(input_variable%scalar_id%variable%long_name), trim(input_variable%scalar_id%variable%units), default=0._rk, pchild=branch)
         end if
      end if
      call append_input(input_variable)
   end subroutine fabm_input_create

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize input
!
! !INTERFACE:
   subroutine configure_gotm_fabm_input_from_nml(namlst, fname)
!
! !DESCRIPTION:
!  Initialize files with observations on FABM variables.
!
! !USES:
   use settings
!
! !INPUT PARAMETERS:
   integer,          intent(in) :: namlst
   character(len=*), intent(in) :: fname
!
!EOP
!
! !LOCAL VARIABLES:
   character(len=maxpathlen)    :: file
   character(len=64)            :: variable,variables(max_variable_count_per_file)
   integer                      :: i,index
   integer                      :: variabletype
   REALTYPE                     :: relax_tau,constant_value
   REALTYPE,dimension(max_variable_count_per_file) :: relax_taus,relax_taus_surf,relax_taus_bot,thicknesses_surf,thicknesses_bot
   REALTYPE,dimension(max_variable_count_per_file) :: constant_values
   logical                      :: file_exists
   REALTYPE, parameter          :: missing_value = huge(_ONE_)
   integer                      :: method
   namelist /observations/ variable,variables,file,index,relax_tau,relax_taus, &
                           relax_taus_surf,relax_taus_bot,thicknesses_surf,thicknesses_bot, &
                           constant_value
   class (type_settings),       pointer :: branch
   class (type_key_value_pair), pointer :: pair
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm_fabm_input'

!  If FABM is not used, return immediately.
   if (.not. fabm_calc) return

!  Open the file that contains zero, one or more namelists specifying input observations.
   open(namlst,file=fname,action='read',status='old',err=98)

   branch => settings_store%get_child('fabm/input')
   do
!     Initialize namelist variables.
      file = ''
      variable = ''
      variables = ''
      index = -1
      relax_tau = 1.d15
      relax_taus = 1.d15
      relax_taus_surf = 1.d15
      relax_taus_bot = 1.d15
      thicknesses_surf = _ZERO_
      thicknesses_bot = _ZERO_
      constant_value = missing_value
      constant_values = missing_value

!     Read a namelist that describes a single file with observations.
!     If no namelist is found, exit the do loop.
      read(namlst,nml=observations,err=99,end=97)

      if (file/='') then
!        Make sure the specified file exists.
         inquire(file=file,exist=file_exists)
         if (.not.file_exists) then
            FATAL 'Input file "'//trim(file)//'", specified in namelist "observations", does not exist.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         if (constant_value/=missing_value) then
            FATAL 'Parameters "file" and "constant_value" cannot both be specified.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
      elseif (constant_value==missing_value) then
!        No file path or constant value specified - report error.
         FATAL 'observations namelist must either contain parameter "file", specifying the &
               &path to the data file, or parameter "constant_value", specifying the value &
               &to use throughout the simulation.'
         stop 'gotm_fabm_input:init_gotm_fabm_input'
      end if

      if (variable/='') then
!        The namelist describes a single variable. Check parameter validity and transfer variable settings to the array-based settings.
         if (any(variables/='')) then
            FATAL 'Parameters "variable" and "variables" in namelist "observations" may not both be used.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         if (any(relax_taus<1.d15)) then
            FATAL 'Parameter "relax_taus" cannot be used in combination with "variable" in namelist "observations"; use "relax_tau" instead.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         if (index==-1) index = 1
         if (index>size(variables)) then
            FATAL 'Parameter "index" in namelist "observations" may not exceed ',size(variables)
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         variables(index) = variable
         relax_taus(index) = relax_tau
         constant_values(index) = constant_value
      else
!        The namelist describes multiple variables. Check parameter validity.
         if (relax_tau<1.d15) then
            FATAL 'Parameter "relax_tau" cannot be used in combination with "variables" in namelist "observations"; use "relax_taus" instead.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         if (index/=-1) then
            FATAL 'Parameter "index" cannot be used in combination with "variables" in namelist "observations".'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         if (constant_value/=missing_value) then
            FATAL 'Parameter "constant_value" cannot be used in combination with "variables" in namelist "observations".'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
      end if

!     If variable names are not provided, raise an error.
      if (all(variables=='')) then
         FATAL 'Neither "variable" nor "variables" is set in namelist "observations".'
         stop 'gotm_fabm_input:init_gotm_fabm_input'
      end if

!     Find the provided variable names in FABM.
      do i=1,size(variables)
!        If this variable is not used, skip to the next.
         if (variables(i)=='') cycle

         method = 2
         if (constant_values(i) /= missing_value) method = 0

         pair => branch%get_node(trim(variables(i)), treat_as_path=.false.)
         call fabm_input_populator%create(pair)
         if (fabm_is_variable_used(last_input_variable%interior_id)) then
            call last_input_variable%profile_input%configure(method=method, path=trim(file), index=i, constant_value=constant_values(i))
            last_input_variable%relax_tau = relax_taus(i)
            last_input_variable%relax_tau_bot = relax_taus_bot(i)
            last_input_variable%relax_tau_surf = relax_taus_surf(i)
            last_input_variable%h_bot = thicknesses_bot(i)
            last_input_variable%h_surf = thicknesses_surf(i)
         else
            call last_input_variable%scalar_input%configure(method=method, path=trim(file), index=i, constant_value=constant_values(i))
            last_input_variable%relax_tau = relax_taus(i)
         end if

      end do

   end do

!  Close the namelist file
97 close(namlst)

98 LEVEL1 'done'
   
   return

99 FATAL 'Error reading namelist "observations" from '//trim(fname)
   stop 'gotm_fabm_input:init_gotm_fabm_input'

   end subroutine configure_gotm_fabm_input_from_nml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize input
!
! !INTERFACE:
   subroutine init_gotm_fabm_input(nlev,h)
!
! !DESCRIPTION:
!  Initialize files with observations on FABM variables.
!
! !USES:
   use settings
!
! !INPUT PARAMETERS:
   integer,          intent(in) :: nlev
   REALTYPE,         intent(in) :: h(1:nlev)
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_input_variable), pointer :: curvariable
   integer                             :: k
   REALTYPE                            :: db,ds,depth
!
!-----------------------------------------------------------------------
!BOC
!  Calculate depth (used to determine whether in surface/bottom/bulk for relaxation times)
   depth = sum(h)

   curvariable => first_input_variable
   do while (associated(curvariable))
      if (fabm_is_variable_used(curvariable%interior_id)) then
         call register_input(curvariable%profile_input)

         allocate(curvariable%relax_tau_1d(0:nlev))
         curvariable%relax_tau_1d = curvariable%relax_tau

!        Apply separate relaxation times for bottom and surface layer, if specified.
         db = _ZERO_
         ds = depth
         do k=1,nlev
            db = db+0.5*h(k)
            ds = ds-0.5*h(k)
            if (db<=curvariable%h_bot) curvariable%relax_tau_1d(k) = curvariable%relax_tau_bot
            if (ds<=curvariable%h_surf) curvariable%relax_tau_1d(k) = curvariable%relax_tau_surf
            db = db+0.5*h(k)
            ds = ds-0.5*h(k)
         end do

!        Register observed variable with the GOTM-FABM driver.
         call register_observation(curvariable%interior_id, curvariable%profile_input%data, curvariable%relax_tau_1d)
      else
         call register_input(curvariable%scalar_input)
         if (fabm_is_variable_used(curvariable%horizontal_id)) then
            ! Horizontal variable
            call register_observation(curvariable%horizontal_id, curvariable%scalar_input%value, curvariable%relax_tau)
         else
            ! Scalar variable
            call register_observation(curvariable%scalar_id, curvariable%scalar_input%value)
         end if
      end if
      curvariable => curvariable%next
   end do

   end subroutine init_gotm_fabm_input
!EOC

!-----------------------------------------------------------------------

   end module gotm_fabm_input

#endif

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


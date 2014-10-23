#ifdef _FABM_

#include "cppdefs.h"

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
   use fabm, only: fabm_get_bulk_variable_id,fabm_get_horizontal_variable_id,fabm_get_scalar_variable_id,fabm_is_variable_used
   use fabm,only: type_bulk_variable_id,type_horizontal_variable_id,type_scalar_variable_id
   use fabm_types, only:rk
   use gotm_fabm,only:fabm_calc,model,cc,register_observation
   use input,only: register_input_0d,register_input_1d

   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_fabm_input
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
      character(len=64)                       :: name                 ! Variable name
      character(len=maxpathlen)               :: path                 ! File path
      type (type_bulk_variable_id)            :: id                   ! FABM identifier of pelagic variable (not associated if variable is not pelagic)
      type (type_horizontal_variable_id)      :: horizontal_id        ! FABM identifier of horizontal variable (not associated if variable is not horizontal)
      type (type_scalar_variable_id)          :: scalar_id            ! FABM identifier of scalar variable (not associated if variable is not scalar)
      integer                                 :: index                ! Column index of variable in input file
      integer                                 :: ncid                 ! NetCDF id in output file (only used if this variable is included in output)
      REALTYPE                                :: relax_tau_0d         ! Relaxation times for scalars (depth-independent variables)
      REALTYPE, allocatable,dimension(:)      :: relax_tau_1d         ! Relaxation times for profiles (depth-dependent variables)
      REALTYPE                                :: data_0d
      REALTYPE,allocatable,dimension(:)       :: data_1d
      type (type_input_variable),pointer      :: next => null()       ! Next variable in current input file
   end type

!  PRIVATE DATA MEMBERS
   type (type_input_variable),pointer :: first_input_variable
!
!  PRIVATE PARAMETERS
   integer,parameter :: max_variable_count_per_file = 256

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize input
!
! !INTERFACE:
   subroutine init_gotm_fabm_input(namlst,fname,nlev,h)
!
! !DESCRIPTION:
!  Initialize files with observations on FABM variables.
!
! !USES:
!
! !INPUT PARAMETERS:
   integer,          intent(in) :: nlev,namlst
   character(len=*), intent(in) :: fname
   REALTYPE,         intent(in) :: h(1:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   character(len=maxpathlen)    :: file
   character(len=64)            :: variable,variables(max_variable_count_per_file)
   integer                      :: i,k,file_variable_count,index
   integer                      :: variabletype,filetype
   REALTYPE                     :: relax_tau,db,ds,depth,constant_value
   REALTYPE,dimension(max_variable_count_per_file) :: relax_taus,relax_taus_surf,relax_taus_bot,thicknesses_surf,thicknesses_bot
   REALTYPE,dimension(max_variable_count_per_file) :: constant_values
   integer, parameter           :: type_unknown = 0, type_profile = 1, type_scalar = 2
   logical                      :: file_exists
   type (type_input_variable),pointer :: curvariable
   REALTYPE, parameter          :: missing_value = huge(_ONE_)
   namelist /observations/ variable,variables,file,index,relax_tau,relax_taus, &
                           relax_taus_surf,relax_taus_bot,thicknesses_surf,thicknesses_bot, &
                           constant_value
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm_fabm_input'

!  If FABM is not used, return immediately.
   if (.not.fabm_calc) return

!  Initialize empty lists of observations files.
   nullify(first_input_variable)

!  Calculate depth (used to determine whether in surface/bottom/bulk for relaxation times)
   depth = sum(h)

!  Open the file that contains zero, one or more namelists specifying input observations.
   open(namlst,file=fname,action='read',status='old',err=98)

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

!     Initialize file type (profile or scalar) to unknown
      filetype = type_unknown

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
      file_variable_count = 0
      do i=1,size(variables)
!        If this variable is not used, skip to the next.
         if (variables(i)=='') cycle

         if (.not.associated(first_input_variable)) then
!           This is the first variable in the file: create it at the head of the list.
            allocate(first_input_variable)
            curvariable => first_input_variable
         else
!           This is not the first variable in the file: append to previous variable.
            curvariable => first_input_variable
            do while (associated(curvariable%next))
               curvariable => curvariable%next
            end do
            allocate(curvariable%next)
            curvariable => curvariable%next
         end if

!        Store variable name and index in file.
         nullify(curvariable%next)
         curvariable%name = variables(i)
         curvariable%path = file
         curvariable%index = i
         curvariable%ncid = -1

!        First search in pelagic variables
         variabletype = type_profile
         curvariable%id = fabm_get_bulk_variable_id(model,variables(i))

!        If variable was not found, search variables defined on horizontal slice of model domain (e.g., benthos)
         if (.not.fabm_is_variable_used(curvariable%id)) then
            curvariable%horizontal_id = fabm_get_horizontal_variable_id(model,variables(i))
            variabletype = type_scalar
         end if

!        If variable still was not found, search scalar [non-spatial] variables.
         if (.not.(fabm_is_variable_used(curvariable%id).or.fabm_is_variable_used(curvariable%horizontal_id))) then
            curvariable%scalar_id = fabm_get_scalar_variable_id(model,variables(i))
            variabletype = type_scalar
         end if

!        Report an error if the variable was still not found.
         if (.not.(fabm_is_variable_used(curvariable%id).or.fabm_is_variable_used(curvariable%horizontal_id).or.fabm_is_variable_used(curvariable%scalar_id))) then
            FATAL 'Variable '//trim(curvariable%name)//', referenced in namelist observations &
                  &in '//trim(fname)//', was not found in model.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if

!        Make sure profile and scalar variables are not mixed in the same input file.
         if (filetype/=type_unknown .and. filetype/=variabletype) then
            FATAL 'Cannot mix 0d and 1d variables in one observation file, as they require different formats.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         filetype = variabletype

         if (variabletype==type_scalar) then
            if (constant_values(i)/=missing_value) then
                ! Variable fixed to constant value.
                curvariable%data_0d = constant_values(i)
            else
                ! Variable read from file.
                call register_input_0d(curvariable%path,curvariable%index,curvariable%data_0d,curvariable%name)
            end if
            curvariable%relax_tau_0d = relax_taus(i)
            if (fabm_is_variable_used(curvariable%horizontal_id)) then
                ! Horizontal variable
               call register_observation(curvariable%horizontal_id,curvariable%data_0d,curvariable%relax_tau_0d)
            else
                ! Scalar variable
               call register_observation(curvariable%scalar_id,curvariable%data_0d)
            end if
         else
            allocate(curvariable%data_1d(0:nlev))
            allocate(curvariable%relax_tau_1d(0:nlev))
            curvariable%relax_tau_1d = relax_taus(i)
            if (constant_values(i)/=missing_value) then
                ! Variable fixed to constant value.
                curvariable%data_1d = constant_values(i)
            else
                ! Variable read from file.
                call register_input_1d(curvariable%path,curvariable%index,curvariable%data_1d,curvariable%name)
            end if

!           Apply separate relaxation times for bottom and surface layer, if specified.
            db = _ZERO_
            ds = depth
            do k=1,nlev
               db = db+0.5*h(k)
               ds = ds-0.5*h(k)
               if (db<=thicknesses_bot (i)) curvariable%relax_tau_1d(k) = relax_taus_bot(i)
               if (ds<=thicknesses_surf(i)) curvariable%relax_tau_1d(k) = relax_taus_surf(i)
               db = db+0.5*h(k)
               ds = ds-0.5*h(k)
            end do

!           Register observed variable with the GOTM-FABM driver.
            call register_observation(curvariable%id,curvariable%data_1d,curvariable%relax_tau_1d)
         end if

!        Report that this variable will use observations.
         if (constant_values(i)/=missing_value) then
            LEVEL2 'Setting variable '//trim(curvariable%name)//' to constant value',constant_values(i)
         else
            LEVEL2 'Reading observed values for variable '//trim(curvariable%name)//' from '//trim(file)
         end if

      end do

   end do

!  Close the namelist file
97 close(namlst)

98 LEVEL1 'done'

   return

99 FATAL 'Error reading namelist "observations" from '//trim(fname)
   stop 'gotm_fabm_input:init_gotm_fabm_input'

   end subroutine init_gotm_fabm_input
!EOC

!-----------------------------------------------------------------------

   end module gotm_fabm_input

#endif

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


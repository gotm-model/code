#include "cppdefs.h"
#include "fabm_driver.h"
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
   use fabm, only: fabm_get_variable_id
   use fabm_types,only: shape_full,shape_hz,id_not_used
   use gotm_fabm,only:fabm_calc,model,cc,register_observation_0d,register_observation_1d

   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_fabm_input,do_gotm_fabm_input
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!
!  PRIVATE TYPES
   
!  Information on an observed variable
   type type_variable
      character(len=64)                  :: name
      integer                            :: id,type,index
      REALTYPE                           :: data_0d,relax_tau_0d
      REALTYPE, allocatable,dimension(:) :: data_1d,relax_tau_1d
      type (type_variable),pointer       :: next
   end type

!  Information on file with observed profiles
   type type_observed_profile_info
      REALTYPE, dimension(:,:), allocatable :: prof1,prof2,alpha
      integer                               :: jul1,secs1
      integer                               :: jul2,secs2
      integer                               :: unit
      integer                               :: lines,nprofiles
      logical                               :: one_profile
      type (type_variable),pointer          :: first_variable
      type (type_observed_profile_info),pointer :: next
   end type

!  Information on file with observed scalars (time series)
   type type_observed_scalar_info
      REALTYPE, dimension(:), allocatable :: obs1,obs2,alpha
      integer                             :: jul1,secs1
      integer                             :: jul2,secs2
      integer                             :: unit
      type (type_variable),pointer        :: first_variable
      type (type_observed_scalar_info),pointer :: next
   end type

!  Pointers to first files with observed profiles and observed scalars.
   type (type_observed_profile_info),pointer :: first_observed_profile_info,last_observed_profile_info
   type (type_observed_scalar_info), pointer :: first_observed_scalar_info,last_observed_scalar_info

!  PRIVATE DATA MEMBERS

!  Unit to use for next data file.
   integer :: next_unit_no
!
!  PRIVATE PARAMETERS
   integer,parameter :: max_variable_count_per_file = 256
   integer,parameter :: first_unit_no = 555

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
   character(len=64)            :: variable,variables(max_variable_count_per_file),file
   integer                      :: i,k,file_variable_count,index
   integer                      :: filetype
   REALTYPE                     :: relax_tau,db,ds,depth
   REALTYPE,dimension(max_variable_count_per_file) :: relax_taus,relax_taus_surf,relax_taus_bot,thicknesses_surf,thicknesses_bot
   integer, parameter           :: type_unknown = 0, type_profile = 1, type_scalar = 2
   logical                      :: file_exists
   type (type_variable),pointer :: curvariable,firstvariable
   namelist /observations/ variable,variables,file,index,relax_tau,relax_taus, &
                           relax_taus_surf,relax_taus_bot,thicknesses_surf,thicknesses_bot
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm_fabm_input'

!  If FABM is not used, return immediately.
   if (.not.fabm_calc) return

!  Initialize empty lists of observations files.
   nullify(first_observed_profile_info)
   nullify(first_observed_scalar_info)
   
!  Calculate depth (used to determine whether in surface/bottom/bulk for relaxation times)
   depth = sum(h)

!  Take first unit to use from module-level parameter.
   next_unit_no = first_unit_no

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

!     Initialize file type (profile or scalar) to unknown
      filetype = type_unknown

!     Read a namelist that describes a single file with observations.
!     If no namelist is found, exit the do loop.
      read(namlst,nml=observations,err=99,end=97)

!     Make sure the file path is specified.
      if (file.eq.'') then
         FATAL 'observations namelist must contain parameter "file", specifying the path to the data file.'
         stop 'gotm_fabm_input:init_gotm_fabm_input'
      end if

!     Make sure the specified file exists.
      inquire(file=file,exist=file_exists)
      if (.not.file_exists) then
         FATAL 'Input file "'//trim(file)//'", specified in namelist "observations", does not exist.'
         stop 'gotm_fabm_input:init_gotm_fabm_input'
      end if

      if (variable.ne.'') then
!        The namelist describes a single variable. Check parameter validity and transfer variable settings to the array-based settings.
         if (any(variables.ne.'')) then
            FATAL 'Parameters "variable" and "variables" in namelist "observations" may not both be used.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         if (any(relax_taus<1.d15)) then
            FATAL 'Parameter "relax_taus" cannot be used in combination with "variable" in namelist "observations"; use "relax_tau" instead.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         if (index.eq.-1) index = 1
         if (index>size(variables)) then
            FATAL 'Parameter "index" in namelist "observations" may not exceed ',size(variables)
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         variables(index) = variable
         relax_taus(index) = relax_tau
      else
!        The namelist describes multiple variables. Check parameter validity.
         if (relax_tau<1.d15) then
            FATAL 'Parameter "relax_tau" cannot be used in combination with "variables" in namelist "observations"; use "relax_taus" instead.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         if (index.ne.-1) then
            FATAL 'Parameters "index" cannot be used in combination with "variables" in namelist "observations".'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
      end if

!     If variable names are not provided, raise an error.
      if (all(variables.eq.'')) then
         FATAL 'Neither "variable" nor "variables" is set in namelist "observations".'
         stop 'gotm_fabm_input:init_gotm_fabm_input'
      end if
      
!     Reset reference to first variable in file.
      nullify(firstvariable)

!     Find the provided variable names in FABM.
      file_variable_count = 0
      do i=1,size(variables)
!        If this variable is not used, skip to the next.
         if (variables(i).eq.'') cycle

!        Update number of last used column.
         file_variable_count = i
         
         if (.not.associated(firstvariable)) then
!           This is the first variable in the file: create it at the head of the list.
            allocate(firstvariable)
            curvariable => firstvariable            
         else
!           This is not the first variable in the file: append to previous variable.
            allocate(curvariable%next)
            curvariable => curvariable%next
         end if

!        Store variable name and index in file.
         nullify(curvariable%next)
         curvariable%name = variables(i)
         curvariable%index = i
         
!        First search in pelagic variables
         curvariable%type = type_profile
         curvariable%id = fabm_get_variable_id(model,variables(i),shape_full)

!        If variable was not found, search variables defined on horizontal slice of model domain (e.g., benthos)
         if (curvariable%id.eq.id_not_used) then
            curvariable%id = fabm_get_variable_id(model,variables(i),shape_hz)
            curvariable%type = type_scalar
         end if
         
!        Report an error if the variable was still not found.
         if (curvariable%id.eq.id_not_used) then
            FATAL 'Variable '//trim(curvariable%name)//', referenced in namelist observations &
                  &in '//trim(fname)//', was not found in model.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if

         if (curvariable%type.eq.type_scalar) then
            curvariable%data_0d = _ZERO_
            curvariable%relax_tau_0d = relax_taus(i)
            call register_observation_0d(curvariable%id,curvariable%data_0d,curvariable%relax_tau_0d)
         else
            allocate(curvariable%data_1d(0:nlev))
            allocate(curvariable%relax_tau_1d(0:nlev))
            curvariable%data_1d = _ZERO_
            curvariable%relax_tau_1d = relax_taus(i)

!           Apply separate relaxation times for bottom and surface layer, if specified.
            db = _ZERO_
            ds = depth
            do k=1,nlev
               db = db+0.5*h(k)
               ds = ds-0.5*h(k)
               if (db.le.thicknesses_bot (i)) curvariable%relax_tau_1d(k) = relax_taus_bot(i)
               if (ds.le.thicknesses_surf(i)) curvariable%relax_tau_1d(k) = relax_taus_surf(i)
               db = db+0.5*h(k)
               ds = ds-0.5*h(k)
            end do

!           Register observed variable with the GOTM-FABM driver.
            call register_observation_1d(curvariable%id,curvariable%data_1d,curvariable%relax_tau_1d)
         end if
         
!        Make sure profile and scalar variables are not mixed in the same input file.
         if (filetype.ne.type_unknown .and. filetype.ne.curvariable%type) then
            FATAL 'Cannot mix 0d and 1d variables in one observation file, as they require different formats.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         filetype = curvariable%type

!        Report that this variable will use observations.
         LEVEL2 'Reading observed values for variable '//trim(curvariable%name)//' from '//trim(file)
         
      end do

!     Initialize profile file.
      select case (filetype)
         case (type_profile)
            call create_observed_profile_info(file,file_variable_count,firstvariable,nlev)
         case (type_scalar)
            call create_observed_scalar_info(file,file_variable_count,firstvariable)
      end select

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
!BOP
!
! !IROUTINE: Read input observations
!
! !INTERFACE:
   subroutine do_gotm_fabm_input(jul,secs,nlev,z)
!
! !DESCRIPTION:
!  Read observations for all FABM variables for the current time.
!
! !USES:
!
! !INPUT PARAMETERS:
   integer,  intent(in) :: jul,secs,nlev
   REALTYPE, intent(in) :: z(:)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_observed_profile_info),pointer :: cur_profile_info
   type (type_observed_scalar_info),pointer  :: cur_scalar_info
!-----------------------------------------------------------------------
!BOC
!  If FABM is not used, return immediately.
   if (.not.fabm_calc) return
   
!  Loop over files with observed profiles.
   cur_profile_info => first_observed_profile_info
   do while (associated(cur_profile_info))
      if (cur_profile_info%unit.ne.-1) call get_observed_profiles(cur_profile_info,jul,secs,nlev,z)
      cur_profile_info => cur_profile_info%next
   end do

!  Loop over files with observed scalars.
   cur_scalar_info => first_observed_scalar_info
   do while (associated(cur_scalar_info))
      if (cur_scalar_info%unit.ne.-1) call get_observed_scalars(cur_scalar_info,jul,secs)
      cur_scalar_info => cur_scalar_info%next
   end do

   end subroutine do_gotm_fabm_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize a single input file with depth-explicit (1D) variables
!
! !INTERFACE:
   subroutine create_observed_profile_info(file,variablecount,firstvariable,nlev)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !USES:
!
! !INPUT PARAMETERS:
   character(len=64),           intent(in) :: file
   integer,                     intent(in) :: variablecount
   type (type_variable),pointer,intent(in) :: firstvariable
   integer,                     intent(in) :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: rc
!
!-----------------------------------------------------------------------
!BOC
!  Add entry to the observed profile file list.
   if (.not.associated(first_observed_profile_info)) then
      allocate(first_observed_profile_info)
      last_observed_profile_info => first_observed_profile_info
   else
      allocate(last_observed_profile_info%next)
      last_observed_profile_info => last_observed_profile_info%next
   end if

!  Initialize derived type members that will hold information on the status of the input file.
   nullify(last_observed_profile_info%next)
   last_observed_profile_info%jul2  = 0
   last_observed_profile_info%secs2 = 0
   last_observed_profile_info%lines = 0
   last_observed_profile_info%nprofiles = 0
   last_observed_profile_info%one_profile = .false.
   last_observed_profile_info%unit = -1
   last_observed_profile_info%first_variable => firstvariable

!  Open the input file.
   open(next_unit_no,file=file,status='unknown',err=80)

!  Opening was successful - store the file unit, and increment the next unit with 1.
   last_observed_profile_info%unit = next_unit_no
   next_unit_no = next_unit_no + 1

   allocate(last_observed_profile_info%prof1(0:nlev,variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (prof1)'
   last_observed_profile_info%prof1 = _ZERO_

   allocate(last_observed_profile_info%prof2(0:nlev,variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (prof2)'
   last_observed_profile_info%prof2 = _ZERO_

   allocate(last_observed_profile_info%alpha(0:nlev,variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (alpha)'

   return

80 FATAL 'Unable to open "',trim(file),'" for reading'
   stop 'gotm_fabm_input::create_observed_profile_info'

   end subroutine create_observed_profile_info
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read 1D data from a single input file
!
! !INTERFACE:
   subroutine get_observed_profiles(info,jul,secs,nlev,z)
!
! !DESCRIPTION:
!  Get observations for the current time from a single input file.
!  This reads in new observations if necessary (and available),
!  and performs linear interpolation in time and vertical space.
!
! !USES:
   use time, only: time_diff,julian_day
   use observations, only: read_profiles
!
! !INPUT PARAMETERS:
   integer,                          intent(in)   :: jul,secs
   integer,                          intent(in)   :: nlev
   REALTYPE,                         intent(in)   :: z(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
   type(type_observed_profile_info), intent(inout):: info
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                      :: rc
   integer                      :: yy,mm,dd,hh,min,ss
   REALTYPE                     :: t,dt
   type (type_variable),pointer :: curvar
!
!-----------------------------------------------------------------------
!BOC
!  Currently, observed profiles are used for initialization only.
!  If this is complete, do not read any further.
   if (info%nprofiles.gt.0) return

!  This part reads in new values if necessary.
   if(.not. info%one_profile .and. time_diff(info%jul2,info%secs2,jul,secs) .lt. 0) then
      do
         info%jul1 = info%jul2
         info%secs1 = info%secs2
         info%prof1 = info%prof2
         call read_profiles(info%unit,nlev,ubound(info%prof2,2),yy,mm,dd,hh,min,ss,z,info%prof2,info%lines,rc)
         if(rc .ne. 0) then
            if(info%nprofiles .eq. 1) then
               LEVEL3 'Only one set of profiles is present.'
               info%one_profile = .true.
               curvar => info%first_variable
               do while (associated(curvar))
                  curvar%data_1d = info%prof1(:,curvar%index)
                  curvar => curvar%next
               end do
            else
               FATAL 'Error reading profiles around line #',info%lines
               stop 'gotm_fabm_input:get_observed_profiles'
            end if
            EXIT
         else
            info%nprofiles = info%nprofiles + 1
            call julian_day(yy,mm,dd,info%jul2)
            info%secs2 = hh*3600 + min*60 + ss
            if(time_diff(info%jul2,info%secs2,jul,secs) .gt. 0) EXIT
         end if
      end do
      if( .not. info%one_profile) then
         dt = time_diff(info%jul2,info%secs2,info%jul1,info%secs1)
         info%alpha = (info%prof2-info%prof1)/dt
      end if
   end if

!  Do the time interpolation - only if more than one profile
   if( .not. info%one_profile) then
      t  = time_diff(jul,secs,info%jul1,info%secs1)
      curvar => info%first_variable
      do while (associated(curvar))
         curvar%data_1d = info%prof1(:,curvar%index) + t*info%alpha(:,curvar%index)
         curvar => curvar%next
      end do
   end if

   end subroutine get_observed_profiles
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize a single input file with horizontal (0D) variables.
!
! !INTERFACE:
   subroutine create_observed_scalar_info(file,variablecount,firstvariable)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !USES:
!
! !INPUT PARAMETERS:
   character(len=64),           intent(in) :: file
   integer,                     intent(in) :: variablecount
   type (type_variable),pointer,intent(in) :: firstvariable
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: rc
!
!-----------------------------------------------------------------------
!BOC
!  Add entry to the observed profile file list.
   if (.not.associated(first_observed_scalar_info)) then
      allocate(first_observed_scalar_info)
      last_observed_scalar_info => first_observed_scalar_info
   else
      allocate(last_observed_scalar_info%next)
      last_observed_scalar_info => last_observed_scalar_info%next
   end if

!  Initialize derived type members that will hold information on the status of the input file.
   nullify(last_observed_scalar_info%next)
   last_observed_scalar_info%jul2  = 0
   last_observed_scalar_info%secs2 = 0
   last_observed_scalar_info%unit = -1
   last_observed_scalar_info%first_variable => firstvariable

!  Open the input file.
   open(next_unit_no,file=file,status='unknown',err=80)

!  Opening was successful - store the file unit, and increment the next unit with 1.
   last_observed_scalar_info%unit = next_unit_no
   next_unit_no = next_unit_no + 1

   allocate(last_observed_scalar_info%obs1(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_scalar_info: Error allocating memory (obs1)'
   last_observed_scalar_info%obs1 = _ZERO_

   allocate(last_observed_scalar_info%obs2(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_scalar_info: Error allocating memory (obs2)'
   last_observed_scalar_info%obs2 = _ZERO_

   allocate(last_observed_scalar_info%alpha(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_scalar_info: Error allocating memory (alpha)'

   return

80 FATAL 'Unable to open "',trim(file),'" for reading'
   stop 'gotm_fabm_input:create_observed_scalar_info'

   end subroutine create_observed_scalar_info
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read 0D data from a single input file
!
! !INTERFACE:
   subroutine get_observed_scalars(info,jul,secs)
!
! !DESCRIPTION:
!  Get observations for the current time from a single input file.
!  This reads in new observations if necessary (and available),
!  and performs linear interpolation in time.
!
! !USES:
   use time, only: time_diff,julian_day
   use observations, only: read_obs
!
! !INPUT PARAMETERS:
   integer,                         intent(in)   :: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
   type(type_observed_scalar_info), intent(inout):: info
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                      :: rc
   integer                      :: yy,mm,dd,hh,min,ss
   REALTYPE                     :: t,dt
   type (type_variable),pointer :: curvar
!
!-----------------------------------------------------------------------
!BOC
!  This part reads in new values if necessary.
   if(time_diff(info%jul2,info%secs2,jul,secs) .lt. 0) then
      do
         info%jul1 = info%jul2
         info%secs1 = info%secs2
         info%obs1 = info%obs2
         call read_obs(info%unit,yy,mm,dd,hh,min,ss,size(info%obs2),info%obs2,rc)
         call julian_day(yy,mm,dd,info%jul2)
         info%secs2 = hh*3600 + min*60 + ss
         if(time_diff(info%jul2,info%secs2,jul,secs) .gt. 0) exit
      end do
      dt = time_diff(info%jul2,info%secs2,info%jul1,info%secs1)
      info%alpha = (info%obs2-info%obs1)/dt
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,info%jul1,info%secs1)
   curvar => info%first_variable
   do while (associated(curvar))
      curvar%data_0d = info%obs1(curvar%index) + t*info%alpha(curvar%index)
      curvar => curvar%next
   end do

   end subroutine get_observed_scalars
!EOC

!-----------------------------------------------------------------------

   end module gotm_fabm_input

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


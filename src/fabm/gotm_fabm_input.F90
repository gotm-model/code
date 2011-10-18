!$Id: gotm_fabm_input.F90,v 1.1 2011-04-05 13:45:01 jorn Exp $
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
   use gotm_fabm,only:fabm_calc,model,cc,obs_1d,obs_0d,relax_tau_0d,relax_tau_1d, &
                      cc_ben_obs_indices,cc_obs_indices,obs_1d_ids,obs_0d_ids
   
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
   type type_profile_pointer
      REALTYPE, dimension(:),pointer :: data
   end type
   type type_scalar_pointer
      REALTYPE, pointer :: data
   end type

   type type_observed_profile_info
      REALTYPE, dimension(:,:), allocatable :: prof1,prof2,alpha
      integer             :: jul1,secs1
      integer             :: jul2,secs2
      integer             :: unit
      type (type_profile_pointer),dimension(:),allocatable :: targets
      integer             :: lines,nprofiles
      logical             :: one_profile
      integer,dimension(:),allocatable :: variableids
      REALTYPE,dimension(:),allocatable :: relax_tau
   end type

   type type_observed_scalar_info
      REALTYPE, dimension(:), allocatable :: obs1,obs2,alpha
      integer             :: jul1,secs1
      integer             :: jul2,secs2
      integer             :: unit
      type (type_scalar_pointer),dimension(:),allocatable :: targets
      integer,dimension(:),allocatable :: variableids
      REALTYPE,dimension(:),allocatable :: relax_tau
   end type
   
!  PRIVATE DATA MEMBERS

!  Lists with information on 0D and 1D data files
   type (type_observed_profile_info),pointer :: observed_profile_info(:)
   type (type_observed_scalar_info), pointer :: observed_scalar_info(:)

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
   subroutine init_gotm_fabm_input(_LOCATION_,namlst,fname)
!
! !DESCRIPTION:
!  Initialize files with observations on FABM variables.
!
! !USES:
!
! !INPUT PARAMETERS:
   integer,          intent(in) :: _LOCATION_,namlst
   character(len=*), intent(in) :: fname
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   character(len=64)         :: variable,variables(max_variable_count_per_file),file
   integer                   :: i,j,variable_count,varcount_1d,varcount_0d,index
   integer                   :: curtype,filetype,fabm_ids(max_variable_count_per_file),istatevar
   REALTYPE                  :: relax_tau,relax_taus(max_variable_count_per_file)
   integer, parameter        :: type_unknown = 0, type_profile = 1, type_scalar = 2
   logical                   :: file_exists
   namelist /observations/ variable,variables,file,index,relax_tau,relax_taus
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm_fabm_input'

!  If FABM is not used, return immediately.
   if (.not.fabm_calc) return

!  Initialize the number of observed profiles to zero.
   allocate(observed_profile_info(0))
   allocate(observed_scalar_info(0))

!  Take first unit to use from module-level parameter.
   next_unit_no = first_unit_no
   
!  Initialize global variable counts to zero.   
   varcount_1d = 0
   varcount_0d = 0

!  Allocate arrays linking that specify an observation index (-1 if not set) for every state variable.
   allocate(cc_obs_indices    (1:ubound(model%info%state_variables,1)))
   allocate(cc_ben_obs_indices(1:ubound(model%info%state_variables_ben,1)))
   cc_obs_indices = -1
   cc_ben_obs_indices = -1

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
         if (index>ubound(variables,1)) then
            FATAL 'Parameter "index" in namelist "observations" may not exceed ',ubound(variables,1)
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

!     Find the provided variable names in FABM.
      fabm_ids = -1
      do i=1,ubound(variables,1)
!        If this variable is not used, skip to the next.
         if (variables(i).eq.'') cycle
         
!        Update number of last used column.
         variable_count = i
         
!        First search in pelagic variables
         fabm_ids(i) = fabm_get_variable_id(model,variables(i),shape_full)

!        If variable was not found, search variables defined on horizontal slice of model domain (e.g., benthos)
         if (fabm_ids(i).eq.id_not_used) then
            fabm_ids(i) = fabm_get_variable_id(model,variables(i),shape_hz)
            curtype = type_scalar
         else
            curtype = type_profile
         end if
         
!        Report an error if the variable was still not found.
         if (fabm_ids(i).eq.id_not_used) then
            FATAL 'Variable '//trim(variables(i))//', referenced in namelist observations &
                  &in '//trim(fname)//', was not found in model.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if

!        Make sure profile and scalar variables are not mixed in the same input file.
         if (filetype.ne.type_unknown .and. filetype.ne.curtype) then
            FATAL 'Cannot mix 0d and 1d variables in one observation file, as they require different formats.'
            stop 'gotm_fabm_input:init_gotm_fabm_input'
         end if
         filetype = curtype
         
!        Report that this variable will use observations.
         LEVEL2 'Reading observed values for variable '//trim(variables(i))//' from '//trim(file)

!        Increment count of variables with observations, and determine for each variable
!        whether it maps to a model state variable. If so, store the observation index for
!        that state variable.
         if (filetype.eq.type_profile) then
            varcount_1d = varcount_1d + 1
            istatevar = -1
            do j=1,ubound(model%info%state_variables,1)
               if (variables(i).eq.model%info%state_variables(j)%name) then
                  istatevar = j
                  exit
               end if
            end do
            if (istatevar.ne.-1) cc_obs_indices(istatevar) = varcount_1d
         else
            varcount_0d = varcount_0d + 1
            istatevar = -1
            do j=1,ubound(model%info%state_variables_ben,1)
               if (variables(i).eq.model%info%state_variables_ben(j)%name) then
                  istatevar = j
                  exit
               end if
            end do
            if (istatevar.ne.-1) cc_ben_obs_indices(istatevar) = varcount_0d
         end if

!        Describe how observation data will be used.
         if (istatevar.ne.-1) then
            if (relax_taus(i).ge.1.d15) then
               LEVEL3 'These values will be used for initialization only.'
            else
               LEVEL3 'These values will be used for initialization and relaxation,'
               LEVEL3 'with relax_tau =',relax_taus(i)
            end if
         else
            if (relax_taus(i).lt.1.d15) then
               FATAL 'Relaxation times set in namelist "observations" can only be used for state variables, not for '//trim(variables(i))//'.'
               stop 'gotm_fabm_input:init_gotm_fabm_input'
            end if
            LEVEL3 'These values will be used throughout the simulation.'
         end if
      end do

!     Initialize profile file.
      select case (filetype)
         case (type_profile)
            call create_observed_profile_info(file,fabm_ids(1:variable_count),relax_taus(1:variable_count),_LOCATION_)
         case (type_scalar)
            call create_observed_scalar_info(file,fabm_ids(1:variable_count),relax_taus(1:variable_count))
      end select

   end do
   
!  Close the namelist file
97 close(namlst)

!  Allocate target arrays for observations and relaxation times.
98 allocate(obs_0d(1:varcount_0d))
   allocate(obs_1d(0:_LOCATION_,1:varcount_1d))
   allocate(obs_0d_ids(1:varcount_0d))
   allocate(obs_1d_ids(1:varcount_1d))
   allocate(relax_tau_0d(1:varcount_0d))
   allocate(relax_tau_1d(0:_LOCATION_,1:varcount_1d))

!  Assign target arrays to all profile information and retrieve relaxtion times.
   varcount_1d = 0
   do i=1,ubound(observed_profile_info,1)
      do j=1,ubound(observed_profile_info(i)%targets,1)
         if (observed_profile_info(i)%variableids(j).ne.-1) then
            varcount_1d = varcount_1d + 1
            observed_profile_info(i)%targets(j)%data => obs_1d(:,varcount_1d)
            obs_1d_ids  (  varcount_1d) = observed_profile_info(i)%variableids(j)
            relax_tau_1d(:,varcount_1d) = observed_profile_info(i)%relax_tau(j)
         end if
      end do
   end do

!  Assign target scalars to all scalar information and retrieve relaxtion times.
   varcount_0d = 0
   do i=1,ubound(observed_scalar_info,1)
      do j=1,ubound(observed_scalar_info(i)%targets,1)
         if (observed_scalar_info(i)%variableids(j).ne.-1) then
            varcount_0d = varcount_0d + 1
            observed_scalar_info(i)%targets(j)%data => obs_0d(varcount_0d)
            obs_0d_ids  (varcount_0d) = observed_scalar_info(i)%variableids(j)
            relax_tau_0d(varcount_0d) = observed_scalar_info(i)%relax_tau(j)
         end if
      end do
   end do
   
   LEVEL1 'done'

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
   subroutine do_gotm_fabm_input(jul,secs,nlev,z,init_state)
!
! !DESCRIPTION:
!  Read observations for all FABM variables for the current time.
!
! !USES:
!
! !INPUT PARAMETERS:
   integer,  intent(in) :: jul,secs,nlev
   REALTYPE, intent(in) :: z(:)
   logical,  intent(in) :: init_state
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer             :: i,n
!-----------------------------------------------------------------------
!BOC
!  If FABM is not used, return immediately.
   if (.not.fabm_calc) return

!  Loop over all files with observations, and for each, obtain current values for all contained variables.
   do i=1,ubound(observed_profile_info,1)
      if (observed_profile_info(i)%unit.ne.-1) call get_observed_profiles(observed_profile_info(i),jul,secs,nlev,z)
   end do
   do i=1,ubound(observed_scalar_info,1)
      if (observed_scalar_info(i)%unit.ne.-1) call get_observed_scalars(observed_scalar_info(i),jul,secs)
   end do

!  If requested, initialize FABM state variables with observed values.
   if (init_state) then
      do n=1,ubound(model%info%state_variables,1)
         if (cc_obs_indices(n).ne.-1) cc(n,:) = obs_1d(:,cc_obs_indices(n))
      end do
      do n=1,ubound(model%info%state_variables_ben,1)
         if (cc_ben_obs_indices(n).ne.-1) cc(ubound(model%info%state_variables,1)+n,1) = obs_0d(cc_ben_obs_indices(n))
      end do
   end if
   
   end subroutine do_gotm_fabm_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize a single input file with depth-explicit (1D) variables
!
! !INTERFACE:
   subroutine create_observed_profile_info(file,variableids,relax_tau,nlev)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !USES:
!
! !INPUT PARAMETERS:
   character(len=64),                intent(in) :: file
   integer,                          intent(in) :: variableids(:),nlev
   REALTYPE,                         intent(in) :: relax_tau(:)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type(type_observed_profile_info),pointer :: observed_profile_info_old(:),info
   integer :: i,rc,variablecount
!
!-----------------------------------------------------------------------
!BOC
!  Add entry to the observed profile file list.
   observed_profile_info_old => observed_profile_info
   allocate(observed_profile_info(ubound(observed_profile_info_old,1)+1))
   observed_profile_info(1:ubound(observed_profile_info_old,1)) = observed_profile_info_old(:)
   deallocate(observed_profile_info_old)
   
!  Get shortcut to new observed profile info.
   info => observed_profile_info(ubound(observed_profile_info,1))

!  Initialize derived type members that will hold information on the status of the input file.
   info%jul2  = 0
   info%secs2 = 0
   info%lines = 0
   info%nprofiles = 0
   info%one_profile = .false.
   info%unit = -1

!  Open the input file.
   open(next_unit_no,file=file,status='unknown',err=80)

!  Opening was successful - store the file unit, and increment the next unit with 1.
   info%unit = next_unit_no
   next_unit_no = next_unit_no + 1
   
   variablecount = ubound(variableids,1)

   allocate(info%variableids(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (variableids)'
   info%variableids = variableids

   allocate(info%relax_tau(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (relax_tau)'
   info%relax_tau = relax_tau
   
   allocate(info%targets(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (targets)'
   do i=1,variablecount
      nullify(info%targets(i)%data)
   end do

   allocate(info%prof1(0:nlev,variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (prof1)'
   info%prof1 = _ZERO_

   allocate(info%prof2(0:nlev,variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (prof2)'
   info%prof2 = _ZERO_

   allocate(info%alpha(0:nlev,variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (alpha)'

   return

80 FATAL 'Unable to open "',trim(file),'" for reading'
   stop 'gotm_fabm_input::init_observed_profiles'
   
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
   integer                   :: rc,n
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,dt
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
         call read_profiles(info%unit,nlev,ubound(info%targets,1),yy,mm,dd,hh,min,ss,z,info%prof2,info%lines,rc)
         if(rc .ne. 0) then
            if(info%nprofiles .eq. 1) then
               LEVEL3 'Only one set of profiles is present.'
               info%one_profile = .true.
               do n=1,ubound(info%targets,1)
                  if (associated(info%targets(n)%data)) info%targets(n)%data = info%prof1(:,n)
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
      do n=1,ubound(info%targets,1)
         if (associated(info%targets(n)%data)) info%targets(n)%data = info%prof1(:,n) + t*info%alpha(:,n)
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
   subroutine create_observed_scalar_info(file,variableids,relax_tau)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !USES:
!
! !INPUT PARAMETERS:
   character(len=64),                intent(in) :: file
   integer,                          intent(in) :: variableids(:)
   REALTYPE,                         intent(in) :: relax_tau(:)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type(type_observed_scalar_info),pointer :: observed_scalar_info_old(:),info
   integer :: i,rc,variablecount
!
!-----------------------------------------------------------------------
!BOC
!  Add entry to the observed profile file list.
   observed_scalar_info_old => observed_scalar_info
   allocate(observed_scalar_info(ubound(observed_scalar_info_old,1)+1))
   observed_scalar_info(1:ubound(observed_scalar_info_old,1)) = observed_scalar_info_old(:)
   deallocate(observed_scalar_info_old)
   
!  Get shortcut to new observed profile info.
   info => observed_scalar_info(ubound(observed_scalar_info,1))

!  Initialize derived type members that will hold information on the status of the input file.
   info%jul2  = 0
   info%secs2 = 0
   info%unit = -1

!  Open the input file.
   open(next_unit_no,file=file,status='unknown',err=80)

!  Opening was successful - store the file unit, and increment the next unit with 1.
   info%unit = next_unit_no
   next_unit_no = next_unit_no + 1
   
   variablecount = ubound(variableids,1)

   allocate(info%variableids(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (variableids)'
   info%variableids = variableids

   allocate(info%relax_tau(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_profile_info: Error allocating memory (relax_tau)'
   info%relax_tau = relax_tau

   allocate(info%targets(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_scalar_info: Error allocating memory (targets)'
   do i=1,variablecount
      nullify(info%targets(i)%data)
   end do

   allocate(info%obs1(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_scalar_info: Error allocating memory (obs1)'
   info%obs1 = _ZERO_

   allocate(info%obs2(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_scalar_info: Error allocating memory (obs2)'
   info%obs2 = _ZERO_

   allocate(info%alpha(variablecount),stat=rc)
   if (rc /= 0) stop 'gotm_fabm_input:create_observed_scalar_info: Error allocating memory (alpha)'

   return

80 FATAL 'Unable to open "',trim(file),'" for reading'
   stop 'gotm_fabm_input:init_observed_scalars'
   
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
   integer                   :: rc,n
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,dt
!
!-----------------------------------------------------------------------
!BOC
!  This part reads in new values if necessary.
   if(time_diff(info%jul2,info%secs2,jul,secs) .lt. 0) then
      do
         info%jul1 = info%jul2
         info%secs1 = info%secs2
         info%obs1 = info%obs2
         call read_obs(info%unit,yy,mm,dd,hh,min,ss,ubound(info%targets,1),info%obs2,rc)
         call julian_day(yy,mm,dd,info%jul2)
         info%secs2 = hh*3600 + min*60 + ss
         if(time_diff(info%jul2,info%secs2,jul,secs) .gt. 0) exit
      end do
      dt = time_diff(info%jul2,info%secs2,info%jul1,info%secs1)
      info%alpha = (info%obs2-info%obs1)/dt
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,info%jul1,info%secs1)
   do n=1,ubound(info%targets,1)
      if (associated(info%targets(n)%data)) info%targets(n)%data = info%obs1(n) + t*info%alpha(n)
   end do

   end subroutine get_observed_scalars
!EOC

!-----------------------------------------------------------------------

   end module gotm_fabm_input

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


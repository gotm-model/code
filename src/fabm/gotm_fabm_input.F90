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
!
! !USES:
   use gotm_fabm,only:fabm_calc,model,cc,cc_obs,relax_tau
   
   implicit none
   
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_fabm_input,do_gotm_fabm_input
!
   integer,parameter :: max_variable_count_per_file = 256
   integer,parameter :: first_unit = 555
!
! !DERIVED TYPES:
   type type_observed_profile_info
      REALTYPE, dimension(:,:), allocatable :: prof1,prof2,alpha
      integer             :: jul1,secs1
      integer             :: jul2,secs2
      integer             :: unit
      integer,dimension(:),allocatable :: variables
      integer             :: lines,nprofiles
      logical             :: one_profile
   end type
   
   type (type_observed_profile_info),pointer :: observed_profile_info(:)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------

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
   character(len=64)         :: variables(max_variable_count_per_file),file
   integer                   :: i,j,variablecount,ivariables(max_variable_count_per_file),next_unit,ifabm_state_var
   REALTYPE                  :: RelaxTaus(max_variable_count_per_file)
   namelist /observed_profiles/ file,variablecount,variables,RelaxTaus
   type (type_observed_profile_info),pointer :: observed_profile_info_old(:),curinfo
!
!-----------------------------------------------------------------------
!BOC
!  If FABM is not used, return immediately.
   if (.not.fabm_calc) return

!  Initialize the number of observed profiles to zero.
   allocate(observed_profile_info(0))

!  Open the file that contains zero, one or more namelists specifying input observations.
   open(namlst,file=fname,action='read',status='old',err=98)

!  Take first unit to use from module-level parameter.
   next_unit = first_unit

   do
   
!     Initialize namelist variables.
      file = ''
      variables = ''
      variablecount = -1
      RelaxTaus = 1.d15

!     Read a namelist that describes a single file with observations.
!     If no namelist if found, exit the do loop.
      read(namlst,nml=observed_profiles,err=99,end=97)
   
!     Add entry to the observed profile file list.
      observed_profile_info_old => observed_profile_info
      allocate(observed_profile_info(ubound(observed_profile_info_old,1)+1))
      observed_profile_info(1:ubound(observed_profile_info_old,1)) = observed_profile_info_old(:)
      deallocate(observed_profile_info_old)

!     Obtain pointer to the current structure with file information.
      curinfo => observed_profile_info(ubound(observed_profile_info,1))

!     Initialize current unit to -1 to indicate not-in-use.
      curinfo%unit = -1

!     If variable names are not provided, set them equal to the model state variables.
      if (all(variables.eq.'')) then
         LEVEL2 'Names of variables in FABM input file "'//trim(file)//'" are not provided in the namelist. Defaulting to &
                &all FABM state variables.'
         if (variablecount.ne.-1 .and. variablecount.ne.ubound(model%info%state_variables,1)) then
            FATAL 'init_gotm_fabm_input: if the names of variables in the input file are not provided, variablecount &
                  &must not be set, or it must be equal to the number of FABM state variables.'
            stop 'init_gotm_fabm_input'
         end if
         variablecount = ubound(model%info%state_variables,1)
         do i=1,ubound(model%info%state_variables,1)
            variables(i) = model%info%state_variables(i)%name
         end do
      end if

!     If variable count is not provided, set it equal to the index of the last provided variable name.
      if (variablecount.eq.-1) then
         do i=1,max_variable_count_per_file
            if (variables(i).ne.'') variablecount = i
         end do
         LEVEL2 'Count of variables in FABM input file '//trim(file)//' is not provided in the namelist. Defaulting to &
                &index of last specified variable, i.e., ',variablecount
      end if

!     Check upper bound on the number of variables per input file.
      if (variablecount>max_variable_count_per_file) then
         FATAL 'init_gotm_fabm_input: specified number of input variables exceeds the maximum number of variables per &
               &input file. Please split the variables over multiple input files and specify the variable names &
               &explicitly in the "observed_profiles" namelists.'
         stop 'init_gotm_fabm_input'
      end if
   
!     Find the provided variable names in FABM.
      ivariables = -1
      do i=1,variablecount
!        If this variable is not used, skip to the next.
         if (variables(i).eq.'') cycle
         
!        Find FABM state variable index from specified name.
         ifabm_state_var = -1
         do j=1,ubound(model%info%state_variables,1)
            if (variables(i).eq.model%info%state_variables(j)%name) then
               ifabm_state_var = j
               exit
            end if
         end do

!        Report an error if the variable was not found.
         if (ifabm_state_var.eq.-1) then
            FATAL 'init_gotm_fabm_input: state variable '//trim(variables(i))//', referenced in the observed profile &
                  &namelist, was not found in model.'
            stop 'init_gotm_fabm_input'
         end if

!        Store position of variable in observed value array, as well as the variable's FABM identifier.
         ivariables(i) = ifabm_state_var
         relax_tau(:,ifabm_state_var) = RelaxTaus(i)
      end do

!     Initialize the file with observations.
      call init_observed_profiles(curinfo,file,next_unit,ivariables(1:variablecount),_LOCATION_)
      if (curinfo%unit.ne.-1) next_unit = next_unit + 1
   
   end do
   
!  Close the namelist file
97 close(namlst)
   
   return

98 LEVEL2 'I could not open '//trim(fname)
   return

99 FATAL 'I could not read '//trim(fname)
   stop 'init_gotm_fabm_obs'

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
   integer, intent(in) :: jul,secs,nlev
   REALTYPE, intent(in) :: z(:)
   logical, intent(in) :: init_state
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

!  Loop over all files with observations, and for each, obtain current profiles for all contained variables.
   do i=1,ubound(observed_profile_info,1)
      if (observed_profile_info(i)%unit.ne.-1) then
!        Get variable values from the current data file.
         call get_observed_profiles(observed_profile_info(i),jul,secs,nlev,z)

!        If requested, copy new variable values to the FABM state variable array.
         if (init_state) then
            do n=1,ubound(observed_profile_info(i)%variables,1)
               cc(observed_profile_info(i)%variables(n),:) = cc_obs(:,observed_profile_info(i)%variables(n))
            end do
         end if
      end if
   end do
   
   end subroutine do_gotm_fabm_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize a single input file
!
! !INTERFACE:
   subroutine init_observed_profiles(info,file,unit,variables,nlev)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !USES:
!
! !INPUT PARAMETERS:
   character(len=64),                intent(in) :: file
   integer,                          intent(in) :: unit,variables(:),nlev
!
! !OUTPUT PARAMETERS:
   type(type_observed_profile_info), intent(out):: info
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
!  Initialize derived type members that will hold information on the status of the input file.
   info%jul2  = 0
   info%secs2 = 0
   info%lines = 0
   info%nprofiles = 0
   info%one_profile = .false.
   info%unit = unit

!  Open the input file.
   open(unit,file=file,status='unknown',err=80)
   
   allocate(info%variables(ubound(variables,1)))
   info%variables = variables

   allocate(info%prof1(0:nlev,ubound(info%variables,1)),stat=rc)
   if (rc /= 0) stop 'gotm_fabm:get_observed_profiles: Error allocating memory (prof1)'
   info%prof1 = _ZERO_

   allocate(info%prof2(0:nlev,ubound(info%variables,1)),stat=rc)
   if (rc /= 0) stop 'gotm_fabm:get_observed_profiles: Error allocating memory (prof2)'
   info%prof2 = _ZERO_

   allocate(info%alpha(0:nlev,ubound(info%variables,1)),stat=rc)
   if (rc /= 0) stop 'gotm_fabm:get_observed_profiles: Error allocating memory (alpha)'

   return

80 FATAL 'Unable to open "',trim(file),'" for reading'
   stop 'gotm_fabm:init_observed_profiles'
   
   end subroutine init_observed_profiles
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read data from a single input file
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
   use time
   use observations, only: read_profiles
!
! !INPUT PARAMETERS:
   type(type_observed_profile_info), intent(inout):: info
   integer,                          intent(in)   :: jul,secs
   integer,                          intent(in)   :: nlev
   REALTYPE,                         intent(in)   :: z(0:nlev)
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
         call read_profiles(info%unit,nlev,ubound(info%variables,1),yy,mm,dd,hh,min,ss,z,info%prof2,info%lines,rc)
         if(rc .ne. 0) then
            if(info%nprofiles .eq. 1) then
               LEVEL3 'Only one set of profiles are present.'
               info%one_profile = .true.
               do n=1,ubound(info%variables,1)
                  if (info%variables(n).ne.-1) cc_obs(info%variables(n),:) = info%prof1(:,n)
               end do
            else
               FATAL 'Error reading profiles around line #',info%lines
               stop 'get_bio_profiles'
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
      do n=1,ubound(info%variables,1)
         cc_obs(:,info%variables(n)) = info%prof1(:,n) + t*info%alpha(:,n)
      end do
   end if

   end subroutine get_observed_profiles
!EOC

!-----------------------------------------------------------------------

   end module gotm_fabm_input

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


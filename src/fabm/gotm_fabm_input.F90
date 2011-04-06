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

   use gotm_fabm,only:fabm_calc,model,z,cc
   
   implicit none
   
   private
  
   public init_gotm_fabm_input,do_gotm_fabm_input

   type type_observed_profile_info
      REALTYPE, dimension(:,:), allocatable :: prof1,prof2,alpha
      integer             :: jul1,secs1
      integer             :: jul2,secs2
      integer             :: unit
      integer,dimension(:),allocatable :: variables
      integer             :: lines,nprofiles
      logical             :: one_profile
   end type
   
   type (type_observed_profile_info) :: observed_profile_info

   contains
   
   subroutine init_gotm_fabm_input(_LOCATION_,namlst,fname)
   
      integer,intent(in)        :: _LOCATION_,namlst
      character(len=*), intent(in)        :: fname
   
      character(len=64)         :: variables(256),file
      integer                   :: i,j,variablecount,ivariables(256)
      namelist /observed_profiles/ file,variablecount,variables
      
      if (.not.fabm_calc) return

      observed_profile_info%unit = -1
      open(namlst,file=fname,action='read',status='old',err=98)

      ! Open files with observed profiles (if any)
      variables = ''
      read(namlst,nml=observed_profiles,err=99,end=100)
      ivariables = -1
      do i=1,variablecount
         if (variables(i).eq.'') cycle
         do j=1,ubound(model%info%state_variables,1)
            if (variables(i).eq.model%info%state_variables(j)%name) then
               ivariables(i) = j
               exit
            end if
         end do
         if (ivariables(i).eq.-1) then
            FATAL 'init_gotm_fabm: state variable '//trim(variables(i))//', referenced in the observed profile namelist, was not found in model.'
            stop 1
         end if
      end do
      call init_observed_profiles(observed_profile_info,file,555,ivariables(1:variablecount),_LOCATION_)

      ! Close the namelist file
100   close(namlst)

      return

98 LEVEL2 'I could not open '//trim(fname)
   return

99 FATAL 'I could not read '//trim(fname)
   stop 'init_gotm_fabm_obs'

   end subroutine init_gotm_fabm_input
   
   subroutine do_gotm_fabm_input(jul,secs,nlev)
   
   integer, intent(in) :: jul,secs,nlev

   if (.not.fabm_calc) return

   ! If observed profiles are available, retrieve them.
   if (observed_profile_info%unit.ne.-1) call get_observed_profiles(observed_profile_info,jul,secs,nlev,z)
   
   end subroutine do_gotm_fabm_input

   subroutine init_observed_profiles(info,file,unit,variables,nlev)
      type(type_observed_profile_info), intent(out):: info
      character(len=64),                intent(in) :: file
      integer,                          intent(in) :: unit,variables(:),nlev
      
      integer :: rc
      
      info%jul2  = 0
      info%secs2 = 0
      info%lines = 0
      info%nprofiles = 0
      info%one_profile = .false.
      info%unit = unit

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

   subroutine get_observed_profiles(info,jul,secs,nlev,z)
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
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: rc,n
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,dt

!  Currently, observed profiles are used for initialization only.
!  If this is complete, do not read any further.
   if (info%nprofiles.gt.0) return

!  This part ireads in new values if necessary.
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
                  if (info%variables(n).ne.-1) cc(info%variables(n),:) = info%prof1(:,n)
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
         cc(info%variables(n),:) = info%prof1(:,n) + t*info%alpha(:,n)
      end do
   end if

   end subroutine get_observed_profiles

   end module gotm_fabm_input

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


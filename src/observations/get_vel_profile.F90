!$Id: get_vel_profile.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_vel_profile
!
! !INTERFACE:
   subroutine get_vel_profile(unit,jul,secs,nlev,z)
!
! !DESCRIPTION:
!  This routine will get the velocity profile - either by setting to a 
!  constant or be reading from a file.
!
! !USES:
   use time
   use observations, only: read_profiles,uprof,vprof
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: unit
   integer, intent(in)	:: jul,secs
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: z(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  $Log: get_vel_profile.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: rc
   integer		:: yy,mm,dd,hh,min,ss
   REALTYPE		:: t,dt
   integer, save        :: jul1,secs1
   integer, save	:: jul2=0,secs2=0
   REALTYPE, save, dimension(:,:), allocatable	:: prof1,prof2,alpha
   integer, parameter	:: cols=2
   integer, save	:: lines=0
   integer, save	:: nprofiles=0
   logical, save	:: one_profile=.false.
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if ( .not. allocated(prof1)) then
      allocate(prof1(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_vel_profile: Error allocating memory (prof1)'
      prof1 = 0.
   end if
   if ( .not. allocated(prof2)) then
      allocate(prof2(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_vel_profile: Error allocating memory (prof2)'
      prof2 = 0.
   end if
   if ( .not. allocated(alpha)) then
      allocate(alpha(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_vel_profile: Error allocating memory (alpha)'
   end if

!  This part initialises and reads in new values if necessary.
   if(.not. one_profile .and. time_diff(jul2,secs2,jul,secs) .lt. 0) then 
      do
         jul1 = jul2
         secs1 = secs2
         prof1 = prof2
         call read_profiles(unit,nlev,cols,yy,mm,dd,hh,min,ss,z,prof2,lines,rc)
         if(rc .ne. 0) then
            if(nprofiles .eq. 1) then
               LEVEL3 'Only one velocity profile present.'
               one_profile = .true.
               uprof = prof1(:,1)
               vprof = prof1(:,2)
            else
               FATAL 'Error reading velocity profile around line #',lines
            end if
            EXIT
         else
            nprofiles = nprofiles + 1
            call julian_day(yy,mm,dd,jul2)
            secs2 = hh*3600 + min*60 + ss
            if(time_diff(jul2,secs2,jul,secs) .gt. 0) EXIT
         end if
      end do
      if( .not. one_profile) then
         dt = time_diff(jul2,secs2,jul1,secs1)
         alpha = (prof2-prof1)/dt
      end if
   end if

!  Do the time interpolation - only if more than one profile
   if( .not. one_profile) then
      t  = time_diff(jul,secs,jul1,secs1)
      uprof = prof1(:,1) + t*alpha(:,1)
      vprof = prof1(:,2) + t*alpha(:,2)
   end if

   return
   end subroutine get_vel_profile
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard
!-----------------------------------------------------------------------

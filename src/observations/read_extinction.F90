!$Id: read_extinction.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_extinction
!
! !INTERFACE:
   subroutine read_extinction(unit,jul,secs)
!
! !DESCRIPTION:
!  This routine will provide the extinction coefficients - either by Jerlov
!  classes or by reading from file.
!
! !USES:
   use time
   use observations, only : read_obs
   use observations, only : A,g1,g2
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: unit,jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  $Log: read_extinction.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: yy,mm,dd,hh,min,ss
   REALTYPE		:: t
   REALTYPE, SAVE	:: dt
   integer, save        :: jul1,secs1
   integer, save	:: jul2=0,secs2=0
   REALTYPE, save	:: alpha(3)
   REALTYPE, save	:: obs(3),obs1(3),obs2(3)=0.
   integer		:: rc
!
!EOP
!-----------------------------------------------------------------------
!BOC
!  This part initialise and read in new values if necessary.
   if(time_diff(jul2,secs2,jul,secs) .lt. 0) then 
      do
         jul1 = jul2
         secs1 = secs2
         obs1 = obs2
         call read_obs(unit,yy,mm,dd,hh,min,ss,3,obs2,rc)
         call julian_day(yy,mm,dd,jul2)
         secs2 = hh*3600 + min*60 + ss
         if(time_diff(jul2,secs2,jul,secs) .gt. 0) EXIT
      end do
      dt = time_diff(jul2,secs2,jul1,secs1)
      alpha = (obs2-obs1)/dt
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,jul1,secs1)

   obs = obs1 + t*alpha 

   A = obs(1)
   g1 = obs(2)
   g2 = obs(3)

   return
   end subroutine read_extinction
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard

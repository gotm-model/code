!$Id: time.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  time - keeps control of time
!
! !INTERFACE:
   MODULE time
!
! !DESCRIPTION:
!  This module provides a number of routine/functions and variables
!  related to time.
!  The basic concept used in this module is that time is expressed
!  as two integers - one is the true Julian day and the other is
!  seconds since midnight. All calculations with time then becomes very simple
!  since it becomes operations on integers.
!  In the future it is the intention to extend this module - so resolutions
!  higher than secs can be used.
!
! !USES:
   IMPLICIT NONE
!
!  Default all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public :: init_time, calendar_date, julian_day, update_time
   public :: write_time_string
   public :: time_diff

! !PUBLIC DATA MEMBERS:
   character(len=19), public	:: timestr
   integer, public	:: julianday,secondsofday
   REALTYPE, public	:: timestep
   REALTYPE, public	:: fsecs,simtime
   integer, public	:: timefmt
   integer, public	:: MinN,MaxN
   character(len=19), public	:: start='2000-01-01 00:00:00',stop
!
! !PRIVATE DATA MEMBERS:
   logical		:: HasRealTime=.true. 
   integer		:: jul0=-1,secs0=-1
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: time.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_time - initialise the time system.
!
! !INTERFACE:
   subroutine init_time(MinN,MaxN)
!
! !DESCRIPTION:
!  Reads the namelist and makes calls to the init functions of the
!  various model components.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   integer, intent(out)	:: MinN,MaxN
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See time module
!
! !LOCAL VARIABLES:
   integer		:: jul1,secs1,jul2,secs2
   integer		:: ndays,nsecs
!
!EOP
!-------------------------------------------------------------------------
!BOC
!
!  Read time specific things from the namelist. 
!
   LEVEL1 'init_time'
!
!  Calculate MaxN -> MinN is 1 if not changed by HotStart
!
   MinN = 1
   LEVEL2 'Time step:      ',timestep,' seconds'
   LEVEL2 'Time format:    ',timefmt
   select case (timefmt)
      case (1)
         HasRealTime=.false.
         LEVEL2 '# of timesteps: ',MaxN
         start='2000-01-01 00:00:00'
         LEVEL2 'Fake start:     ',start
      case (2)
         LEVEL2 'Start:          ',start
         LEVEL2 'Stop:           ',stop
         Call String2JulSecs(start,jul1,secs1)
         Call String2JulSecs(stop,jul2,secs2)

         nsecs = time_diff(jul2,secs2,jul1,secs1)
         MaxN  = nint(nsecs/timestep)

         ndays = jul2-jul1
         if (nsecs .lt. 86400 .and. jul1 .ne. jul2) ndays = ndays-1
         nsecs = nsecs - 86400*ndays
         STDERR '        ==> ',ndays,' day(s) and ',nsecs,' seconds ==> ',MaxN,' micro time steps' 
      case (3)
         LEVEL2 'Start:          ',start
         LEVEL2 '# of timesteps: ',MaxN

         call String2JulSecs(start,jul1,secs1)

         nsecs = nint(MaxN*timestep) + secs1
         ndays = nsecs/86400 
         jul2  = jul1 + ndays
         secs2 = mod(nsecs,86400)

         call write_time_string(jul2,secs2,stop)
         LEVEL2 'Stop:           ',stop
      case default
         STDERR 'Fatal error: A non valid input format has been chosen'
         stop 'init_time'
   end select
 
   jul0  = jul1
   secs0 = secs1 

   julianday    = jul0
   secondsofday = secs0

   simtime = timestep*(MaxN-MinN+1)

   return
   end subroutine init_time
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  calendar_date() - converts true Julian day to calender date
!
! !INTERFACE:
   subroutine calendar_date(julian,yyyy,mm,dd)
!
! !DESCRIPTION:
!  Converts a Julian day to a calendar date - year, month and day.
!  Based on a similar routine in \em{Numerical Recipes}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer		:: julian
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   integer		:: yyyy,mm,dd
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See time module
!
! !LOCAL VARIABLES:
   integer, parameter	:: IGREG=2299161
   integer 		:: ja,jb,jc,jd,je
   REAL			:: x
!EOP
!-----------------------------------------------------------------------
!BOC
   if(julian .ge. IGREG ) then
      x = ((julian-1867216)-0.25)/36524.25
      ja = julian+1+int(x)-int(0.25*x)
   else
      ja = julian
   end if

   jb = ja+1524
   jc = int(6680 + ((jb-2439870)-122.1)/365.25)
   jd = int(365*jc+(0.25*jc))
   je = int((jb-jd)/30.6001)

   dd = jb-jd-int(30.6001*je)
   mm = je-1
   if (mm .gt. 12) mm = mm-12
   yyyy = jc - 4715
   if (mm .gt. 2) yyyy = yyyy-1
   if (yyyy .le. 0) yyyy = yyyy-1

   return
   end subroutine calendar_date
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  julian_day() - converts a calendar date to true Julian day
!
! !INTERFACE:
   subroutine julian_day(yyyy,mm,dd,julian)
!
! !DESCRIPTION:
!  Converts a calendar date to a aJulian day.
!  Based on a similar routine in \em{Numerical Recipes}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer		:: yyyy,mm,dd
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   integer		:: julian
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See time module
!
! !LOCAL VARIABLES:
   integer, PARAMETER	:: IGREG=15+31*(10+12*1582)
   integer 		:: ja,jy,jm
!EOP
!-----------------------------------------------------------------------
!BOC
   jy = yyyy
   if(jy .lt. 0) jy = jy+1
   if (mm .gt. 2) then
      jm = mm+1
   else
      jy = jy-1
      jm = mm+13
   end if
   julian = int(floor(365.25*jy)+floor(30.6001*jm)+dd+1720995)
   if (dd+31*(mm+12*yyyy) .ge. IGREG) then
      ja = int(0.01*jy)
      julian = julian+2-ja+int(0.25*ja)
   end if

   return
   end subroutine julian_day
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  update_time() - keep track of real time (Julian Days and secs)
!
! !INTERFACE:
   subroutine update_time(n)
!
! !DESCRIPTION:
!  Based on a starting time - this routine calculates the actual time
!  in a model integration using n and the timestep (dt).
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: n
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See time module
!
! !LOCAL VARIABLES:
   integer		:: nsecs
!EOP
!-----------------------------------------------------------------------
!BOC

   nsecs = nint(n*timestep) + secs0
   fsecs = n*timestep + secs0
   julianday    = jul0 + nsecs/86400
   secondsofday = mod(nsecs,86400)

   return
   end subroutine update_time
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  String2JulSecs() - converts time string to julian day and secs
!
! !INTERFACE:
   subroutine String2JulSecs(timestr,jul,secs)
!
! !DESCRIPTION:
!  Converts a time string to Julian day ans seconds of that day.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=19)	:: timestr
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   integer, intent(out)	:: jul,secs
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See time module
!
! !LOCAL VARIABLES:
   character		:: c1,c2,c3,c4
   integer		:: yy,mm,dd,hh,min,ss
!EOP
!-----------------------------------------------------------------------
!BOC
   read(timestr,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)')  &
                          yy,c1,mm,c2,dd,hh,c3,min,c4,ss
   call julian_day(yy,mm,dd,jul)
   secs = 3600*hh + 60*min + ss

   return
   end subroutine String2JulSecs
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  write_time_string() - a nice formatted time string
!
! !INTERFACE:
   subroutine write_time_string(jul,secs,timestr)
!
! !DESCRIPTION:
!  Formats a Julian day and seconds of that day to a readeble string
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   character(len=19)	:: timestr
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See time module
!
! !LOCAL VARIABLES:
   integer		:: ss,min,hh,dd,mm,yy
!EOP
!-----------------------------------------------------------------------
!BOC
   hh   = secs/3600
   min  = (secs-hh*3600)/60
   ss   = secs - 3600*hh - 60*min
   Call calendar_date(jul,yy,mm,dd)
   write(timestr,'(i4.4,a1,i2.2,a1,i2.2,1x,i2.2,a1,i2.2,a1,i2.2)')  &
                        yy,'-',mm,'-',dd,hh,':',min,':',ss
   return
   end subroutine write_time_string
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  time_diff() - the time difference in seconds
!
! !INTERFACE:
   integer FUNCTION time_diff(jul1,secs1,jul2,secs2)
!
! !DESCRIPTION:
!  The time difference (in seconds) betwen two times.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: jul1,secs1,jul2,secs2
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See time module
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   time_diff = 86400*(jul1-jul2) + (secs1-secs2)
   return
   end function  time_diff

!EOC

!-----------------------------------------------------------------------

   end module time

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard

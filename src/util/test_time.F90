#include "cppdefs.h"
!
   program test_time
!
! !DESCRIPTION:
!  program to test the time module
!
!  To execute:
!  make test_time
!
! !USES:
   use time
   implicit none
!
! !LOCAL VARIABLES
   integer                   :: jul1,secs1,jul2,secs2
   integer                   :: ndays,nsecs
   integer                   :: yyyy,mm,dd
!EOP
!-----------------------------------------------------------------------
!BOC
   start='2000-01-01 00:00:00'
   stop='2001-01-01 00:00:00'

   call read_time_string(start,jul1,secs1)
   call read_time_string(stop,jul2,secs2)

   LEVEL1 "Time string to Julian day and seconds:"
   LEVEL2 start,' --> ',jul1,secs1
   LEVEL2 stop,' --> ',jul2,secs2

   LEVEL1 ''
   LEVEL1 "Time difference between 2 Julian Days:"
   nsecs = time_diff(jul2,secs2,jul1,secs1)
   ndays = jul2-jul1
   LEVEL2 "Seconds: ",nsecs
   LEVEL2 "Days:    ",ndays

   LEVEL1 ''
   LEVEL1 "Julian Day to calendar date:"
   call calendar_date(jul1,yyyy,mm,dd)
   LEVEL2 jul1,' ---> ',yyyy,mm,dd
   call calendar_date(jul2,yyyy,mm,dd)
   LEVEL2 jul2,' ---> ',yyyy,mm,dd

   end program test_time
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

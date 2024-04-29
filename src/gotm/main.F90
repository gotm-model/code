#include<cppdefs.h>
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: GOTM --- the main program  \label{sec:main}
!
! !INTERFACE:
   program main
!
! !DESCRIPTION:
! This is the main program of GOTM. However, because GOTM has been programmed
! in a modular way, this routine is very short and merely calls internal
! routines of other modules. Its main purpose is to update the time and to
! call the internal routines {\tt init\_gotm()}, {\tt time\_loop()}, and
! {\tt clean\_up()}, which are defined in the module {\tt gotm} as discussed in
! \sect{sec:gotm}.
!
! !USES:
   use cmdline
   use time
   use gotm
!
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:

   character(LEN=8)          :: systemdate
   character(LEN=10)         :: systemtime
   real                      :: t1=-1,t2=-1
!
!-----------------------------------------------------------------------
!BOC
   call parse_cmdline('gotm')

!  monitor CPU time and report system time and date
#ifdef FORTRAN95
   call CPU_Time(t1)

   call Date_And_Time(DATE=systemdate,TIME=systemtime)

   STDERR LINE
   STDERR 'GOTM started on ', systemdate(1:4), '/', &
                              systemdate(5:6), '/', &
                              systemdate(7:8),      &
                      ' at ', systemtime(1:2), ':', &
                              systemtime(3:4), ':', &
                              systemtime(5:6)
   STDERR LINE
#else
   STDERR LINE
   STDERR 'GOTM'
   STDERR LINE
#endif

!  run the model
   call initialize_gotm()
   call integrate_gotm()
   call finalize_gotm()

!  report system date and time at end of run
#ifdef FORTRAN95
   call Date_And_Time(DATE=systemdate,TIME=systemtime)

   STDERR LINE
   STDERR 'GOTM finished on ', systemdate(1:4), '/', &
                               systemdate(5:6), '/', &
                               systemdate(7:8),      &
                       ' at ', systemtime(1:2), ':', &
                               systemtime(3:4), ':', &
                               systemtime(5:6)
   STDERR LINE
#else
   STDERR LINE
   STDERR 'GOTM'
   STDERR LINE
#endif

!  report CPU time used for run
#ifdef FORTRAN95
   call CPU_Time(t2)

   STDERR 'CPU time:                    ',t2-t1,' seconds'
   STDERR 'Simulated time/CPU time:     ',simtime/(t2-t1)
#endif

   call print_version()

end program

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

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
   call cmdline()

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
#if 1
   call init_gotm()
#else
   call init_gotm(t1='1998-02-01 00:00:00',t2='1998-07-01 00:00:00')
#endif
   call time_loop()
   call clean_up()

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

   contains

!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Parse the command line
!
! !INTERFACE:
   subroutine cmdline
!
! !DESCRIPTION:
!
! !LOCAL VARIABLES:
   character(len=32) :: arg
   integer :: i
!EOP
!-----------------------------------------------------------------------
!BOC
   i = 1
   do while (i <= command_argument_count())
      call get_command_argument(i, arg)
      select case (arg)
      case ('-v', '--version')
         call print_version()
         stop
      case ('-c', '--compile')
         call print_version()
         call compilation_options()
         stop
      case ('-h', '--help')
         call print_help()
         stop
      case default
         print '(a,a,/)', 'Unrecognized command-line option: ', arg
         call print_help()
         stop
      end select
      i = i+1
   end do

   end subroutine  cmdline

   subroutine compilation_options()
#ifdef _FABM_
      LEVEL1 '_FABM_'
#endif
#ifdef SEAGRASS
      LEVEL1 'SEAGRASS'
#endif
#ifdef SPM
      LEVEL1 'SPM'
#endif
#ifdef SEDIMENT
      LEVEL1 'SEDIMENT'
#endif
      STDERR LINE
   end subroutine compilation_options

   subroutine print_help()
      print '(a)', 'Usage: gotm [OPTIONS]'
      print '(a)', ''
      print '(a)', 'Options:'
      print '(a)', ''
      print '(a)', '  -h, --help        print usage information and exit'
      print '(a)', '  -v, --version     print version information'
      print '(a)', '  -c, --compiler    print compilation options'
      print '(a)', ''
   end subroutine print_help

end program

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

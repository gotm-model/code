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

!  monitor CPU time and report system time and date
#ifdef FORTRAN95
   call CPU_Time(t1)

   call Date_And_Time(DATE=systemdate,TIME=systemtime)

   STDERR LINE
   STDERR 'GOTM ver. ',    RELEASE,                           &
          ' started on ', systemdate(1:4), '/',               &
                          systemdate(5:6), '/',               &
                          systemdate(7:8),                    &
          ' at ',         systemtime(1:2), ':',               &
                          systemtime(3:4), ':',               &
                          systemtime(5:6)
   STDERR LINE
#else
   STDERR LINE
   STDERR 'GOTM ver. ',    RELEASE
   STDERR LINE
#endif

!  run the model
   call init_gotm()
   call time_loop()
   call clean_up()

!  report system date and time at end of run
#ifdef FORTRAN95
   call Date_And_Time(DATE=systemdate,TIME=systemtime)

   STDERR LINE
   STDERR 'GOTM ver. ',     RELEASE,                          &
          ' finished on ', systemdate(1:4), '/',              &
                           systemdate(5:6), '/',              &
                           systemdate(7:8),                   &
          ' at ',          systemtime(1:2), ':',              &
                           systemtime(3:4), ':',              &
                           systemtime(5:6)
   STDERR LINE
#else
   STDERR LINE
   STDERR 'GOTM ver. ',    RELEASE
   STDERR LINE
#endif

!  report CPU time used for run
#ifdef FORTRAN95
   call CPU_Time(t2)

   STDERR 'CPU time:                    ',t2-t1,' seconds'
   STDERR 'Simulated time/CPU time:     ',simtime/(t2-t1)
#endif

   call compilation_options

   end
!EOC

!-----------------------------------------------------------------------
   subroutine compilation_options
#ifndef GFORTRAN
   use, intrinsic ::  iso_Fortran_env
#endif
   IMPLICIT NONE
   STDERR LINE
!   STDERR 'GOTM:     www.gotm.net'
!   STDERR 'version:  ',RELEASE
!   STDERR 'git:      ',GIT_REVISION
!   STDERR 'compiler: ',FORTRAN_VERSION
   STDERR 'Compilation options: '
   STDERR LINE
!
#ifndef GFORTRAN
!   STDERR compiler_version()
!   STDERR compiler_options()
#else
#ifdef FORTRAN90
   LEVEL1 'Fortran 90 compilation'
#endif
#ifdef FORTRAN95
   LEVEL1 'Fortran 95 compilation'
#endif
#ifdef FORTRAN2003
   LEVEL1 'Fortran 2003 compilation'
#endif
#endif
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

   return
   end

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

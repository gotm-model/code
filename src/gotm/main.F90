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
   character(LEN=8)          :: datestr
   real                      :: t1=-1,t2=-1
REALTYPE :: sunrise, sunset
!
!-----------------------------------------------------------------------
!BOC
#ifdef FORTRAN95
   call CPU_Time(t1)
#endif
call sunrise_sunset(56.d0,23.26d0,sunrise,sunset)
STDERR sunrise,sunset

   call Date_And_Time(datestr,timestr)
   STDERR LINE
   STDERR 'GOTM ver. ',RELEASE,': Started on  ',datestr,' ',timestr
   STDERR LINE

   call init_gotm()
   call time_loop()
   call clean_up()

#ifdef FORTRAN95
   call CPU_Time(t2)
#endif
   call Date_And_Time(datestr,timestr)
   STDERR LINE
   STDERR 'GOTM ver. ',RELEASE,': Finished on ',datestr,' ',timestr
#ifdef FORTRAN95
   STDERR 'CPU-time was in loop:  ',t2-t1,' seconds'
   STDERR 'Sim-time/CPU-time:     ',simtime/(t2-t1)
#endif
   call compilation_options

   end
!EOC

!-----------------------------------------------------------------------
   subroutine compilation_options
   IMPLICIT NONE
!
   STDERR LINE
!   STDERR 'GOTM:     www.gotm.net'
!   STDERR 'version:  ',RELEASE
!   STDERR 'git:      ',GIT_REVISION
!   STDERR 'compiler: ',FORTRAN_VERSION
   STDERR 'Compilation options: '
   STDERR LINE
!
#ifdef FORTRAN90
   LEVEL1 'Fortran 90 compilation'
#endif
#ifdef FORTRAN95
   LEVEL1 'Fortran 95 compilation'
#endif
#ifdef FORTRAN2003
   LEVEL1 'Fortran 2003 compilation'
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

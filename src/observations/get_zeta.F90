#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_zeta
!
! !INTERFACE:
   subroutine get_zeta(method,unit,jul,secs)
!
! !DESCRIPTION:
!  This routine will provide sea surface elevation - either by an
!  analytical expression or read from file.
!  The subroutine is called in the {\tt get\_all\_obs()} subroutine
!  as part of the main integration loop.
!  The spatial interpolation is done via the reading routine
!  and the temporal interpolation is done in this routine.
!
! !USES:
   use time,         only: time_diff,julian_day,fsecs
   use observations, only: pi,init_saved_vars,read_obs
   use observations, only: period_1,amp_1,phase_1,period_2,amp_2,phase_2
   use observations, only: zeta,zeta_0
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: method,unit,jul,secs
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t
   REALTYPE, save            :: dt
   integer, save             :: jul1,secs1
   integer, save             :: jul2,secs2
   REALTYPE, save            :: alpha(1)
   REALTYPE, save            :: obs1(1),obs2(1)
   integer                   :: rc
!
!-----------------------------------------------------------------------
!BOC
   if (init_saved_vars) then
      jul2=0
      secs2=0
      obs2(1)=_ZERO_
   end if

   select case(method)
      case(0)                                         ! constant
         zeta = zeta_0
      case(1)                                         ! tides
         Zeta = amp_1*sin(2*pi*(fsecs-phase_1)/period_1) &
               +amp_2*sin(2*pi*(fsecs-phase_2)/period_2) &
               +zeta_0
      case(2)                                         ! from file
!        This part initialise and read in new values if necessary.
         if(time_diff(jul2,secs2,jul,secs) .lt. 0) then
            do
               jul1 = jul2
               secs1 = secs2
               obs1 = obs2
               call read_obs(unit,yy,mm,dd,hh,min,ss,1,obs2,rc)
               call julian_day(yy,mm,dd,jul2)
               secs2 = hh*3600 + min*60 + ss
               if(time_diff(jul2,secs2,jul,secs) .gt. 0) EXIT
            end do
            dt = time_diff(jul2,secs2,jul1,secs1)
            alpha = (obs2-obs1)/dt
         end if

!        Do the time interpolation
         t  = time_diff(jul,secs,jul1,secs1)

         zeta = obs1(1) + t*alpha(1)
      case default
   end select

   return
   end subroutine get_zeta
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

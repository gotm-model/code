!$Id: get_w_adv.F90,v 1.5 2005-06-27 13:44:07 kbk Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_w_adv
!
! !INTERFACE:
   subroutine get_w_adv(method,unit,jul,secs)
!
! !DESCRIPTION:
!  This routine is responsible for providing sane values to `observed'
!  vertical velocity.
!  The subroutine is called in the {\tt get\_all\_obs()} subroutine
!  as part of the main integration loop.
!  In case of observations from file the temporal interpolation is
!  done in this routine.
!
! !USES:
   use time,         only: time_diff,julian_day
   use observations, only: read_obs
   use observations, only: w_adv,w_adv0,w_height
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: method,unit,jul,secs
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  $Log: get_w_adv.F90,v $
!  Revision 1.5  2005-06-27 13:44:07  kbk
!  modified + removed traling blanks
!
!  Revision 1.4  2003/03/28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.3  2003/03/28 09:02:09  kbk
!  removed tabs
!
!  Revision 1.2  2003/03/10 08:51:57  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t
   REALTYPE, save            :: dt
   integer, save             :: jul1,secs1
   integer, save             :: jul2=0,secs2=0
   REALTYPE, save            :: alpha(2)
   REALTYPE, save            :: obs1(2),obs2(2)=0.
   integer                   :: rc
!
!-----------------------------------------------------------------------
!BOC
   select case(method)
      case(0)                               ! no vertical advection
         w_adv = _ZERO_
      case(1)
         w_adv = w_adv0
      case(2)                               ! from file
!        This part initialises and reads in new values if necessary.
         if(time_diff(jul2,secs2,jul,secs) .lt. 0) then
            do
               jul1 = jul2
               secs1 = secs2
               obs1 = obs2
               call read_obs(unit,yy,mm,dd,hh,min,ss,2,obs2,rc)
               call julian_day(yy,mm,dd,jul2)
               secs2 = hh*3600 + min*60 + ss
               if(time_diff(jul2,secs2,jul,secs) .gt. 0) EXIT
            end do
            dt = time_diff(jul2,secs2,jul1,secs1)
            alpha = (obs2-obs1)/dt
         end if

!        Do the time interpolation
         t  = time_diff(jul,secs,jul1,secs1)

         w_adv    = obs1(1) + t*alpha(1)
         w_height = obs1(2) + t*alpha(2)

      case default
   end select

   return
   end subroutine get_w_adv
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

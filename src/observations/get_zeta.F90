!$Id: get_zeta.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
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
!
! !USES:
   use time, only: time_diff,julian_day,fsecs
   use observations, only: pi,read_obs
!   use observations, only: zeta_method,zeta_unit
   use observations, only: Period1,Amp1,Phase1,Period2,Amp2,Phase2
   use observations, only: zeta,zeta0
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: method,unit,jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  $Log: get_zeta.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: yy,mm,dd,hh,min,ss
   REALTYPE		:: t
   REALTYPE, save	:: dt
   integer, save        :: jul1,secs1
   integer, save	:: jul2=0,secs2=0
   REALTYPE, save	:: alpha(1)
   REALTYPE, save	:: obs1(1),obs2(1)=0.
   integer		:: rc
!
!EOP
!-----------------------------------------------------------------------
!BOC
   select case(method)
      case(0)	! constant
         zeta = zeta0
      case(1)	! tides
         Zeta = Amp1*sin(2*pi*(fsecs-Phase1)/Period1) &
               +Amp2*sin(2*pi*(fsecs-Phase2)/Period2) &
               +zeta0
      case(2)	! from file
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
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard

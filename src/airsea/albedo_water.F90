#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculate the albeo over water\label{sec:albedo}
!
! !INTERFACE:
   REALTYPE function albedo_water(method,zenith_angle,yday)
!
! !DESCRIPTION:
!  The albedo of water is returned as function of solar zenith
!  and year day (unless the constant method is selected). Presently
!  2two different methods are available. The first is according to
!  Payne (1972) and the second according to Cogley (1979).
!  In principle they are equal as they interpolate in tabulated values.
!  Payne only interpolates in latitude where as Cogley also interpolates
!  in time (monthly values).
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: method
   REALTYPE, intent(in)                :: zenith_angle
   integer, intent(in)                 :: yday
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !DEFINED PARAMETERS:
   integer, parameter        :: CONST=0
   integer, parameter        :: PAYNE=1
   integer, parameter        :: COGLEY=2
!
! !LOCAL VARIABLES:
   REALTYPE                  :: albedo_payne,albedo_cogley
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (method)
      case (CONST)
         albedo_water = _ZERO_
      case (PAYNE)
!        calculates the albedo as a function of the solar zenith angle :
!        (after Payne jas 1972)
         albedo_water=albedo_payne(zenith_angle)
      case (COGLEY)
         albedo_water=albedo_cogley(zenith_angle,yday)
      case default
         FATAL "A non-valide albedo method has been used"
         stop 'albedo_water()'
   end select

   return
   end function albedo_water
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Albedo over water asccording to Payne(1972)
!
! !INTERFACE:
   REALTYPE function albedo_payne(zen)
!
! !DESCRIPTION:
!  The albedo monthly values from \cite{Payne72} are given  here
!  as means of the values between at 30$^{\circ}$ N and 40$^{\circ}$ N
!  for the Atlantic Ocean (hence the same latitudinal band of the
!  Mediterranean Sea).
!  Linear interpolation is done in latitude - time variation is included
!  via the solar zenith angle.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)      :: zen
!
! !DEFINED PARAMETERS:
   REALTYPE, parameter       :: alb1(20) = &
                 (/.719,.656,.603,.480,.385,.300,.250,.193,.164, &
                   .131,.103,.084,.071,.061,.054,.039,.036,.032,.031,.030 /)
   REALTYPE, parameter       :: za(20) = &
                 (/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,  &
                   66.,62.,58.,54.,50.,40.,30.,20.,10.,0.0 /)
   REALTYPE                  :: dza(19)
   data           dza/8*2.0, 6*4.0, 5*10.0/
!
! !LOCAL VARIABLES:
   integer                   :: jab
   REALTYPE                  :: dzen
!EOP
!-----------------------------------------------------------------------
!BOC
   if(zen .ge. 74.)then
      jab=.5*(90.-zen)+1.
   else if (zen .ge. 50.) then
      jab=.23*(74.-zen)+9.
   else
      jab=.10*(50.-zen)+15.
   endif
   if (jab .eq. 20) then
      dzen = _ZERO_
      albedo_payne=alb1(jab)
   else
      dzen=(za(jab)-zen)/dza(jab)
      albedo_payne=alb1(jab)+dzen*(alb1(jab+1)-alb1(jab))
   end if

   end function albedo_payne
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Albedo over water asccording to Cogley()
!
! !INTERFACE:
   REALTYPE function albedo_cogley(zen,yd)
!
! !DESCRIPTION:
!  The albedo monthly values are from  Graham Cogley (1979)
!  applied bilinear interpolation in latitude and time.
!  Likely the search of the indices used to look up values in the
!  different tables icould be optimized. This is a reference implementation
!  based on code provided by Adolf Stips, JRC.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)      :: zen
   integer, intent(in)       :: yd
!
! !DEFINED PARAMETERS:
   REALTYPE, parameter       :: a1(12,10) = reshape( &
!  Maybe 0.293 should be changed to 0.239 to give a smooth curve - KB 2015-05-06
!  From Table 5 in Cogley 1979
      (/1.   ,1.   ,0.301,0.293,0.171,0.148,0.160,0.246,0.342,1.   ,1.   ,1.   ,  &
        1.   ,0.301,0.319,0.225,0.16 ,0.131,0.145,0.206,0.294,0.305,1.   ,1.   ,  &
        0.301,0.338,0.229,0.148,0.116,0.112,0.114,0.134,0.202,0.313,0.301,1.   ,  &
        0.339,0.240,0.155,0.105,0.088,0.084,0.086,0.098,0.136,0.216,0.321,0.355,  &
        0.220,0.161,0.108,0.084,0.075,0.073,0.074,0.08 ,0.099,0.144,0.210,0.241,  &
        0.145,0.111,0.085,0.073,0.068,0.067,0.068,0.071,0.08 ,0.103,0.138,0.161,  &
        0.103,0.086,0.073,0.067,0.065,0.064,0.064,0.066,0.071,0.082,0.100,0.111,  &
        0.083,0.074,0.067,0.064,0.063,0.063,0.063,0.064,0.066,0.072,0.081,0.087,  &
        0.072,0.067,0.064,0.063,0.064,0.064,0.064,0.063,0.063,0.066,0.071,0.074,  &
        0.066,0.064,0.063,0.064,0.066,0.068,0.067,0.064,0.063,0.064,0.066,0.068/) &
       , shape(a1))
   REALTYPE, parameter       :: za(10) = (/90.,80.,70.,60.,50.,40.,30.,20.,10.,0.0/)
   REALTYPE, parameter       :: dz =  10.0
#if 0 
! This is the original provided by AS - as Fortran code
   REALTYPE, parameter       :: tim(12)= (/ &
                                           15.21,45.62,76.03,106.44,136.85,167.26, &
                                          197.67,228.08,258.49,288.90, 319.31, 349.72 &
                                         /)
#else
!  The corresponds to the R test code  - also from AS
   REALTYPE, parameter       :: tim(12)= (/ &
                                 1,32,60,91,121,152,182,213,244,274,305,335 &
                                         /)
#endif
   REALTYPE, parameter       :: dt= 365.25/12.
!
! !LOCAL VARIABLES:
   integer                   :: jab,tab,jab1,tab1
   REALTYPE                  :: dzen,dti,r1,r2,x
!EOP
!-----------------------------------------------------------------------
!BOC
   jab=min(max(((90.-zen)/dz+1d0),1d0),10d0)
   tab=min(max(floor(yd/dt)+1d0,1d0),12d0)

   dzen=(za(jab)-zen)/dz
   dti=(yd-tim(tab))/dt

!  interploate the two latitudes
   jab1=min(jab+1,10)
   tab1 = tab+1
   if (tab1 .gt. 12) tab1 = 1
   r1=a1(tab,jab) +dzen*(a1(tab,jab1) -a1(tab,jab))
   r2=a1(tab1,jab)+dzen*(a1(tab1,jab1)-a1(tab1,jab))

!  interpolate the time
   x=r1+dti*(r2-r1)
   albedo_cogley=(dmax1(dmin1(x,1d0),0d0))

   end function albedo_cogley
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

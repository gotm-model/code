#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculate the short--wave radiation \label{sec:swr}
!
! !INTERFACE:
   REALTYPE function albedo_water(method,yday,hh,dlon,dlat,bio_albedo)
!
! !DESCRIPTION:
!  The albedo monthly values from \cite{Payne72} are given  here
!  as means of the values between
!  at 30$^{\circ}$ N and 40$^{\circ}$ N for the Atlantic Ocean
!  (hence the same latitudinal band of the Mediterranean Sea).
!  Corrections for albedo due to biological substances are included.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: method
   integer, intent(in)                 :: yday
   REALTYPE, intent(in)                :: hh
   REALTYPE, intent(in)                :: dlon,dlat
   REALTYPE, intent(in)                :: bio_albedo
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !DEFINED PARAMETERS:
   integer, parameter        :: CONST=0
   integer, parameter        :: PAYNE=1

   REALTYPE, parameter       :: alb1(20) = &
                 (/.719,.656,.603,.480,.385,.300,.250,.193,.164, &
                   .131,.103,.084,.071,.061,.054,.039,.036,.032,.031,.030 /)

   REALTYPE, parameter       :: za(20) = &
                 (/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,  &
                   66.,62.,58.,54.,50.,40.,30.,20.,10.,0.0 /)

   REALTYPE                  :: dza(19)
   data           dza/8*2.0, 6*4.0, 5*10.0/
!
   REALTYPE, parameter       :: pi=3.14159265358979323846
   REALTYPE, parameter       :: deg2rad=pi/180.
   REALTYPE, parameter       :: rad2deg=180./pi

! !LOCAL VARIABLES:
   REALTYPE                  :: th0,th02,th03,sundec
   REALTYPE                  :: alon,alat
   REALTYPE                  :: thsun,coszen,zen,dzen
   integer                   :: jab
   REALTYPE                  :: yrdays
!EOP
!-----------------------------------------------------------------------
!BOC
!  from now on everything in radians
   alon = deg2rad*dlon
   alat = deg2rad*dlat

   yrdays=365.

   th0  = 2.*pi*yday/yrdays
   th02 = 2.*th0
   th03 = 3.*th0

!  sun declination :
   sundec = 0.006918 - 0.399912*cos(th0) + 0.070257*sin(th0)         &
           - 0.006758*cos(th02) + 0.000907*sin(th02)                 &
           - 0.002697*cos(th03) + 0.001480*sin(th03)

!  sun hour angle :
   thsun = (hh-12.)*15.*deg2rad + alon

!  cosine of the solar zenith angle :
   coszen =sin(alat)*sin(sundec)+cos(alat)*cos(sundec)*cos(thsun)
   if (coszen .le. _ZERO_) coszen = _ZERO_

   select case (method)
      case (CONST)
         albedo_water = _ZERO_
      case (PAYNE)
!        calculates the albedo as a function of the solar zenith angle :
!        (after Payne jas 1972)
!        solar zenith angle in degrees :
         zen=(rad2deg)*acos(coszen)
         if(zen .ge. 74.)then
            jab=.5*(90.-zen)+1.
         else if (zen .ge. 50.) then
            jab=.23*(74.-zen)+9.
         else
            jab=.10*(50.-zen)+15.
         endif
         dzen=(za(jab)-zen)/dza(jab)
         albedo_water=alb1(jab)+dzen*(alb1(jab+1)-alb1(jab))
      case default
         FATAL "A non-valide albedo method has been used"
         stop 'albedo_water()'
   end select

   return
   end function albedo_water
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

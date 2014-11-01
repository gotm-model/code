#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculate the short--wave radiation \label{sec:swr}
!
! !INTERFACE:
   REALTYPE function short_wave_radiation(yday,hh,dlon,dlat,cloud)
!
! !DESCRIPTION:
!  This subroutine calculates the short--wave net radiation based on
!  latitude, longitude, time, fractional cloud cover.
!  No corrections for albedo - must be done by calls to albedo_water() and
!  if ice is included albedo_ice().
!  The basic formula for the short-wave radiation at the surface, $Q_s$,
!  has been taken from \cite{RosatiMiyacoda88}, who adapted the work
!  of \cite{Reed77} and \cite{SimpsonPaulson99}:
!
!  \begin{equation}
!  Q_s=Q_{tot} (1-0.62 C + 0.0019 \beta) (1-\alpha),
!  \end{equation}
!
!  with the total radiation reaching the surface under clear skies,
!  $Q_{tot}$, the fractional cloud cover, $C$, the solar noon altitude,
!  $\beta$, and the albedo, $\alpha$.
!  This piece of code has been taken the MOM-I (Modular Ocean Model)
!  version at the INGV (Istituto Nazionale di Geofisica e Vulcanologia,
!  see {\tt http://www.bo.ingv.it/}).
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: yday
   REALTYPE, intent(in)                :: hh
   REALTYPE, intent(in)                :: dlon,dlat
   REALTYPE, intent(in)                :: cloud
!
! !OUTPUT PARAMETERS:
!   REALTYPE                            :: swr
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   REALTYPE, parameter       :: pi=3.14159265358979323846
   REALTYPE, parameter       :: deg2rad=pi/180.
   REALTYPE, parameter       :: rad2deg=180./pi

   REALTYPE, parameter       :: solar=1350.
   REALTYPE, parameter       :: eclips=23.439*deg2rad
   REALTYPE, parameter       :: tau=0.7
   REALTYPE, parameter       :: aozone=0.09

   REALTYPE                  :: th0,th02,th03,sundec
   REALTYPE                  :: thsun,coszen,zen,dzen,sunbet
   REALTYPE                  :: qatten,qzer,qdir,qdiff,qtot,qshort
   REALTYPE                  :: rlon,rlat,eqnx
   REALTYPE                  :: yrdays
!EOP
!-----------------------------------------------------------------------
!BOC
!  from now on everything in radians
   rlon = deg2rad*dlon
   rlat = deg2rad*dlat

   yrdays=365.
   eqnx = (yday-81.)/yrdays*2.*pi

   th0 = 2.*pi*yday/yrdays
   th02 = 2.*th0
   th03 = 3.*th0
!  sun declination :
   sundec = 0.006918 - 0.399912*cos(th0) + 0.070257*sin(th0)         &
           - 0.006758*cos(th02) + 0.000907*sin(th02)                 &
           - 0.002697*cos(th03) + 0.001480*sin(th03)
!  sun hour angle :
   thsun = (hh-12.)*15.*deg2rad + rlon

!  cosine of the solar zenith angle :
   coszen =sin(rlat)*sin(sundec)+cos(rlat)*cos(sundec)*cos(thsun)
   if (coszen .le. 0.0) then
      coszen = 0.0
      qatten = 0.0
   else
      qatten = tau**(_ONE_/coszen)
   end if
   qzer  = coszen * solar
   qdir  = qzer * qatten
   qdiff = ((_ONE_-aozone)*qzer - qdir) * 0.5
   qtot  =  qdir + qdiff

!  sin of the solar noon altitude in radians :
   sunbet=sin(rlat)*sin(eclips*sin(eqnx))+cos(rlat)*cos(eclips*sin(eqnx))
!  solar noon altitude in degrees :
   sunbet = asin(sunbet)*rad2deg

!  radiation as from Reed(1977), Simpson and Paulson(1979)
!  calculates SHORT WAVE FLUX ( watt/m*m )
!  Rosati,Miyakoda 1988 ; eq. 3.8
!  clouds from COADS perpetual data set
#if 1
   qshort  = qtot*(1-0.62*cloud + .0019*sunbet)
   if(qshort .gt. qtot ) then
      qshort  = qtot
   end if
#else
!  original implementation
   if(cloud .lt. 0.3) then
      qshort  = qtot
   else
      qshort  = qtot*(1-0.62*cloud + 0.0019*sunbet)
   endif
#endif

   short_wave_radiation = qshort

   return
   end function short_wave_radiation
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

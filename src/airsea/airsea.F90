!$Id: airsea.F90,v 1.6 2003-03-28 09:20:34 kbk Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: airsea --- atmopheric fluxes \label{sec:airsea}
!
! !INTERFACE:
   module airsea
!
! !DESCRIPTION:
!  This module provides various ways to calculate the heat, momentum 
!  and freshwater fluxes. They are either prescribed as constant values,
!  see {\tt airsea.inp}, read in from files or calculated by means of
!  bulk formulae, using observed or modelled meteorological parameters.
!
! !USES:
   use time,only: julian_day, time_diff, calendar_date
   use observations, only: read_obs
!
   IMPLICIT NONE
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public                              :: init_air_sea
   public                              :: air_sea_interaction
   public                              :: set_sst
   public                              :: integrated_fluxes
!
! !PUBLIC DATA MEMBERS:
   logical,  public                    :: calc_fluxes=.false.
!  tx and ty are the surface stress components:
   REALTYPE, public                    :: tx,ty
!  I_0 and heat are short-wave radiation and heat flux at the surface:
   REALTYPE, public                    :: I_0,heat
!  sst and sss are sea surface temperature and sea surface salinity:
   REALTYPE, public                    :: sst,sss
!  integrated short-wave radiation, heat flux and total heat flux:
   REALTYPE, public                    :: int_sw=0.,int_hf=0.
   REALTYPE, public                    :: int_total=0.
!
! !DEFINED PARAMETERS:
   integer, parameter                  :: meteo_unit=20
   integer, parameter                  :: heat_unit=21
   integer, parameter                  :: momentum_unit=22
   integer, parameter                  :: p_e_unit=23
   integer, parameter                  :: sst_unit=24
   integer, parameter                  :: sss_unit=25
   integer, parameter                  :: airt_unit=26

   REALTYPE, parameter                 :: cpa=1008.
   REALTYPE, parameter                 :: cp=3985.
   REALTYPE, parameter                 :: emiss=0.97
   REALTYPE, parameter                 :: bolz=5.67e-8
   REALTYPE, parameter                 :: Kelvin=273.16
   REALTYPE, parameter                 :: const06=0.62198
   REALTYPE, parameter                 :: pi=3.14159265358979323846
   REALTYPE, parameter                 :: deg2rad=pi/180.
   REALTYPE, parameter                 :: rad2deg=180./pi

   integer, parameter                  :: CONSTVAL=1
   integer, parameter                  :: FROMFILE=2
!
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!
!  $Log: airsea.F90,v $
!  Revision 1.6  2003-03-28 09:20:34  kbk
!  added new copyright to files
!
!  Revision 1.5  2003/03/28 08:13:47  kbk
!  removed tabs
!
!  Revision 1.4  2003/03/10 08:37:56  gotm
!  HB fixed the Kondo calculations
!
!  Revision 1.3  2001/11/18 11:43:48  gotm
!  Cleaned
!
!  Revision 1.2  2001/06/13 07:40:39  gotm
!  Lon, lat was hardcoded in meteo.F90 - now passed via init_meteo()
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! private data members
   integer                   :: heat_method
   integer                   :: momentum_method
   integer                   :: p_e_method
   integer                   :: sst_method
   integer                   :: sss_method
   integer                   :: airt_method

   character(len=PATH_MAX)   :: meteo_file
   character(len=PATH_MAX)   :: heatflux_file
   character(len=PATH_MAX)   :: momentumflux_file
   character(len=PATH_MAX)   :: p_e_flux_file
   character(len=PATH_MAX)   :: sss_file
   character(len=PATH_MAX)   :: sst_file
   character(len=PATH_MAX)   :: airt_file

   REALTYPE                  :: wx,wy
   REALTYPE                  :: w
   REALTYPE                  :: airp
   REALTYPE                  :: airt,twet
   REALTYPE                  :: cloud
   REALTYPE                  :: rh
   REALTYPE                  :: rho_air
   REALTYPE                  :: const_tx,const_ty
   REALTYPE                  :: const_qin,const_qout

   REALTYPE                  :: es,ea,qs,qa,L,S
   REALTYPE                  :: cdd,chd,ced

   REALTYPE                  :: alon,alat
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the air--sea interaction module
!
! !INTERFACE:
   subroutine init_air_sea(namlst,lat,lon)
!
! !DESCRIPTION:
!  This routine initialises the air--sea module by reading various variables
!  from a namelist and open relevant files.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: namlst
   REALTYPE, intent(in)                :: lat,lon
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!
! !LOCAL VARIABLES:
   namelist /airsea/ calc_fluxes, &
                     meteo_file, &
                     heat_method, &
                     const_qin,const_qout, &
                     heatflux_file, &
                     momentum_method, &
                     const_tx,const_ty, &
                     momentumflux_file, &
                     p_e_method,p_e_flux_file, &
                     sst_method, sst_file, &
                     sss_method, sss_file, &
                     airt_method, airt_file
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_air_sea'

   open(namlst,file='airsea.inp',action='read',status='old',err=90)
   read(namlst,nml=airsea,err=91)
   close(namlst)

   if (calc_fluxes) then

      open(meteo_unit,file=meteo_file,action='read',status='old',err=92)
      LEVEL2 'Reading meteo data from:'
      LEVEL3 trim(meteo_file)

   else

!     The heat fluxes
      select case (heat_method)
         case (FROMFILE)
            open(heat_unit,file=heatflux_file,action='read', &
                 status='old',err=93)
            LEVEL2 'Reading heat fluxes from:'
            LEVEL3 trim(heatflux_file)
         case default
      end select

!     The momentum fluxes
      select case (momentum_method)
         case (FROMFILE)
            open(momentum_unit,file=momentumflux_file,action='read', &
                 status='old',err=94)
            LEVEL2 'Reading momentum fluxes from:'
            LEVEL3 trim(momentumflux_file)
         case default
      end select

!     The fresh water fluxes
      select case (p_e_method)
         case (FROMFILE)
            open(p_e_unit,file=p_e_flux_file,action='read', &
                 status='old',err=95)
            LEVEL2 'Reading precipitatio/evaporation data from:'
            LEVEL3 trim(p_e_flux_file)
         case default
      end select

!     The sea surface temperature
      select case (sst_method)
         case (FROMFILE)
            open(sst_unit,file=sst_file,action='read',status='old',err=96)
            LEVEL2 'Reading sea surface temperature from:'
            LEVEL3 trim(sst_file)
         case default
      end select

!     The sea surface salinity
      select case (sss_method)
         case (FROMFILE)
            open(sss_unit,file=sss_file,action='read',status='old',err=97)
            LEVEL2 'Reading sea surface salinity from:'
            LEVEL3 trim(sss_file)
         case default
      end select

!     The air temperature
      select case (airt_method)
         case (FROMFILE)
            open(airt_unit,file=airt_file,action='read',status='old',err=98)
            LEVEL2 'Reading air temperatur from from:'
            LEVEL3 trim(airt_file)
         case default
      end select

   end if

   twet=0.
   rh=0.
   cloud=0.
   sss=0.
   airt=0.

   alon = deg2rad*lon
   alat = deg2rad*lat

   return

90 FATAL 'I could not open airsea.inp'
   stop 'init_airsea'
91 FATAL 'I could not read airsea namelist'
   stop 'init_airsea'
92 FATAL 'I could not open ',trim(meteo_file)
   stop 'init_airsea'
93 FATAL 'I could not open ',trim(heatflux_file)
   stop 'init_airsea'
94 FATAL 'I could not open ',trim(momentumflux_file)
   stop 'init_airsea'
95 FATAL 'I could not open ',trim(p_e_flux_file)
   stop 'init_airsea'
96 FATAL 'I could not open ',trim(sst_file)
   stop 'init_airsea'
97 FATAL 'I could not open ',trim(sss_file)
   stop 'init_airsea'
98 FATAL 'I could not open ',trim(airt_file)
   stop 'init_airsea'

   end subroutine init_air_sea
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Obtain the air--sea fluxes
!
! !INTERFACE:
   subroutine air_sea_interaction(jul,secs)
!
! !DESCRIPTION:
!
!  Depending on the value of the boolean variable {\tt calc\_fluxes},
!  the calculation of the fluxes and the short wave radiation are
!  called or the fluxes are directly read in from the namelist
!  {\tt airsea.inp} as constants or read in from files. With the present
!  version of GOTM, the 
!  surface momentum flux and the surface heat flux can be caluclated.
!  The surface salinity flux is not coded yet. 
!
!  On the long run this will be the routine to call, to calculate and
!  to obtain air--sea related variables. 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: jul,secs
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (calc_fluxes) then

      call flux_from_meteo(jul,secs)
      call short_wave_radiation(jul,secs,alon,alat)
   else
!     The heat fluxes
      select case (heat_method)
         case (CONSTVAL)
            I_0=const_qin
            heat=const_qout
         case (FROMFILE)
            call read_heat_flux(jul,secs,I_0,heat)
         case default
      end select
!     The momentum fluxes
      select case (momentum_method)
         case (CONSTVAL)
            tx=const_tx
            ty=const_ty
         case (FROMFILE)
            call read_momentum_flux(jul,secs,tx,ty)
         case default
      end select
!     The sea surface temperature
      select case (sst_method)
         case (FROMFILE)
            call read_sst(jul,secs,sst)
         case default
      end select
!     The sea surface salinity
      select case (sss_method)
         case (FROMFILE)
         case default
      end select
!     The air temperature
      select case (airt_method)
         case (FROMFILE)
         case default
      end select
   end if

   return
   end subroutine air_sea_interaction
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the air--sea interactions
!
! !INTERFACE:
   subroutine finish_air_sea_interaction
!
! !DESCRIPTION:
!  Various files are closed in this routine.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (calc_fluxes) then
      close(meteo_unit)
   else
      if (heat_method     .eq. FROMFILE) close(heat_unit)
      if (momentum_method .eq. FROMFILE) close(momentum_unit)
      if (p_e_method      .eq. FROMFILE) close(p_e_unit)
      if (sst_method      .eq. FROMFILE) close(sst_unit)
      if (sss_method      .eq. FROMFILE) close(sss_unit)
      if (airt_method     .eq. FROMFILE) close(airt_unit)
   end if
   return
   end subroutine finish_air_sea_interaction
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute the exchange coefficients \label{sec:calcCoeff}
!
! !INTERFACE:
   subroutine exchange_coefficients()
!
! !DESCRIPTION:
!  Based on the wind vector at 10 m, the sea surface temperature (either 
!  from the model or from a data file), the 
!  relative humidity, the air temperature and the air pressure at 2 m,
!  this function computes the surface momentum flux and the latent and 
!  sensible heat flux coefficients according to the \cite{Kondo75} 
!  bulk formulae.
!
! !USES:
   IMPLICIT NONE
!
! !DEFINED PARAMETERS:
   REALTYPE, parameter                 :: a1=6.107799961
   REALTYPE, parameter                 :: a2=4.436518521e-1
   REALTYPE, parameter                 :: a3=1.428945805e-2
   REALTYPE, parameter                 :: a4=2.650648471e-4
   REALTYPE, parameter                 :: a5=3.031240396e-6
   REALTYPE, parameter                 :: a6=2.034080948e-8
   REALTYPE, parameter                 :: a7=6.136820929e-11
   REALTYPE, parameter                 :: eps=1.0e-12
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for the airsear module
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: tvirt,s,s0
   REALTYPE                  :: ae_d,be_d,pe_d
   REALTYPE                  :: ae_h,be_h,ce_h,pe_h
   REALTYPE                  :: ae_e,be_e,ce_e,pe_e
   REALTYPE                  :: x
!
!-----------------------------------------------------------------------
!BOC

   w = sqrt(wx*wx+wy*wy)
   L = (2.5-0.00234*sst)*1.e6
   es = a1 +sst*(a2+sst*(a3+sst*(a4+sst*(a5+sst*(a6+sst*a7)))))
   es = es * 100.0 ! Conversion millibar --> Pascal
   qs = const06*es/(airp-0.377*es)

   if (rh .lt. 0.0) then
      ea = es - 67.*(airt-twet);
      x = (airt-twet)/(CONST06*L);
      ea = (es-cpa*airp*x)/(1+cpa*x);
      if(ea .lt. 0.0) ea = 0.0
      qa = CONST06*ea/(airp-0.377*ea);
   else
      qa = 0.01*rh*qs;
      ea = qa*airp/(const06 + 0.377*qa);
   end if

   tvirt = (airt+Kelvin)*(1+qa/const06)/(1+qa)
   rho_air = airp/(287.05*Tvirt)

!  Stability
   s0=0.25*(sst-airt)/(w+1.0e-10)**2
   s=s0*abs(s0)/(abs(s0)+0.01)

!  Transfer coefficient for heat and momentum

   if (w .lt. 2.2) then
      ae_d=0.0;   be_d=1.08;                  pe_d=-0.15;
      ae_h=0.0;   be_h=1.185;  ce_h=0.0;      pe_h=-0.157;
      ae_e=0.0;   be_e=1.23;   ce_e=0.0;      pe_e=-0.16;
   else if (w .lt. 5.0) then
      ae_d=0.771; be_d=0.0858;                pe_d=1.0;
      ae_h=0.927; be_h=0.0546; ce_h=0.0;      pe_h=1.0;
      ae_e=0.969; be_e=0.0521; ce_e=0.0;      pe_e=1.0;
   else if (w .lt. 8.0) then
      ae_d=0.867; be_d=0.0667;                pe_d=1.0;
      ae_h=1.15;  be_h=0.01;   ce_h=0.0;      pe_h=1.0;
      ae_e=1.18;  be_e=0.01;   ce_e=0.0;      pe_e=1.0;
   else if (w .lt. 25.0) then
      ae_d=1.2;   be_d=0.025;                 pe_d=1.0;
      ae_h=1.17;  be_h=0.0075; ce_h=-0.00045; pe_h=1.0;
      ae_e=1.196; be_e=0.008;  ce_e=-0.0004;  pe_e=1.0
   else
      ae_d=0.0;   be_d=0.073;                 pe_d=1.0;
      ae_h=1.652; be_h=-0.017; ce_h=0.0;      pe_h=1.0;
      ae_e=1.68;  be_e=-0.016; ce_e=0;        pe_e=1.0;
   end if

   cdd=(ae_d+be_d*exp(pe_d*log(w+eps)))*1.0e-3
   chd=(ae_h+be_h*exp(pe_h*log(w+eps))+ce_h*(w-8.0)**2)*1.0e-3
   ced=(ae_e+be_e*exp(pe_e*log(w+eps))+ce_e*(w-8.0)**2)*1.0e-3

   if(s .lt. 0.) then
      if (s .gt. -3.3) then
         x = 0.1+0.03*s+0.9*exp(4.8*s)
      else
         x = 0.0
      end if
      cdd=x*cdd
      chd=x*chd
      ced=x*ced
   else
      cdd=cdd*(1.0+0.47*sqrt(s))
      chd=chd*(1.0+0.63*sqrt(s))
      ced=ced*(1.0+0.63*sqrt(s))
   end if

   return
   end subroutine exchange_coefficients
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate the heat fluxes \label{sec:calcFluxes}
!
! !INTERFACE:
   subroutine do_calc_fluxes(heatf,taux,tauy)
!
! !DESCRIPTION:
!  The latent and the sensible heat flux, the long-wave back
!  radiation (and thus the total net surface heat flux) and
!  the surface momentum flux are calclated here. For the
!  long--wave back radiation, the formulae of \cite{Clarketal74} 
!  and \cite{HastenrathLamb78} may be used as alternatives. 
!
! !USES:
   IMPLICIT NONE
!
! !OUTPUT PARAMETERS:
   REALTYPE, optional, intent(out)     :: heatf,taux,tauy
!
! !DEFINED PARAMETERS:
   integer, parameter                  :: clark=1
   integer, parameter                  :: hastenrath=2
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: tmp
   REALTYPE                  :: qe,qh,qb
   integer                   :: back_radiation_method=clark
!
!-----------------------------------------------------------------------
!BOC

   qe=ced*L*rho_air*w*(qs-qa)            ! latent
   qh=chd*cpa*rho_air*w*(sst-airt)       ! sensible

   tmp=sst+Kelvin
   select case(back_radiation_method)    ! back radiation
      case(clark)
         qb=(1.0-.8*cloud*cloud)                                     &
            *emiss*bolz*(tmp**4)*(0.39-0.05*sqrt(ea/100.0))          &
            +4.0*emiss*bolz*(tmp**3)*(sst-airt)
      case(hastenrath)                    ! qa in g(water)/kg(wet air)
         qb=(1.0-.8*cloud*cloud)                                     &
            *emiss*bolz*(tmp**4)*(0.39-0.056*sqrt(1000*qa))          &
            +4.0*emiss*bolz*(tmp**3)*(sst-airt)
      case default
   end select

   if(present(heatf)) then
     heatf = -(qe+qh+qb)
   else
     heat = -(qe+qh+qb)
   end if

   tmp = -cdd*rho_air*w
   if(present(taux)) then
     taux  = tmp*wx
   else
     tx = tmp*wx
   end if
   if(present(tauy)) then
     tauy  = tmp*wy
   else
     ty = tmp*wy
   end if

   return
   end subroutine do_calc_fluxes
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate the short--wave radiation \label{sec:swr}
!
! !INTERFACE:
   subroutine short_wave_radiation(jul,secs,lon,lat,swr)
!
! !DESCRIPTION:
!  This subroutine calculates the short--wave net radiation based on 
!  latitude, longitude, time, fractional cloud cover and albedo.
!  The albedo monthly values from \cite{Payne72} are given  here
!  as means of the values between 
!  at 30$^{\circ}$ N and 40$^{\circ}$ N for the Atlantic Ocean 
!  (hence the same latitudinal band of the Mediterranean Sea).
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: jul,secs
   REALTYPE, intent(in)                :: lon,lat
!
! !OUTPUT PARAMETERS:
   REALTYPE, optional, intent(out)     :: swr
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!
! !LOCAL VARIABLES:
!
   REALTYPE                  :: solar=1350.
   REALTYPE                  :: eclips=23.439*deg2rad
   REALTYPE                  :: tau=0.7
   REALTYPE                  :: aozone=0.09


   REALTYPE                  :: th0,th02,th03,sundec
   REALTYPE                  :: thsun,coszen,zen,dzen,sunbet
   REALTYPE                  :: qatten,qzer,qdir,qdiff,qtot,qshort
   REALTYPE                  :: albedo
   integer                   :: jab
   integer                   :: yy,mm,dd
   REALTYPE                  :: yrdays,days,hour,tjul

   integer                   :: yday(12) = &
                 (/ 0,31,59,90,120,151,181,212,233,273,304,334 /)

   REALTYPE                  :: alb1(20) = &
                 (/.719,.656,.603,.480,.385,.300,.250,.193,.164, &
                   .131,.103,.084,.071,.061,.054,.039,.036,.032,.031,.030 /)

   REALTYPE                  :: za(20) = &
                 (/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,  &
                   66.,62.,58.,54.,50.,40.,30.,20.,10.,0.0 /)

   REALTYPE                  :: dza(19)
   data           dza/8*2.0, 6*4.0, 5*10.0/
!
!-----------------------------------------------------------------------
!BOC

!  number of days in a year :
   call calendar_date(jul,yy,mm,dd)
   days=float(yday(mm))+float(dd)
   hour=1.0*secs/3600.
!kbk   if (mod(yy,4) .eq. 0 ! leap year I forgot
   yrdays=365.

   th0 = 2.*pi*days/yrdays
   th02 = 2.*th0
   th03 = 3.*th0
!  sun declination :
   sundec = 0.006918 - 0.399912*cos(th0) + 0.070257*sin(th0)         &
           - 0.006758*cos(th02) + 0.000907*sin(th02)                 &
           - 0.002697*cos(th03) + 0.001480*sin(th03)

!  sun hour angle :
   thsun = (hour-12.)*15.*deg2rad + alon

!  cosine of the solar zenith angle :
   coszen =sin(alat)*sin(sundec)+cos(alat)*cos(sundec)*cos(thsun)
   if (coszen .le. 0.0) then
      coszen = 0.0
      qatten = 0.0
   else
      qatten = tau**(1./coszen)
   end if
   qzer  = coszen * solar
   qdir  = qzer * qatten
   qdiff = ((1.-aozone)*qzer - qdir) * 0.5
   qtot  =  qdir + qdiff

   tjul = (days-81.)/yrdays*2.*pi

!  sin of the solar noon altitude in radians :
   sunbet=sin(alat)*sin(eclips*sin(tjul))+cos(alat)*cos(eclips*sin(tjul))
!  solar noon altitude in degrees :
   sunbet = asin(sunbet)*rad2deg

!  calculates the albedo as a function of the solar zenith angle :
!  (after Payne jas 1972)
!  solar zenith angle in degrees :
   zen=(180./pi)*acos(coszen)
   if(zen .ge. 74.)then
      jab=.5*(90.-zen)+1.
   else if (zen .ge. 50.) then
      jab=.23*(74.-zen)+9.
   else
      jab=.10*(50.-zen)+15.
   endif

   dzen=(za(jab)-zen)/dza(jab)
   albedo=alb1(jab)+dzen*(alb1(jab+1)-alb1(jab))

!  radiation as from Reed(1977), Simpson and Paulson(1979)
!  calculates SHORT WAVE FLUX ( watt/m*m )
!  Rosati,Miyakoda 1988 ; eq. 3.8
!  clouds from COADS perpetual data set
   if(cloud .lt. 0.3) then
      qshort  = qtot
   else
      qshort  = qtot*(1-0.62*cloud + .0019*sunbet)*(1.-albedo)
   endif

   if (present(swr)) then
      swr = qshort
   else
      I_0 = qshort
   end if

   return
   end subroutine short_wave_radiation
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read meteo data, interpolate in time
!
! !INTERFACE:
   subroutine flux_from_meteo(jul,secs)
!
! !DESCRIPTION:
!  This routine reads meteo data from a file and calculates the 
!  fluxes of heat and momentum, and the
!  short--wave radiation, from these data as described in 
!  \sect{sec:calcCoeff}, \sect{sec:calcFluxes}, and \sect{sec:swr}.
!  Then, the results are interpolated in time.

!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: jul,secs
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t
   REALTYPE, SAVE            :: dt
   integer, save             :: meteo_jul1,meteo_secs1
   integer, save             :: meteo_jul2=0,meteo_secs2=0
   REALTYPE, save            :: obs(6)
   REALTYPE, save            :: alpha(4)
   REALTYPE, save            :: I1,h1,tx1,ty1
   REALTYPE, save            :: I2=0.,h2=0.,tx2=0.,ty2=0.
   logical, save             :: first=.true.
   integer                   :: rc
!
!-----------------------------------------------------------------------
!BOC
!  This part initialises and reads in new values if necessary.
   if(time_diff(meteo_jul2,meteo_secs2,jul,secs) .lt. 0) then
      do
         meteo_jul1 = meteo_jul2
         meteo_secs1 = meteo_secs2
         call read_obs(meteo_unit,yy,mm,dd,hh,min,ss,6,obs,rc)
         call julian_day(yy,mm,dd,meteo_jul2)
         meteo_secs2 = hh*3600 + min*60 + ss
         if(time_diff(meteo_jul2,meteo_secs2,jul,secs) .gt. 0) EXIT
      end do
      wx    = obs(1)
      wy    = obs(2)
      airp  = obs(3)*100. !kbk mbar/hPa --> Pa
      airt  = obs(4)
      rh    = obs(5)
      cloud = obs(6)
      call exchange_coefficients()
      if (first) then
         call do_calc_fluxes(heatf=h1,taux=tx1,tauy=ty1)
         call short_wave_radiation(jul,secs,alon,alat,swr=I1)
         I2  = I1
         h2  = h1
         tx2 = tx1
         ty2 = ty1
         first = .false.
      else
         I1  = I2
         h1  = h2
         tx1 = tx2
         ty1 = ty2
         call do_calc_fluxes(heatf=h2,taux=tx2,tauy=ty2)
         call short_wave_radiation(jul,secs,alon,alat,swr=I2)
      end if
      dt = time_diff(meteo_jul2,meteo_secs2,meteo_jul1,meteo_secs1)
      alpha(1) = (I2-I1)/dt
      alpha(2) = (h2-h1)/dt
      alpha(3) = (tx2-tx1)/dt
      alpha(4) = (ty2-ty1)/dt
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,meteo_jul1,meteo_secs1)
   I_0  = I1  + t*alpha(1)
   heat = h1  + t*alpha(2)
   tx   = tx1 + t*alpha(3)
   ty   = ty1 + t*alpha(4)

   return
   end subroutine flux_from_meteo
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read heat flux data, interpolate in time
!
! !INTERFACE:
   subroutine read_heat_flux(jul,secs,I_0,heat)
!
! !DESCRIPTION:
!  This routine reads heat fluxes from a file and interpolates them in
!  time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: jul,secs
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: I_0,heat
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,alpha
   REALTYPE, SAVE            :: dt
   integer, save             :: heat_jul1,heat_secs1
   integer, save             :: heat_jul2=0,heat_secs2=0
   REALTYPE, save            :: obs1(2),obs2(2)=0.
   integer                   :: rc
!
!-----------------------------------------------------------------------
!BOC
!  This part initialise and read in new values if necessary.
   if(time_diff(heat_jul2,heat_secs2,jul,secs) .lt. 0) then
      do
         heat_jul1 = heat_jul2
         heat_secs1 = heat_secs2
         obs1 = obs2
         call read_obs(heat_unit,yy,mm,dd,hh,min,ss,2,obs2,rc)
         call julian_day(yy,mm,dd,heat_jul2)
         heat_secs2 = hh*3600 + min*60 + ss
         if(time_diff(heat_jul2,heat_secs2,jul,secs) .gt. 0) EXIT
      end do
      dt = time_diff(heat_jul2,heat_secs2,heat_jul1,heat_secs1)
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,heat_jul1,heat_secs1)

   alpha = (obs2(1)-obs1(1))/dt
   I_0 = obs1(1) + t*alpha
   alpha = (obs2(2)-obs1(2))/dt
   heat = obs1(2) + t*alpha

   return
   end subroutine read_heat_flux
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read momentum flux data, interpolate in time
!
! !INTERFACE:
   subroutine read_momentum_flux(jul,secs,tx,ty)
!
! !DESCRIPTION:
!  This routine reads momentum fluxes from a file and interpolates them in
!  time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,intent(in)                  :: jul,secs
!
! !OUTPUT PARAMETERS:
   REALTYPE,intent(out)                :: tx,ty
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
! !LOCAL VARIABLES:
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,alpha
   REALTYPE, save            :: dt
   integer, save             :: mom_jul1,mom_secs1
   integer, save             :: mom_jul2=0,mom_secs2=0
   REALTYPE, save            :: obs1(2),obs2(2)=0.
   integer                   :: rc
!
!EOP
!-----------------------------------------------------------------------
!BOC

!  This part initialise and read in new values if necessary.
   if(time_diff(mom_jul2,mom_secs2,jul,secs) .lt. 0) then
      do
         mom_jul1 = mom_jul2
         mom_secs1 = mom_secs2
         obs1 = obs2
         call read_obs(momentum_unit,yy,mm,dd,hh,min,ss,2,obs2,rc)
         call julian_day(yy,mm,dd,mom_jul2)
         mom_secs2 = hh*3600 + min*60 + ss
         if(time_diff(mom_jul2,mom_secs2,jul,secs) .gt. 0) EXIT
      end do
      dt = time_diff(mom_jul2,mom_secs2,mom_jul1,mom_secs1)
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,mom_jul1,mom_secs1)
   alpha = (obs2(1)-obs1(1))/dt
   tx = obs1(1) + t*alpha
   alpha = (obs2(2)-obs1(2))/dt
   ty = obs1(2) + t*alpha

   return
   end subroutine read_momentum_flux
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read SST, interpolate in time
!
! !INTERFACE:
   subroutine read_sst(jul,secs,sst)
!
! !DESCRIPTION:
!  This routine reads sea surface temperature (SST) from a file 
!  and interpolates in time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: jul,secs
!
! !OUTPUT PARAMETERS:
   REALTYPE,intent(out)                :: sst
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,alpha
   REALTYPE, save            :: dt
   integer, save             :: sst_jul1,sst_secs1
   integer, save             :: sst_jul2=0,sst_secs2=0
   REALTYPE, save            :: obs1(1),obs2(1)=0.
   integer                   :: rc
!
!-----------------------------------------------------------------------
!BOC
!  This part initialise and read in new values if necessary.
   if(time_diff(sst_jul2,sst_secs2,jul,secs) .lt. 0) then
      do
         sst_jul1 = sst_jul2
         sst_secs1 = sst_secs2
         obs1 = obs2
         call read_obs(sst_unit,yy,mm,dd,hh,min,ss,1,obs2,rc)
         call julian_day(yy,mm,dd,sst_jul2)
         sst_secs2 = hh*3600 + min*60 + ss
         if(time_diff(sst_jul2,sst_secs2,jul,secs) .gt. 0) EXIT
      end do
      dt = time_diff(sst_jul2,sst_secs2,sst_jul1,sst_secs1)
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,sst_jul1,sst_secs1)
   alpha = (obs2(1)-obs1(1))/dt
   sst = obs1(1) + t*alpha

   return
   end subroutine read_sst
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Integrate short--wave and sea surface fluxes
!
! !INTERFACE:
   subroutine integrated_fluxes(dt)
!
! !DESCRIPTION:
!  This utility routine integrates the short--wave radiation 
!  and heat--fluxes over time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: dt
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!-----------------------------------------------------------------------
!BOC
   int_sw = int_sw + dt*I_0
   int_hf = int_hf + dt*heat
   int_total = int_total + int_sw + int_hf
   return
   end subroutine integrated_fluxes
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set the SST to be used from model.
!
! !INTERFACE:
   subroutine set_sst(temp)
!
! !DESCRIPTION:
!  This routine sets the sea surface temperature (SST) to be used for 
!  the surface flux calculations.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: temp
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See log for airsea module
!
!EOP
!-----------------------------------------------------------------------
!BOC
   sst = temp
   return
   end subroutine set_sst
!EOC

!-----------------------------------------------------------------------

   end module airsea

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

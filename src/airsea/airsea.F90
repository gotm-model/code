!$Id: airsea.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: airsea - the air sea interaction.
!
! !INTERFACE:
   module airsea 
!
! !DESCRIPTION: 
!  This module provides various ways to obtain the heat, momentum and freshwater
!  fluxes. In the future the air/sea interaction will hopefully spin off as
!  a separate Open Source project - but with close links to GOTM.
!
! !USE:
   use time, only: julian_day, time_diff, calendar_date
   use observations, only: read_obs
   IMPLICIT NONE
!
!  Default all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public		:: init_air_sea,air_sea_interaction
   public		:: set_sst
   public		:: integrated_fluxes
!
! !PUBLIC DATA MEMBERS:
   logical, public	:: calc_fluxes=.false.
   REALTYPE, public	:: tx,ty,I_0,heat
   REALTYPE, public	:: sst,sss 
   REALTYPE, public	:: int_sw=0.,int_hf=0.,int_total=0.
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!
!  $Log: airsea.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer, private	:: heat_method
   integer, private	:: momentum_method
   integer, private	:: p_e_method
   integer, private	:: sst_method
   integer, private	:: sss_method
   integer, private	:: airt_method

   character(len=PATH_MAX), private	:: meteo_file
   character(len=PATH_MAX), private	:: heatflux_file
   character(len=PATH_MAX), private	:: momentumflux_file
   character(len=PATH_MAX), private	:: p_e_flux_file
   character(len=PATH_MAX), private	:: sss_file
   character(len=PATH_MAX), private	:: sst_file
   character(len=PATH_MAX), private	:: airt_file

   integer, private, parameter	:: meteo_unit=20
   integer, private, parameter	:: heat_unit=21
   integer, private, parameter	:: momentum_unit=22
   integer, private, parameter	:: p_e_unit=23
   integer, private, parameter	:: sst_unit=24
   integer, private, parameter	:: sss_unit=25
   integer, private, parameter	:: airt_unit=26

   REALTYPE, private	:: wx,wy
   REALTYPE, private	:: w
   REALTYPE, private	:: airp
   REALTYPE, private	:: airt,twet
   REALTYPE, private	:: cloud
   REALTYPE, private	:: rh
   REALTYPE, private	:: rho_air
   REALTYPE, private	:: const_tx,const_ty
   REALTYPE, private	:: const_qin,const_qout

   REALTYPE, private	:: es,ea,qs,qa,L,S
   REALTYPE, private	:: cee_heat,ced_heat
   REALTYPE, private	:: cee_mom,ced_mom

   REALTYPE, private, parameter	:: cpa=1008. 
   REALTYPE, private, parameter	:: cp=3985. 
   REALTYPE, private, parameter	:: emiss=0.97
   REALTYPE, private, parameter	:: bolz=5.67e-8
   REALTYPE, private, parameter	:: Kelvin=273.16
   REALTYPE, private, parameter	:: const06=0.62198
   REALTYPE, private, parameter :: pi=3.14159265358979323846
   REALTYPE, private, parameter	:: deg2rad=pi/180.
   REALTYPE, private, parameter	:: rad2deg=180./pi

   integer, parameter	:: CONSTVAL=1
   integer, parameter	:: FROMFILE=2
!
! !BUGS
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the air-sea interaction module
!
! !INTERFACE:
   subroutine init_air_sea(namlst)
!
! !DESCRIPTION:
!  This routine initialises the Air-Sea module by reading various variables 
!  from a namelist and open relevant files.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)		:: namlst
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
   namelist /airsea/ calc_fluxes,		&
                     meteo_file,		&
                     heat_method,		&
                     const_qin,const_qout,	&
                     heatflux_file,		&
                     momentum_method,		&
                     const_tx,const_ty,		&
                     momentumflux_file,		&
                     p_e_method,p_e_flux_file, 	&
                     sst_method, sst_file,	&
                     sss_method, sss_file,	&
                     airt_method, airt_file
!
!EOP
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
            open(heat_unit,file=heatflux_file,action='read',status='old',err=93)
            LEVEL2 'Reading heat fluxes from:'
            LEVEL3 trim(heatflux_file)
         case default
      end select 

!     The momentum fluxes
      select case (momentum_method)
         case (FROMFILE)
            open(momentum_unit,file=momentumflux_file,action='read',status='old',err=94)
            LEVEL2 'Reading momentum fluxes from:'
            LEVEL3 trim(momentumflux_file)
         case default
      end select

!     The fresh water fluxes
      select case (p_e_method)
         case (FROMFILE)
            open(p_e_unit,file=p_e_flux_file,action='read',status='old',err=95)
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
! !IROUTINE: air_sea_interaction
!
! !INTERFACE:
   subroutine air_sea_interaction(jul,secs)
!
! !DESCRIPTION:
!  In the longer run this will be the routine to call to calculate and
!  obtain air/sea related variables. For now only simple things are done.
!  The short-wave radiation, heat and momentum fluxes are either read from
!  file or set to constant values.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)		:: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
!
!EOP
!-----------------------------------------------------------------------
!BOC


   if (calc_fluxes) then

#define TESTING
#undef TESTING
#ifdef TESTING
   wx  = 11.03
   wy  = 7.84
   w   = sqrt(wx*wx+wy*wy)
   airp=1013.*100 !kbk mbar/hPa ---> Pa
   rh=83.2
   cloud=0.88
   airt=8.47
   sst=5.
   call misc_variables()
   call do_calc_fluxes()
   STDERR heat,tx,ty
STOP 'TESTING'
#endif

      call flux_from_meteo(jul,secs)
      call short_wave_radiation(jul,secs)

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
! !IROUTINE: finish_air_sea_interaction
!
! !INTERFACE:
   subroutine finish_air_sea_interaction
!
! !DESCRIPTION:
!  Sofar only files are closed in this routine.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (calc_fluxes) then
      close(meteo_unit)
   else
      if (heat_method .eq. FROMFILE) close(heat_unit)
      if (momentum_method .eq. FROMFILE) close(momentum_unit)
      if (p_e_method .eq. FROMFILE) close(p_e_unit)
      if (sst_method .eq. FROMFILE) close(sst_unit)
      if (sss_method .eq. FROMFILE) close(sss_unit)
      if (airt_method .eq. FROMFILE) close(airt_unit)
   end if
   return
   end subroutine finish_air_sea_interaction
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: misc_variables()
!
! !INTERFACE:
   subroutine misc_variables()
!
! !DESCRIPTION:
!  Based on modelled SST the heatfluxes are culculated.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
   REALTYPE		:: tvirt,s,s0 
   REALTYPE		:: ae_h,be_h,ce_h,pe_h
   REALTYPE		:: ae_m,be_m,ce_m,pe_m
   REALTYPE, parameter	:: a1=6.107799961
   REALTYPE, parameter	:: a2=4.436518521e-1
   REALTYPE, parameter	:: a3=1.428945805e-2
   REALTYPE, parameter	:: a4=2.650648471e-4
   REALTYPE, parameter	:: a5=3.031240396e-6
   REALTYPE, parameter	:: a6=2.034080948e-8
   REALTYPE, parameter	:: a7=6.136820929e-11
   REALTYPE		:: x
   REALTYPE, parameter	:: eps=1.0e-12
!
!EOP
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
      ae_h=0.0;   be_h=1.23;   ce_h=0.0;      pe_h=-0.16;
      ae_m=0.0;   be_m=1.185;  ce_m=0.0;      pe_m=-0.157;
   else if (w .lt. 5.0) then
      ae_h=0.969; be_h=0.0521; ce_h=0.0;      pe_h=1.0;
      ae_m=0.927; be_m=0.0546; ce_m=0.0;      pe_m=1.0;
   else if (w .lt. 8.0) then
      ae_h=1.18;  be_h=0.01;   ce_h=0.0;      pe_h=1.0;
      ae_m=1.15;  be_m=0.01;   ce_m=0.0;      pe_m=1.0;
   else if (w .lt. 25.0) then
      ae_h=1.196; be_h=0.008;  ce_h=-0.0004;  pe_h=1.0
      ae_m=1.17;  be_m=0.0075; ce_m=-0.00044; pe_m=1.0;
   else
      ae_h=1.68;  be_h=-0.016; ce_h=0;        pe_h=1;
      ae_m=1.652; be_m=-0.017; ce_m=0.0;      pe_m=1.0;
   end if

   cee_heat=(ae_h+be_h*exp(pe_h*log(w+eps))+ce_h*(w-8.0)**2)*1.0e-3
   cee_mom =(ae_m+be_m*exp(pe_m*log(w+eps)))*1.0e-3

   if(s .lt. 0.) then
      x = 0.1+0.03*s+0.9*exp(4.8*s)
      ced_heat=x*cee_heat
      ced_mom =x*cee_mom
   else
      ced_heat=cee_heat*(1.0+0.63*sqrt(s))
      ced_mom =cee_mom *(1.0+0.47*sqrt(s))
   end if

   return
   end subroutine misc_variables
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculates the heat fluxes using SST from model
!
! !INTERFACE:
   subroutine do_calc_fluxes(heatf,taux,tauy)
!
! !DESCRIPTION:
!  Based on modelled SST the heatfluxes are culculated.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE, optional, intent(out)	:: heatf,taux,tauy
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
   REALTYPE		:: tmp
   REALTYPE		:: qe,qh,qb

   integer, parameter	:: clark=1	! Clark et. al, 1974
   integer, parameter	:: hastenrath=2	! Hastenrath and Lamb, 1978
   integer		:: back_radiation_method=clark
!
!EOP
!-----------------------------------------------------------------------
!BOC
   qe=ced_heat*L*rho_air*w*(qs-qa)			! latent
   qh=ced_heat*cpa*rho_air*w*(sst-airt)			! sensible

   tmp=sst+Kelvin
   select case(back_radiation_method)			! back radiation
      case(clark)
         qb=(1.0-.8*cloud*cloud)				&
            *emiss*bolz*(tmp**4)*(0.39-0.05*sqrt(ea/100.0))	&
            +4.0*emiss*bolz*(tmp**3)*(sst-airt)
      case(hastenrath) ! qa in g(water)/kg(wet air)
         qb=(1.0-.8*cloud*cloud)				&
            *emiss*bolz*(tmp**4)*(0.39-0.056*sqrt(1000*qa))		&
            +4.0*emiss*bolz*(tmp**3)*(sst-airt)
      case default
   end select

   if(present(heatf)) then
     heatf = -(qe+qh+qb)
   else
     heat = -(qe+qh+qb)
   end if

   tmp = -ced_mom*rho_air*w
!kbk
!kbk   tmp = 1.2*tmp
!kbk
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
! !IROUTINE: Calculates the short wave radiation
!
! !INTERFACE:
   subroutine short_wave_radiation(jul,secs,swr)
!
! !DESCRIPTION:
! Calculates the SW radiation - based on lat,lon,time,cloud and albedo. 
!
!  albedo monthly values from Payne (1972) as means of the values
!  at 40N and 30N for the Atlantic Ocean ( hence the same latitudinal
!  band of the Mediterranean Sea ) :
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)			:: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE, optional, intent(out)	:: swr
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
   REALTYPE	:: alat,alon
!kbk   integer	:: year_day
   REALTYPE	:: solar=1350.
   REALTYPE	:: eclips=23.439*deg2rad
   REALTYPE	:: tau=0.7
   REALTYPE	:: aozone=0.09
!kbk   REALTYPE	:: sunalpha=0.03

   REALTYPE	:: alb1(20) = (/.719,.656,.603,.480,.385,.300,.250,.193,.164, &
                      .131,.103,.084,.071,.061,.054,.039,.036,.032,.031,.030 /)

   REALTYPE	:: za(20)   = (/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,  &
                                66.,62.,58.,54.,50.,40.,30.,20.,10.,0.0 /)
   REALTYPE	:: dza(19)
   data            dza/8*2.0, 6*4.0, 5*10.0/

   integer	:: yday(12) = (/ 0,31,59,90,120,151,181,212,233,273,304,334 /)

   REALTYPE	:: th0,th02,th03,sundec
   REALTYPE	:: thsun,coszen,zen,dzen,sunbet
   REALTYPE	:: qatten,qzer,qdir,qdiff,qtot,qshort
   REALTYPE	:: albedo
   integer	:: jab
   integer	:: yy,mm,dd
   REALTYPE	:: yrdays,days,hour,tjul
!
!EOP
!-----------------------------------------------------------------------
!BOC
!  number of days in a year :
!kbk   days = 30.*float(imonth-1) + float(iday) -1.
   call calendar_date(jul,yy,mm,dd)
   days=float(yday(mm))+float(dd)
   hour=1.0*secs/3600.
!kbk   if (mod(yy,4) .eq. 0 ! leap year I forgot
   yrdays=365.

   alat = deg2rad*59.33333
   alon = deg2rad*1.283333

   th0 = 2.*pi*days/yrdays
   th02 = 2.*th0
   th03 = 3.*th0
!  sun declination :
   sundec = 0.006918 - 0.399912*cos(th0) + 0.070257*sin(th0)	&
           - 0.006758*cos(th02) + 0.000907*sin(th02)		&
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
!kbkSTDERR 'no cloud effect'
      qshort  = qtot
   else
      qshort  = qtot*(1-0.62*cloud + .0019*sunbet)*(1.-albedo)
   endif

!kbk
!kbk   qshort = 0.75*qshort
!kbk

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
! !IROUTINE: flux_from_meteo
!
! !INTERFACE:
   subroutine flux_from_meteo(jul,secs)
!
! !DESCRIPTION:
!  This routine will read meteo data from a file and do interpolation in
!  time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
   integer		:: yy,mm,dd,hh,min,ss
   REALTYPE		:: t
   REALTYPE, SAVE	:: dt
   integer, save        :: meteo_jul1,meteo_secs1
   integer, save	:: meteo_jul2=0,meteo_secs2=0
   REALTYPE, save	:: obs(6)
   REALTYPE, save	:: alpha(4)
   REALTYPE, save	:: I1,h1,tx1,ty1
   REALTYPE, save	:: I2=0.,h2=0.,tx2=0.,ty2=0.
   logical, save	:: first=.true.
   integer		:: rc
!
!EOP
!-----------------------------------------------------------------------
!BOC
!  This part initialise and read in new values if necessary.
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
      call misc_variables()
      if (first) then
         call do_calc_fluxes(heatf=h1,taux=tx1,tauy=ty1)
         call short_wave_radiation(jul,secs,swr=I1)
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
         call short_wave_radiation(jul,secs,swr=I2)
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
! !IROUTINE: read_heat_flux
!
! !INTERFACE:
   subroutine read_heat_flux(jul,secs,I_0,heat)
!
! !DESCRIPTION:
!  This routine will read heat-fluxes from a file and do interpolation in
!  time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out):: I_0,heat
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
   integer		:: yy,mm,dd,hh,min,ss
   REALTYPE		:: t,alpha
   REALTYPE, SAVE	:: dt
   integer, save        :: heat_jul1,heat_secs1
   integer, save	:: heat_jul2=0,heat_secs2=0
   REALTYPE, save	:: obs1(2),obs2(2)=0.
   integer		:: rc
!
!EOP
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
! !IROUTINE: read_momentum_flux
!
! !INTERFACE:
   subroutine read_momentum_flux(jul,secs,tx,ty)
!
! !DESCRIPTION:
!  This routine will read momentum-fluxes from a file and do interpolation in
!  time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,intent(in)	:: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE,intent(out)	:: tx,ty
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
   integer		:: yy,mm,dd,hh,min,ss
   REALTYPE		:: t,alpha
   REALTYPE, save	:: dt
   integer, save        :: mom_jul1,mom_secs1
   integer, save	:: mom_jul2=0,mom_secs2=0
   REALTYPE, save	:: obs1(2),obs2(2)=0.
   integer		:: rc
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
! !IROUTINE: read_sst
!
! !INTERFACE:
   subroutine read_sst(jul,secs,sst)
!
! !DESCRIPTION:
!  This routine will read the sea surface temperature from a file and 
!  do interpolation in time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE,intent(out)	:: sst
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
   integer		:: yy,mm,dd,hh,min,ss
   REALTYPE		:: t,alpha
   REALTYPE, save	:: dt
   integer, save        :: sst_jul1,sst_secs1
   integer, save	:: sst_jul2=0,sst_secs2=0
   REALTYPE, save	:: obs1(1),obs2(1)=0.
   integer		:: rc
!
!EOP
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
! !IROUTINE: Integrate shortwave and sea surface fluxes.
!
! !INTERFACE:
   subroutine integrated_fluxes(dt)
!
! !DESCRIPTION:
!  This routine integrates the short-wave radiation and heat-fluxes over time.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)	:: dt
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
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
! !IROUTINE: Set the SST from model.
!
! !INTERFACE:
   subroutine set_sst(temp)
!
! !DESCRIPTION:
!  This routine sets the sst to be used fro flux calculations.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)	:: temp
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  See airsea module
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   sst = temp
   return
   end subroutine set_sst
!EOC

   end module airsea

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard

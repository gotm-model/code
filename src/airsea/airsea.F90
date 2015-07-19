#include "cppdefs.h"
!#define INTERPOLATE_METEO 1
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: airsea --- atmospheric fluxes \label{sec:airsea}
!
! !INTERFACE:
   module airsea
!
! !DESCRIPTION:
!  This module calculates the heat, momentum
!  and freshwater fluxes between the ocean and the atmosphere as well as
!  the incoming solar radiation. Fluxes and solar radiation may be
!  prescribed. Alternatively, they may be calculated by means
!  of bulk formulae from observed or modelled meteorological
!  parameters and the solar radiation may be calculated
!  from longitude, latitude, time and cloudiness. 
!  Albedo correction is applied according to a namelist variable.
!  For the prescibed fluxes and solar radiation,
!  values may be constant or read in from files. All necessary
!  setting have to be made in the namelist file {\tt airsea.nml}.
!
! !USES:
   use airsea_variables
   use time,         only: julian_day, yearday, time_diff
   use input,        only: register_input_0d,read_obs
!
   IMPLICIT NONE

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public                              :: init_air_sea
   public                              :: do_air_sea
   public                              :: clean_air_sea
   public                              :: set_sst
   public                              :: set_ssuv
   public                              :: integrated_fluxes
#ifdef _PRINTSTATE_
   public                              :: print_state_airsea
#endif
!
! !PUBLIC DATA MEMBERS:
   logical,  public                    :: calc_fluxes
!
!  Meteorological forcing variables
   integer,  public                    :: hum_method
   REALTYPE, public, target            :: u10,v10
   REALTYPE, public, target            :: airp,airt
   REALTYPE, public, target            :: rh
   REALTYPE, public                    :: twet,tdew
   REALTYPE, public, target            :: cloud
!
!  wind speed (m/s)
   REALTYPE, public, target            :: w
!
!  surface short-wave radiation
!  and surface heat flux (W/m^2)
   REALTYPE, public, target            :: I_0,albedo
   REALTYPE, public, target            :: heat
   REALTYPE, public                    :: qe,qh,qb

!  surface stress components (Pa)
   REALTYPE, public, target            :: tx,ty

!  precipitation and  evaporation
!  (m/s)
   REALTYPE, public, target            :: precip
   REALTYPE, public, target            :: evap

!  sea surface temperature (degC), sea surface salinity (psu),
!  sea surface current components (m/s)
   REALTYPE, public                    :: sst
   REALTYPE, public, target            :: sst_obs
   REALTYPE, public, target            :: sss
   REALTYPE, public                    :: ssu
   REALTYPE, public                    :: ssv

!  integrated precipitationa and
!  evaporation + sum (m)
   REALTYPE, public                    :: int_precip,int_evap,int_fwf

!  integrated short-wave radiation,
!  surface heat flux (J/m^2)
   REALTYPE, public                    :: int_swr,int_heat

!  sum of short wave radiation
!  and surface heat flux (J/m^2)
   REALTYPE, public                    :: int_total
!
!  feedbacks to drag and albedo by biogeochemistry
   REALTYPE, target, public            :: bio_drag_scale,bio_albedo
!
! !DEFINED PARAMETERS:
#ifndef INTERPOLATE_METEO
   integer,  parameter                 :: meteo_unit=20
#endif
   integer,  parameter                 :: CONSTVAL=1
   integer,  parameter                 :: FROMFILE=2
!
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!
! !LOCAL VARIABLES:
#ifndef INTERPOLATE_METEO
   logical                   :: init_saved_vars
#endif
   integer, public           :: swr_method
   integer, public           :: albedo_method
   REALTYPE, public          :: const_albedo
   integer, public           :: fluxes_method
   integer, public           :: back_radiation_method
   integer                   :: heat_method
   integer                   :: momentum_method
   integer                   :: precip_method
   integer                   :: sst_method
   integer                   :: sss_method
   integer                   :: ssuv_method
   logical, public           :: rain_impact
   logical, public           :: calc_evaporation

   character(len=PATH_MAX)   :: meteo_file
   character(len=PATH_MAX)   :: swr_file
   character(len=PATH_MAX)   :: heatflux_file
   character(len=PATH_MAX)   :: momentumflux_file
   character(len=PATH_MAX)   :: precip_file
   character(len=PATH_MAX)   :: sss_file
   character(len=PATH_MAX)   :: sst_file

   REALTYPE                  :: wind_factor
   REALTYPE                  :: const_swr
   REALTYPE                  :: swr_factor
   REALTYPE                  :: const_heat
   REALTYPE                  :: const_tx,const_ty
   REALTYPE                  :: const_precip
   REALTYPE                  :: precip_factor
   REALTYPE                  :: dlon,dlat
!
! !BUGS:
!  Wind speed - w - is not entirely correct.
!  No temporal interpolation is done. If the momentum fluxes tx,ty are
!  specified w=0.
!  The Fairall and Kondo methods calculate their own w internally.
!  w is used by e.g. bio.F90
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the air--sea interaction module \label{sec:init-air-sea}
!
! !INTERFACE:
   subroutine init_air_sea(namlst,lat,lon)
!
! !DESCRIPTION:
!  This routine initialises the air-sea module by reading various variables
!  from the namelist {\tt airsea.nml} and opens relevant files.
!  These parameters are:
!
!  \begin{tabular}{ll}
! {\tt calc\_fluxes}     & {\tt .true.}: Sensible, latent and back-radiation are calculated by    \\
!                          & means of bulk formulae. In this case, {\tt meteo\_file} must be      \\
!                          & given and {\tt hum\_method} must be specified.                         \\
!                          & {\tt .false.}: Surface fluxes and solar radiation are prescribed.      \\
! {\tt fluxes\_method}   & Select which parameterisation to use for latent and sensible fluxes:   \\
!                          & 1: Kondo (1975)                                                        \\
!                          & 2: Fairall et al. (1996)                                               \\
! {\tt back\_radiation\_method}   & Select which parameterisation to use:                        \\
!                          & 1: Clark et al. (1974)                                                 \\
!                          & 2: Hastenrath and Lamb (1978)                                          \\
!                          & 3: Bignami et al. (1995)                                               \\
!                          & 4: Berliandand Berliand (1952)                                         \\
! {\tt meteo\_file}      & file with meteo data (for {\tt calc\_fluxes=.true.}) with            \\
!                          & date: {\tt yyyy-mm-dd hh:mm:ss}                                        \\
!                          & $x$-component of wind (10 m) in m\,s$^{-1}$                            \\
!                          & $y$-component of wind (10 m) in m\,s$^{-1}$                            \\
!                          & air pressure (2 m) in hectopascal                                      \\
!                          & dry air temperature (2 m) in Celsius                                   \\
!                          & rel. hum. in \% or wet bulb temp. in C or dew point temp. in C         \\
!                          & cloud cover in 1/10                                                    \\
!                          & Example:                                                               \\
!                          & {\tt 1998-01-01 00:00:00    6.87  10.95 1013.0   6.80   73.2   0.91}   \\
! {\tt hum\_method}        & 1: relative humidity in \% given as 7.\ column in {\tt meteo\_file}        \\
!                          & 2: wet bulb temperature in Celsius given as 7. column in {\tt meteo\_file}  \\
!                          & 3: dew point temperature in Celsius given as 7. column in {\tt meteo\_file} \\
!                          & 4: specific humidity in kg\,kg$^{-1}$ given as 7. column in {\tt meteo\_file} \\
! {\tt heat\_method}     & 0: heat flux not prescribed                                            \\
!                          & 1: constant value for short wave radiation ({\tt const\_swr})        \\
!                          &    and surface heat flux ({\tt const\_qh})                           \\
!                          & 2: {\tt swr}, {\tt heat} are read from {\tt heatflux\_file}          \\
! {\tt rain\_impact}     & {\tt .true.}: include sensible- and momentum-flux from precipitation   \\
! {\tt calc\_evaporation}& {\tt .true.}: calculate evaporation/condensation (m/s)                 \\
! {\tt swr\_method}      & 1: constant value for short wave radiation ({\tt const\_swr})        \\
!                          & 2: read short wave radiation from file                                 \\
!                          & 3: Solar radiation is calculated from time, longitude, latitude,       \\
!                          & and cloud cover.                                                       \\
! {\tt albedo\_method}    & 0=const, 1=Payne, 2=Cogley \\
! {\tt const\_albedo}     & used if {tt albedo\_method}=0 - must be <= 1.0    \\
! {\tt const\_swr}       & constant value for short wave radiation in W\,m$^{-2}$                 \\
!                          & (always positive)                                                      \\
! {\tt swr\_file}        & file with short wave radiation in W\,m$^{-2}$                          \\
! {\tt swr\_factor}      & scales data read from file to  W\,m$^{-2}$ - defaults to 1             \\
! {\tt const\_heat }     & constant value for surface heat flux in  W\,m$^{-2}$                   \\
!                          & (negative for heat loss)                                               \\
! {\tt heatflux\_file}   & file with date and {\tt heat} in W\,m$^{-2}$                           \\
! {\tt momentum\_method} & 0: momentum flux not prescribed                                        \\
!                          & 1: constant momentum fluxes {\tt const\_tx}, {\tt const\_tx} given \\
!                          & 2: surface momentum fluxes given from file {\tt momentumflux\_file}  \\
! {\tt const\_tx}        & $x$-component of constant surface momentum flux in N\,m$^{-2}$         \\
! {\tt const\_ty}        & $y$-component of constant surface momentum flux in N\,m$^{-2}$         \\
! {\tt momentumflux\_file} & File with date, {\tt tx} and {\tt ty} given                          \\
! {\tt precip\_method}   & 0: precipitation not included == precip=0.                             \\
!                          & 1: constant value for precipitation in  m\,s$^{-1}$                    \\
!                          & 2: values for precipitation read from file {\tt precip\_file}         \\
! {\tt const\_precip}    & value for precip in m\,s$^{-1}$                                        \\
! {\tt precip\_file}     & file with date and {\tt precip} in m\,s$^{-1}$                         \\
! {\tt precip\_factor}   & scales data read from file to  m\,s$^{-1}$ - defaults to 1             \\
! {\tt sst\_method}      & 0: no independent SST observation is read from file                    \\
!                          & 2: independent SST observation is read from file, only for output      \\
! {\tt sst\_file}        & file with date and SST (sea surface temperature) in Celsius            \\
! {\tt sss\_method}      & 0: no independent SSS observation is read from file                    \\
!                          & 2: independent SSS observation is read from file, only for output      \\
! {\tt sss\_file}        & file with date and SSS (sea surface salinity) in psu                   \\
!  \end{tabular}
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
!EOP
!
! !LOCAL VARIABLES:
   namelist /airsea/ calc_fluxes, &
                     fluxes_method, &
                     back_radiation_method, &
                     meteo_file, &
                     wind_factor, &
                     hum_method, &
                     heat_method, &
                     rain_impact, &
                     calc_evaporation, &
                     swr_method,albedo_method,const_albedo,const_swr,swr_file,swr_factor, &
                     const_heat, &
                     heatflux_file, &
                     momentum_method, &
                     const_tx,const_ty, &
                     momentumflux_file, &
                     precip_method,const_precip,precip_file,precip_factor,&
                     sst_method, sst_file, &
                     sss_method, sss_file, &
                     ssuv_method
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_air_sea'

#ifndef INTERPOLATE_METEO
!  Ensure that variables with the "save" attribute will be initialized later on.
   init_saved_vars = .true.
#endif

!  wind speed (m/s)
   w = _ZERO_

!  surface short-wave radiation and surface heat flux (W/m^2)
   I_0  = _ZERO_
   albedo = _ZERO_
   heat = _ZERO_

!  surface stress components (Pa)
   tx = _ZERO_
   ty = _ZERO_

!  precipitation and  evaporation (m/s)
   precip = _ZERO_
   evap   = _ZERO_

!  sea surface temperature (degC) and sea surface salinity (psu)
   sst     = _ZERO_
   sst_obs = _ZERO_
   sss     = _ZERO_

!  sea surface velocities (m/s)
   ssu = _ZERO_
   ssv = _ZERO_

!  cloud cover
   cloud = _ZERO_

!  relative humidity (various measures)
   twet = _ZERO_
   tdew = _ZERO_
   rh   = _ZERO_

!  air temperature
   airt = _ZERO_

!  u and v components of wind at 10 m
   u10 = _ZERO_
   v10 = _ZERO_

!  air pressure
   airp = _ZERO_

!  initialize additional variables defined in airsea_variables module
   es   = _ZERO_
   ea   = _ZERO_
   qs   = _ZERO_
   qa   = _ZERO_
   L    = _ZERO_
   rhoa = _ZERO_

!  Initialize feedbacks to drag and albedo from biogeochemistry
   bio_drag_scale = _ONE_
   bio_albedo     = _ZERO_

!  initialize integrated freshwater and heat fluxes
   int_precip= _ZERO_
   int_evap  = _ZERO_
   int_fwf   = _ZERO_
   int_swr   = _ZERO_
   int_heat  = _ZERO_
   int_total = _ZERO_

!  store provided longitude and latitude
   dlon = lon
   dlat = lat

!  initialize namelist variables to reasonable defaults.
   calc_fluxes=.false.
   ssuv_method=0
   fluxes_method=1
   back_radiation_method=1
   meteo_file = ''
   wind_factor=_ONE_
   hum_method = 0
   heat_method = 0
   rain_impact=.false.
   calc_evaporation=.false.
   swr_method=0
   albedo_method=1
   const_swr=_ZERO_
   swr_file = ''
   swr_factor=_ONE_
   const_heat = _ZERO_
   heatflux_file = ''
   momentum_method = 0
   const_tx = _ZERO_
   const_ty = _ZERO_
   momentumflux_file = ''
   precip_method=0
   const_precip=_ZERO_
   precip_file = ''
   precip_factor=_ONE_
   sst_method=0
   sst_file = ''
   sss_method=0
   sss_file = ''

!  Read namelist variables from file.
   open(namlst,file='airsea.nml',action='read',status='old',err=90)
   read(namlst,nml=airsea,err=91)
   close(namlst)

!  The short wave radiation
   select case (swr_method)
      case (CONSTVAL)
         LEVEL2 'Using constant short wave radiation= ',const_swr
         I_0 = const_swr
      case (FROMFILE)
         call register_input_0d(swr_file,1,I_0,'surface short wave radiation',scale_factor=swr_factor)
         LEVEL2 'Reading short wave radiation data from:'
         LEVEL3 trim(swr_file)
         if (swr_factor .ne. _ONE_) then
            LEVEL3 'applying swr factor= ',swr_factor
         end if
      case (3)
         if (.not. calc_fluxes) then
            LEVEL2 'Not possible to calculate swr - when calc_fluxes=.false.'
            stop 'init_airsea'
         else
            LEVEL2 'Calculating swr=swr(t(lon),lat,cloud)'
         end if
         LEVEL2 'Albedo method: ',albedo_method
      case default
   end select

   if (calc_fluxes) then

#ifndef INTERPOLATE_METEO
      open(meteo_unit,file=meteo_file,action='read',status='old',err=93)
#else
      call register_input_0d(meteo_file,1,u10,'wind speed: x-direction',scale_factor=wind_factor)
      call register_input_0d(meteo_file,2,v10,'wind speed: y-direction',scale_factor=wind_factor)
      call register_input_0d(meteo_file,3,airp,'air pressure',scale_factor=100.d0)
      call register_input_0d(meteo_file,4,airt,'air temperature')
      call register_input_0d(meteo_file,5,rh,'relative humidity')
      call register_input_0d(meteo_file,6,cloud,'cloud cover')
#endif
      LEVEL2 'Air-sea exchanges will be calculated'
      LEVEL2 'Reading meteo data from:'
      LEVEL3 trim(meteo_file)
      if (wind_factor .ne. _ONE_) then
         LEVEL3 'applying wind factor= ',wind_factor
      end if

      LEVEL3 'heat- and momentum-fluxes:'
      select case (fluxes_method)
         case(1)
            LEVEL4 'using Kondo formulation'
         case(2)
            LEVEL4 'using Fairall et. all formulation'
         case default
      end select
      LEVEL3 'long-wave back radiation:'
      select case (back_radiation_method)
         case(1)
            LEVEL4 'using Clark formulation'
         case(2)
            LEVEL4 'using Hastenrath formulation'
         case(3)
            LEVEL4 'using Bignami formulation'
         case(4)
            LEVEL4 'using Berliand formulation'
         case default
      end select

   else

!     The heat fluxes
      select case (heat_method)
         case (CONSTVAL)
            heat = const_heat
         case (FROMFILE)
            call register_input_0d(heatflux_file,1,heat,'surface heat flux')
            LEVEL2 'Reading heat fluxes from:'
            LEVEL3 trim(heatflux_file)
         case default
      end select

!     The momentum fluxes
      select case (momentum_method)
         case (CONSTVAL)
            tx = const_tx
            ty = const_ty
         case (FROMFILE)
            call register_input_0d(momentumflux_file,1,tx,'surface momentum flux: x-direction')
            call register_input_0d(momentumflux_file,2,ty,'surface momentum flux: y-direction')
            LEVEL2 'Reading momentum fluxes from:'
            LEVEL3 trim(momentumflux_file)
         case default
      end select

   end if

!  The fresh water fluxes (used for calc_fluxes=.true. and calc_fluxes=.false.)
   select case (precip_method)
      case (CONSTVAL)
         LEVEL2 'Using constant precipitation= ',const_precip
         LEVEL2 'rain_impact=      ',rain_impact
         LEVEL2 'calc_evaporation= ',calc_evaporation
         precip = const_precip
      case (FROMFILE)
         call register_input_0d(precip_file,1,precip,'precipitation',scale_factor=precip_factor)
         LEVEL2 'Reading precipitation data from:'
         LEVEL3 trim(precip_file)
         if (precip_factor .ne. _ONE_) then
            LEVEL3 'applying factor= ',precip_factor
         end if
         LEVEL2 'rain_impact=      ',rain_impact
         LEVEL2 'calc_evaporation= ',calc_evaporation
      case default
   end select

!  The observed sea surface temperature
   select case (sst_method)
      case (FROMFILE)
         call register_input_0d(sst_file,1,sst_obs,'sea surface temperature')
         LEVEL2 'Reading sea surface temperature from:'
         LEVEL3 trim(sst_file)
      case default
   end select

!  The observed sea surface salinity
   select case (sss_method)
      case (FROMFILE)
         call register_input_0d(sss_file,1,sss,'sea surface salinity')
         LEVEL2 'Reading sea surface salinity from:'
         LEVEL3 trim(sss_file)
      case default
   end select

   return

90 FATAL 'I could not open airsea.nml'
   stop 'init_airsea'
91 FATAL 'I could not read airsea namelist'
   stop 'init_airsea'
93 FATAL 'I could not open ',trim(meteo_file)
   stop 'init_airsea'

   end subroutine init_air_sea
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Obtain the air--sea fluxes
!
! !INTERFACE:
   subroutine do_air_sea(jul,secs)
!
! !DESCRIPTION:
!
!  Depending on the value of the boolean variable {\tt calc\_fluxes},
!  the subroutines for the calculation of the fluxes
!  and the short wave radiation are
!  called or the fluxes are directly read in from the namelist
!  {\tt airsea.nml} as constants or read in from files.
!  Furthermore, the surface freshwater flux is set to a constant
!  value or is read in from a file.
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
! !LOCAL VARIABLES:
   REALTYPE        :: hh
   REALTYPE        :: solar_zenith_angle
   REALTYPE        :: short_wave_radiation
   REALTYPE        :: zenith_angle
   REALTYPE        :: albedo_water
   logical         :: have_zenith_angle
!EOP
!-----------------------------------------------------------------------
!BOC

   if (calc_fluxes) then
!     Calculate bulk fluxes from meteorological conditions and surface state (sst,ssu,ssv).
      call flux_from_meteo(jul,secs)

!     Optionally calculate surface shortwave radiation from location, time, cloud cover.
      have_zenith_angle = .false.
      if (swr_method .eq. 3) then
         hh = secs*(_ONE_/3600)
         zenith_angle = solar_zenith_angle(yearday,hh,dlon,dlat)
         have_zenith_angle = .true.
         I_0 = swr_factor*short_wave_radiation(zenith_angle,yearday,dlon,dlat,cloud)
      end if
   else
!     If using constant momentum flux, apply time-varying feedback from biogeochemistry to drag.
      if (momentum_method==CONSTVAL) then
         tx = const_tx*bio_drag_scale
         ty = const_ty*bio_drag_scale
      end if

   end if

   if (swr_method .ne. CONSTVAL) then
      if (.not. have_zenith_angle) then
         hh = secs*(_ONE_/3600)
         zenith_angle = solar_zenith_angle(yearday,hh,dlon,dlat)
      end if
      albedo = albedo_water(albedo_method,zenith_angle,yearday)
      I_0 = I_0*(_ONE_-albedo-bio_albedo)
   end if


!  If reading SST from file, overwrite current (model) SST with observed value,
!  to be used in output.
   if (sst_method==FROMFILE) sst = sst_obs

#ifndef INTERPOLATE_METEO
   if (init_saved_vars) init_saved_vars = .false.
#endif

   end subroutine do_air_sea
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the air--sea interactions
!
! !INTERFACE:
   subroutine clean_air_sea
!
! !DESCRIPTION:
!  All files related to air-sea interaction which have been opened
!  are now closed by this routine.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC

#ifndef INTERPOLATE_METEO
   if (calc_fluxes) close(meteo_unit)
#endif

   end subroutine clean_air_sea
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
!  For {\tt calc\_fluxes=.true.}, this routine reads meteo data
!  from {\tt meteo\_file} and calculates for each time step
!  in  {\tt meteo\_file} the
!  fluxes of heat and momentum, and the
!  long-wave back radiation by calling the routines {\tt humidity},
!  {\tt back\_radiation} and {\tt airsea\_fluxes}, see sections
!  \sect{sec:humidity}, \sect{sec:back-rad}, and \sect{sec:airsea-fluxes},
!  a wrapper routine for using the bulk fomulae from either \cite{Kondo75}
!  or \cite{Fairalletal96}. Afterwards, the airsea fluxes
!  are interpolated to the actual time step of GOTM. Finally, the
!  incoming short-wave radiation is calculated by using the interpolated
!  cloud cover and the actual UTC time of GOTM, see the routine
!  {\tt short\_wave\_radiation} described in \sect{sec:swr}.

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
!EOP
!
! !LOCAL VARIABLES:
#ifndef INTERPOLATE_METEO
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t
   REALTYPE, SAVE            :: dt
   integer, save             :: meteo_jul1,meteo_secs1
   integer, save             :: meteo_jul2,meteo_secs2
   REALTYPE, save            :: obs(7)
   REALTYPE, save            :: alpha(5)
   REALTYPE, save            :: h1,tx1,ty1,cloud1
   REALTYPE, save            :: h2,tx2,ty2,cloud2
   integer                   :: rc
#endif
   REALTYPE                  :: ta_k,tw,tw_k
!
!-----------------------------------------------------------------------
!BOC
#ifndef INTERPOLATE_METEO
   if (init_saved_vars) then
      meteo_jul2  = 0
      meteo_secs2 = 0
      h2     = _ZERO_
      tx2    = _ZERO_
      ty2    = _ZERO_
      cloud2 = _ZERO_
   end if
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
      u10   = obs(1)*wind_factor
      v10   = obs(2)*wind_factor
      airp  = obs(3)*100. !kbk mbar/hPa --> Pa
      airt  = obs(4)
      rh    = obs(5)
      cloud = obs(6)

      if (sst .lt. 100.) then
         tw  = sst
         tw_k= sst+KELVIN
      else
         tw  = sst-KELVIN
         tw_k= sst
      end if

      if (airt .lt. 100.) then
         ta_k  = airt + KELVIN
         ta = airt
      else
         ta  = airt - KELVIN
         ta_k = airt
      end if

      h1     = h2
      tx1    = tx2
      ty1    = ty2
      cloud1 = cloud2

      call humidity(hum_method,rh,airp,tw,ta)
      call back_radiation(back_radiation_method, &
                          dlat,tw_k,ta_k,cloud,qb)
#if 0
      call airsea_fluxes(fluxes_method,rain_impact,calc_evaporation, &
                         tw,ta,u10-ssu,v10-ssv,precip,evap,tx2,ty2,qe,qh)
#else
      call airsea_fluxes(fluxes_method, &
                         tw,ta,u10-ssu,v10-ssv,precip,evap,tx2,ty2,qe,qh)
#endif
      h2     = qb+qe+qh
      cloud2 = cloud

      if (init_saved_vars) then
         h1     = h2
         tx1    = tx2
         ty1    = ty2
         cloud1 = cloud2
      end if

      dt = time_diff(meteo_jul2,meteo_secs2,meteo_jul1,meteo_secs1)
      alpha(2) = (h2-h1)/dt
      alpha(3) = (tx2-tx1)/dt
      alpha(4) = (ty2-ty1)/dt
      alpha(5) = (cloud2-cloud1)/dt
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,meteo_jul1,meteo_secs1)
   heat  = h1  + t*alpha(2)
   tx    = tx1 + t*alpha(3)
   ty    = ty1 + t*alpha(4)
   cloud = cloud1 + t*alpha(5)
#else
   if (sst .lt. 100.) then
      tw  = sst
      tw_k= sst+KELVIN
   else
      tw  = sst-KELVIN
      tw_k= sst
   end if

   if (airt .lt. 100.) then
      ta_k  = airt + KELVIN
      ta = airt
   else
      ta  = airt - KELVIN
      ta_k = airt
   end if

   call humidity(hum_method,rh,airp,tw,ta)
   call back_radiation(back_radiation_method, &
                       dlat,tw_k,ta_k,cloud,qb)
   call airsea_fluxes(fluxes_method, &
                      tw,ta,u10-ssu,v10-ssv,precip,evap,tx,ty,qe,qh)
   heat = (qb+qe+qh)
#endif

   w = sqrt((u10-ssu)*(u10-ssu)+(v10-ssv)*(v10-ssv))

   tx = tx*bio_drag_scale
   ty = ty*bio_drag_scale

   end subroutine flux_from_meteo
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
!EOP
!-----------------------------------------------------------------------
!BOC
   int_precip= int_precip + dt*precip
   int_evap  = int_evap   + dt*evap
   int_fwf   = int_precip + int_evap
   int_swr   = int_swr    + dt*I_0
   int_heat  = int_heat   + dt*heat
   int_total = int_swr    + int_heat
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
!  This routine sets the simulated
!  sea surface temperature (SST) to be used for
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
!EOP
!-----------------------------------------------------------------------
!BOC
   sst = temp
   return
   end subroutine set_sst
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set the surface velocities to be used from model.
!
! !INTERFACE:
   subroutine set_ssuv(uvel,vvel)
!
! !DESCRIPTION:
!  This routine sets the simulated
!  sea surface velocities to be used for
!  the surface flux calculations.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: uvel,vvel
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (ssuv_method.ne.0) then
      ssu = uvel
      ssv = vvel
   end if

   end subroutine set_ssuv
!EOC

#ifdef _PRINTSTATE_
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Print the current state of the air--sea interaction module.
!
! !INTERFACE:
   subroutine print_state_airsea()
!
! !DESCRIPTION:
!  This routine writes the value of all module-level variables to screen.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'State of airsea module:'
   LEVEL2 'calc_fluxes',calc_fluxes
   LEVEL2 'w',w
   LEVEL2 'I_0',I_0
   LEVEL2 'heat',heat
   LEVEL2 'tx,ty',tx,ty
   LEVEL2 'precip,evap',precip,evap
   LEVEL2 'sst,sst_obs,sss',sst,sst_obs,sss
   LEVEL2 'ssu,ssv',ssu,ssv
   LEVEL2 'int_swr,int_heat,int_total',int_swr,int_heat,int_total
   LEVEL2 'cloud',cloud

   LEVEL2 'swr_method',swr_method
   LEVEL2 'fluxes_method',fluxes_method
   LEVEL2 'back_radiation_method',back_radiation_method
   LEVEL2 'heat_method',heat_method
   LEVEL2 'momentum_method',momentum_method
   LEVEL2 'precip_method',precip_method
   LEVEL2 'sst_method',sst_method
   LEVEL2 'sss_method',sss_method
   LEVEL2 'ssuv_method',ssuv_method
   LEVEL2 'hum_method',hum_method
   LEVEL2 'rain_impact',rain_impact
   LEVEL2 'calc_evaporation',calc_evaporation

   LEVEL2 'meteo_file',meteo_file
   LEVEL2 'swr_file',swr_file
   LEVEL2 'heatflux_file',heatflux_file
   LEVEL2 'momentumflux_file',momentumflux_file
   LEVEL2 'precip_file',precip_file
   LEVEL2 'sss_file',sss_file
   LEVEL2 'sst_file',sst_file

   LEVEL2 'u10,v10',u10,v10
   LEVEL2 'airp',airp
   LEVEL2 'airt,twet,tdew',airt,twet,tdew
   LEVEL2 'rh',rh

   LEVEL2 'const_swr',const_swr
   LEVEL2 'swr_factor',swr_factor
   LEVEL2 'const_heat',const_heat
   LEVEL2 'const_tx,const_ty',const_tx,const_ty
   LEVEL2 'const_precip',const_precip
   LEVEL2 'precip_factor',precip_factor
   LEVEL2 'dlon,dlat',dlon,dlat

   LEVEL2 'es,ea,qs,qa,L,rhoa',es,ea,qs,qa,L,rhoa

   end subroutine print_state_airsea
!EOC
#endif

!-----------------------------------------------------------------------

   end module airsea

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

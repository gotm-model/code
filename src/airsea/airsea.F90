#include "cppdefs.h"

! The preprocessor macro INTERPOLATE_METEO defined below activates temporal interpolation
! of meteo forcing variables (wind speed, air temperature, etc.) The alternative to this
! is interpolation of heat/momentum fluxes: if INTERPOLATE_METEO is not defined,
! GOTM will compute the heat/momentum fluxes for each time in the meteo file, and
! interpolate those fluxes in time instead. However, the fluxes will in that case be
! based on a sea state that lags one meteo time step behind the meteo data - GOTM
! must read one meteo time step ahead to enable interpolation, and will compute fluxes
! at the time of the read. This lagged sea state can cause problems, particularly if
! the meteo time step is large. Hence, we recommend interpolation of the meteo data.
#define INTERPOLATE_METEO 1

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: airsea --- atmospheric fluxes \label{sec:airsea}
!
! !INTERFACE:
   module airsea_driver
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
   use input,        only: read_obs, type_scalar_input, method_unsupported, register_input
!
   IMPLICIT NONE

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public                              :: init_airsea, post_init_airsea
   public                              :: do_airsea
   public                              :: clean_airsea
   public                              :: set_sst
   public                              :: set_ssuv
   public                              :: surface_fluxes
   public                              :: integrated_fluxes
#ifdef _PRINTSTATE_
   public                              :: print_state_airsea
#endif

   interface init_airsea
      module procedure init_airsea_nml
      module procedure init_airsea_yaml
   end interface

!
! !PUBLIC DATA MEMBERS:
!
!  Meteorological forcing variables
   integer,  public                    :: hum_method
   character(len=PATH_MAX)   :: meteo_file
   type (type_scalar_input), public, target            :: u10,v10
   type (type_scalar_input), public, target            :: airp,airt
   type (type_scalar_input), public, target            :: hum
   type (type_scalar_input), public, target            :: cloud
!
!  wind speed (m/s)
   REALTYPE, public, target            :: w
!
!  surface shortwave radiation
!  and surface heat flux (W/m^2)
   type (type_scalar_input), public, target :: I_0, ql
   REALTYPE, public, target            :: albedo
   type (type_scalar_input), public, target            :: heat
   REALTYPE, public                    :: qe,qh

!  surface stress components (Pa)
   REALTYPE, public, target                 :: tx,ty
   type (type_scalar_input), public, target :: tx_,ty_

!  precipitation and  evaporation
!  (m/s)
   type (type_scalar_input), public, target            :: precip
   REALTYPE, public, target            :: evap

!  sea surface temperature (degC), sea surface salinity (psu),
!  sea surface current components (m/s)
   REALTYPE, public                    :: sst
   type (type_scalar_input), public, target            :: sst_obs
   type (type_scalar_input), public, target            :: sss
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
   integer, public           :: albedo_method
   REALTYPE, public          :: const_albedo
   integer, public           :: fluxes_method
   integer                   :: ssuv_method

   REALTYPE                  :: dlon,dlat
   integer                   :: mjul,msecs
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
   subroutine init_airsea_nml(namlst)
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
!                          & 5: Josey et al. (2003) - 1                                         \\
!                          & 6: Josey et al. (2003) - 2                                         \\
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
! {\tt shf\_factor}      & scales surface heat fluxes - defaults to 1                             \\
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
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
   logical                   :: calc_fluxes
   integer :: swr_method
   REALTYPE                  :: const_swr
   REALTYPE                  :: swr_factor
   character(len=PATH_MAX)   :: swr_file

   integer                   :: momentum_method
   integer                   :: precip_method
   integer                   :: sst_method
   integer                   :: sss_method
   character(len=PATH_MAX)   :: back_radiation_file
   character(len=PATH_MAX)   :: heatflux_file
   character(len=PATH_MAX)   :: momentumflux_file
   character(len=PATH_MAX)   :: precip_file
   character(len=PATH_MAX)   :: sss_file
   character(len=PATH_MAX)   :: sst_file

   REALTYPE                  :: wind_factor
   REALTYPE                  :: shf_factor
   REALTYPE                  :: const_heat
   REALTYPE                  :: const_tx,const_ty
   REALTYPE                  :: const_precip
   REALTYPE                  :: precip_factor
   integer                   :: back_radiation_method
   integer                   :: heat_method

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
                     back_radiation_file, &
                     shf_factor,const_heat, &
                     heatflux_file, &
                     momentum_method, &
                     const_tx,const_ty, &
                     momentumflux_file, &
                     precip_method,const_precip,precip_file,precip_factor,&
                     sst_method, sst_file, &
                     sss_method, sss_file, &
                     ssuv_method
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_airsea_nml'

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
   back_radiation_file = ''
   shf_factor=_ONE_
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
   open(10,file='airsea.nml',action='read',status='old',err=90)
   read(10,nml=airsea,err=91)
   close(10)

   call I_0%configure(method=swr_method, path=swr_file, index=1, constant_value=const_swr, scale_factor=swr_factor)

   call u10%configure(method=2, path=meteo_file, index=1, scale_factor=wind_factor)
   call v10%configure(method=2, path=meteo_file, index=2, scale_factor=wind_factor)
   call airp%configure(method=2, path=meteo_file, index=3, scale_factor=100.d0)
   call airt%configure(method=2, path=meteo_file, index=4)
   call hum%configure(method=2, path=meteo_file, index=5)
   call cloud%configure(method=2, path=meteo_file, index=6)

   call tx_%configure(method=momentum_method, path=momentumflux_file, index=1, constant_value=const_tx)
   call ty_%configure(method=momentum_method, path=momentumflux_file, index=2, constant_value=const_ty)
   call heat%configure(method=heat_method, path=heatflux_file, index=1, scale_factor=shf_factor, constant_value=const_heat)
   call ql%configure(method=back_radiation_method, path=back_radiation_file, index=1)
   call sst_obs%configure(method=sst_method, path=sst_file, index=1)
   call sss%configure(method=sss_method, path=sss_file, index=1)
   call precip%configure(method=precip_method, path=precip_file, index=1, scale_factor=precip_factor, constant_value=const_precip)

   if (.not. calc_fluxes) fluxes_method = 0

   LEVEL2 'done'
   return

90 FATAL 'I could not open airsea.nml'
   stop 'init_airsea'
91 FATAL 'I could not read airsea namelist'
   stop 'init_airsea'
93 FATAL 'I could not open ',trim(meteo_file)
   stop 'init_airsea'

   end subroutine init_airsea_nml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the air--sea interaction module \label{sec:init-air-sea}
!
! !INTERFACE:
   subroutine init_airsea_yaml()
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
!                          & 5: Josey et al. (2003) - 1                                         \\
!                          & 6: Josey et al. (2003) - 2                                         \\
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
! {\tt shf\_factor}      & scales surface heat fluxes - defaults to 1                             \\
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
   use settings
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
   class (type_gotm_settings), pointer :: branch, twig, leaf
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_airsea_yaml'

   branch => settings_store%get_typed_child('surface')

   twig => branch%get_typed_child('fluxes', 'heat and momentum fluxes')
   call twig%get(fluxes_method, 'method', 'method to calculate fluxes from meteorology', &
                options=(/option(0, 'use prescribed fluxes'), option(1, 'Kondo (1975)'), option(2, 'Fairall et al. (1996)')/), default=0)
   call twig%get(heat, 'heat', 'prescribed total heat flux (sensible, latent and net back-radiation)', 'W/m^2', &
                default=0._rk)
   call twig%get(tx_, 'tx', 'prescribed momentum flux in West-East direction', 'Pa', &
                default=0._rk)
   call twig%get(ty_, 'ty', 'prescribed momentum flux in South-North direction', 'Pa', &
                default=0._rk)
   
   twig => branch%get_typed_child('meteo')
   call twig%get(u10, 'u10', 'wind speed in West-East direction @ 10 m', 'm/s', &
                default=0._rk)
   call twig%get(v10, 'v10', 'wind speed in South-North direction @ 10 m', 'm/s', &
                default=0._rk)
   call twig%get(airp, 'airp', 'air pressure', 'Pa', &
                default=0._rk)
   call twig%get(airt, 'airt', 'air temperature @ 2 m', 'Celsius or K', &
                default=0._rk)
   call twig%get(hum, 'hum', 'humidity @ 2 m', '', &
                default=0._rk, pchild=leaf)
   call leaf%get(hum_method, 'type', 'humidity metric', &
                options=(/option(1, 'relative humidity (%)'), option(2, 'wet-bulb temperature'), &
                option(3, 'dew point temperature'), option(4 ,'specific humidity (kg/kg)')/), default=1)
   call twig%get(cloud, 'cloud', 'cloud cover', '1', &
                minimum=0._rk, maximum=1._rk, default=0._rk)
   call twig%get(I_0, 'swr', 'shortwave radiation', 'W/m^2', &
                minimum=0._rk,default=0._rk, extra_options=(/option(3, 'from time, location and cloud cover')/))
   call twig%get(precip, 'precip', 'precipitation', 'm/s', &
                default=0._rk, pchild=leaf)
   call leaf%get(rain_impact, 'flux_impact', 'include effect on fluxes of sensible heat and momentum', &
                default=.false.)
   call twig%get(calc_evaporation, 'calc_evaporation', 'calculate evaporation from meteorological conditions', &
                default=.false.)
   call twig%get(ssuv_method, 'ssuv_method', 'wind treatment', &
                options=(/option(0, 'use absolute wind speed'), option(1, 'use wind speed relative to current velocity')/), default=1, display=display_advanced)

   call branch%get(ql, 'longwave_radiation', 'net longwave radiation', 'W/m^2', &
                default=0._rk, method_file=0, method_constant=method_unsupported, &
               extra_options=(/option(1, 'Clark'), option(2, 'Hastenrath'), option(3, 'Bignami'), option(4, 'Berliand'), option(5, 'Josey-1'), option(6, 'Josey-2')/), default_method=1)

   twig => branch%get_typed_child('albedo')
   call twig%get(albedo_method, 'method', 'method to compute albedo', &
                options=(/option(0, 'constant'), option(1, 'Payne (1972)'), option(2, 'Cogley (1979)')/), default=1)
   call twig%get(const_albedo, 'constant_value', 'constant value to use throughout the simulation', '1', &
                minimum=0._rk,maximum=1._rk,default=0._rk)

   call branch%get(sst_obs, 'sst', 'observed surface temperature', 'Celsius', &
                default=0._rk, display=display_advanced)
   call branch%get(sss, 'sss', 'observed surface salinity', 'psu', &
                default=0._rk, display=display_advanced)
   LEVEL2 'done'
   return
   end subroutine init_airsea_yaml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the air--sea interaction module \label{sec:init-air-sea}
!
! !INTERFACE:
   subroutine post_init_airsea(lat,lon)
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
! {\tt shf\_factor}      & scales surface heat fluxes - defaults to 1                             \\
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
   REALTYPE, intent(in)                :: lat,lon
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'post_init_airsea'

#ifndef INTERPOLATE_METEO
!  Ensure that variables with the "save" attribute will be initialized later on.
   init_saved_vars = .true.
#endif

!  wind speed (m/s)
   w = _ZERO_

!  surface short-wave radiation and surface heat flux (W/m^2)
   albedo = _ZERO_

!  precipitation and  evaporation (m/s)
   evap   = _ZERO_

!  sea surface temperature (degC) and sea surface salinity (psu)
   sst     = _ZERO_

!  sea surface velocities (m/s)
   ssu = _ZERO_
   ssv = _ZERO_

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

!  The short wave radiation
   select case (I_0%method)
      case (3)
         if (fluxes_method == 0) then
            LEVEL2 'Not possible to calculate swr if heat and momentum fluxes are prescribed'
            stop 'init_airsea'
         else
            LEVEL2 'Calculating swr=swr(t(lon),lat,cloud)'
         end if
         LEVEL2 'Albedo method: ',albedo_method
      case default
         call register_input(I_0)
   end select

   if (fluxes_method /= 0) then

#ifndef INTERPOLATE_METEO
      open(meteo_unit,file=meteo_file,action='read',status='old',err=93)
#else
      call register_input(u10)
      call register_input(v10)
      call register_input(airp)
      call register_input(airt)
      call register_input(hum)
      call register_input(cloud)
#endif
      LEVEL2 'Air-sea exchanges will be calculated'

      LEVEL3 'heat- and momentum-fluxes:'
      select case (fluxes_method)
         case(1)
            LEVEL4 'using Kondo formulation'
         case(2)
            LEVEL4 'using Fairall et. all formulation'
         case default
      end select
      LEVEL3 'net longwave radiation:'
      select case (ql%method)
         case(0) ! Read from file instead of calculating
            call register_input(ql)
         case(1)
            LEVEL4 'using Clark formulation'
         case(2)
            LEVEL4 'using Hastenrath formulation'
         case(3)
            LEVEL4 'using Bignami formulation'
         case(4)
            LEVEL4 'using Berliand formulation'
         case(5)
            LEVEL4 'using Josey-1 formulation'
         case(6)
            LEVEL4 'using Josey-2 formulation'
         case default
      end select

   else

!     The heat fluxes
      call register_input(heat)

!     The momentum fluxes
      call register_input(tx_)
      call register_input(ty_)

   end if

!  The fresh water fluxes (used with prescribed and calculated fluxes of heat/momentum)
   call register_input(precip)
   LEVEL2 'rain_impact=      ',rain_impact
   LEVEL2 'calc_evaporation= ',calc_evaporation

!  The observed sea surface temperature
   call register_input(sst_obs)

!  The observed sea surface salinity
   call register_input(sss)
   LEVEL2 'done'
   return

93 FATAL 'I could not open ',trim(meteo_file)
   stop 'init_airsea'

   end subroutine post_init_airsea
!EOC

!-----------------------------------------------------------------------
!BOP
   subroutine surface_fluxes(surface_temp,sensible,latent,longwave_radiation)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: surface_temp
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: sensible,latent,longwave_radiation
!
!EOP
!-----------------------------------------------------------------------
!BOC
   call set_sst(surface_temp)
   call flux_from_meteo(mjul,msecs)
   sensible = qh
   latent = qe
#if 1
   if (qe .lt. _ZERO_) then
      STDERR 'Stefan# ',qh/qe
   end if
#endif
   longwave_radiation = ql%value
   return
   end subroutine surface_fluxes
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Obtain the air--sea fluxes
!
! !INTERFACE:
   subroutine do_airsea(jul,secs)
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
   REALTYPE        :: shortwave_radiation
   REALTYPE        :: zenith_angle
   REALTYPE        :: albedo_water
   logical         :: have_zenith_angle
!EOP
!-----------------------------------------------------------------------
!BOC

   have_zenith_angle = .false.
   if (fluxes_method /= 0) then
!     Calculate bulk fluxes from meteorological conditions and surface state (sst,ssu,ssv).
      call flux_from_meteo(jul,secs)

!     Optionally calculate surface shortwave radiation from location, time, cloud cover.
      if (I_0%method .eq. 3) then
         hh = secs*(_ONE_/3600)
         zenith_angle = solar_zenith_angle(yearday,hh,dlon,dlat)
         have_zenith_angle = .true.
         I_0%value = I_0%scale_factor*shortwave_radiation(zenith_angle,yearday,dlon,dlat,cloud%value)
      end if
      heat%value = heat%scale_factor*heat%value
   end if

   if (I_0%method .ne. CONSTVAL) then
      if (.not. have_zenith_angle) then
         hh = secs*(_ONE_/3600)
         zenith_angle = solar_zenith_angle(yearday,hh,dlon,dlat)
      end if
      if (albedo_method .eq. 0) then
         albedo = const_albedo
      else
         albedo = albedo_water(albedo_method,zenith_angle,yearday)
      end if
   end if

   tx = tx_%value*bio_drag_scale
   ty = ty_%value*bio_drag_scale

!  If reading SST from file, overwrite current (model) SST with observed value,
!  to be used in output.
   if (sst_obs%method==FROMFILE) sst = sst_obs%value

#ifndef INTERPOLATE_METEO
   if (init_saved_vars) init_saved_vars = .false.
#endif

   end subroutine do_airsea
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the air--sea interactions
!
! !INTERFACE:
   subroutine clean_airsea
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
   if (fluxes_method /= 0) close(meteo_unit)
#endif

   end subroutine clean_airsea
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
!  fluxes of heat and momentum, and the net
!  longwave radiation by calling the routines {\tt humidity},
!  {\tt back\_radiation} and {\tt airsea\_fluxes}, see sections
!  \sect{sec:humidity}, \sect{sec:back-rad}, and \sect{sec:airsea-fluxes},
!  a wrapper routine for using the bulk fomulae from either \cite{Kondo75}
!  or \cite{Fairalletal96a}. Afterwards, the airsea fluxes
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
   integer, save             :: line
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
      line = 0
   end if
!  This part initialises and reads in new values if necessary.
   if(time_diff(meteo_jul2,meteo_secs2,jul,secs) .lt. 0) then
      do
         meteo_jul1 = meteo_jul2
         meteo_secs1 = meteo_secs2
         call read_obs(meteo_unit,yy,mm,dd,hh,min,ss,6,obs,rc,line=line)
         if (rc>0) then
            FATAL 'Error reading time series from '//trim(meteo_file)//' at line ',line
            stop 'flux_from_meteo'
         elseif (rc<0) then
            FATAL 'End of file reached while attempting to read new data from '//trim(meteo_file)//'. Does this file span the entire simulated period?'
            stop 'flux_from_meteo'
         end if
         call julian_day(yy,mm,dd,meteo_jul2)
         meteo_secs2 = hh*3600 + min*60 + ss
         if(time_diff(meteo_jul2,meteo_secs2,jul,secs) .gt. 0) EXIT
      end do
      u10%value   = obs(1)*u10%scale_factor
      v10%value   = obs(2)*v10%scale_factor
      airp%value  = obs(3)*100. !kbk mbar/hPa --> Pa
      airt%value  = obs(4)
      hum%value   = obs(5)
      cloud%value = obs(6)

      if (sst .lt. 100.) then
         tw  = sst
         tw_k= sst+KELVIN
      else
         tw  = sst-KELVIN
         tw_k= sst
      end if

      if (airt%value .lt. 100.) then
         ta_k  = airt%value + KELVIN
         ta = airt%value
      else
         ta  = airt%value - KELVIN
         ta_k = airt%value
      end if

      h1     = h2
      tx1    = tx2
      ty1    = ty2
      cloud1 = cloud2

      call humidity(hum_method,hum,airp,tw,ta)
      if (ql%method .gt. 0) then
         call longwave_radiation(ql%method, &
                                 dlat,tw_k,ta_k,cloud,ql)
      end if
#if 0
      call airsea_fluxes(fluxes_method,rain_impact,calc_evaporation, &
                         tw,ta,u10%value-ssu,v10%value-ssv,precip%value,evap,tx2,ty2,qe,qh)
#else
      call airsea_fluxes(fluxes_method, &
                         tw,ta,u10%value-ssu,v10%value-ssv,precip%value,evap,tx2,ty2,qe,qh)
#endif
      h2     = ql%value+qe+qh
      cloud2 = cloud%value

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
   heat%value  = h1  + t*alpha(2)
   tx_%value    = tx1 + t*alpha(3)
   ty_%value    = ty1 + t*alpha(4)
   cloud%value = cloud1 + t*alpha(5)
#else
   if (sst .lt. 100.) then
      tw  = sst
      tw_k= sst+KELVIN
   else
      tw  = sst-KELVIN
      tw_k= sst
   end if

   if (airt%value .lt. 100.) then
      ta_k  = airt%value + KELVIN
      ta = airt%value
   else
      ta  = airt%value - KELVIN
      ta_k = airt%value
   end if

   call humidity(hum_method,hum%value,airp%value,tw,ta)
   if (ql%method .gt. 0) then
      call longwave_radiation(ql%method, &
                          dlat,tw_k,ta_k,cloud%value,ql%value)
   endif
   call airsea_fluxes(fluxes_method, &
                      tw,ta,u10%value-ssu,v10%value-ssv,precip%value,evap,tx_%value,ty_%value,qe,qh)
   heat%value = (ql%value+qe+qh)
#endif

   w = sqrt((u10%value-ssu)*(u10%value-ssu)+(v10%value-ssv)*(v10%value-ssv))

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
   int_precip= int_precip + dt*precip%value
   int_evap  = int_evap   + dt*evap
   int_fwf   = int_precip + int_evap
   int_swr   = int_swr    + dt*I_0%value
   int_heat  = int_heat   + dt*heat%value
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
   LEVEL2 'longwave_radiation_method',longwave_radiation_method
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
   LEVEL2 'airt',airt
   LEVEL2 'hum',hum

   LEVEL2 'const_swr',const_swr
   LEVEL2 'swr_factor',swr_factor
   LEVEL2 'shf_factor',shf_factor
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

   end module airsea_driver

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

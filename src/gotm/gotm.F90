!$Id: gotm.F90,v 1.11 2003-09-16 12:17:10 hb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: gotm --- the general framework \label{sec:gotm}
!
! !INTERFACE:
   module gotm
!
! !DESCRIPTION:
! This is 'where it all happens'. This module provides the internal
! routines {\tt init\_gotm()} to initialise the whole model and 
! {\tt time\_loop()} to manage the time-stepping of all fields. These 
! two routines in turn call more specialised routines e.g.\ of the 
! {\tt meanflow} and {\tt turbulence} modules to delegate the job.
!
!  Here is also the place for a few words on Fortran `units' we used. 
!  The method of Fotran units is quite rigid and also a bit dangerous,
!  but lacking a better alternative we adopted it here. This requires
!  the definition of ranges of units for different purposes. In GOTM
!  we strongly suggest to use units according to the following
!  conventions.
!  \begin{itemize}
!     \item unit=10 is reserved for reading namelists.
!     \item units 20-29 are reserved for the {\tt airsea} module.
!     \item units 30-39 are reserved for the {\tt meanflow} module.
!     \item units 40-49 are reserved for the {\tt turbulence} module.
!     \item units 50-59 are reserved for the {\tt output} module.
!     \item units 60-69 are reserved for the {\tt extra} modules 
!           like those dealing with sediments or sea--grass.
!     \item units 70- are \emph{not} reserved and can be used as you 
!           wish.
!  \end{itemize}
!
! !USES:
   use airsea, only: init_air_sea,air_sea_interaction
   use airsea, only: set_sst,integrated_fluxes
   use airsea, only: calc_fluxes
   use airsea, only: tx,ty,I_0,heat
   use meanflow
   use turbulence
   use observations
   use output
   use time
   use bio
   use mtridiagonal
   use eqstate

#ifdef SEDIMENT
   use sediment
#endif
#ifdef SEAGRASS
   use seagrass
#endif
#ifdef BIO
   use bio
#endif
!
   IMPLICIT NONE
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm, time_loop, clean_up
!
! !DEFINED PARAMETERS:
   integer, parameter                  :: namlst=10
#ifdef SEDIMENT
   integer, parameter                  :: unit_sediment=61
#endif
#ifdef SEAGRASS
   integer, parameter                  :: unit_seagrass=62
#endif
#ifdef BIO
   integer, parameter                  :: unit_bio=63
#endif
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: gotm.F90,v $
!  Revision 1.11  2003-09-16 12:17:10  hb
!  added new biological model - bio_iow
!
!  Revision 1.10  2003/07/23 12:14:07  hb
!  preparing for general bio interface
!
!  Revision 1.9  2003/04/04 14:25:52  hb
!  First iteration of four-compartment geobiochemical model implemented
!
!  Revision 1.8  2003/04/01 17:01:00  hb
!  Added infrastructure for geobiochemical model
!
!  Revision 1.7  2003/03/28 09:20:34  kbk
!  added new copyright to files
!
!  Revision 1.6  2003/03/28 09:11:30  kbk
!  removed tabs
!
!  Revision 1.5  2003/03/10 09:20:27  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!  Revision 1.3  2001/11/18 15:58:02  gotm
!  Vertical grid can now be read from file
!
!  Revision 1.2  2001/06/13 07:40:39  gotm
!  Lon, lat was hardcoded in meteo.F90 - now passed via init_meteo()
!
!  Revision 1.1.1.1  2001/02/12 15:55:59  gotm
!  initial import into CVS
!
!EOP
!
!  private data members initialised via namelists
   character(len=80)         :: title
   integer                   :: nlev
   REALTYPE                  :: dt
   REALTYPE                  :: cnpar
   integer                   :: buoy_method
!  station description
   character(len=80)         :: name
   REALTYPE                  :: latitude,longitude
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the model \label{initGOTM}
!
! !INTERFACE:
   subroutine init_gotm()
!
! !DESCRIPTION:
!  This internal routine triggers the initialization of the model.
!  The first section reads the namelists of {\tt gotmrun.inp} with
!  the user specifications. Then, one by one each of the modules are
!  initialised with help of more specialised routines like 
!  {\tt init\_meanflow()} or {\tt init\_turbulence()} defined inside 
!  their modules, respectively.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See log for the gotm module
!
!EOP
!
! !LOCAL VARIABLES:
   namelist /model_setup/ title,nlev,dt,cnpar,buoy_method
   namelist /station/     name,latitude,longitude,depth
   namelist /time/        timefmt,MaxN,start,stop
   namelist /output/      out_fmt,out_dir,out_fn,nsave,variances, &
                          diagnostics,mld_method,diff_k,Ri_crit,rad_corr
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm'
   STDERR LINE

!  open the namelist file.
   LEVEL2 'reading model setup namelists..'
   open(namlst,file='gotmrun.inp',status='old',action='read',err=90)
   read(namlst,nml=model_setup,err=91)
   timestep = dt ! timestep comes from the time module.
   read(namlst,nml=station,err=92)
   read(namlst,nml=time,err=93)
   read(namlst,nml=output,err=94)
   depth0=depth
   LEVEL2 'done.'

   LEVEL2 trim(title)
   LEVEL2 'Using ',nlev,' layers to resolve a depth of',depth
   LEVEL2 'The station ',trim(name),' is situated at (lat,long) ', &
           latitude,longitude
   LEVEL2 trim(name)

   LEVEL2 'initializing modules....'
   call init_time(MinN,MaxN)
   call init_eqstate(namlst)
   close (namlst)

!  From here - each init_? is responsible for opening and closing the
!  namlst - unit.
   call init_meanflow(namlst,'gotmmean.inp',nlev,latitude)
   call init_tridiagonal(nlev) 
   call updategrid(nlev,dt,zeta)
   call init_turbulence(namlst,'gotmturb.inp',nlev)
   call init_observations(namlst,'obs.inp',julianday,secondsofday, &
                          depth,nlev,z,h)
   s = sprof
   t = tprof
   u = uprof
   v = vprof
   call init_output(title,nlev,latitude,longitude)
   call init_air_sea(namlst,latitude,longitude)

!  Initialise each of the extra features/modules
#ifdef SEDIMENT
   call init_sediment(namlst,'sediment.inp',unit_sediment,nlev, &
                      gravity,rho_0)
#endif
#ifdef SEAGRASS
   call init_seagrass(namlst,'seagrass.inp',unit_seagrass,nlev,h)
#endif
#ifdef BIO
   call init_bio(namlst,'bio.inp',unit_bio,nlev)
#endif
   LEVEL2 'done.'
   STDERR LINE

   return

90 FATAL 'I could not open gotmrun.inp for reading'
   stop 'init_gotm'
91 FATAL 'I could not read the "model_setup" namelist'
   stop 'init_gotm'
92 FATAL 'I could not read the "station" namelist'
   stop 'init_gotm'
93 FATAL 'I could not read the "time" namelist'
   stop 'init_gotm'
94 FATAL 'I could not read the "output" namelist'
   stop 'init_gotm'
95 FATAL 'I could not read the "eqstate" namelist'
   stop 'init_gotm'
96 FATAL 'I could not read the "turbulence" namelist'
   stop 'init_gotm'
   end subroutine init_gotm
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Manage global time--stepping \label{timeLoop}
!
! !INTERFACE:
   subroutine time_loop()
!
! !DESCRIPTION:
! This internal routine is the heart of the code. It contains
! the main time--loop inside of which all routines required 
! during the time step are called. The following main processes are 
! successively triggered.
! \begin{enumerate}
!  \item The model time is updated and the output is prepared.
!  \item Air--sea interactions (flux, SST) are computed.
!  \item The time step is performed on the mean-flow equations
!        (momentum, temperature).
!  \item Some quantities related to shear and stratification are updated 
!        (shear--number, buoyancy frequency, etc).
!  \item Turbulence is updated depending on what turbulence closure
!        model has been specified by the user.
!  \item The results are written to the output files.
! \end{enumerate}
!
! Depending on macros set for the Fortran pre--processor, extra features
! like the effects of sea--grass or sediments are considered in this routine 
! (see \sect{sec:extra}).
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See log for the gotm module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: n
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'time_loop'

   do n=MinN,MaxN

      call update_time(n)
      call prepare_output(n)

!     all observations/data
      call get_all_obs(julianday,secondsofday,nlev,z)

!     external forcing
      if( calc_fluxes ) then
         call set_sst(T(nlev))
      end if
      call air_sea_interaction(julianday,secondsofday)
      tx = tx/rho_0
      ty = ty/rho_0

!     meanflow integration starts
      call updategrid(nlev,dt,zeta)
      call coriolis(nlev,dt)
      SS = 0.
      call uequation(nlev,dt,cnpar,tx,num,PressMethod)
      call vequation(nlev,dt,cnpar,ty,num,PressMethod)
      call extpressure(PressMethod,nlev)
      if (int_press_method .ne. 0) call intpressure(nlev)
      call friction(kappa,avmolu,tx,ty)
#ifdef SEAGRASS
      call calc_seagrass(nlev,dt)
#endif
      if (s_prof_method .ne. 0.) call salinity(nlev,dt,cnpar,nuh)
      if (t_prof_method .ne. 0.) &
         call temperature(nlev,dt,cnpar,I_0,heat,nuh,rad)
      call stratification(nlev,buoy_method,dt,cnpar,gravity,rho_0,nuh)
#ifdef SEDIMENT
      call calc_sediment(nlev,dt)
#endif
#ifdef BIO
      call do_bio(nlev,I_0,dt,h,t,nuh,rad,bioshade)
#endif
      select case (turb_method)
         case (0)
            call convectiveadjustment(nlev,num,nuh,const_num,const_nuh,&
                                      buoy_method,gravity,rho_0)
         case (1)
            STDERR '... turb_method=1 is not coded yet.'
            STDERR 'Choose  turb_method=0 or turb_method=2.'
            STDERR 'Program execution stopped ...'
            stop 'time_loop'
         case (2)
            call production(nlev,alpha,num,nuh)
            call stabilityfunctions(nlev,NN,SS,1)
            call do_tke(nlev,dt,u_taus,u_taub,z0s,z0b,h,SS,NN,P,B)
            call lengthscale(nlev,dt,z0b,z0s,u_taus,u_taub,depth,h,NN,P,B)
            call turbulence_adv(nlev,dt,h)
            call kolpran(nlev)
         case default
      end select

      call internal_wave(nlev,NN,SS)

      call do_output(n,nlev)

      call integrated_fluxes(dt)

!kbk - next version      call heat_content()

!     write out variances
      if(variances) then
         call do_variances(nlev)
      end if

!     Diagnostic output
      if(diagnostics) then
         call do_diagnostics(n,nlev,buoy_method,dt, &
                             u_taus,u_taub,I_0,heat)
      end if
   end do
   STDERR LINE

   return
   end subroutine time_loop
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: The run is over --- now clean up.
!
! !INTERFACE:
   subroutine clean_up()
!
! !DESCRIPTION:
! This function is just a wrapper for the external routine 
! {\tt close\_output()} discussed in \sect{sec:output}. All open files
! will be closed after this call.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See log for the gotm module
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'clean_up'

   call close_output()

   return
   end subroutine clean_up
!EOC

!-----------------------------------------------------------------------

   end module gotm

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

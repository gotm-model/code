!$Id: gotm.F90,v 1.1 2001-02-12 15:55:59 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: Initialize, integrate and finish GOTM
!
! !INTERFACE:
   module gotm
!
! !DESCRIPTION: 
!  This is 'where it all happens'. This module provides routines for
!  initializing, integrating and finishing \em{GOTM}.
!  Units in Fortran is really anoying - since you have to specify a unit.
!  We have chosen the following method - which is far from optimal - and which
!  we might change in the future when we have a better alternative. 
!  \begin{itemize}
!     \item unit=10 is reserved for reading namelists.
!     \item units 20-29 is reserved for the \em{airsea} module.
!     \item units 30-39 is reserved for the \em{meanflow} module.
!     \item units 40-49 is reserved for the \em{turbulence} module.
!     \item units 50-59 is reserved for the \em{output} module.
!     \item units 60-69 is reserved for the extra modules and will be passed 
!           from this module - see SEDIMENT or SEAGRASS for an example.
!     \item units 70- is \em{not} reserved and can be used as you wish.
!  \end{itemize}
!  The right solution is ofcourse that you ask the system to give you 
!  a file handle and the system keeps track of resources - like in C and
!  also used in \em{NetCDF}.
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
   use mtridiagonal
   use eqstate
!  Additional modules/features are to be added here.
#ifdef SEDIMENT
   use sediment
#endif
#ifdef SEAGRASS
   use seagrass
#endif
   IMPLICIT NONE
!
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm, time_loop, clean_up
!
! !PUBLIC DATA MEMBERS:
!  These are initialized via namelists.
!  General model setup - integration related.
   character(len=80)		:: title
   integer			:: nlev
   REALTYPE			:: dt
   REALTYPE			:: cnpar
   integer			:: buoy_method
   
!  Station description.
   character(len=80)		:: name
   REALTYPE			:: latitude,longitude
!
! !PRIVATE DATA MEMBERS:
   integer, parameter		:: namlst=10
#ifdef SEDIMENT
   integer, parameter		:: unit_sediment=61
#endif
#ifdef SEAGRASS
   integer, parameter		:: unit_seagrass=62
#endif
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: gotm.F90,v $
!  Revision 1.1  2001-02-12 15:55:59  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
!
! !BUGS
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize GOTM. 
!
! !INTERFACE:
   subroutine init_gotm()
!
! !DESCRIPTION:
!  Initializes the various modules - reading namelists and calling the init-
!  routines for the different modules.
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
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See gotm module
!
! !LOCAL VARIABLES:
   namelist /model_setup/ title,nlev,dt,cnpar,buoy_method
   namelist /station/ 	name,latitude,longitude,depth
   namelist /time/ 	timefmt,MaxN,start,stop
   namelist /output/ 	out_fmt,out_dir,out_fn,nsave,variances,	&
                        diagnostics,mld_method,diff_k,Ri_crit,rad_corr
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_gotm'
   STDERR LINE
!  Open the namelist file.
   LEVEL2 'reading model setup namelists..'
   open(namlst,file='gotmrun.inp',status='old',action='read',err=90)
   read(namlst,nml=model_setup,err=91)
   timestep = dt ! timestep comes from the time module.
   read(namlst,nml=station,err=92)
   read(namlst,nml=time,err=93)
   read(namlst,nml=output,err=94)
   LEVEL2 'done.'

   LEVEL2 trim(title)
   LEVEL2 'Using ',nlev,' layers to resolve a depth of',depth
   LEVEL2 'The station ',trim(name),' is situated at (lat,long) ',latitude,longitude
   LEVEL2 trim(name)

   LEVEL2 'initializing modules....'
   call init_time(MinN,MaxN)
   call init_eqstate(namlst)
   close (namlst)

!  From here - each init_? is responsible for opening and closing the
!  namlst - unit.
   call init_meanflow(namlst,'gotmmean.inp',nlev,latitude)
   call updategrid(nlev,ddu,ddl,zeta)
   call init_turbulence(namlst,'gotmturb.inp',nlev)
   call init_observations(namlst,'obs.inp',julianday,secondsofday,depth,nlev,z,h)
   s = sprof
   t = tprof
   u = uprof
   v = vprof
   call init_tridiagonal(nlev)
   call init_output(title,nlev,latitude,longitude)
   call init_air_sea(namlst)

!  Initialize each of the extra features/modules
#ifdef SEDIMENT
   call init_sediment(namlst,'sediment.inp',unit_sediment,nlev,gravity,rho_0)
#endif
#ifdef SEAGRASS
   call init_seagrass(namlst,'seagrass.inp',unit_seagrass,nlev,h)
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
! !IROUTINE: The actual model integration. 
!
! !INTERFACE:
   subroutine time_loop()
!
! !DESCRIPTION:
!  Please actual integration is done in this routine. The relevant 
!  time-dependent routines are called in a do-loop.
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
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See gotm module
!
! !LOCAL VARIABLES:
   integer		:: n
!
!EOP
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
      call updategrid(nlev,ddu,ddl,zeta)
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
      call light_absorbtion(nlev,I_0)
      if (s_prof_method .ne. 0.) call salinity(nlev,dt,cnpar,nuh)
      if (t_prof_method .ne. 0.) call temperature(nlev,dt,cnpar,I_0,heat,nuh,rad)
      call stratification(nlev,buoy_method,dt,gravity,rho_0,nuh)
#ifdef SEDIMENT
      call calc_sediment(nlev,dt)
#endif
      select case (turb_method)
         case (0)
            call convectiveadjustment(nlev,num,nuh,const_num,const_nuh, &
                                      buoy_method,gravity,rho_0)
         case (1)
            STDERR '... turb_method=1 is not coded yet.'
            STDERR 'Choose  turb_method=0 or turb_method=2.'
            STDERR 'Program execution stopped ...'
            stop 'time_loop'
         case (2)
            call production(nlev,alpha,num,nuh)
            call stabilityfunctions(nlev,NN,SS)
            call do_tke(nlev,dt,u_taus,u_taub,z0s,h,NN,SS,P,B)
            call lengthscale(nlev,dt,z0b,z0s,u_taus,u_taub,depth,h,NN,P,B)
            call kolpran(nlev,u_taub,u_taus,z0b,z0s)
         case default
      end select

      call internal_wave(nlev,NN,SS)

      call do_output(n,nlev)

      call integrated_fluxes(dt)

!kbk - next version      call heat_content() 

!     Write out variances
      if(variances) then
         call do_variances(nlev)
      end if

!     Diagnostic output
      if(diagnostics) then
         call do_diagnostics(n,nlev,buoy_method,dt,u_taus,u_taub,I_0,heat)
      end if

   end do
   STDERR LINE

   return
   end subroutine time_loop 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: The run is over - now clean up.
!
! !INTERFACE:
   subroutine clean_up()
!
! !DESCRIPTION:
!  We have finished integration - and closes all open files.
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
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See gotm module
!
! !LOCAL VARIABLES:
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
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard.
!-----------------------------------------------------------------------

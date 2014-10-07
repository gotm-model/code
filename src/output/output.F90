#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: output --- saving the results
!
! !INTERFACE:
   module output
!
! !DESCRIPTION:
!  This module acts as an interface between GOTM and modules/routines
!  doing the actual output. In order to add a new output format it is only
!  necessary to add hooks in this module and write the actual output
!  routines. It is not necessary to change anything in GOTM itself.
!
! !USES:
   use time, ONLY: write_time_string,julianday,secondsofday,timestep,timestepkind
   use asciiout
#ifdef NETCDF_FMT
   use ncdfout, ONLY:  init_ncdf,do_ncdf_out,close_ncdf
#endif

   IMPLICIT NONE
!
! !PUBLIC DATA MEMBERS:
   logical                             :: write_results
   integer, public, parameter          :: ascii_unit=50
   integer, public, parameter          :: grads_unit=51
   character(len=19)                   :: ts
   integer                             :: out_fmt
   character(len=PATH_MAX)             :: out_dir
   character(len=PATH_MAX)             :: out_fn
   integer                             :: nfirst,nsave
   integer                             :: sync_out
   logical                             :: diagnostics
   integer                             :: mld_method
   REALTYPE                            :: diff_k
   REALTYPE                            :: Ri_crit
   logical                             :: rad_corr
   logical                             :: init_diagnostics

!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!
!EOP
!-----------------------------------------------------------------------
!
! !PRIVATE DATA MEMBERS:
!  Used for diagnostic output
   integer, private, parameter         :: temp_unit=54
   integer, private, parameter         :: mld_unit=55
   integer, private, parameter         :: sf_unit=56
   integer, private, parameter         :: fric_unit=57
   integer, private, parameter         :: heat_unit=58
   integer, private, parameter         :: energy_unit=59
   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize the output module
!
! !INTERFACE:
   subroutine init_output(title,nlev,latitude,longitude)
!
! !DESCRIPTION:
!  Calls the initialization routine based on output format selected by
!  the user.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   character(len=*), intent(in)        :: title
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: latitude,longitude
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   character(len=PATH_MAX)   :: ext,fname,tmp_fn
!
!-------------------------------------------------------------------------
!BOC
   LEVEL1 'init_output'

   call write_time_string(julianday,secondsofday,ts)

   select case (out_fmt)
      case (ASCII)
         EXT = 'out'
         fname = TRIM(out_dir) //'/'// TRIM(out_fn) // '.' // ext
         LEVEL2 'Output in ASCII:'
         LEVEL2 TRIM(fname)
         call init_ascii(fname,title,ascii_unit)
      case (NETCDF)
#ifdef NETCDF_FMT
         EXT = 'nc'
         fname = TRIM(out_dir) //'/'// TRIM(out_fn) // '.' // ext
         LEVEL2 'Output in NetCDF (time unit is set to seconds):'
         LEVEL2 TRIM(fname)
         call init_ncdf(fname,title,latitude,longitude,nlev,ts,0)
      case (GRADS)
         EXT = 'nc'
         fname = TRIM(out_dir) //'/'// TRIM(out_fn) // '.' // ext
         LEVEL2 'Output in GrADS specific NetCDF (time unit is hours):'
         call init_ncdf(fname,title,latitude,longitude,nlev,ts,2)
#else
         FATAL 'You have selected NetCDF (or GrADS) output.'
         FATAL 'It is not supported in this version of GOTM.'
         FATAL 'You have to download a version which support NetCDF from www.gotm.net'
         FATAL 'Thank you for using GOTM - The GOTM team.'
         stop 'init_output'
#endif
      case default
        LEVEL1 'Fatal error: A non valid output format has been chosen'
        stop 'init_output'
   end select

   if (diagnostics) then
      tmp_fn = TRIM(out_dir) //'/'// TRIM(out_fn) // '.temp'
      open(temp_unit,file=trim(tmp_fn),err=100,status='unknown')
      tmp_fn = TRIM(out_dir) //'/'// TRIM(out_fn) // '.mld'
      open(mld_unit,file=trim(tmp_fn),err=100,status='unknown')
      tmp_fn = TRIM(out_dir) //'/'// TRIM(out_fn) // '.sf'
      open(sf_unit,file=trim(tmp_fn),err=100,status='unknown')
      tmp_fn = TRIM(out_dir) //'/'// TRIM(out_fn) // '.fric'
      open(fric_unit,file=trim(tmp_fn),err=100,status='unknown')
      tmp_fn = TRIM(out_dir) //'/'// TRIM(out_fn) // '.heat'
      open(heat_unit,file=trim(tmp_fn),err=100,status='unknown')
      tmp_fn = TRIM(out_dir) //'/'// TRIM(out_fn) // '.energy'
      open(energy_unit,file=trim(tmp_fn),err=100,status='unknown')
      init_diagnostics = .true.
   end if
   return
100 FATAL 'unable to open ',trim(tmp_fn),' for output.'
   stop

   end subroutine init_output
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set some variables related to output
!
! !INTERFACE:
   subroutine prepare_output(n)
!
! !DESCRIPTION:
!  This routine check whether output should  be written at
!  the current time step. If this is the case, the model
!  time is written to a string for display on the screen.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer(kind=timestepkind), intent(in) :: n
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-------------------------------------------------------------------------
!BOC
   write_results = n .ge. nfirst .and. mod(n-nfirst,int(nsave,kind=timestepkind)).eq.0
   if (write_results) call write_time_string(julianday,secondsofday,ts)

   end subroutine prepare_output
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Save the model results in file
!
! !INTERFACE:
   subroutine do_output(n,nlev)
!
! !DESCRIPTION:
!  Calls the routine, which will do the actual storing of results, depending
!  on the output format.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer(kind=timestepkind), intent(in) :: n
   integer, intent(in)                    :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   REALTYPE                   :: secs
   logical                    :: sync
!EOP
!-------------------------------------------------------------------------
!BOC
   LEVEL2 'Saving....',ts
   secs = n*timestep
   select case (out_fmt)
      case (ASCII)
         call do_ascii_out(nlev,ts,ascii_unit)
#ifdef NETCDF_FMT
      case (NETCDF, GRADS)
         if (sync_out .ne. 0 .and. mod(n,int(nsave*sync_out,kind=timestepkind)) .eq. 0) then
            sync = .true.
         else
            sync = .false.
         end if
         call do_ncdf_out(nlev,secs,sync)
#endif
      case default
         LEVEL1 'Fatal error: A non valid output format has been chosen'
         stop 'do_output'
   end select
   end subroutine do_output
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Close files used for saving model results
!
! !INTERFACE:
   subroutine close_output()
!
! !DESCRIPTION:
!  Call routines for closing any open output files.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-------------------------------------------------------------------------
!BOC
   select case (out_fmt)
      case (ASCII)
         call close_ascii(ascii_unit)
#ifdef NETCDF_FMT
      case (NETCDF, GRADS)
         call close_ncdf()
#endif
      case default
        LEVEL1 'Fatal error: A non valid output format has been chosen'
        stop 'do_output'
   end select

   if(diagnostics) then
      close(temp_unit); close(mld_unit); close(sf_unit);
      close(fric_unit); close(heat_unit); close(energy_unit)
   end if

   return
   end subroutine close_output
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute various diagnostic/integrated variables
!
! !INTERFACE:
   subroutine do_diagnostics(n,nlev,BuoyMeth,dt,u_taus,u_taub,I_0,heat)
!
! !DESCRIPTION:
!  This subroutine calculates the following diagnostic/integrated variables.
!
! !USES:
   use airsea,       only: sst
   use meanflow,     only: gravity,rho_0,cp
   use meanflow,     only: h,u,v,s,t,NN,SS,buoy,rad
   use turbulence,   only: kappa
   use turbulence,   only: tke
   use observations, only: tprof,b_obs_sbf
   use eqstate,      only: eqstate1
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer(kind=timestepkind), intent(in) :: n
   integer, intent(in)                    :: nlev,BuoyMeth
   REALTYPE, intent(in)                   :: dt
   REALTYPE, intent(in)                   :: u_taus,u_taub
   REALTYPE, intent(in)                   :: I_0,heat
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: mld_surf,mld_bott
   REALTYPE                  :: ekin,epot,eturb
   REALTYPE, save            :: epot0
   REALTYPE                  :: heat_sim,heat_obs
   REALTYPE, save            :: heat_sim0,heat_obs0,heat_flux
   REALTYPE                  :: z,dtt,dtb,x
   REALTYPE                  :: wstar,tstar
   REALTYPE                  :: sbf,stf,MOL
   REALTYPE                  :: Ri(0:nlev)
!
!-----------------------------------------------------------------------
!BOC
   select case(mld_method)
      case(1)          ! MLD according to TKE criterium
         mld_surf    = 0.0
         i=nlev
100      i=i-1
         mld_surf=mld_surf+h(i+1)
         if ((tke(i) .gt. diff_k) .and. (i .gt. 0)) goto 100
         mld_bott    = 0.0
         i=0
101      i=i+1
         mld_bott=mld_bott+h(i)
         if ((tke(i) .gt. diff_k) .and. (i .lt. nlev)) goto 101
      case(2)          ! MLD according to critical Ri number
         do i=1,nlev-1
            Ri(i)=NN(i)/(SS(i)+1.e-10)
         end do
         mld_surf    = 0.0
         i=nlev
200      i=i-1
         mld_surf=mld_surf+h(i+1)
         if ((Ri(i) .lt. Ri_crit) .and. (i .gt. 0)) goto 200
         mld_bott    = 0.0
         i=0
201      i=i+1
         mld_bott=mld_bott+h(i)
         if ((Ri(i) .lt. Ri_crit) .and. (i .lt. nlev)) goto 201
      case default
   end select

!  Here, the surface buoyancy flux (sbf) and the surface temperature
!  flux (stf) are calculated.

   if (BuoyMeth .eq. 1) then
      dtt=0.01
      x=0.5*h(nlev)/10.
      dTb=(eqstate1(S(nlev),T(nlev)+0.5*dtt,x,gravity,rho_0)         &
          -eqstate1(S(nlev),T(nlev)-0.5*dtt,x,gravity,rho_0))/dtt
      sbf=-heat/cp/rho_0*dTb
!     Correction of surface buoyancy and temperature flux
!     for solar radiation penetration
      if (rad_corr) then
         z=0.
         i=nlev+1
444      i=i-1
         z=z+h(i)
         if (z.lt.mld_surf) goto 444
         sbf=sbf-dTb*(rad(nlev)-rad(i))
                                    !Using dTb such as here is not fully
                                    !correct, but we assume that the
                                    !thermal expansion coefficient does not
                                    !vary significantly over the mixed layer.
      end if
      stf=sbf/dTb
   else
      sbf=b_obs_sbf
      stf=0.
   end if
   if (sbf.ge.0) then
      wstar=(sbf*mld_surf)**(1./3.)
   else
      wstar=-(-sbf*mld_surf)**(1./3.)
   end if
   if (wstar.eq.0) then
      TStar=1.e15
   else
      tstar=stf/wstar
   end if
!  Calculation of Monin-Obukhov length MOL:
   if (abs(sbf).lt.1.e-10) then
      if (sbf.ge.0) MOL=-1.e10
      if (sbf.lt.0) MOL= 1.e10
   else
      MOL=-u_taus**3/kappa/sbf
   end if

   heat_sim=0
   heat_obs=0
   do i=1,nlev
      heat_sim=heat_sim+T(i)*h(i)*rho_0*cp
      heat_obs=heat_obs+tprof(i)*h(i)*rho_0*cp
   end do
   if (init_diagnostics) then
      heat_sim0=heat_sim
      heat_obs0=heat_obs
      heat_flux=0.
   end if
   heat_sim=heat_sim-heat_sim0
   heat_obs=heat_obs-heat_obs0

   heat_flux=heat_flux+dt*(I_0+heat)

   ekin=0.
   epot=0.
   eturb=0.
   z=0.
   do i=1,nlev
      z=z-0.5*h(i)
      ekin=ekin+0.5*h(i)*(u(i)**2+v(i)**2)
      eturb=eturb+h(i)*(tke(i)+tke(i-1))
      epot=epot+h(i)*buoy(i)*z
      z=z-0.5*h(i)
   end do
   if (init_diagnostics) then
      epot0=epot
   end if
   epot=epot-epot0
   ekin=ekin*rho_0
   epot=epot*rho_0
   eturb=eturb*rho_0

!  The output parameters are:
!  mld_surf:  Surface mixed layer depth
!  mld_bott:  Bottom mixed layer depth
!  sbf    :  Surface buoyancy flux
!  stf    :  Surface temperature flux
!  MOL    :  Monin-Obukhov length
!  wstar  :  Deardorff convective velocity scale
!  tstar  :  Deardorff convective temperature scale
!  u_taus :  Surface friction velocity
!  u_taub :  Bottom friction velocity
!  heat_flux: Accumulated surface heat flux J/m^2
!  heat_sim: Relative heat content from simulated T-profiles J/m^2
!  heat_obs: Relative heat content from observed T-profiles J/m^2
!  ekin   : Kinetic energy of the water column J/m^2
!  epot   : Potential energy of the water column J/m^2
!  eturb  : Turbulent energy of the water column J/m^2

   x = N*dt/(86400.)
   write(temp_unit,111)   x,sst,t(nlev)
   write(mld_unit,111)    x,mld_surf,mld_bott
   write(sf_unit,111)     x,sbf,stf,MOL
   write(fric_unit,111)   x,wstar,tstar,u_taus,u_taub
   write(heat_unit,111)   x,heat_flux,heat_sim,heat_obs
   write(energy_unit,111) x,ekin,epot,eturb

   init_diagnostics=.false.
111 format(F10.5,1x,4(E12.5,1x))

   return
   end subroutine do_diagnostics
!EOC

!-----------------------------------------------------------------------

   end module output

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

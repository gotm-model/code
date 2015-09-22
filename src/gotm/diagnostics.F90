#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: diagnostics --- additional diagnostics
!
! !INTERFACE:
   module diagnostics
!
! !DESCRIPTION:
!  This module acts as an interface between GOTM and modules/routines
!  doing the actual output. In order to add a new output format it is only
!  necessary to add hooks in this module and write the actual output
!  routines. It is not necessary to change anything in GOTM itself.
!
! !USES:
   IMPLICIT NONE
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public do_diagnostics
!
! !PUBLIC DATA MEMBERS:
   REALTYPE, public                    :: ekin,epot,eturb
#if 0
   logical                             :: diagnostics
   integer                             :: mld_method
   REALTYPE                            :: diff_k
   REALTYPE                            :: Ri_crit
   logical                             :: rad_corr
#endif
   logical                             :: init_diagnostics=.true.

!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Jorn Bruggeman
!
! !PRIVATE DATA MEMBERS:

!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute various diagnostic/integrated variables
!
! !INTERFACE:
   subroutine do_diagnostics(nlev)
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
   integer, intent(in)                    :: nlev
#if 0
   integer(kind=timestepkind), intent(in) :: n
   integer, intent(in)                    :: nlev,BuoyMeth
   REALTYPE, intent(in)                   :: dt
   REALTYPE, intent(in)                   :: u_taus,u_taub
   REALTYPE, intent(in)                   :: I_0,heat
#endif
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE, save            :: epot0
   REALTYPE                  :: z
   integer                   :: i
#if 0
   integer                   :: i
   REALTYPE                  :: mld_surf,mld_bott
   REALTYPE                  :: heat_sim,heat_obs
   REALTYPE, save            :: heat_sim0,heat_obs0,heat_flux
   REALTYPE                  :: z,dtt,dtb,x
   REALTYPE                  :: wstar,tstar
   REALTYPE                  :: sbf,stf,MOL
   REALTYPE                  :: Ri(0:nlev)
#endif
!
!-----------------------------------------------------------------------
!BOC
#if 0
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
#endif

   ekin=_ZERO_
   epot=_ZERO_
   eturb=_ZERO_
   z=_ZERO_
   do i=1,nlev
      z=z-_HALF_*h(i)
      ekin=ekin+_HALF_*h(i)*(u(i)**2+v(i)**2)
      eturb=eturb+h(i)*(tke(i)+tke(i-1))
      epot=epot+h(i)*buoy(i)*z
      z=z-_HALF_*h(i)
   end do
   if (init_diagnostics) then
      epot0=epot
   end if
   epot=epot-epot0
   ekin=ekin*rho_0
   epot=epot*rho_0
   eturb=eturb*rho_0

#if 0
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

111 format(F10.5,1x,4(E12.5,1x))
#endif
   init_diagnostics=.false.

   return
   end subroutine do_diagnostics
!EOC

!-----------------------------------------------------------------------

   end module diagnostics

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

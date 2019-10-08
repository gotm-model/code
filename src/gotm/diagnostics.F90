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
!  This module calculates different diagnostics. It is very easy to extend
!  the number of diagnostics calculated - and have those newly defined
!  values saved in a file. In addition to adding the variable as a public
!  variable in this module - like e.g. ekin - add an appropriate line in
!  register_all_variables.F90 - use agin ekin as example.
!
! !USES:
   IMPLICIT NONE
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_diagnostics, do_diagnostics, clean_diagnostics
!
! !PUBLIC DATA MEMBERS:
   REALTYPE, public                    :: ekin,epot,eturb
   REALTYPE                            :: epot0
   REALTYPE, public, allocatable       :: taux(:),tauy(:)
   integer, public                     :: mld_method=1
   REALTYPE, public                    :: mld_surf,mld_bott
   REALTYPE                            :: diff_k = 1e-05
   REALTYPE                            :: Ri_crit = 0.5
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Jorn Bruggeman and Hans Burchard
!
! !PRIVATE DATA MEMBERS:
!EOP
!-----------------------------------------------------------------------

   contains
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize various diagnostic/integrated variables
!
! !INTERFACE:
   subroutine init_diagnostics(nlev)
!
! !DESCRIPTION:
!  This subroutine initializes the following diagnostic/integrated variables.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                    :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   integer                   :: rc
!EOP
!-----------------------------------------------------------------------
!BOC
   epot0=epot

   allocate(taux(0:nlev),stat=rc)
   if (rc /= 0) stop 'diagnostics: Error allocating (taux)'
   taux = _ZERO_
   allocate(tauy(0:nlev),stat=rc)
   if (rc /= 0) stop 'diagnostics: Error allocating (tauy)'
   tauy = _ZERO_

   return
   end subroutine init_diagnostics
!EOC

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
   use airsea_driver,only: sst,tx,ty
   use meanflow,     only: gravity,rho_0,cp,drag
   use meanflow,     only: h,u,v,s,t,rho,NN,SS,buoy,rad
   use turbulence,   only: turb_method
   use turbulence,   only: kappa
   use turbulence,   only: num
   use turbulence,   only: tke
   use observations, only: tprof,b_obs_sbf
   use eqstate,      only: eqstate1
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                    :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   REALTYPE                  :: z
   integer                   :: i
   integer                   :: j(1)
   REALTYPE                  :: Ri(0:nlev)
!EOP
!-----------------------------------------------------------------------
!BOC

   if (turb_method.eq.99) return

   select case(mld_method)
      case(1)          ! MLD according to TKE criterium
         mld_surf = _ZERO_
         do i=nlev,1,-1
            if (tke(i) .lt. diff_k) exit
            mld_surf=mld_surf+h(i)
         end do
         mld_bott = _ZERO_
         do i=1,nlev
            if (tke(i) .lt. diff_k) exit
            mld_bott=mld_bott+h(i)
         end do
      case(2)          ! MLD according to critical Ri number
         do i=1,nlev-1
            Ri(i)=NN(i)/(SS(i)+1.e-10)
         end do
         mld_surf    = h(nlev)
         do i=nlev-1,1,-1
            if (Ri(i) .gt. Ri_crit) exit
            mld_surf=mld_surf+h(i)
         end do
      case(3)          ! MLD according to maxiumun NN
         j = maxloc(NN(1:nlev))
         mld_surf = sum(h(j(1):nlev))
         mld_bott = _ZERO_
      case default
   end select

   ! Turbulent momentum fluxes
   taux(nlev) = -tx
   tauy(nlev) = -ty
   do i=nlev-1,1,-1
      taux(i)=-num(i)*(u(i+1)-u(i))/(0.5*(h(i+1)+h(i)))
      tauy(i)=-num(i)*(v(i+1)-v(i))/(0.5*(h(i+1)+h(i)))
   end do
   taux(0)=-drag(1)*u(1)*sqrt(u(1)**2+v(1)**2)
   tauy(0)=-drag(1)*v(1)*sqrt(u(1)**2+v(1)**2)

#if 0
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
      heat_obs=heat_obs+tprof%data(i)*h(i)*rho_0*cp
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
   epot=epot-epot0
   ekin=ekin*rho_0
   epot=epot*rho_0
   eturb=eturb*rho_0

   return
   end subroutine do_diagnostics
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Clean various diagnostic/integrated variables
!
! !INTERFACE:
   subroutine clean_diagnostics
!
! !DESCRIPTION:
!  This subroutine initializes the following diagnostic/integrated variables.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   if (allocated(taux)) deallocate(taux)
   if (allocated(tauy)) deallocate(tauy)

   return
   end subroutine clean_diagnostics
!EOC

!-----------------------------------------------------------------------

   end module diagnostics

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

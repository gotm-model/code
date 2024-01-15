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
   use density,      only: rho0
   use meanflow,     only: gravity,cp,drag
   use meanflow,     only: h,u,v,s,t,NN,SS,buoy,rad
   use turbulence,   only: turb_method
   use turbulence,   only: kappa
   use turbulence,   only: num
   use turbulence,   only: tke
   use observations, only: tprof_input,b_obs_sbf
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
   ekin=ekin*rho0
   epot=epot*rho0
   eturb=eturb*rho0

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

!$Id: friction_lake.F90,v 0.1 2011-07-13 14:46:00 schueler Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The vertical friction for horizontally integrated models
!
! !INTERFACE:
   subroutine friction_lake(kappa,avmolu,tx,ty,nlev)
!
! !DESCRIPTION:
!  For a description see subroutine friction.
!  The main difference is that this subroutine calculates friction for every
!  grid cell.
!
! !USES:
   use meanflow,      only: h,z0b,h0b,MaxItz0b,z0s,za
   use meanflow,      only: u,v,gravity
   use meanflow,      only: u_taub,u_taus,drag
   use meanflow,      only: charnock,charnock_val,z0s_min

!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: kappa,avmolu,tx,ty
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   integer                             :: i, j
   REALTYPE                            :: rr
!
!-----------------------------------------------------------------------
!BOC

   drag = _ZERO_

!  use the Charnock formula to compute the surface roughness
   if (charnock) then
      z0s=charnock_val*u_taus**2/gravity
      if (z0s.lt.z0s_min) z0s=z0s_min
   else
      z0s=z0s_min
   end if

!  iterate bottom roughness length MaxItz0b times
!  iterate from nlev to 1 so that u_taub is located at 1 at the end
!  this is important for other modules
   do j=nlev,1,-1
      do i=1,MaxItz0b

         if (avmolu.le.0) then
            z0b=0.03*h0b + za
         else
            z0b=0.1*avmolu/max(avmolu,u_taub)+0.03*h0b + za
         end if

!        compute the factor r (version 1, with log-law)
         rr=kappa/(log((z0b+h(j)/2)/z0b))

!        compute the factor r (version 2, with meanvalue log-law)
!        frac=(z0b+h(j))/z0b
!        rr=kappa/((z0b+h(j))/h(j)*log(frac)-1.)

!        compute the friction velocity at every grid cell
         u_taub = rr*sqrt( u(j)*u(j) + v(j)*v(j) )

      end do
!     add bottom friction as source term for the momentum equation
      drag(j) = drag(j) +  rr*rr
   end do

!  be careful: tx and ty are the surface shear-stresses
!  already divided by rho!
   u_taus=(tx**2+ty**2)**(1./4.)

   return
   end subroutine friction_lake
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

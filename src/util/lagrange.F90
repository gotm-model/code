!$Id: lagrange.F90,v 1.2 2004-03-22 10:14:24 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: lagrangian particles
! 
! !INTERFACE:
   subroutine lagrange(nlev,dt,zlev,nuh,w,npar,active,zi,zp)
!
! !DESCRIPTION:
!
! !USES:
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS: 
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: zlev(0:nlev)
   REALTYPE, intent(in)                :: nuh(0:nlev)
   REALTYPE, intent(in)                :: w
   integer, intent(in)                 :: npar
   logical, intent(in)                 :: active(npar)
!
! !INPUT/OUTPUT PARAMETERS: 
   integer, intent(inout)              :: zi(npar)
   REALTYPE, intent(inout)             :: zp(npar)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: lagrange.F90,v $
!  Revision 1.2  2004-03-22 10:14:24  kbk
!  cleaned, store old index -> much faster, fixed conc. calc.
!
!  Revision 1.1  2004/03/04 09:28:41  kbk
!  general lagrangian 1D solver
!
! !LOCAL VARIABLES:
   integer         :: i,n,ni
   REALTYPE        :: rnd(npar)
   REALTYPE        :: depth,dz(nlev),dzn(nlev),step,zp_old
   REALTYPE        :: visc,rat,dt_inv
!EOP
!-----------------------------------------------------------------------
!BOC

   dt_inv=1./dt

   call random_number(rnd)
   rnd=(2.*rnd-1.)

   do i=1,nlev 
      dz(i)=zlev(i)-zlev(i-1)
      dzn(i)=(nuh(i)-nuh(i-1))/dz(i)
   end do

   depth=-zlev(0)
   do n=1,npar
      i=zi(n)
      rat=(zp(n)-zlev(i-1))/dz(i)
      visc=rat*nuh(i)+(1.-rat)*nuh(i-1)
      zp_old=zp(n)
      step=dt*(sqrt(6*dt_inv*visc)*rnd(n)+w+dzn(i))
      zp(n)=zp(n)+step
      if (zp(n) .lt. -depth) zp(n)=-depth+(-depth-zp(n))
      if (zp(n) .gt. _ZERO_) zp(n)=-zp(n)
      step=zp(n)-zp_old
      if (step.gt.0) then ! search new index above old index
         do i=zi(n),nlev
            if (zlev(i) .gt. zp(n)) EXIT
         end do
      else                ! search new index below old index
         do i=zi(n),1,-1
            if (zlev(i-1) .lt. zp(n)) EXIT
         end do
      end if
      zi(n)=i
   end do

   return
   end subroutine lagrange
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

!$Id: lagrange.F90,v 1.1 2004-03-04 09:28:41 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: lagrangian particles
! 
! !INTERFACE:
   subroutine lagrange(nlev,dt,zlev,nuh,w,npar,active,zp)
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
   REALTYPE, intent(inout)             :: zp(npar)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: lagrange.F90,v $
!  Revision 1.1  2004-03-04 09:28:41  kbk
!  general lagrangian 1D solver
!
!
! !LOCAL VARIABLES:
   integer         :: i,n
   REALTYPE        :: rnd(npar)
   REALTYPE        :: depth,dz,visc,rat
!#define USE_BRACKETING
#ifdef USE_BRACKETING
   integer         :: il,iu,im
   REALTYPE        :: z
#endif
!EOP
!-----------------------------------------------------------------------
!BOC

   call random_number(rnd)
   rnd=(2.*rnd-1.)

   depth=-zlev(0)
   do n=1,npar
!     find vertical grid box
#ifdef USE_BRACKETING
      z=zp(n)
      il=0 ; iu=nlev+1
      do while ( iu-il .gt. 1 )
         im=(iu+il)/2
         if(z .gt. zlev(im)) then
            il=im
         else
            iu=im
         end if
      end do
      i=il+1
#else
      do i=1,nlev
         if (zlev(i) .gt. zp(n)) EXIT
      end do
#endif

      dz=zlev(i)-zlev(i-1)
      rat=(zp(n)-zlev(i-1))/dz

      zp(n)=zp(n)+dt*(nuh(i)-nuh(i-1))/dz

!      rat=(zp(n)-zlev(i-1))/dz

if( rat .lt. _ZERO_ .or. rat .gt. _ONE_ ) then
STDERR i,n
STDERR zp(n)
STDERR zlev(i-1),zlev(i),dz
STDERR rat
stop
end if

      visc=rat*nuh(i)+(1.-rat)*nuh(i-1)
      zp(n)=zp(n)+sqrt(6.*dt*visc)*rnd(n)+dt*w

      if (zp(n) .lt. -depth) zp(n)=-depth+(-depth-zp(n))
      if (zp(n) .gt. _ZERO_) zp(n)=-zp(n)
   end do

   return
   end subroutine lagrange
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

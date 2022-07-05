#include "cppdefs.h"
!
   program test_tridiagonal
!
! !DESCRIPTION:
!  program to test the tridiagonal inter action routines
!
!  To execute:
!  make test_tridiagonal
!
! !USES:
   use mtridiagonal, only: init_tridiagonal, tridiagonal, clean_tridiagonal
   use mtridiagonal, only: au, bu, cu, du
   implicit none
!
! !LOCAL VARIABLES
   integer, parameter :: rk = kind(_ONE_)
   integer, parameter :: Nmax = 4
   integer            :: N
   REALTYPE           :: res(0:Nmax)

!EOP
!-----------------------------------------------------------------------
!BOC

   LEVEL1 "tridiag( 1 , 2 , 1 ) X = 1"
   do N = Nmax,0,-1
      LEVEL0 LINE
      LEVEL1 "N = ", N
      call init_tridiagonal(N)
      au = 1._rk
      bu = 2._rk
      cu = 1._rk
      du = 1._rk
      call tridiagonal(N, 0, N, res(0:N))
      LEVEL1 "X = ", real(res(0:N))
      call clean_tridiagonal()
   end do

   end program test_tridiagonal
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

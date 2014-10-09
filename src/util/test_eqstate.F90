#include "cppdefs.h"
!
   program test_eqstate
!
! !DESCRIPTION:
!  program to test the eqstate inter action routines
!
!  To execute:
!  make test_eqstate
!
! !USES:
   use eqstate, only: eqstate1,eq_state_method,eq_state_mode
   implicit none
!
! !LOCAL VARIABLES
!  basic eqstate interaction variables
   REALTYPE :: T0=10.
   REALTYPE :: S0=35.
   REALTYPE :: p0=35.
   REALTYPE :: dtr0=-0.17
   REALTYPE :: dsr0=0.78
   REALTYPE :: rho_0=1025.
   REALTYPE :: g=9.81
   REALTYPE :: S,T,p

   REALTYPE :: x ! from buoyancy to density

!EOP
!-----------------------------------------------------------------------
!BOC
   S=25.
   T=10.
   p=100.

   LEVEL1 'S=   ',S
   LEVEL1 'T=   ',T
   LEVEL1 'p=   ',p
   LEVEL1 'rho0=',rho_0
   LEVEL1 'g=   ',g

   LEVEL1 ''

   eq_state_mode=1
   LEVEL1 'UNESCO:'
   eq_state_method=1
   x = eqstate1(S,T,p,g,rho_0)*rho_0/g+rho_0
   LEVEL2 'Full pressure:   ',x
   eq_state_method=2
   x = eqstate1(S,T,p,g,rho_0)*rho_0/g+rho_0
   LEVEL2 'Surface pressure:',x

   LEVEL1 ''

   eq_state_mode=2
   LEVEL1 'Jackett:'
   eq_state_method=1
   x = eqstate1(S,T,p,g,rho_0)*rho_0/g+rho_0
   LEVEL2 'Full pressure:   ',x
   eq_state_method=2
   x = eqstate1(S,T,p,g,rho_0)*rho_0/g+rho_0
   LEVEL2 'Surface pressure:',x

   end program test_eqstate
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

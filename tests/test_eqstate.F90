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
   use eqstate, only: T0,S0,p0,dtr0,dsr0
   use eqstate, only: config_equation_of_state
   use eqstate, only: eqstate1,eq_state_method,eq_state_mode
   implicit none
!
! !LOCAL VARIABLES
!  basic eqstate interaction variables
   integer, parameter :: rk = kind(_ONE_)

   REALTYPE :: rho_0=1025._rk
   REALTYPE :: g=9.81_rk
   REALTYPE :: S,T,p

   REALTYPE :: x ! from buoyancy to density
   REALTYPE :: x1,x2,x3
   integer  :: n
!EOP
!-----------------------------------------------------------------------
!BOC
   S0 = 30._rk
   T0 = 10._rk
   p0 = 1000._rk
   S = 30.
   T = 10.
   p = 0.
   p = 1000.

   LEVEL1 'S0=   ',S0
   LEVEL1 'T0=   ',T0
   LEVEL1 'p0=   ',p0
   LEVEL1 'S=    ',S
   LEVEL1 'T=    ',T
   LEVEL1 'p=    ',p
   LEVEL1 'rho_0=',rho_0
   LEVEL1 'g=    ',g

   LEVEL1 ''
!#define BUOY
   eq_state_mode=1
   LEVEL1 'UNESCO:'
   eq_state_method=1
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Full pressure:   ',x,rho_0
   eq_state_method=2
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Surface pressure:',x,rho_0
   eq_state_method=3
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Linearized:      ',x,rho_0
   LEVEL2 'dsr0= ',dsr0,'dtr0= ',dtr0

   LEVEL1 ''

   eq_state_mode=2
   LEVEL1 'JMFWG-06 - Jacket06:'
   eq_state_method=1
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Full pressure:   ',x,rho_0
   eq_state_method=2
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Surface pressure:',x,rho_0
   eq_state_method=3
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Linearized:      ',x,rho_0
   LEVEL2 'dsr0= ',dsr0,'dtr0= ',dtr0

   LEVEL1 ''

   eq_state_mode=3
   LEVEL1 'TEOS-10:'
   eq_state_method=1
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Full pressure:   ',x,rho_0
   eq_state_method=2
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Surface pressure:',x,rho_0
   eq_state_method=3
!   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL2 'Linearized:      ',x,rho_0
   LEVEL2 'dsr0= ',dsr0,'dtr0= ',dtr0

   LEVEL1 ''

   eq_state_method=4
   dtr0 = -0.17_rk
   dsr0 =  0.78_rk
   call config_equation_of_state(rho_0)
#ifdef BUOY
   x = eqstate1(S,T,p,g)
#else
   x = eqstate1(S,T,p)
#endif
   LEVEL1 'Linearized:      ',x,rho_0
   LEVEL2 'S-S0= ',S-S0,'T-T0= ',T-T0
   LEVEL2 'dsr0= ',dsr0,'dtr0= ',dtr0

   T=10._rk
   write(400,*) 'S TEOS10_UN80 TEOS10_JMFWG06 JMFWG06_UN80'
   do n=0,84
      S=n*0.5
      eq_state_method=1
      eq_state_mode=1
      call config_equation_of_state(rho_0)
      x1 = eqstate1(S,T,p) - 1000._rk
      eq_state_mode=2
      call config_equation_of_state(rho_0)
      x2 = eqstate1(S,T,p) - 1000._rk
      eq_state_mode=3
      call config_equation_of_state(rho_0)
      x3 = eqstate1(S,T,p) - 1000._rk
      write(400,"(4F12.7)") S,x3-x1,x3-x2,x2-x1
   end do

   end program test_eqstate
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

!$Id: cmue_cb.F90,v 1.2 2003-03-10 09:02:03 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: \cite{Canutoetal2001a} non-eq.\ stability func.\ ver.\ B
! 
! !INTERFACE:
   subroutine cmue_cb(nlev)
!
! !DESCRIPTION:
!  \label{cmue_cb}
!  This subroutine computes non-equilibrium version of the stability functions
!  according to \cite{Canutoetal2001a}. The version `B' is implemented as follows:
! \begin{equation}\label{StabCanutoB}
! \begin{array}{l}
! \displaystyle
! c_{\mu}=\frac{0.1270+0.01526\alpha_N-0.00016\alpha_M}{A},
! \\
! \\
! \displaystyle
! c'_{\mu}=\frac{0.1190+0.004294\alpha_N+0.00066\alpha_M}{A},
! \end{array}
! \end{equation}
! with
! $A=1+0.2\alpha_N+0.0315\alpha_M
!             +0.0058 \alpha_N^2+0.004\alpha_N\alpha_M
! 	                -0.00004\alpha_M^2$.
!
! !USES:
   use turbulence, only: an,as
   use turbulence, only: cmue1,cmue2,cm0
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: cmue_cb.F90,v $
!  Revision 1.2  2003-03-10 09:02:03  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
! 
!EOP
!
! !LOCAL VARIABLES:
   integer:: i
   REALTYPE, parameter       :: L1=0.1270
   REALTYPE, parameter       :: L2=0.00336
   REALTYPE, parameter       :: L3=0.0906
   REALTYPE, parameter       :: L4=0.1010
   REALTYPE, parameter       :: L5=11.2000
   REALTYPE, parameter       :: L6=0.4000
   REALTYPE, parameter       :: L7=0.0000
   REALTYPE, parameter       :: L8=0.3180
   REALTYPE, parameter       :: tnmin=-12.27
   REALTYPE                  :: a2_cm03
   REALTYPE                  :: s0,s1,s2,s4,s5,s6
   REALTYPE                  :: d,d0,d1,d2,d3,d4,d5
   REALTYPE                  :: tsmax
   REALTYPE                  :: tn,ts,sm,sh
!
!-----------------------------------------------------------------------
!BOC
   a2_cm03=2./cm0**3

   s0 = 1.5*L1*L5*L5
   s1 = -L4*(L6+L7)+2.*L4*L5*(L1-1./3.*L2-L3)+1.5*L1*L5*L8
   s2 = -0.375*L1*(L6*L6-L7*L7)
   s4 = 2.*L5
   s5 = 2.*L4
   s6 = 2./3.*L5*(3.*L3*L3-L2*L2)-0.5*L5*L1*(3.*L3-L2)+0.75*L1*(L6-L7)

   d0 = 3.*L5*L5
   d1 = L5*(7.*L4+3.*L8)
   d2 = L5*L5*(3.*L3*L3-L2*L2)-0.75*(L6*L6-L7*L7)
   d3 = L4*(4.*L4+3.*L8)
   d4 = L4*(L2*L6-3.*L3*L7-L5*(L2*L2-L3*L3))+L5*L8*(3.*L3*L3-L2*L2)
   d5 = 0.25*(L2*L2-3*L3*L3)*(L6*L6-L7*L7)

   do i=1,nlev-1
      tn = 4./cm0**6 * an(i)
      if (tn .lt. tnmin) tn = tnmin

      ts = 4./cm0**6 * as(i)
      tsmax = (d0+d1*tn+d3*tn*tn)/(d2+d4*tn)
      if (ts.gt.tsmax) ts = tsmax

      d = d0 + d1*tn + d2*ts + d3*tn*tn + d4*tn*ts + d5*ts*ts

      sm = (s0 + s1*tn + s2*ts) / d
      sh = (s4 + s5*tn + s6*ts) / d

      cmue1(i) = a2_cm03 * sm
      cmue2(i) = a2_cm03 * sh
   end do

   return
   end subroutine cmue_cb
!EOC

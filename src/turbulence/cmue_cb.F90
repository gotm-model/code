!$Id: cmue_cb.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Canuto et al. [2000] version B non-equilibrium stability functions. 
! 
! !INTERFACE:
   subroutine cmue_cb(nlev)
!
! !DESCRIPTION:
!  This subroutine computes Canuto et al. [2000] version B non-equilibrium
!  stability functions.
!
! !USES:
   use turbulence, only: an,as
   use turbulence, only: cmue1,cmue2
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: cmue_cb.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE, parameter	:: L1=0.1270
   REALTYPE, parameter	:: L2=0.00336
   REALTYPE, parameter	:: L3=0.0906
   REALTYPE, parameter	:: L4=0.1010
   REALTYPE, parameter	:: L5=11.2000
   REALTYPE, parameter	:: L6=0.4000
   REALTYPE, parameter	:: L7=0.0000
   REALTYPE, parameter	:: L8=0.3180
   REALTYPE, parameter	:: cm0=0.5540
   REALTYPE, parameter	:: tnmin=-12.27
   REALTYPE, parameter	:: a2_cm03=2./cm0**3
   REALTYPE		:: s0,s1,s2,s4,s5,s6
   REALTYPE		:: d,d0,d1,d2,d3,d4,d5
   REALTYPE		:: tsmax
   REALTYPE		:: tn,ts,sm,sh
! 
!EOP
!-----------------------------------------------------------------------
!BOC
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

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard & Karsten Bolding.

!$Id: cmue_bbqe.F90,v 1.2 2003-03-10 09:02:03 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: \cite{BurchardBaumert95} quasi-eq.\ stability func.
! 
! !INTERFACE:
   subroutine cmue_bbqe(nlev)
!
! !DESCRIPTION:
!  This subroutine computes quasi-equilibrium version of the stability functions
!  according to \cite{BurchardBaumert95}, see section \ref{cmue_bb}. Here, the 
!  quasi-equilibrium assumption (\ref{quasieq}) is applied in addition.
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
!  $Log: cmue_bbqe.F90,v $
!  Revision 1.2  2003-03-10 09:02:03  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i,j
   REALTYPE                  :: X,D,A,E,H,ww,as0
   REALTYPE                  :: Prinv
   REALTYPE, parameter       :: c1=1.8
   REALTYPE, parameter       :: c2=0.6
   REALTYPE, parameter       :: c1t=3.0
! 
!-----------------------------------------------------------------------
!BOC
   an = 1./cm0**6 * an
   do i=1,nlev-1 
      X=0.0
      A=1./(1.+1.072*an(i)/(3.0+0.5*X))
      D=1.+0.4*an(i)/(1.8+X)/(3.0+0.5*X)
      E=1.+1.2*A*an(i)/(1.8+X)/(3.0+0.5*X)
      H=1.-0.67*A*an(i)/(3.0+0.5*X)/(3.0+0.5*X)

      Prinv = A*D/H*c1/(c1t*(1-c2))

      cmue1(i)=0.4/(1.8+X)*H/D*0.33

      do j=1,1000         ! Loop for iteration to calculate as
         as0=as(i)
         as(i) = 1. + an(i)*Prinv + 1/cmue1(i)
         ww=2./3.*0.8/(E*(1.8+X)-0.16/(1.8+X)*H/D*as(i))
         cmue1(i)  = 0.4/(1.8+X)*H/D*ww
	 if (abs(as0-as(i)).lt.1e-12) goto 111
      end do  

111   ww=2./3.*0.8/(E*(1.8+X)-0.16/(1.8+X)*H/D*as(i))

      cmue1(i) = 0.4/(1.8+X)*H/D*ww
      cmue2(i) = A/(3.0+0.5*X)*ww
   end do

   cmue1 = 1./cm0**3 * cmue1
   cmue2 = 1./cm0**3 * cmue2

   return
   end subroutine cmue_bbqe
!EOC

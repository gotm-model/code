!$Id: cmue_rf.F90,v 1.2 2002-02-08 08:59:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Simple formula stability functions.
! 
! !INTERFACE:
   subroutine cmue_rf(nlev,NN,SS)
!
! !DESCRIPTION: 
!  This subroutine computes stability functions with some "simple" formulas
!
!  Stab=Isprastab
!  \begin{equation}
!  \begin{array}{ll}
!  c_{\mu} = cm0 \\
!  c_{\mu}'= \frac {1} {Prandtl0} (1-R_f)^{1/2}
!  \end{array}
!  \end{equation}
!  \begin{equation} 
!  1-R_f=(\sqrt{R_i^2+1}-R_i)^2 
!  \end{equation}
!  \begin{equation}
!  R_i=\frac {1} {2 Prandtl0} \frac {N^2} {S^2}
!  \end{equation}
!
!  $R_f$ is limited for supercritically stable stratification $1.8<R_f$.  
!
! !USES:
   use turbulence, only: cm0,Prandtl0,xRF 
   use turbulence, only: cmue1,cmue2 
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: NN(0:nlev),SS(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY: 
!  Original author(s):  Manuel Ruiz Villarreal, Hans Burchard 
!                       & Karsten Bolding
!
!  $Log: cmue_rf.F90,v $
!  Revision 1.2  2002-02-08 08:59:58  gotm
!  Added Manuel as author and copyright holder
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE		:: Ri,Prandtl_inv
!EOP
!-----------------------------------------------------------------------
!BOC
!  Calculation of xRf=(1-Rf), where Rf is the flux Richardson number
   do i=1,nlev-1
      Ri=0.5/Prandtl0*NN(i)/(SS(i)+1e-8)
      xRf(i)=(sqrt(Ri*Ri+1)-Ri)**2 
      if (xRf(i) .gt. 2.) xRf(i)=2.
      Prandtl_inv=1/Prandtl0*sqrt(xRf(i))

      if (Prandtl_inv.lt.0.18) Prandtl_inv=0.18
      if (Prandtl_inv.gt.2.0)  Prandtl_inv=2.0
   
      cmue1(i)=cm0
      cmue2(i)=cm0*Prandtl_inv
   end do
   return
   end subroutine cmue_rf
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard, Karsten Bolding 
!                     & Manuel Ruiz Villarreal.
!-----------------------------------------------------------------------

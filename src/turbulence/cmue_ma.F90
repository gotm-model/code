!$Id: cmue_ma.F90,v 1.2 2002-02-08 08:59:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Munk-Anderson stability functions. 
! 
! !INTERFACE:
   subroutine cmue_ma(nlev,NN,SS)
!
! !DESCRIPTION: 
!  This subroutine computes stability functions 
!  according to Munk \& Anderson [1948]:  
!
!  \begin{equation}
!  \begin{array}{ll}
!  c_{\mu} = cm0 \\
!  c_{\mu}'= \frac{c_{\mu}}{P_r^0}
!  \frac{(1+10 R_i)^{1/2}}{(1+3.33 R_i)^{3/2}}, &  R_i \geq 0,\\
!  c_{\mu}'= c_{\mu}, &  R_i<0,
!  \end{array}
!  \end{equation}
!
! !USES:
   use turbulence, only: cm0,Prandtl0
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
!  Original author(s): Hans Burchard, Karsten Bolding 
!                      & Manuel Ruiz Villarreal
!
!  $Log: cmue_ma.F90,v $
!  Revision 1.2  2002-02-08 08:59:58  gotm
!  Added Manuel as author and copyright holder
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE		:: Ri,Prandtl
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      Ri=NN(i)/(SS(i)+1e-8)   ! Gradient Richardson number 
      if (Ri.ge.1e-10) then 
         Prandtl=Prandtl0*(1.+3.33*Ri)**1.5/sqrt(1.+10.0*Ri)
      else
         Prandtl=Prandtl0
      end if
      cmue1(i)=cm0
      cmue2(i)=cm0/Prandtl
   end do

   return
   end subroutine
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard, Karsten Bolding 
!                     & Manuel Ruiz Villarreal.
!-----------------------------------------------------------------------

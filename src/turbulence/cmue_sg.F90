!$Id: cmue_sg.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Schumann and Gerz [1995] parameterization of the Prandtl number. 
! 
! !INTERFACE:
   subroutine cmue_sg(nlev,NN,SS)
!
! !DESCRIPTION:
!  This subroutine computes Schumann and Gerz [1995] stability functions.
!
! \begin{equation}
! c_{\mu}=c_{\mu}^0,\qquad c'_{\mu}=\frac{c_{\mu}^0}{P_r}
! \end{equation}
!
! with constant $c_{\mu}^0$. We choose here for the Prandtl number $P_r$ a
! formulation suggested by {\it Schumann and Gerz} [1995]:
!
! \begin{equation}
! P_r=P_r^0\exp\left(-\frac{R_i}{P_r^0R_i^{\infty}}\right)
! -\frac{R_i}{R_i^{\infty}}
! \end{equation}
!
! with the neutral Prandtl number $P_r^0=0.74$ and $R_i^{\infty}=0.25$.
!
! !USES:
   use turbulence, only: Prandtl0,cm0
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
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: cmue_sg.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
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
         Prandtl=Prandtl0*exp(-Ri/(Prandtl0*0.25))+Ri/0.25
      else
         Prandtl=Prandtl0
      end if

      cmue1(i)=cm0
      cmue2(i)=cm0/Prandtl

   end do

   return
   end subroutine cmue_sg
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard & Karsten Bolding.

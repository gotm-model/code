!$Id: tkealgebraic.F90,v 1.2 2002-02-08 08:59:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Algebraic equation for TKE (local equalibrium).
! 
! !INTERFACE:
   subroutine tkealgebraic(nlev,u_taus,u_taub,NN,SS)
!
! !DESCRIPTION:
!  This subroutine calculates the turbulent kinetic energy based
!  on the local equilibrium assumption
!
!  \begin{equation}
!  P+B-\varepsilon=0.
!  \end{equation}  
!
! !USES:
   use turbulence, only: tkeo,tke,L,k_min,cmue2,cde,cmue1,cm0
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: u_taus,u_taub
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
!  $Log: tkealgebraic.F90,v $
!  Revision 1.2  2002-02-08 08:59:58  gotm
!  Added Manuel as author and copyright holder
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
! !LOCAL VARIABLES:
   integer		:: i
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      tkeo(i)=tke(i)
      tke(i)=L(i)*L(i)/cde*(cmue1(i)*SS(i)-cmue2(i)*NN(i))
   end do 
   tke(0)=u_taub*u_taub/sqrt(cm0*cde) 
   tke(nlev)=u_taus*u_taus/sqrt(cm0*cde)
   do i=0,nlev
      if (tke(i).lt.k_min) tke(i)=k_min 
   end do 
 
   return
   end subroutine tkealgebraic
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard, Karsten Bolding 
!                     & Manuel Ruiz Villarreal.
!-----------------------------------------------------------------------

!$Id: production.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Various useful variables. 
!
! !INTERFACE:
   subroutine production(nlev,alpha,num,nuh)
!
! !DESCRIPTION:
!  This subroutine calculates different parameters needed for the
!  turbulence equations such as:
!
!  \begin{itemize}
!  \item P : shear production of turbulent kinetic energy
!  \item B : buoyancy production of turbulent kinetic energy
!  \end{itemize}
!
!  xP is an extra production term which might come from e.g. seagrass friction.
!
! !USES:
   use meanflow, only: NN,SS,xP,P,B
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: alpha
   REALTYPE, intent(in)	:: num(0:nlev),nuh(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: production.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
!EOP
!-----------------------------------------------------------------------
!BOC
   P=num*(SS+alpha*NN) + xP
   B=-nuh*NN

   return
   end subroutine production 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard.

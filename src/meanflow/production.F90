!$Id: production.F90,v 1.5 2003-03-28 08:56:56 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Turbulence production  \label{sec:production}
!
! !INTERFACE:
   subroutine production(nlev,alpha,num,nuh)
!
! !DESCRIPTION:
!  This subroutine calculates the production terms of TKE as defined
!  in \eq{PandG}. The shear--production follows from
!  \begin{equation}
!    \label{computeP}
!     P=\nu_t (M^2 + \alpha_w N^2) + X_P
!    \comma
!  \end{equation}
!  with the turbulent diffusivity of momentum, $\nu_t$, defined in 
!  \eq{nu}. The shear--frequency, $M$, is discretised as described
!  in \sect{sec:uequation}. The term with $\alpha_w$ traces back to
! a prameterisation of breaking internal waves suggested by 
! \cite{Mellor89}. $X_P$ is an extra production term, connected for
! example with turbulence production caused by sea--grass, see 
! \eq{sgProduction} in  \sect{sec:seagrass}.
!
!  Similarly, the buoyancy production is computed from the expression
!  \begin{equation}
!   \label{computeB}
!    B=-\nu_t'N^2
!    \comma
!  \end{equation}
!  with the turbulent diffusivity of heat, $\nu'_t$, defined in 
!  \eq{nu}. The buoyancy--frequency, $N$, is discretised as described
!  in \sect{sec:stratification}.
!
! !USES:
   use meanflow, only: NN,SS,xP,P,B,no_shear 
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: alpha
   REALTYPE, intent(in)                :: num(0:nlev),nuh(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!
!  $Log: production.F90,v $
!  Revision 1.5  2003-03-28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.4  2003/03/10 08:50:07  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.3  2002/02/08 08:59:57  gotm
!
!  Revision 1.2  2001/11/18 16:02:16  gotm
!  Allow no_shear calculation
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (no_shear) then
      P= xP
   else 
      P=num*(SS+alpha*NN) + xP
   end if 
   B=-nuh*NN

   return
   end subroutine production 
!EOC

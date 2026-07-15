#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The stability function as implemented in ICON \label{sec:icon}
!
! !INTERFACE:
   subroutine cmue_icon(nlev)
!
! !DESCRIPTION:
!  This subroutine computes stability functions according to
! \begin{equation}
! c_{\mu}=c_{\mu}^0,\qquad c'_{\mu}=\frac{c_{\mu}^0}{Pr_t}
! \end{equation}
! as a function of the turbulent Prandtl number $Pr_t$, where  $c_{\mu}^0$ is
! a constant to be specified in {\tt gotm.yaml}. The turbulent Prandtl--number
! is specified as
! \begin{equation}
!   Pr_t = \min  \left(1,  \max \left( 10, 6.6*Ri \right)  \right)
!   \comma
! \end{equation}
! where where $Ri$ is the gradient Richardson-number. This model is part of the
! implementation of the model by \cite{Gaspardetal1990} in ICON as described in
! \cite{BrueggemannEtAl2024}. The model coefficients by \cite{Gaspardetal1990}
! translate to $c_\mu^0=0.514$ .
!
! !USES:
   use turbulence, only: cm0_fix
   use turbulence, only: cmue1,cmue2,as,an
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: Ri(0:nlev), Pr(0:nlev)
   REALTYPE,parameter        :: RiFac =  6.6
   REALTYPE,parameter        :: PrMin =  1.0, PrMax = 10.0
   REALTYPE,parameter        :: Small =  1.e-8
!
!-----------------------------------------------------------------------
!BOC

   Ri = an / (as + Small)
   Pr = min(PrMax,RiFac*Ri)
   Pr = max(PrMin,Pr)

   Pr(0)    = 1.
   Pr(nlev) = 1.

   cmue1 = cm0_fix
   cmue2 = cm0_fix/Pr

   
   return
   end subroutine cmue_icon
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

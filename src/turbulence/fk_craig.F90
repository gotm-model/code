#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: TKE flux from wave-breaking\label{sec:fkCraig}
!
! !INTERFACE:
   REALTYPE  function fk_craig(u_tau,eta)
!
! !DESCRIPTION:
! This functions returns the flux of $k$ caused by breaking surface waves
! according to
! \begin{equation}
!  \label{craig}
!   F_k = \eta u_*^3
!  \point
! \end{equation}
! This form has also been used by \cite{CraigBanner94}, who suggested
! $\eta \approx 100$.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: u_tau
   REALTYPE, intent(in)                :: eta
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!-----------------------------------------------------------------------
!BOC
   fk_craig = eta*u_tau**3.

   end function fk_craig
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

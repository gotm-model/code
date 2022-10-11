#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Update dimensionless alpha's\label{sec:alpha}
!
! !INTERFACE:
   subroutine alpha_mnb(nlev,NN,SS, SSCSTK, SSSTK)
!
! !DESCRIPTION:
! This subroutine updates the dimensionless numbers $\alpha_M$, $\alpha_N$,
! and $\alpha_b$ according to \eq{alphaMN}. Note that according to \eq{Nbar}
! and \eq{NbarVertical} the following identities are valid
! \begin{equation}
!  \label{alphaIdentities}
!    \alpha_M = \overline{S}^2 \comma
!    \alpha_N = \overline{N}^2 \comma
!    \alpha_b = \overline{T}   \point
! \end{equation}
!
!
! !USES:
  use turbulence,  only:     tke,eps,kb
  use turbulence,  only:     as,an,at
  use turbulence,  only:     av, aw
  IMPLICIT NONE
!
! !INPUT PARAMETERS:
  integer,  intent(in)      :: nlev
  REALTYPE, intent(in)      :: NN(0:nlev),SS(0:nlev)

!  Stokes-Eulerian cross-shear (1/s^2)
   REALTYPE, intent(in), optional      :: SSCSTK(0:nlev)

!  Stokes shear squared (1/s^2)
   REALTYPE, intent(in), optional      :: SSSTK (0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!-----------------------------------------------------------------------
! !LOCAL VARIABLES:
  integer              :: i
  REALTYPE             :: tau2(0:nlev)

!-----------------------------------------------------------------------
!BOC

   do i=0,nlev
      tau2(i) = tke(i)*tke(i) / ( eps(i)*eps(i) )
      as(i)   = tau2(i) * SS(i)
      an(i)   = tau2(i) * NN(i)
      at(i)   = tke(i)/eps(i) * kb(i)/eps(i)

!     clip negative values
      as(i) = max(as(i),1.e-10*_ONE_)
      at(i) = max(at(i),1.e-10*_ONE_)
   end do

   if (present(SSCSTK) .and. present(SSSTK)) then
      do i=0,nlev
         av(i)  = tau2(i) * SSCSTK(i)
         aw(i)  = tau2(i) * SSSTK(i)

!        clip negative values
         aw(i) = max(aw(i),1.e-10*_ONE_)
      end do
   end if

  return
end subroutine alpha_mnb

!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

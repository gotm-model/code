#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: const_NNT
!
! !INTERFACE:
   subroutine const_NNT(nlev,z,zi,T_top,S_const,NN,gravity,T)
!
! !DESCRIPTION:
! This routine creates a vertical profile {\tt prof} with value
! {\tt v1}
!
! !USES:
   use density, only: get_alpha, alpha
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: nlev
   REALTYPE, intent(in)                :: z(0:nlev)
   REALTYPE, intent(in)                :: zi(0:nlev)
   REALTYPE, intent(in)                :: T_top,S_const,NN
   REALTYPE, intent(in)                :: gravity
!
! !INOUT PARAMETERS:
   REALTYPE, intent(inout)             :: T(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
! !LOCAL VARIABLES:
   integer :: n
!EOP
!-----------------------------------------------------------------------
!BOC
   T(nlev) = T_top ! must be done here
   call get_alpha(nlev,gravity,S_const,NN,z,zi,T)

   do n=nlev-1,1,-1
      T(n) = T(n+1) - _ONE_/(gravity*alpha(n))*NN*(z(n+1)-z(n))
   end do
   end subroutine const_NNT
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

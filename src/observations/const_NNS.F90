#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: const_NNS
!
! !INTERFACE:
   subroutine const_NNS(nlev,z,zi,S_top,T_const,NN,gravity,S)
!
! !DESCRIPTION:
! This routine creates a vertical profile {\tt prof} with value
! {\tt v1}
!
! !USES:
   use density, only: get_beta, beta
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: nlev
   REALTYPE, intent(in)                :: z(0:nlev)
   REALTYPE, intent(in)                :: zi(0:nlev)
   REALTYPE, intent(in)                :: S_top,T_const,NN
   REALTYPE, intent(in)                :: gravity
!
! !INOUT PARAMETERS:
   REALTYPE, intent(inout)             :: S(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
! !LOCAL VARIABLES:
   integer :: n
!EOP
!-----------------------------------------------------------------------
!BOC
   S(nlev) = S_top ! must be done here
   call get_beta(nlev,gravity,T_const,NN,z,zi,S)

   do n=nlev-1,1,-1
      S(n) = S(n+1) + _ONE_/(gravity*beta(n))*NN*(z(n+1)-z(n)) 
   end do
   end subroutine const_NNS
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

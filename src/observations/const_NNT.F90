#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: const_NNT
!
! !INTERFACE:
   subroutine const_NNT(nlev,z,T_top,S_const,NN,gravity,rho_0,T)
!
! !DESCRIPTION:
! This routine creates a vertical profile {\tt prof} with value
! {\tt v1}
!
! !USES:
!KB   use gsw_mod_toolbox, only: gsw_rho_alpha_beta_bsq
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: nlev
   REALTYPE, intent(in)                :: z(0:nlev)
   REALTYPE, intent(in)                :: T_top,S_const,NN
   REALTYPE, intent(in)                :: gravity,rho_0
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: T(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: rho,alpha,pFace
!EOP
!-----------------------------------------------------------------------
!BOC
#if 0
   T(nlev) = T_top

   do i=nlev-1,1,-1

      pFace    = 0.5/gravity*(z(i+1)+z(i));
#if 1
      call gsw_rho_alpha_beta_bsq(S_const,T(i+1),pFace,rho,alpha=alpha)
#else
      alpha     = eos_alpha(S_const,T(i+1),pFace,gravity,rho_0)
#endif

      T(i) = T(i+1) - _ONE_/(gravity*alpha)*NN*(z(i+1)-z(i))

   enddo
#endif

   return
   end subroutine const_NNT
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

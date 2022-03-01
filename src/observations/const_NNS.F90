#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: const_NNS
!
! !INTERFACE:
   subroutine const_NNS(nlev,z,S_top,T_const,NN,gravity,rho0,S)
!
!
! !DESCRIPTION:
! This routine creates a vertical profile {\tt prof} with value
! {\tt v1}


! !USES:
!KB   use gsw_mod_toolbox, only: gsw_specvol_alpha_beta
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: nlev
   REALTYPE, intent(in)                :: z(0:nlev)
   REALTYPE, intent(in)                :: S_top,T_const,NN
   REALTYPE, intent(in)                :: gravity,rho0
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: S(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: pFace
   REALTYPE                  :: beta
!
!-----------------------------------------------------------------------
!BOC

   S(nlev) = S_top

   do i=nlev-1,1,-1

      pFace    = 0.5/gravity*(z(i+1)+z(i));
#if 1
!KB      call gsw_specvol_alpha_beta(S(i+1),T_const,pFace,beta=beta)
#else
      beta     = eos_beta(S(i+1),T_const,pFace,gravity,rho0)
#endif

      S(i) = S(i+1) + _ONE_/(gravity*beta)*NN*(z(i+1)-z(i))

   enddo


   return
   end subroutine const_NNS
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: const_NNS
!
! !INTERFACE:
   subroutine const_NNS(nlev,z,zi,S_top,T_const,NN,gravity,S)
!
!
! !DESCRIPTION:
! This routine creates a vertical profile {\tt prof} with value
! {\tt v1}


! !USES:
   use gsw_mod_toolbox, only: gsw_beta
   use density, only: density_method,dsr0
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
   integer                   :: i
   REALTYPE                  :: beta
!EOP
!-----------------------------------------------------------------------
!BOC
   beta=dsr0 ! density_method=2,3

   S(nlev) = S_top
   do i=nlev-1,1,-1
      if (density_method == 1) then
stop 'Constant NN configuration has been disabled for full TEOS-10 - for the time being'
         beta = gsw_beta(S(i+1),T_const,-zi(i))
      end if
      S(i) = S(i+1) + _ONE_/(gravity*beta)*NN*(z(i+1)-z(i))
   end do
   end subroutine const_NNS
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

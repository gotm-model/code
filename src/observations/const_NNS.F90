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
   use density, only: get_beta
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
   integer                               :: i
    REALTYPE                             :: lbeta   
!EOP
!-----------------------------------------------------------------------
!BOC
    S(nlev) = S_top ! must be done here

    do i=nlev-1,1,-1
      ! estimate interface beta based on S above the interface
      lbeta = get_beta(S(i+1),T_const,-zi(i))

      ! use this to estimate S below the interface
      S(i)  = S(i+1) + _ONE_/(gravity*lbeta)*NN*(z(i+1)-z(i))

      ! compute improved interface beta
      lbeta  = get_beta(0.5*(S(i+1)+S(i)),T_const,-zi(i))

      ! compute final salinity profile
      S(i)   = S(i+1) + _ONE_/(gravity*lbeta)*NN*(z(i+1)-z(i))
   end do
   
   end subroutine const_NNS   
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

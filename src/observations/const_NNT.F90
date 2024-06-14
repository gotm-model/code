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
   use density, only: get_alpha
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
   integer                             :: i
   REALTYPE                           :: lalpha
   
!EOP
!-----------------------------------------------------------------------
!BOC
    T(nlev) = T_top ! must be done here

    do i=nlev-1,1,-1
      ! estimate interface alpha based on T above the interface
      lalpha = get_alpha(S_const,T(i+1),-zi(i))

      ! use this to estimate T below the interface
      T(i)   = T(i+1) - _ONE_/(gravity*lalpha)*NN*(z(i+1)-z(i))

      ! compute improved interface alpha
      lalpha = get_alpha(S_const,0.5*(T(i+1)+T(i)),-zi(i))

      ! compute final temperature profile
      T(i)   = T(i+1) - _ONE_/(gravity*lalpha)*NN*(z(i+1)-z(i))
   end do

   end subroutine const_NNT
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

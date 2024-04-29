#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: wequation
!
! !INTERFACE:
   subroutine wequation(nlev,dt)
!
! !DESCRIPTION:
!  This subroutine calculates vertical velocity profiles, if
!  {\tt w\_adv\_method} is 1 or 2, which has to be chosen in the
!  {\tt w\_advspec} namelist in {\tt obs.nml}. The profiles of vertical
!  velocity are determined by two values,
!  the height of maximum absolute value of vertical velocity, {\tt w\_height},
!  and the vertical velocity at this height, {\tt w\_adv}. From {\tt w\_height},
!  the vertical velocity is linearly decreasing towards the surface and
!  the bottom, where its value is zero.
!
! !USES:
   use meanflow    , only: zi,w
   use observations, only: w_adv,w_height
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: z_crit
!-----------------------------------------------------------------------
!BOC

!  Vertical velocity calculation:

   select case(w_adv%method)
      case(0)
         ! no vertical advection
      case(1,2)
         ! linearly varying advection velocity with peak at "w_height"
         z_crit=zi(nlev)-0.01*(zi(nlev)-zi(0))
         if (w_height%value.gt.z_crit) w_height%value=z_crit
         z_crit=zi(0)+0.01*(zi(nlev)-zi(0))
         if (w_height%value.lt.z_crit) w_height%value=z_crit
         do i=1,nlev-1
            if (zi(i).gt.w_height%value) then
               w(i)=(zi(nlev)-zi(i))/(zi(nlev)-w_height%value)*w_adv%value
            else
               w(i)=(zi(0)-zi(i))/(zi(0)-w_height%value)*w_adv%value
            end if
         end do
         w(0)    =_ZERO_
         w(nlev) =_ZERO_
      case default
   end select

   return

   end subroutine wequation
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

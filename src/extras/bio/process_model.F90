!$Id: process_model.F90,v 1.1 2003-07-23 12:27:31 hb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Initialise the bio module
!
! !INTERFACE:
   subroutine process_model(first,numc,nlev,cc,pp,dd)
!
! !DESCRIPTION:
!
! !USES:
   use bio_var, only: bio_model,par,I_0
   use bio_template, only: do_bio_template
   use bio_npzd, only: do_bio_npzd
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   logical, intent(in)                 :: first
   integer, intent(in)                 :: numc,nlev
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)             :: cc(1:numc,1:numc,0:nlev)
   REALTYPE, intent(inout)             :: pp(1:numc,1:numc,0:nlev)
   REALTYPE, intent(inout)             :: dd(1:numc,1:numc,0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (bio_model)
      case (-1)
         call do_bio_template(first,numc,nlev,cc,pp,dd)
      case (1)
         call do_bio_npzd(first,numc,nlev,cc,pp,dd,par,I_0)
      case default
         stop "bio: no valid biomodel specified in bio.inp !"
   end select
   return

   end subroutine process_model
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

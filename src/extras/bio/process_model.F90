!$Id: process_model.F90,v 1.3 2003-10-28 10:22:45 hb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Initialise the bio module
!
! !INTERFACE:
   subroutine process_model(first,numc,nlev,cc,pp,dd,t)
!
! !DESCRIPTION:
!
! !USES:
   use bio_var, only: bio_model,par,I_0
   use bio_template, only: do_bio_template
   use bio_npzd, only: do_bio_npzd
   use bio_iow, only: do_bio_iow
   use bio_sed, only: do_bio_sed
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   logical, intent(in)                 :: first
   integer, intent(in)                 :: numc,nlev
   REALTYPE, intent(in)                :: t(0:nlev)
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
      case (2)
         call do_bio_iow(first,numc,nlev,cc,pp,dd,t,par,I_0)
      case (3)
         call do_bio_sed(first,numc,nlev,cc,pp,dd)
      case default
         stop "bio: no valid biomodel specified in bio.inp !"
   end select
   return

   end subroutine process_model
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

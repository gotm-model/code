!$Id: bio_var.F90,v 1.2 2003-09-16 12:11:24 hb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_var --- declaration of biological variables
!
! !INTERFACE:
   module bio_var
!
! !DESCRIPTION:
!  Remember this Hans
!
! !USES:
!  default: all is public.
   public
!
! !PUBLIC DATA MEMBERS:
   integer                               :: bio_model
   REALTYPE                              :: I_0
   REALTYPE, dimension(:), allocatable   :: par
   REALTYPE, dimension(:,:), allocatable :: cc,ws
   REALTYPE, dimension(:), allocatable   :: sfl

   integer, dimension(:), allocatable    :: var_ids
   character(len=64), dimension(:), allocatable :: var_names
   character(len=64), dimension(:), allocatable :: var_units
   character(len=64), dimension(:), allocatable :: var_long
!
! !REVISION HISTORY:!
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: bio_var.F90,v $
!  Revision 1.2  2003-09-16 12:11:24  hb
!  added new biological model - bio_iow
!
!  Revision 1.1  2003/07/23 12:27:31  hb
!  more generic support for different bio models
!
!
!EOP
!-----------------------------------------------------------------------

   end module bio_var

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

!$Id: analytical_profile.F90,v 1.2 2003-03-10 08:51:57 gotm Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: analytical_profile
!
! !INTERFACE:
   subroutine analytical_profile(nlev,z,z1,v1,z2,v2,prof)
!
! !DESCRIPTION:
!  This routine creates an analytical profile of a variable.
!  It can be used to set up a simple two layer density structure.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: nlev
   REALTYPE, intent(in)	               :: z(0:nlev)
   REALTYPE, intent(in)	               :: z1,v1,z2,v2
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: prof(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!  $Log: analytical_profile.F90,v $
!  Revision 1.2  2003-03-10 08:51:57  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: alpha
!   
!-----------------------------------------------------------------------
!BOC
   if (z2-z1 .gt. -1.e-15) then 
         alpha = (v2-v1)/(z2-z1+2.e-15) 
   else
      STDERR '**********************************************'
      STDERR '* Error detected by analytical_profile.F90:  *'
      STDERR '*   z2 should be larger than z1.             *'            
      STDERR '*   Please edit obs.inp and restart GOTM.    *'
      STDERR '**********************************************'
      stop
   end if 

   do i=nlev,1,-1
      if(-1.*z(i) .le. z1) then
         prof(i) = v1
      end if
      if (alpha.le.1.e15) then 
         if(-1.*z(i) .gt. z1 .and. -1.*z(i) .le. z2) then
            prof(i) = v1 + alpha*(-1.*z(i)-z1)
         end if
      end if 
      if(-1.*z(i) .gt. z2) then
         prof(i) = v2
      end if
   end do

   return
   end subroutine analytical_profile
!EOC

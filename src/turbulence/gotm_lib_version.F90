!$Id: gotm_lib_version.F90,v 1.2 2003-03-10 09:02:05 gotm Exp $
#include"cppdefs.h"
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: Printing GOTM library version
!
! !INTERFACE:
      subroutine gotm_lib_version(unit)
!
! !DESCRIPTION:
!  Simply prints the version number of the GOTM turbulence library to unit.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: unit
!
! !REVISION HISTORY: 
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: gotm_lib_version.F90,v $
!  Revision 1.2  2003-03-10 09:02:05  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
!-------------------------------------------------------------------------
!BOC
   write(unit,*) 'GOTM library version: ',RELEASE

   return
   end
!EOC

!$Id: gotm_lib_version.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include"cppdefs.h"
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: gotm_lib_version() - prints the GOTM library version to unit
!
! !INTERFACE:
      subroutine gotm_lib_version(unit)
!
! !DESCRIPTION:
!  Simple prints the version number of the GOTM turbulence library to unit.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: unit
!
! !OUTPUT PARAMETERS:
!
! !BUGS:
!
! !SEE ALSO: 
!
! !SYSTEM ROUTINES:
!
! !FILES USED:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: gotm_lib_version.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
!
!EOP
!-------------------------------------------------------------------------
!BOC
   write(unit,*) 'GOTM library version: ',RELEASE
   return
   end
!EOC

!-------------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard
!-------------------------------------------------------------------------

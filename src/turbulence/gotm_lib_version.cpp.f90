# 1 "gotm_lib_version.F90"
!$Id: gotm_lib_version.cpp.f90,v 1.1 2001-02-12 15:55:58 gotm Exp $
# 1 "../../include//cppdefs.h" 1
! This file is include in all .F90 files and contains very important
! definitions. Infact GOTM will not compile when this file is not
! in a correct format.
! KBK 20000220

# 1 "../../include//version.h" 1


# 7 "../../include//cppdefs.h" 2






! Handy for writing











! Shapes for variables














! For easier reading



! To avoid dividing by zero


! What precision will we use in this compilation














# 3 "gotm_lib_version.F90" 2
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
!  $Log: gotm_lib_version.cpp.f90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
!
!EOP
!-------------------------------------------------------------------------
!BOC
   write(unit,*) 'GOTM library version: ',2.3.1
   return
   end
!EOC

!-------------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard
!-------------------------------------------------------------------------

!$Id: coriolis.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The effects of rotation
!
! !INTERFACE:
   subroutine coriolis(nlev,dt)
!
! !DESCRIPTION:
!  This subroutine carries out the Coriolis rotation by applying a 
!  2x2 rotation matrix with angle Cori*dt on (u,v).  
!
! !USES:
   USE meanflow, only: u,v,cori
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)		:: nlev
   REALTYPE, intent(in)		:: dt
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: coriolis.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer			:: i
   REALTYPE			:: ua,omega,cosomega,sinomega
!
!EOP
!-----------------------------------------------------------------------
!BOC
   omega=cori*dt
   cosomega=cos(omega)
   sinomega=sin(omega)
#ifdef OLD
   do i=1,nlev
      ua=u(i)
      u(i)=u(i)*cos(cori*dt)+v(i)*sin(cori*dt)
      v(i)=-ua*sin(cori*dt)+v(i)*cos(cori*dt)
   end do 
#else
   do i=1,nlev
      ua=u(i)
      u(i)=u(i)*cosomega+v(i)*sinomega
      v(i)=-ua*sinomega+v(i)*cosomega
   end do 
#endif

   return
   end subroutine coriolis
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

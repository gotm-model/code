!$Id: pressuregradient.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The external pressure gradient. 
!
! !INTERFACE:
   subroutine pressuregradient(nlev) 
!
! !DESCRIPTION:
!  This subroutine aplies the external presure gradient. Two methods
!  are implemented here, it is either the velocity vector at a
!  given height prescribed or the vector for the vertical mean
!  velocity. If the external pressure gradient is prescribed as
!  surface slopes, then it is directly applied in uequation() and
!  vequation().  
!
! !USES:
   use meanflow, only:	u,v,h,PressMethod,dpdx,dpdy,h_press
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: pressuregradient.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer 		:: i 
   REALTYPE	 	:: z(0:nlev)
   REALTYPE	 	:: rat,uint,vint,hint
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (PressMethod.eq.1) then !Current measurement at h_press above bed  
      z(1)=0.5*h(1) 
      i   =0 
222   i=i+1 
      z(i+1)=z(i)+0.5*(h(i)+h(i+1))
      if ((z(i+1).lt.h_press).and.(i.lt.nlev)) goto 222  
      rat=(h_press-z(i))/(z(i+1)-z(i))  
      uint=rat*u(i+1)+(1-rat)*u(i) 
      vint=rat*v(i+1)+(1-rat)*v(i) 
      do i=1,nlev
         u(i)=u(i)+dpdx-uint
         v(i)=v(i)+dpdy-vint
      end do
   end if
 
   if (PressMethod.eq.2) then ! Vertical mean of current prescribed   
      uint=0
      vint=0 
      hint=0 
      do i=1,nlev
         hint=hint+h(i) 
         uint=uint+h(i)*u(i) 
         vint=vint+h(i)*v(i) 
      end do
      uint=uint/hint 
      vint=vint/hint 
      do i=1,nlev
         u(i)=u(i)+dpdx-uint
         v(i)=v(i)+dpdy-vint
      end do
   end if

   return
   end subroutine pressuregradient
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

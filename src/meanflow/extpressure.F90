!$Id: extpressure.F90,v 1.2 2003-03-10 08:50:06 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The external pressure--gradient \label{sec:extpressure}
!
! !INTERFACE:
   subroutine extpressure(method,nlev) 
!
! !DESCRIPTION:
!
!  This subroutine calculates the external pressure--gradient. Two methods
!  are implemented here, relating either to the velocity vector at a
!  given height above bed prescribed or to the vector for the vertical mean
!  velocity. For details of this method, see \cite{Burchard99}. 

!  If the external pressure--gradient is prescribed by the
!  surface slope, then it is directly inserted in \eq{uEq} and \eq{vEq}. 
!
! !USES:
   use meanflow, only: u,v,h
   use observations, only: dpdx,dpdy,h_press
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: method,nlev
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: extpressure.F90,v $
!  Revision 1.2  2003-03-10 08:50:06  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer 			:: i 
   REALTYPE	 		:: z(0:nlev)
   REALTYPE	 		:: rat,uint,vint,hint
!
!-----------------------------------------------------------------------
!BOC

   select case (method)
      case (1) ! Current measurement at h_press above bed
         z(1)=0.5*h(1) 
         i   =0 
222      i=i+1 
         z(i+1)=z(i)+0.5*(h(i)+h(i+1))
         if ((z(i+1).lt.h_press).and.(i.lt.nlev)) goto 222  
         rat=(h_press-z(i))/(z(i+1)-z(i))  
         uint=rat*u(i+1)+(1-rat)*u(i) 
         vint=rat*v(i+1)+(1-rat)*v(i) 
         do i=1,nlev
            u(i)=u(i)+dpdx-uint
            v(i)=v(i)+dpdy-vint
         end do
      case (2) ! Vertical mean of current prescribed 
         uint=0.
         vint=0.
         hint=0.
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
      case default
   end select

   return
   end subroutine extpressure
!EOC

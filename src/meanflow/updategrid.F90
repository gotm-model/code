!$Id: updategrid.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The calculation grid. 
!
! !INTERFACE:
   subroutine updategrid(nlev,ddu,ddl,zeta)
!
! !DESCRIPTION:
!  This subroutine calculates for each time step new layer thickness
!  in order to fit them to changing water depth. 
!  Zooming can be applied with according to the following formula:
!
!  \begin{equation}\label{formula_Antoine}
!  h_k = D\frac{\mbox{tanh}\left( (d_l+d_u)\frac{k}{M}-d_l\right)
!  +\mbox{tanh}(d_l)}{\mbox{tanh}(d_l)+\mbox{tanh}(d_u)}-1
!  \end{equation}
!
!  Here, $d_l=d_u=0$ results in equidistant discretizations.
!  $d_l>0, d_u=0$ results in zooming near the bottom.
!  $d_l=0, d_u>0$ results in zooming near the surface.
!  $d_l>0, d_u>0$ results in double zooming surface and bottom.
!
! !USES:
   use meanflow, only:	depth0,depth,z,h,ho
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: ddu,ddl
   REALTYPE, intent(in)	:: zeta
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: updategrid.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer 		:: i,rc
   integer, save	:: GridInit  
   REALTYPE, save, dimension(:), allocatable	:: ga
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (GridInit.eq.0) then ! Build up dimensionless grid (0<=ga<=1)  
      allocate(ga(0:nlev),stat=rc)
      if (rc /= 0) STOP 'updategrid: Error allocating (ga)'
      ga(0)=0 
      if (ddu.le.0.and.ddl.le.0) then 
         do i=1,nlev 
            ga(i)=ga(i-1)+1/float(nlev)  
         end do  
      else 
         do i=1,nlev ! This zooming routine is from Antoine Garapon, ICCH, DK  
            ga(i)=tanh((ddl+ddu)*i/nlev-ddl)+tanh(ddl) 
            ga(i)=ga(i)/(tanh(ddl)+tanh(ddu)) 
         end do 
      end if
      depth = depth0 + Zeta 
      do i=1,nlev
         h(i)=(ga(i)-ga(i-1))*depth
      end do
      GridInit = 1  !  Grid is now initialized ! 
   end if

   depth = depth0 + zeta
   do i=1,nlev
      ho(i) = h(i)
      h(i)  = (ga(i)-ga(i-1)) * depth
   end do 
       
   z(1)=-depth0+0.5*h(1) 
   do i=2,nlev 
      z(i)=z(i-1)+0.5*(h(i-1)+h(i)) 
   end do  

   return
   end subroutine updategrid 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

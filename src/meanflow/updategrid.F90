!$Id: updategrid.F90,v 1.4 2001-11-27 19:51:49 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The calculation grid. 
!
! !INTERFACE:
   subroutine updategrid(nlev,zeta)
!
! !DESCRIPTION:
!  Three different grids can be specified:
!  A) Equidistant grid with possible zooming towards surface and bottom:
!     The number of layers (nlev) and the zooming factors (ddu,ddl) 
!     are specified
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
!  The grid can be also read from file. There are two possibilities:
!  B) Sigma layers: The fraction that every layer occupies is read in from file.
!  C) Cartesian layer: The height of every layer is read in from file.
!     This method is not recommended when a varying sea surface is considered
!
! !USES:
   use meanflow, only:	depth0,depth,z,h,ho,ddu,ddl,grid_method,grid_file
   use observations, only : zeta_method
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
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
!  Revision 1.4  2001-11-27 19:51:49  gotm
!  Cleaned
!
!  Revision 1.3  2001/11/27 15:38:06  gotm
!  Possible to read coordinate distribution from file
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!
! !LOCAL VARIABLES:
   integer 		:: i,rc,j,nlayers
   integer, save	:: gridinit  
   REALTYPE, save, dimension(:), allocatable	:: ga
   integer, parameter   :: grid_unit = 101
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (gridinit .eq. 0) then ! Build up dimensionless grid (0<=ga<=1)  
      allocate(ga(0:nlev),stat=rc)
      if (rc /= 0) STOP 'updategrid: Error allocating (ga)'
      ga(0)=0 
      select case (grid_method)
      case(0) !Equidistant grid with possible zooming to surface and bottom
         LEVEL2 "sigma coordinates (zooming possible)"
         if (ddu .le. 0 .and. ddl .le. 0) then 
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
      case(1) !Sigma, the fraction each layer occupies is specified. 
         LEVEL2 "external specified sigma coordinates"
         open (grid_unit,FILE =grid_file,status='unknown',ERR=100)
	 read (grid_unit,*) nlayers
	 if (nlayers /= nlev) then
	    FATAL "number of layers spefified in file <> # of model layers" 
	   stop 'updategrid'
	 end if  	      
	 depth = _ZERO_
	 j = 0
	 do i=nlev,1,-1 !The first layer to be read is at the surface
            read(grid_unit,*,ERR=101,END=101) ga(i)
            depth = depth + ga(i)
	    j=j+1
         end do	 
	 if (j /= nlayers) then
	    FATAL "number of layers read from file <> # of model layers" 
	    stop 'updategrid'		
         end if			 
	 close (grid_unit)
	 if (depth /= 1.) then
	    FATAL "sum of all layers in grid_file should be 1."
	    stop 'updategrid'
	 end if
     case(2) !Cartesian, the layer thickness is read from file
         LEVEL2 "external specified cartesian coordinates"
	 open (grid_unit,FILE =grid_file,ERR=100)
! Observations is called after meanflow is initialized, and we don#t have
! zeta_method
!	 if (zeta_method /= 0) then
!	     stop "You are using Cartesian coordinates with varying surface elevation"
!	 end if
	 read (grid_unit,*) nlayers
	 if(nlayers /= nlev) then
	    FATAL "nlev must be equal to the number of layers in: ", &
                   trim(grid_file)
	    stop 'updategrid'
	 end if  
	 depth = _ZERO_
	 j=0
	 do i=nlev,1,-1 !The first layer read is the surface
            read(grid_unit,*,ERR=101) h(i)
            depth = depth + h(i)
	    j=j+1
         end do	 
	 if (j /= nlayers) then
	    FATAL "number of layers read from file <> # of model layers" 
	    stop 'updategrid'
         end if	
	 close (grid_unit)
	 if (depth /= depth0) then
	    FATAL "sum of all layers should be equal to the total depth",depth0
	    stop 'updategrid'
	 end if
     case default
         stop "updategrid: No valid grid_method specified" 	 
     end select
     
     gridinit = 1  !  Grid is now initialized ! 
   end if
   
   depth = depth0 + zeta

   select case(grid_method) 
   case (0)   
      do i=1,nlev
         ho(i) = h(i)
         h(i)  = (ga(i)-ga(i-1)) * depth
      end do 
   case (1)
      ho = h
      h = ga *depth
   case (2) 
      ho=h   
    case default
         stop "updategrid: No valid grid_method specified" 	
   end select   
   
   z(1)=-depth0+0.5*h(1) 
   do i=2,nlev 
      z(i)=z(i-1)+0.5*(h(i-1)+h(i)) 
   end do  
   
   return

100 FATAL 'Unable to open ',trim(grid_file),' for reading'
    stop 'updategrid'
101 FATAL 'Error reading grid file ',trim(grid_file)
    stop 'updategrid'    


   end subroutine updategrid 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2001 - Hans Burchard and Karsten Bolding
!-----------------------------------------------------------------------

!$Id: stratification.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculates NN and buoy as function of T,S,p. 
!            Effects of sediments are added in Sediment.F90.  
!
! !INTERFACE:
   subroutine stratification(nlev,buoy_method,dt,g,rho_0,nuh)
!
! !DESCRIPTION:
!  This subroutine calculates different parameters needed for the
!  turbulence equations such as:
!
!  \begin{itemize}
!  \item buoy: buoyancy in [m/s**2]
!  \end{itemize}
!
! !USES:
   use meanflow, only: h,S,T,NN,buoy
   use eqstate, only: eqstate1
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev,buoy_method
   REALTYPE, intent(in)	:: dt,g,rho_0
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out):: nuh(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: stratification.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer 		:: i
   REALTYPE 		:: buoyp,buoym,z
   REALTYPE 		:: dz
   integer,parameter	:: USEEQSTATE=1
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (buoy_method .EQ. USEEQSTATE) then
      z=0.0  
      do i=nlev-1,1,-1 
         z=z+h(i+1)
         dz=0.5*(h(i)+h(i+1)) 
         buoyp=eqstate1(S(i+1),T(i+1),z/10.,g,rho_0) 
         buoym=eqstate1(S(i  ),T(i  ),z/10.,g,rho_0) 
         NN(i)=(buoyp-buoym)/dz 
      end do
      z=0. 
      do i=nlev,1,-1 
         buoy(i)=eqstate1(S(i),T(i),z/10.,g,rho_0) 
      end do
   else
      call buoyancy(nlev,dt,nuh) 
      do i=nlev-1,1,-1 
         dz=0.5*(h(i)+h(i+1)) 
         NN(i)=(buoy(i+1)-buoy(i))/dz 
      end do
   end if

   NN(0)=NN(1)
   NN(nlev)=NN(nlev-1)
 
   return
   end subroutine stratification
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard.

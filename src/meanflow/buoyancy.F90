!$Id: buoyancy.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The 'up-drift' - buoyancy. 
! 
! !INTERFACE:
   subroutine buoyancy(nlev,dt,nuh)
!
! !DESCRIPTION:
!  This subroutine calculates the diffusion equation for buoyancy
!
!  \begin{equation}\label{DefBuoyancy}
!  b=-g\frac{\rho-\rho_0}{\rho_0}.
!  \end{equation}
!
!  \begin{equation}\label{bEq}
!  \partial_tb
!  -\partial_z(\nu'_t(b) \partial_z b)  = 0.  
!  \end{equation}
!
!  No sources or sinks, no boundary fluxes included so far.  
!
!  Used for idealistic simulations such as Kato-Phillips experiment.
!
!  Diffusion treated implicitly in space, and then solved by a 
!  simplified Gauss elimination. 
!
! !USES:
   use mtridiagonal
   use meanflow, only:	h,buoy,avh
   use observations, ONLY: b_obs_NN,b_obs_surf,b_obs_sbf
   IMPLICIT NONE
!
! !INPUT PARAMETERS: 
   integer, intent(in)		:: nlev
   REALTYPE, intent(in)		:: dt
   REALTYPE, intent(in)		:: nuh(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS: 
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: buoyancy.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer			:: i 
   REALTYPE			:: zz
   logical, save		:: first=.true.
!
! !BUGS
!
!EOP
!-----------------------------------------------------------------------
!BOC
! Construct initial linear profile from information in namelist 

   if (first) then
      zz=0.0
      do i=nlev,1,-1
         zz=zz+0.5*h(i)
         buoy(i)  = b_obs_surf - zz*b_obs_NN
         zz=zz+0.5*h(i)
      end do
      first=.false.
   end if

   avh=nuh

   do i=2,nlev-1
      cu(i)=-2*dt*avh(i)/(h(i)+h(i+1))/h(i)
      au(i)=-2*dt*avh(i-1)/(h(i)+h(i-1))/h(i)
      bu(i)=1-au(i)-cu(i)
      du(i)=buoy(i)
   end do

   cu(1)=-2*dt*avh(1)/(h(1)+h(2))/h(1)
   bu(1)=1-cu(1)
   du(1)=buoy(1)

   au(nlev)=-2*dt*avh(nlev-1)/(h(nlev)+h(nlev-1))/h(nlev)
   bu(nlev)=1-au(nlev)
   du(nlev)=buoy(nlev)-dt/h(nlev)*b_obs_sbf 

   call tridiagonal(nlev,1,nlev,buoy)

   return
   end subroutine buoyancy
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

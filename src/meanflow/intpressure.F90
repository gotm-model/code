!$Id: intpressure.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The internal pressure gradient. 
!
! !INTERFACE:
   subroutine intpressure(nlev) 
!
! !DESCRIPTION:
!   With the hydrostatic assumption
!
!  \begin{equation}\label{HydroStat}
!  \partial_z p + g \rho = 0,
!  \end{equation}

!  where $g=9.81$m\,s$^{-2}$ is the gravitational acceleration,
!  and $\rho$ the density,
!  the pressure gradients may be expressed as
!
!  \begin{equation}\label{InternalPressurex}
!  - \frac{1}{\rho_0} \partial_x p=
!  -g \frac{\rho(\zeta)}{\rho_0} \partial_x \zeta
!  +\int_z^{\zeta}\partial_x b \, dz'
!  \end{equation}
!  and
!  \begin{equation}\label{InternalPressurey}
!  - \frac{1}{\rho_0} \partial_y p=
!  -g \frac{\rho(\zeta)}{\rho_0} \partial_y \zeta
!  +\int_z^{\zeta}\partial_y b \, dz'
!  \end{equation}
!
!  where
!
!  \begin{equation}\label{DefBuoyancy}
!  b=-g\frac{\rho-\rho_0}{\rho_0}
!  \end{equation}
!
!  is the buoyancy.
!  The first term on the right hand side
!  in (\ref{InternalPressurex})
!  and (\ref{InternalPressurey}) is the external (due to surface slopes)
!  and the second the internal (due to density gradients)
!  pressure gradient, which are calculated by means of this subroutine.
!  The internal pressure gradients will only be established by
!  gradients of temperature $T$ and salinity $S$, sediment
!  concentration is assumed to be horizontally homogeneous which
!  of course could easily be changed.
!
!  In this subroutine, first, the terms $\partial_xb$ and $\partial_yb$
!  are calculated from the prescribed gradients of salinity ($\partial_xS$
!  and $\partial_yS$) and temperature ($\partial_xT$ and $\partial_yT$)
!  in the following way:
!
!  \begin{equation}
!  \partial_xb=\frac{b(S+\Delta_xS,T+\Delta_xT,p)-b(S,T,p)}{\Delta x}
!  \end{equation}
!
!  \begin{equation}
!  \partial_yb=\frac{b(S+\Delta_yS,T+\Delta_yT,p)-b(S,T,p)}{\Delta y}
!  \end{equation}
!
!  with
!
!  \begin{equation}
!  \Delta_xS=\Delta x \partial_xS;\qquad \Delta_yS=\Delta y \partial_yS
!  \end{equation}
!  and
!  \begin{equation}
!  \Delta_xT=\Delta x \partial_xT;\qquad \Delta_yT=\Delta y \partial_yT
!  \end{equation}
!
!  where $\Delta x$ and $\Delta y$ are lengths to be specified in the
!  namelist file. A small sensitivity to $\Delta x$ and $\Delta y$
!  is expected due to the non-linearity of the equation of state.
!  $\Delta x$ and $\Delta y$ should be small (about 10 m), but large
!  enough to prevent inaccuracies caused by the precision of the
!  computer.
!
! !USES:
   use meanflow, only: gravity,rho_0,h,T,S
   use observations, only: dsdx,dsdy,dtdx,dtdy	
   use observations, only: idpdx,idpdy
   use eqstate, only: eqstate1
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
!  $Log: intpressure.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer 		:: i 
   REALTYPE		:: z,dx,dy
   REALTYPE		:: dSS,dTT,Bl,Br,int
   REALTYPE 		:: dxB(0:nlev),dyB(0:nlev)
!
!EOP
!-----------------------------------------------------------------------
!BOC

   idpdx=0.
   idpdy=0.

   dx=10.
   dy=10.
   z=0.
   do i=nlev,1,-1
      z=z+0.5*h(i)

!     Calculation of buoyancy gradient in x direction
      dSS=dx*dsdx(i)
      dTT=dx*dtdx(i)
      Bl=eqstate1(S(i),T(i),z/10.,gravity,rho_0)
      Br=eqstate1(S(i)+dSS,T(i)+dTT,z/10.,gravity,rho_0)
      dxB(i)=(Br-Bl)/dx

!     Calculation of buoyancy gradient in y direction
      dSS=dy*dsdy(i)
      dTT=dy*dtdy(i)
      Bl=eqstate1(S(i),T(i),z/10.,gravity,rho_0)
      Br=eqstate1(S(i)+dSS,T(i)+dTT,z/10.,gravity,rho_0)
      dyB(i)=(Br-Bl)/dy

      z=z+0.5*h(i)
   end do

!  Calculation of internal pressure gradient in x direction
   int=0.5*h(nlev)*dxB(nlev)
   idpdx(nlev)=int
   do i=nlev-1,1,-1
      int=int+0.5*h(i+1)*dxB(i+1)+0.5*h(i)*dxB(i)
      idpdx(i)=int
   end do

!  Calculation of internal pressure gradient in y direction
   int=0.5*h(nlev)*dyB(nlev)
   idpdy(nlev)=int
   do i=nlev-1,1,-1
      int=int+0.5*h(i+1)*dyB(i+1)+0.5*h(i)*dyB(i)
      idpdy(i)=int
   end do

   return
   end subroutine intpressure
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

!$Id: intpressure.F90,v 1.2 2003-03-10 08:50:06 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The internal pressure--gradient \label{sec:intpressure}
!
! !INTERFACE:
   subroutine intpressure(nlev) 
!
! !DESCRIPTION:
!   With the hydrostatic assumption
!  \begin{equation}\label{HydroStat}
!   \partder{p}{z} + g \rho = 0
!   \comma
!  \end{equation}
!  where $g=9.81$m\,s$^{-2}$ is the gravitational acceleration
!  and $\rho$ the density,
!  the components of the pressure--gradient may be expressed as
!  \begin{equation}
!   \label{InternalPressurex}
!  - \frac{1}{\rho_0} \partder{p}{x}=
!  -g \frac{\rho(\zeta)}{\rho_0} \partder{\zeta}{x}
!  +\int_z^{\zeta}\partder{b}{x} \, dz'
!  \end{equation}
!  and
!  \begin{equation}\label{InternalPressurey}
!  - \frac{1}{\rho_0} \partder{p}{y}=
!  -g \frac{\rho(\zeta)}{\rho_0} \partder{\zeta}{y}
!  +\int_z^{\zeta} \partder{b}{y} \, dz'
!   \comma
!  \end{equation}
!  where $b$ is the buoyancy as defined in \eq{DefBuoyancy}.
!
!  The first term on the right hand side
!  in \eq{InternalPressurex}
!  and \eq{InternalPressurey} is the external pressure--gradient 
!  due to surface slopes,  and the second the internal pressure--gradient
!  due to the density gradient.
!  The internal pressure--gradient will only be established by
!  gradients of temperature $\theta$ and salinity $S$. Sediment
!  concentration is assumed to be horizontally homogeneous, which
!  of course could easily be changed.
!
!  In this subroutine, first, the terms $\partial_xb$ and $\partial_yb$
!  are calculated from the prescribed gradients of salinity ($\partial_xS$
!  and $\partial_yS$) and temperature ($\partial_x\theta$ and $\partial_y\theta$)
!  according to
!  \begin{equation}
!    \partder{b}{x} \approx 
!    \frac{b(S+\Delta_xS,\theta+\Delta_x\theta,p)-b(S,\theta,p)}{\Delta x}
!    \comma
!  \end{equation}
!  \begin{equation}
!    \partder{b}{x} \approx 
!    \frac{b(S+\Delta_yS,\theta+\Delta_y\theta,p)-b(S,\theta,p)}{\Delta y}
!   \comma
!  \end{equation}
!  where we used
!  \begin{equation}
!    \Delta_xS=\Delta x \partial_xS \comma 
!    \Delta_yS=\Delta y \partial_yS \comma
!  \end{equation}
!  and
!  \begin{equation}
!   \Delta_x\theta=\Delta x \partial_x\theta \comma 
!   \Delta_y\theta=\Delta y \partial_y\theta \comma
!  \end{equation}
!  where $\Delta x$ and $\Delta y$ are lengths to be specified in the
!  namelist file. A small sensitivity to $\Delta x$ and $\Delta y$
!  is expected due to the non--linearity of the equation of state.
!  $\Delta x$ and $\Delta y$ should be small (about 10 m), but large
!  enough to prevent inaccuracies caused by the precision of the
!  computer.
!
! !USES:
   use meanflow, only: gravity,rho_0,h,T,S
   use observations, only: dsdx,dsdy,dtdx,dtdy	
   use observations, only: idpdx,idpdy
   use eqstate, only: eqstate1
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: intpressure.F90,v $
!  Revision 1.2  2003-03-10 08:50:06  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer 		:: i 
   REALTYPE		:: z,dx,dy
   REALTYPE		:: dSS,dTT,Bl,Br,int
   REALTYPE 		:: dxB(0:nlev),dyB(0:nlev)
!
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

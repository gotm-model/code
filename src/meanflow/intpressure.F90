#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The internal pressure-gradient \label{sec:intpressure}
!
! !INTERFACE:
   subroutine intpressure(nlev)
!
! !DESCRIPTION:
!   With the hydrostatic assumption
!  \begin{equation}\label{HydroStat}
!   \partder{P}{z} + g \mean{\rho} = 0
!   \comma
!  \end{equation}
!  where $P$ denotes the mean pressure, $g=9.81$m\,s$^{-2}$
!  the gravitational acceleration  and $\mean{\rho}$ the mean density,
!  the components of the pressure-gradient may be expressed as
!  \begin{equation}
!   \label{InternalPressurex}
!  - \frac{1}{\rho_0} \partder{P}{x}=
!  -g \partder{\zeta}{x}
!  +\int_z^{\zeta}\partder{B}{x} \, dz'
!  -\frac{1}{\rho_0} \partder{P(\zeta)}{x}
!  \end{equation}
!  and
!  \begin{equation}\label{InternalPressurey}
!  - \frac{1}{\rho_0} \partder{P}{y}=
!  -g \partder{\zeta}{y}
!  +\int_z^{\zeta} \partder{B}{y} \, dz'
!  -\frac{1}{\rho_0} \partder{P(\zeta)}{y}
!   \comma
!  \end{equation}
!  where $\zeta$ is the surface elevation, and $B$ the
!  mean buoyancy as defined in \eq{DefBuoyancy}.
!
!  The first term on the right hand side
!  in \eq{InternalPressurex}
!  and \eq{InternalPressurey} is the external pressure-gradient
!  due to surface slopes,  the second the internal pressure-gradient
!  due to the density gradient and the third term is the
!  atmoshperic pressure gradient at sea surface height.
!  The internal pressure-gradient will only be established by
!  gradients of mean potential temperature $\Theta$ and mean
!  salinity $S$. Sediment concentration is assumed to be
!  horizontally homogeneous.
!
!  In this subroutine there are two ways to calculate the internal pressure
!  gradient.
!
!  {\bf Scenarios with flat bottom:}
!  First, the horizontal buoyancy gradients,
!  $\partial_xB$ and $\partial_yB$,
!  are calculated from the prescribed gradients of salinity, $\partial_xS$
!  and $\partial_yS$, and temperature, $\partial_x\Theta$ and $\partial_y\Theta$,
!  according to the finite-difference expression
!  \begin{equation}
!    \partder{B}{x} \approx
!    \frac{B(S+\Delta_xS,\Theta+\Delta_x\Theta,P)-B(S,\Theta,P)}{\Delta x}
!    \comma
!  \end{equation}
!  \begin{equation}
!    \partder{B}{y} \approx
!    \frac{B(S+\Delta_yS,\Theta+\Delta_y\Theta,P)-B(S,\theta,P)}{\Delta y}
!   \comma
!  \end{equation}
!  where the defintions
!  \begin{equation}
!    \Delta_xS=\Delta x \partial_xS \comma
!    \Delta_yS=\Delta y \partial_yS \comma
!  \end{equation}
!  and
!  \begin{equation}
!   \Delta_x\Theta=\Delta x \partial_x\Theta \comma
!   \Delta_y\Theta=\Delta y \partial_y\Theta \comma
!  \end{equation}
!  have been used. $\Delta x$ and $\Delta y$ are "small enough", but otherwise
!  arbitrary length scales. The buoyancy gradients computed with this method
!  are then vertically integrated according to \eq{InternalPressurex} and
!  \eq{InternalPressurey}.
!
! The horizontal salinity and temperature gradients have to supplied by the
! user, either as constant values or as profiles given in a file (see
! {\tt obs.nml}).
!
! {\bf Scenarios for dense bottom and buoyant surface plumes in a sloping frame:}
! Assuming for a {\it sloping water-colum model model}
! that all density gradients
! along the sloping surface or bottom vanish, i.e.,
! \begin{equation}
! \partder{B}{x} = - \partder{\zeta}{x} \partder{B}{z}\comma
! \end{equation}
! \begin{equation}
! \partder{B}{y} = - \partder{\zeta}{y} \partder{B}{z}\comma
! \end{equation}
! we obtain
! \begin{equation}
! -\frac{1}{\rho_0} \partder{P}{x} =
! -\frac{1}{\rho_0} \partder {P(\zeta)}{x}
! +\partder{\zeta}{x} B(z)\comma
! \end{equation}
! \begin{equation}
! -\frac{1}{\rho_0} \partder{P}{y} =
! -\frac{1}{\rho_0} \partder {P(\zeta)}{y}
! +\partder{\zeta}{y} B(z).
! \end{equation}
!
! {\it Buoyant plume under shelf ice.}
! For the ambient water below the plume
! with $z\rightarrow -H$
! with the ambient buoyancy, $B(-H)$, we demand that the pressure
! gradient vanishes, i.e.,
! \begin{equation}
! 0=
! -\frac{1}{\rho_0} \partder{P}{x} =
! -\frac{1}{\rho_0} \partder{P(\zeta)}{x}
! +\partder{\zeta}{x} B(-H)\comma
! \end{equation}
! \begin{equation}
! 0=
! -\frac{1}{\rho_0} \partder{P}{y} =
! -\frac{1}{\rho_0} \partder{P(\zeta)}{y}
! +\partder{\zeta}{y} B(-H)\comma
! \end{equation}
! such that we obtain
! \begin{equation}
! -\frac{1}{\rho_0} \partial_x p =
! \partder{\zeta}{x} \left(B(z)-B(-H)\right)\comma
! \end{equation}
! \begin{equation}
! -\frac{1}{\rho_0} \partial_y p =
! \partder{\zeta}{y} \left(B(z)-B(-H)\right).
! \end{equation}
! Those simulations are only useful in situations with
! a sufficient amount of unstratified ambient water left below the plume,
! i.e., the bottom layer must not be entrained into the plume and must stay
! at ambient buoyancy.
!
! {\it Dense plume over sloping topography.}
! Similar considerations lead to the formulation of a bottom-attached
! dense plume:
! \begin{equation}
! -\frac{1}{\rho_0} \partder{P}{x} =
! \partder{\zeta}{x} \left(B(\zeta)-B(z)\right)\comma
! \end{equation}
! \begin{equation}
! -\frac{1}{\rho_0} \partder{P}{y} =
! \partder{\zeta}{y} \left(B(\zeta)-B(z)\right)\comma
! \end{equation}
! where the ambient water is now assumed to be above the plume.
! In this case, the surface layer must not be entrained
! into the plume.
!
! !USES:
   use meanflow,      only: T,S
   use meanflow,      only: gravity,rho_0,h
   use meanflow,      only: buoy
   use observations,  only: int_press_type
   use observations,  only: dsdx_input,dsdy_input,dtdx_input,dtdy_input
   use observations,  only: plume_type,plume_slope_x,plume_slope_y
   use observations,  only: idpdx,idpdy
   use eqstate,       only: eqstate1
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                             :: i
   REALTYPE                            :: z,dx,dy
   REALTYPE                            :: dSS,dTT,Bl,Br,int
   REALTYPE                            :: dxB(0:nlev),dyB(0:nlev)
!
!-----------------------------------------------------------------------
!BOC
   if (int_press_type == 1) then ! T and S gradients

!     initialize local depth
!     and pressure gradient
      z     = _ZERO_
      idpdx = _ZERO_
      idpdy = _ZERO_

!     the spacing for the finite differences
      dx    =  10.
      dy    =  10.

      do i=nlev,1,-1
         z=z+0.5*h(i)

!        buoyancy gradient in x direction
         dSS=dx*dsdx_input%data(i)
         dTT=dx*dtdx_input%data(i)
         Bl=eqstate1(S(i),T(i),z/10.,gravity,rho_0)
         Br=eqstate1(S(i)+dSS,T(i)+dTT,z/10.,gravity,rho_0)
         dxB(i)=(Br-Bl)/dx

!        buoyancy gradient in y direction
         dSS=dy*dsdy_input%data(i)
         dTT=dy*dtdy_input%data(i)
         Bl=eqstate1(S(i),T(i),z/10.,gravity,rho_0)
         Br=eqstate1(S(i)+dSS,T(i)+dTT,z/10.,gravity,rho_0)
         dyB(i)=(Br-Bl)/dy

         z=z+0.5*h(i)
      end do

!     internal pressure gradient in x direction
      int=0.5*h(nlev)*dxB(nlev)
      idpdx(nlev)=int
      do i=nlev-1,1,-1
         int=int+0.5*h(i+1)*dxB(i+1)+0.5*h(i)*dxB(i)
         idpdx(i)=int
      end do

!     internal pressure gradient in y direction
      int=0.5*h(nlev)*dyB(nlev)
      idpdy(nlev)=int
      do i=nlev-1,1,-1
         int=int+0.5*h(i+1)*dyB(i+1)+0.5*h(i)*dyB(i)
         idpdy(i)=int
      end do

   endif

   if (int_press_type == 2) then ! plume

!     surface plume
      if (plume_type .eq. 1) then
         do i=nlev,1,-1
            idpdx(i) = plume_slope_x*(buoy(i)-buoy(1))
            idpdy(i) = plume_slope_y*(buoy(i)-buoy(1))
         end do
      end if

!     bottom plume
      if (plume_type .eq. 2) then
         do i=nlev,1,-1
            idpdx(i) = -plume_slope_x*(buoy(nlev)-buoy(i))
            idpdy(i) = -plume_slope_y*(buoy(nlev)-buoy(i))
         end do
      end if

   endif
   end subroutine intpressure

!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

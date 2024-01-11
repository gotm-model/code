#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculation of the stratification\label{sec:stratification}
!
! !INTERFACE:
   subroutine stratification(nlev)
!
! !DESCRIPTION:
! This routine computes the mean potential density, $\mean{\rho}$, the mean
! potential buoyancy, $B$, defined in \eq{DefBuoyancy}, and the mean buoyancy
! frequency,
!  \begin{equation}
!    \label{DefBuoyancyFrequency}
!     N^2 = - \dfrac{g}{\rho_0} \partder{\rho}{z} = \partder{B}{z}
!     \comma
!  \end{equation}
! which is based on potential density or buoyancy such that for $N^2=0$, the entropy
! is constant in the whole water column and mixing does not work against buoyancy
! forces. If GOTM used as a turbulence library in your own three-dimensional model,
! you have to insure that the $N^2$ computed by you, and passed to the turbulence
! routines in GOTM, is consistent with the concept of potential density and your
! equation of state.
!
! The mean potential density is evaluated from the equation of state, \eq{DefEOS},
! according to
!  \begin{equation}
!    \label{DefPotentialDensity}
!     \mean{\rho} = \hat{\rho} (\Theta,S,P_R)
!     \comma
!  \end{equation}
!  where $\Theta$ denotes the mean potential temperature, $S$ the mean salinity
!  and $P_R$ the mean reference pressure. The buoyancy frequency defined in
! \eq{DefBuoyancyFrequency} can be decomposed into contributions due to
!  potential temperature and salinity stratification,
!  \begin{equation}
!    \label{NDecompostionA}
!     N^2 = N_\Theta^2 + N_S^2
!     \comma
!  \end{equation}
!  where we introduced the quantities
!  \begin{equation}
!    \label{NNT}
!     N_\Theta^2  = - \dfrac{g}{\rho_0} \partder{\rho}{z} \Big|_{S}
!                 = g \alpha(\Theta,S,P_R) \partder{\Theta}{z}
!     \comma
!  \end{equation}
!  with the thermal expansion coefficient defined in \eq{eosAlpha}, and
!  \begin{equation}
!    \label{NNS}
!     N_S^2  = - \dfrac{g}{\rho_0} \partder{\rho}{z} \Big|_{\Theta}
!                 = - g \beta(\Theta,S,P_R) \partder{S}{z}
!  \comma
!  \end{equation}
!  with the saline contraction coefficient defined in \eq{eosBeta}. It is important
!  to note that in the actual code the reference pressure, $P_R$, has been replaced by
!  the (approximate) hydrostatic pressure. Only if this dependence is replaced by
!  the constant reference pressure at the surface in the equation of state,
!  see \sect{sec:eqstate}, the model is truely based on potential temperature and density.
!  Otherwise,  the model is based on \emph{in-situ} quantities.
!
!  Alternatively to the procedure outlined above, depending on the values of the
!  parameter {\tt buoy\_method}, the buoyancy may be calculated by means of the
!  transport equation \eq{bEq}. This equation then replaces the computation of $\Theta$
!  and $S$ and is only recommended for idealized studies.
!
! !USES:
   use density,    only: alpha,beta
   use density,    only: rho0,rho_p
   use meanflow,   only: h,T,S
   use meanflow,   only: buoy
   use meanflow,   only: NN,NNT,NNS
   use meanflow,   only: gravity
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: nlev

! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE :: idz(0:nlev)
   REALTYPE :: dT(0:nlev)
   REALTYPE :: dS(0:nlev)
   integer, parameter :: rk = kind(_ONE_)
!-----------------------------------------------------------------------
!BOC

   idz(1:nlev-1)=2.0_rk/(h(1:nlev-1)+h(2:nlev))
   dT(1:nlev-1)=T(2:nlev)-T(1:nlev-1)
   dS(1:nlev-1)=S(2:nlev)-S(1:nlev-1)

   NNT(1:nlev-1) = alpha(1:nlev-1)*gravity*dT(1:nlev-1)*idz(1:nlev-1)
   NNS(1:nlev-1) = -beta(1:nlev-1)*gravity*dS(1:nlev-1)*idz(1:nlev-1)
   NN(1:nlev-1)  = NNT(1:nlev-1)+NNS(1:nlev-1)

!KB   buoy(1:) = -gravity*(rho_p(1:)-rho0)/rho0

   ! set boundary values
   NN(nlev) = _ZERO_
   NN(0)    = _ZERO_
   end subroutine stratification
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org


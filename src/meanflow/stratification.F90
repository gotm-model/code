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
   use density,    only: calculate_density,rho0
   use meanflow,   only: h,S,T,buoy,rho
   use meanflow,   only: NN,NNT,NNS
   use meanflow,   only: gravity
   use gsw_mod_toolbox, only: gsw_nsquared, gsw_rho
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
   integer :: n
   REALTYPE :: lat(0:nlev)
   REALTYPE :: zc(0:nlev),zi(nlev)
!GSW
!-----------------------------------------------------------------------
!BOC
   lat=_ZERO_ !GSW_KB - need to pass in lat
   zi(nlev)=_ZERO_
   zc(nlev)=_HALF_*h(nlev)
   do n=nlev-1,1,-1
      zc(n) = zc(n+1)+_HALF_*(h(n)+h(n+1))
   end do
   rho(1:) = gsw_rho(S(1:),T(1:),zc(1:)) 
   buoy(1:) = -gravity*(rho(1:)-rho0)/rho0
   call gsw_Nsquared(S(1:),T(1:),zc(1:),lat(1:),NN(1:),zi(1:))

   ! set boundary values
   NN(nlev) = _ZERO_
   NN(0)    = _ZERO_
   end subroutine stratification
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org


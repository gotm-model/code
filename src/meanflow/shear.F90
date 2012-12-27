#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculation of the vertical shear \label{sec:shear}
!
! !INTERFACE:
   subroutine shear(nlev,cnpar)
!
! !DESCRIPTION:
!%  The (square of the) shear frequency is defined as
!% \begin{equation}
!%   \label{MSquared}
!%    M^2 = \left( \partder{U}{z} \right)^2 +
!%          \left( \partder{V}{z} \right)^2
!%    \point
!% \end{equation}
!% It is an important parameter in almost all turbulence models.
!% The $U$- and $V$-contributions to $M^2$ are computed using a new scheme
!% which guarantees conservation of kinetic energy for the convertion
!% from mean to turbulent kinetic energy, see \cite{Burchard2002}. With this method,
!% the discretisation of the $U$-contribution can be written as
!% \begin{equation}
!%   \label{shearsquared}
!%    \left( \partder{U}{z} \right)^2 \approx \frac{(\bar U_{j+1}-\bar U_j)
!%    (\tilde U_{j+1}-\tilde U_j)}{(z_{j+1}-z_j)^2}
!% \end{equation}
!% where $\tilde U_j=\frac12(\hat U_j+U_j)$. The $V$-contribution is computed analogously.
!% The shear obtained from \eq{shearsquared}
!% plus the $V$-contribution is then used for the computation of the turbulence
!% shear production, see equation \eq{computeP}.

! The (square of the) shear frequency is defined as
! \begin{equation}
!   \label{MSquared}
!    M^2 = \left( \partder{U}{z} \right)^2 +
!          \left( \partder{V}{z} \right)^2
!    \point
! \end{equation}
! It is an important parameter in almost all turbulence models.
! The $U$- and $V$-contributions to $M^2$ are computed using a new scheme
! which guarantees conservation of kinetic energy for the conversion
! from mean to turbulent kinetic energy, see \cite{Burchard2002}.
! The shear is calculated by dividing the energy-consistent
! form of the shear production (see equation (14) by \cite{Burchard2002},
! but note the typo in that equation)
! by the eddy viscosity. The correct form of the right hand side of
! equation (14) of
! \cite{Burchard2002} should be:
! \begin{equation}
!   \label{mean_kinetic_energy_dissipation}
! \begin{array}{rcl}
! \displaystyle
!    \left(D_{kin} \right)_j & = &
! \displaystyle
! \phantom{+}   \frac{\nu_{j+1/2}}{2}
! \frac{\sigma \left(\hat U_{j+1}-\hat U_j\right)\left(\hat U_{j+1}-U_j\right)+
! (1-\sigma)\left(U_{j+1}-U_j\right)\left(U_{j+1}-\hat U_j\right)}
! {(z_{j+1/2}-z_{j-1/2})(z_{j+1}-z_j)} \\ \\
! &&
! \displaystyle
! +
!  \frac{\nu_{j-1/2}}{2}\frac{\sigma \left(\hat U_{j}-\hat U_{j-1}\right)
! \left(U_{j}-\hat U_{j-1}\right)+
! (1-\sigma)\left(U_{j}-U_{j-1}\right)\left(\hat U_{j}-U_{j-1}\right)}
! {(z_{j+3/2}-z_{j+1/2})(z_{j+1}-z_j)} \\ \\
! & = &
! \displaystyle
! P_{j+1/2}^l + P_{j-1/2}^u,
! \end{array}
! \end{equation}
! with the mean kinetic energy dissipation, $\left(D_{kin} \right)_j$.
! The two terms on the right hand side are the contribution
! of energy dissipation from below the interface at $j+1/2$ and the
! contribution from above the interface at $j-1/2$.
! With (\ref{mean_kinetic_energy_dissipation}),
! an energy-conserving discretisation of the shear production at $j+1/2$ should
! be
! \begin{equation}
! P_{j+1/2} = P_{j+1/2}^l + P_{j+1/2}^u,
! \end{equation}
! such that a consistent discretisation of the square of the shear in
! $x$-direction should be
! \begin{equation}
!   \label{shearsquared}
! \begin{array}{rcl}
! \displaystyle
!    \left( \partder{U}{z} \right)^2 & \approx &
! \displaystyle
! \frac{P_{j+1/2}}{\nu_{j+1/2}} \\ \\
! &=&
! \displaystyle
! \phantom{+}   \frac12\frac{\sigma \left(\hat U_{j+1}-\hat U_j\right)
! \left(\hat U_{j+1}-U_j\right)+
! (1-\sigma)\left(U_{j+1}-U_j\right)\left(U_{j+1}-\hat U_j\right)}
! {(z_{j+1/2}-z_{j-1/2})(z_{j+1}-z_j)} \\ \\
! &&
! \displaystyle
! +
!  \frac12\frac{\sigma \left(\hat U_{j+1}-\hat U_j\right)
! \left(U_{j+1}-\hat U_j\right)+
! (1-\sigma)\left(U_{j+1}-U_j\right)\left(\hat U_{j+1}-U_j\right)}
! {(z_{j+3/2}-z_{j+1/2})(z_{j+1}-z_j)}.
! \end{array}
! \end{equation}
! The $V$-contribution is computed analogously.
! The shear obtained from \eq{shearsquared}
! plus the $V$-contribution is then used for the computation of the turbulence
! shear production, see equation \eq{computeP}.
!
! !USES:
   use meanflow,   only: h,u,v,uo,vo
   use meanflow,   only: SS,SSU,SSV

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: nlev

!  numerical "implicitness" parameter
   REALTYPE, intent(in)                :: cnpar
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
!
!-----------------------------------------------------------------------
!BOC
!  Discretisation of vertical shear squared according to Burchard (2002)
!  in order to guarantee conservation of kinetic energy when transformed
!  from mean kinetic energy to turbulent kinetic energy.

   do i=1,nlev-1

      SSU(i)= 0.5*(                                                     &
                  (cnpar*(u(i+1)-u(i))*(u(i+1)-uo(i))+                  &
                  (1.-cnpar)*(uo(i+1)-uo(i))*(uo(i+1)-u(i)))            &
                  /(0.5*(h(i+1)+h(i)))/h(i)                             &
                 +(cnpar*(u(i+1)-u(i))*(uo(i+1)-u(i))+                  &
                  (1.-cnpar)*(uo(i+1)-uo(i))*(u(i+1)-uo(i)))            &
                  /(0.5*(h(i+1)+h(i)))/h(i+1)                           &
                  )

      SSV(i)= 0.5*( &
                  (cnpar*(v(i+1)-v(i))*(v(i+1)-vo(i))+                  &
                  (1.-cnpar)*(vo(i+1)-vo(i))*(vo(i+1)-v(i)))            &
                  /(0.5*(h(i+1)+h(i)))/h(i)                             &
                 +(cnpar*(v(i+1)-v(i))*(vo(i+1)-v(i))+                  &
                  (1.-cnpar)*(vo(i+1)-vo(i))*(v(i+1)-vo(i)))            &
                  /(0.5*(h(i+1)+h(i)))/h(i+1)                           &
                  )

      SS(i) = SSU(i) + SSV(i)

   end do

   SSU(0   ) = SSU(1    )
   SSU(nlev) = SSU(nlev-1)

   SSV(0   ) = SSV(1    )
   SSV(nlev) = SSV(nlev-1)

   SS (0   ) = SS (1    )
   SS (nlev) = SS (nlev-1)

   return
   end subroutine shear
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

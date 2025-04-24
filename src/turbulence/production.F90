#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Update turbulence production\label{sec:production}
!
! !INTERFACE:
   subroutine production(nlev,NN,SS,xP, SSCSTK, SSSTK)
!
! !DESCRIPTION:
!  This subroutine calculates the production terms of turbulent kinetic
!  energy as defined in \eq{PandG} and the production of buoyancy
!  variance as defined in \eq{Pbvertical}.
!  The Eulerian shear-production is computed according to
!  \begin{equation}
!    \label{computeP}
!     P = \nu_t (M^2 + \alpha_w N^2) + \nu^S_t S_c^2
!    \comma
!  \end{equation}
!  with the turbulent diffusivity of momentum, $\nu_t$, defined in
!  \eq{nu}. The shear-frequency, $M$, is discretised as described
!  in \sect{sec:shear}.
!   The term multiplied by $\alpha_w$ traces back to
!  a parameterisation of breaking internal waves suggested by
!  \cite{Mellor89}.
!  The turbulent momentum fluxes due to Stokes velocities induce the
!  Stokes-Eulerian cross-shear term
!  $S_c^2 = \frac{\partial u}{\partial z}\frac{\partial u_s}{\partial z} + \frac{\partial v}{\partial z}\frac{\partial v_s}{\partial z}$
!  with corresponding diffusivity $\nu^S_t$, and the additional
!  Stokes shear-production
!  \begin{equation}
!    \label{computePs}
!     P_s = \nu_t S_c^2 + \nu^S_t S_s^2
!  \end{equation}
!  with squared Stokes shear
!  $S_s^2 = \frac{\partial u_s}{\partial z}^2 + \frac{\partial v_s}{\partial z}^2$.
!  $X_P$ is an extra production term, connected for
!  example with turbulence production caused by sea-grass, see
!  \eq{sgProduction} in  \sect{sec:seagrass}. {\tt xP} is an {\tt optional}
!  argument in the FORTRAN code.
!
!  Similarly, according to \eq{PeVertical}, the buoyancy production
!  is computed from the expression
!  \begin{equation}
!   \label{computeG}
!    G=-\nu^B_t N^2 + \tilde{\Gamma}_B
!    \comma
!  \end{equation}
!  with the turbulent diffusivity, $\nu^B_t$, defined in
!  \eq{nu}. The second term in \eq{computeG} represents the non-local
!  buoyancy flux. The buoyancy-frequency, $N$, is discretised as described
!  in \sect{sec:stratification}.
!
!  The production of buoyancy variance by vertical meanflow gradients follows
!  from \eq{PeVertical} and \eq{computeG}
!  \begin{equation}
!   \label{computePb}
!    P_b = -G N^2
!    \point
!  \end{equation}
!  Thus, according to the definition of the potential energy \eq{defkb},
!  the buoyancy production $G$ describes the conversion between turbulent
!  kinetic and potential energy in \eq{tkeA} and \eq{kbeq}, respectively.
!
! !USES:
   use turbulence, only: P,B,Pb,Px,PSTK
   use turbulence, only: num,nuh, nucl
   use turbulence, only: alpha,iw_model
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: nlev

!  boyancy frequency squared (1/s^2)
   REALTYPE, intent(in)                :: NN(0:nlev)

!  shear-frequency squared (1/s^2)
   REALTYPE, intent(in)                :: SS(0:nlev)

!  TKE production due to seagrass
!  friction (m^2/s^3)
   REALTYPE, intent(in), optional      :: xP(0:nlev)

!  Stokes-Eulerian cross-shear (1/s^2)
   REALTYPE, intent(in), optional      :: SSCSTK(0:nlev)

!  Stokes shear squared (1/s^2)
   REALTYPE, intent(in), optional      :: SSSTK (0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!
!EOP
!-----------------------------------------------------------------------
! !LOCAL VARIABLES:
   REALTYPE                      :: alpha_eff
   integer                       :: i
!-----------------------------------------------------------------------
!BOC
   alpha_eff=_ZERO_
   if (iw_model.eq.1) then
      alpha_eff=alpha
   end if

   do i=0,nlev
      P(i)    =  num(i)*( SS(i)+alpha_eff*NN(i) )
      B(i)    = -nuh(i)*NN(i)
      Pb(i)   = -  B(i)*NN(i)
   end do

   if ( PRESENT(xP) ) then
      do i=0,nlev
         Px(i) = xP(i)
      end do
   end if

!  P is -<u'w'>du/dz for q2l production with e1,
!  PSTK is -<u'w'>dus/dz for q2l prodcution with e6,
!  see  (6) in Harcourt2015.
!  -<u'w'> = num*(du/dz) + nucl*(dus/dz), see (7) in Harcourt2015.
   if ( PRESENT(SSCSTK) ) then
      do i=0,nlev
         P(i)    =  P(i) + nucl(i)*SSCSTK(i)
         PSTK(i) =         num (i)*SSCSTK(i)
      end do
   end if
   if ( PRESENT(SSSTK) ) then
      if ( .not. PRESENT(SSCSTK) ) PSTK = _ZERO_
      do i=0,nlev
         PSTK(i) =  PSTK(i) + nucl(i)*SSSTK(i)
      end do
   end if

   return
   end subroutine production
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

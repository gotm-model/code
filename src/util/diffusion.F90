#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Diffusion schemes --- grid centers\label{sec:diffusionMean}
!
! !INTERFACE:
   subroutine diffusion(N, dt, cnpar, posconc, h, Vc, Af, Bcup, Bcdw,  &
                        Yup, Ydw, nuY, Lsour, Qsour, Taur, Yobs, Y)
!
! !DESCRIPTION:
! This subroutine solves the one-dimensional diffusion equation
! including source terms,
!  \begin{equation}
!   \label{YdiffCenter}
!    \partder{Y}{t}
!    = \partder{}{z} \left( \nu_Y \partder{Y}{z} \right)
!    - \frac{1}{\tau_R}(Y-Y_{obs})
!    + Y L_{\text{sour}} + Q_{\text{sour}}
!    \comma
!  \end{equation}
! for al variables defined at the centers of the grid cells, and
! a diffusion coefficient $\nu_Y$ defined at the faces.
! Relaxation with time scale $\tau_R$ towards observed values
! $Y_{\text{obs}}$ is possible. $L_{\text{sour}}$ specifies a
! linear source term, and $Q_{\text{sour}}$ a constant source term.
! Central differences are used to discretize the problem
! as discussed in \sect{SectionNumericsMean}. The diffusion term,
! the linear source term, and the linear part arising from the
! relaxation term are treated
! with an implicit method, whereas the constant source term is treated
! fully explicit.
!
! The input parameters {\tt Bcup} and {\tt Bcdw} specify the type
! of the upper and lower boundary conditions, which can be either
! Dirichlet or Neumann-type. {\tt Bcup} and {\tt Bcdw} must have integer
! values corresponding to the parameters {\tt Dirichlet} and {\tt Neumann}
! defined in the module {\tt util}, see \sect{sec:utils}.
! {\tt Yup} and {\tt Ydw} are the values of the boundary conditions at
! the surface and the bottom. Depending on the values of {\tt Bcup} and
! {\tt Bcdw}, they represent either fluxes or prescribed values.
! The integer {\tt posconc} indicates if a quantity is
! non-negative by definition ({\tt posconc}=1, such as for concentrations)
! or not ({\tt posconc}=0). For {\tt posconc}=1 and negative
! boundary fluxes, the source term linearisation according to
! \cite{Patankar80} is applied.
!
! Note that fluxes \emph{entering} a boundary cell are counted positive
! by convention. The lower and upper position for prescribing these fluxes
! are located at the lowest und uppermost grid faces with index "0" and
! index "N", respectively. If values are prescribed, they are located at
! the centers with index "1" and index "N", respectively.
!
! !USES:
   use util,          only  : Dirichlet, Neumann
   use mtridiagonal

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: N

!  time step (s)
   REALTYPE, intent(in)                :: dt

!  "implicitness" parameter
   REALTYPE, intent(in)                :: cnpar

!  1: non-negative concentration, 0: else
   integer, intent(in)                 :: posconc

!  layer thickness (m)
   REALTYPE, intent(in)                :: h(0:N)

!  cell volume
   REALTYPE, intent(in)                :: Vc(0:N)

!  interface area
   REALTYPE, intent(in)                :: Af(0:N)

!  type of upper BC
   integer,  intent(in)                :: Bcup

!  type of lower BC
   integer,  intent(in)                :: Bcdw

!  value of upper BC
   REALTYPE, intent(in)                :: Yup

!  value of lower BC
   REALTYPE, intent(in)                :: Ydw

!  diffusivity of Y
   REALTYPE, intent(in)                :: nuY(0:N)

!  linear source term
!  (treated implicitly)
   REALTYPE, intent(in)                :: Lsour(0:N)

!  constant source term
!  (treated explicitly)
   REALTYPE, intent(in)                :: Qsour(0:N)

!  relaxation time (s)
   REALTYPE, intent(in)                :: Taur(0:N)

!  observed value of Y
   REALTYPE, intent(in)                :: Yobs(0:N)
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)             :: Y(0:N)
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: dV(0:N), dY(0:N)
   REALTYPE                  :: fac, wup, wdw
!
!-----------------------------------------------------------------------
!BOC
!
!  set up matrix
   dV(0) = _ZERO_
   dY(0) = _ZERO_
   dV(N) = _ZERO_
   dY(N) = _ZERO_
   do concurrent ( i = 1 : N-1 )
      dV(i) = dt * Af(i) * nuY(i) / ( _HALF_ * ( h(i) + h(i+1) ) )
      dY (i) = Y(i+1) - Y(i)
   end do

   do concurrent ( i = 1 : N )
      cu(i) = - cnpar * dV(i  )
      au(i) = - cnpar * dV(i-1)
      bu(i) = Vc(i) * ( _ONE_ - dt*Lsour(i) )                          &
              + cnpar * ( dV(i-1) + dV(i) )
      du(i) = Vc(i) * ( Y(i) + dt*Qsour(i) )                           &
              + ( _ONE_ - cnpar ) * ( dV(i)*dY(i) - dV(i-1)*dY(i-1) )
   end do

!  set up upper boundary condition
   wup = _ZERO_
   select case(Bcup)
   case(Neumann)
      if (posconc.eq.1 .and. Yup.lt._ZERO_) then ! Patankar (1980) trick
         fac = -Y(N) / ( dt * Af(N) * Yup )
         if ( fac.gt.TINY(_ONE_) ) bu(N) = bu(N) + _ONE_/fac
      else
         du(N) = du(N) + dt * Af(N) * Yup
      end if
   case(Dirichlet)
      au(N) = _ZERO_
      bu(N) = Vc(N)
      du(N) = Vc(N) * Yup
      wup = _ONE_
   case default
      FATAL 'invalid boundary condition type for upper boundary'
      stop  'diffusion.F90'
   end select

!  set up lower boundary condition
   wdw = _ZERO_
   select case(Bcdw)
   case(Neumann)
      if (posconc.eq.1 .and. Ydw.lt._ZERO_) then ! Patankar (1980) trick
         fac = -Y(1) / ( dt * Af(1) * Ydw )
         if ( fac.gt.TINY(_ONE_) ) bu(1) = bu(1) + _ONE_/fac
      else
         du(1) = du(1) + dt * Af(1) * Ydw
      end if
   case(Dirichlet)
      cu(1) = _ZERO_
      bu(1) = Vc(1)
      du(1) = Vc(1) * Ydw
      wdw = _ONE_
   case default
      FATAL 'invalid boundary condition type for lower boundary'
      stop  'diffusion.F90'
   end select

   if (N .eq. 1) then
      if (Bcup.eq.Dirichlet .or. Bcdw.eq.Dirichlet) then
!        overwrite all earlier assignments
         bu(1) = Vc(1)
         du(1) = Vc(1) * ( wup*Yup + wdw*Ydw ) / ( wup + wdw )
      end if
   end if

!  relaxation to observed value
   if (minval(Taur).lt.1.d10) then
      do concurrent ( i = 1 : N )
         bu(i) = bu(i) + dt / Taur(i) * Vc(i)
         du(i) = du(i) + dt / Taur(i) * Vc(i) * Yobs(i)
      end do
   end if

!  solve linear system
   call tridiagonal(N, 1, N, Y)

   return
   end subroutine diffusion
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

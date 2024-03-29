#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Advection schemes --- grid centers\label{sec:advectionMean}
!
! !INTERFACE:
   subroutine adv_center(N,dt,h,ho,ww,Bcup,Bcdw,Yup,Ydw,method,mode,Y)
!
! !DESCRIPTION:
!
     !! This subroutine solves a one-dimensional advection equation. There are two
     !! options, depending whether the advection should be conservative or not.
     !! Conservative advection has to be applied when settling of sediment or
     !! rising of phytoplankton is considered. In this case the advection is of
     !! the form
     !!  \begin{equation}
     !!   \label{Yadvection_cons}
     !!    \partder{Y}{t} = - \partder{F}{z}
     !!    \comma
     !!  \end{equation}
     !! where $F=wY$ is the flux caused by the advective velocity, $w$.
     !!
     !! Non-conservative advective transport has to be applied, when the water
     !! has a non-zero vertical velocity. In three-dimensional applications,
     !! this transport would be conservative, since vertical divergence would be
     !! compensated by horizontal convergence and vice versa. However, the
     !! key assumption of one-dimensional modelling is horizontal homogeneity,
     !! such that we indeed have to apply a vertically non-conservative method,
     !! which is of the form
     !!  \begin{equation}
     !!   \label{Yadvection_noncons}
     !!    \partder{Y}{t} = - w\partder{Y}{z}
     !!                   = - \left(\partder{F}{z} - Y\partder{w}{z} \right).
     !!  \end{equation}
     !!
     !! The discretized form of \eq{Yadvection_cons} is
     !!  \begin{equation}
     !!   \label{advDiscretized_cons}
     !!   Y_i^{n+1} = Y_i^n
     !!   - \dfrac{\Delta t}{h_i}
     !!    \left( F^n_{i} - F^n_{i-1} \right)
     !!   \comma
     !!  \end{equation}
     !! where the integers $n$ and $i$ correspond to the present time and space
     !! level, respectively.
     !!
     !! For the non-conservative form \eq{Yadvection_noncons},
     !! an extra term needs to be included:
     !!  \begin{equation}
     !!   \label{advDiscretized_noncons}
     !!   Y_i^{n+1} = Y_i^n
     !!   - \dfrac{\Delta t}{h_i}
     !!    \left( F^n_{i} - F^n_{i-1} -Y_i^n \left(w_k-w_{k-1}  \right)\right).
     !!  \end{equation}
     !!
     !!  Which advection method is applied is decided by the flag {\tt mode},
     !!  which gives conservative advection \eq{advDiscretized_cons}
     !!  for {\tt mode=1} and
     !!  non-conservative advection \eq{advDiscretized_noncons} for {\tt mode=0}.
     !!
     !! Fluxes are defined at the grid faces, the variable $Y_i$ is defined at the
     !!  grid centers. The fluxes are computed in an upstream-biased way,
     !!  \begin{equation}
     !!   \label{upstream}
     !!   F^n_{i} = \dfrac{1}{\Delta t}
     !!   \int_{z^\text{Face}_{i} - w \Delta t}^{z^\text{Face}_{i}} Y(z') dz'
     !!   \point
     !!  \end{equation}
     !!  For a third-order polynomial approximation of $Y$ (see \cite{Pietrzak98}),
     !!  these fluxes can be written the in so-called Lax-Wendroff form as
     !!  \begin{equation}
     !!   \label{fluxDiscretized}
     !!    \begin{array}{rcll}
     !!      F_{i} &=& w_{i} \left(Y_i +  \dfrac{1}{2} \Phi^+_{i}
     !!      \left(1-\magn{c_{i}} \right) \left( Y_{i+1} - Y_i \right) \right)
     !!      \quad & \text{for} \quad w_{i} > 0
     !!      \comma  \\[5mm]
     !!      F_{i} &=& w_{i} \left(Y_{i+1} +  \dfrac{1}{2} \Phi^-_{i}
     !!      \left(1-\magn{c_{i}} \right) \left( Y_i - Y_{i+1} \right) \right)
     !!      \quad & \text{for} \quad w_{i} < 0
     !!      \comma
     !!    \end{array}
     !!  \end{equation}
     !!  where $c_{i} = 2 w_{i} \Delta t / (h_i+h_{i+1})$ is the Courant number.
     !!  The factors appearing in \eq{fluxDiscretized} are defined as
     !!  \begin{equation}
     !!   \label{phiDiscretized}
     !!  \Phi^+_{i} =  \alpha_{i} +  \beta_{i}  r^+_{i}
     !!  \comma
     !!  \Phi^-_{i} =  \alpha_{i} +  \beta_{i}  r^-_{i}
     !!  \comma
     !!  \end{equation}
     !!  where
     !!  \begin{equation}
     !!   \label{alphaDiscretized}
     !!   \alpha_{i} = \dfrac{1}{2}
     !!    + \dfrac{1}{6} \left( 1- 2 \magn{c_{i}} \right) \comma
     !!   \beta_{i} = \dfrac{1}{2}
     !!    - \dfrac{1}{6} \left( 1- 2 \magn{c_{i}} \right)
     !!  \point
     !!  \end{equation}
     !!  The upstream and downstream slope parameters are
     !!  \begin{equation}
     !!   \label{slopeDiscretized}
     !!   r^+_{i} = \dfrac{Y_i - Y_{i-1}}{Y_{i+1}-Y_{i}}  \comma
     !!   r^-_{i} = \dfrac{Y_{i+2} - Y_{i+1}}{Y_{i+1}-Y_{i}}
     !!  \point
     !!  \end{equation}
!
     !!  To obtain monotonic and positive schemes also in the presence of strong
     !!  gradients, so-called slope limiters are aplied for the factors $\Phi^+_{i}$
     !!  and $\Phi^-_{i}$. The two most obvious cases are
     !!  the first-order upstream discretisation with $\Phi^+_{i}=\Phi^-_{i}=0$
     !!  and the Lax-Wendroff scheme with  $\Phi^+_{i}=\Phi^-_{i}=1$.
     !!  The subroutine {\tt adv\_center.F90} provides six different slope-limiters,
     !!  all discussed in detail by \cite{Pietrzak98}:
!
     !! \begin{itemize}
     !!  \item first-order upstream ({\tt method=UPSTREAM})
     !!  \item second-order upstream-biased polynomial scheme ({\tt method=P1},
     !!        not yet implemented)
     !!  \item third-order upstream-biased polynomial scheme ({\tt method=P2})
     !!  \item third-order scheme (TVD) with Superbee limiter ({\tt method=Superbee})
     !!  \item third-order scheme (TVD) with MUSCL limiter ({\tt method=MUSCL})
     !!  \item third-order scheme (TVD) with ULTIMATE QUICKEST limiter
     !!        ({\tt method=P2\_PDM})
     !! \end{itemize}
     !!
     !!
     !! If during a certain time step the maximum Courant number is larger
     !! than one, a split iteration will be carried out which guarantees that the
     !! split step Courant numbers are just smaller than 1.
     !!
     !! Several kinds of boundary conditions are implemented for the upper
     !! and lower boundaries. They are set by the integer values {\tt Bcup}
     !! and {\tt Bcdw}, that have to correspond to the parameters defined
     !! in the module {\tt util}, see \sect{sec:utils}. The
     !! following choices exist at the moment:
     !!
     !! For the value {\tt flux}, the boundary values {\tt Yup} and {\tt Ydw} are
     !! interpreted as specified fluxes at the uppermost and lowest interface.
     !! Fluxes into the boundary cells are counted positive by convention.
     !! For the value {\tt value}, {\tt Yup} and {\tt Ydw} specify the value
     !! of $Y$ at the interfaces, and the flux is computed by multiplying with
     !! the (known) speed  at the interface. For the value {\tt oneSided},
     !! {\tt Yup} and {\tt Ydw} are ignored and the flux is computed
     !! from a one-sided first-order upstream discretisation using the speed
     !! at the interface and the value of $Y$ at the center of the boundary cell.
     !! For the value {\tt zeroDivergence}, the fluxes into and out of the
     !! respective boundary cell are set equal.
     !! This corresponds to a zero-gradient formulation, or to zero
     !! flux divergence in the boundary cells.
     !!
     !! Be careful that your boundary conditions are mathematically well defined.
     !! For example, specifying an inflow into the boundary cell with the
     !! speed at the boundary being directed outward does not make sense.
!
!
! !USES:
   use util
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: N

!  time step (s)
   REALTYPE, intent(in)                :: dt

!  layer thickness (m)
   REALTYPE, intent(in)                :: h(0:N)

!  old layer thickness (m)
   REALTYPE, intent(in)                :: ho(0:N)

!  vertical advection speed
   REALTYPE, intent(in)                :: ww(0:N)

!  type of upper BC
   integer,  intent(in)                :: Bcup

!  type of lower BC
   integer,  intent(in)                :: Bcdw

!  value of upper BC
   REALTYPE, intent(in)                :: Yup

!  value of lower BC
   REALTYPE, intent(in)                :: Ydw

!  type of advection scheme
   integer,  intent(in)                :: method

!  advection mode (0: non-conservative, 1: conservative)
   integer,  intent(in)                :: mode
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE, intent(inout)             :: Y(0:N)
!
! !DEFINED PARAMETERS:
   REALTYPE,     parameter             :: one6th=1.0d0/6.0d0
   integer,      parameter             :: itmax=100
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   integer                              :: i,k,it
   REALTYPE                             :: Yu,Yc,Yd
   REALTYPE                             :: c,cmax
   REALTYPE                             :: cu(0:N)
!
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(*,*) 'adv_center # ',Ncall
#endif

!  initialize interface fluxes with zero
   cu   = _ZERO_

!  initialize maximum Courant number
   cmax = _ZERO_

!  compute maximum Courant number
   do k=1,N-1
      c=abs(ww(k))*dt/(0.5*(h(k)+h(k+1)))
      if (c.gt.cmax) cmax=c
   enddo

   it=min(itmax,int(cmax)+1)

#ifdef DEBUG
   if (it .gt. 1) then
      STDERR 'In adv_center():'
      STDERR 'Maximum Courant number is ',cmax
      STDERR it,' iterations used for vertical advection'
   endif
#endif
   if (it .eq. itmax) then
      STDERR 'adv_center: clipped to maximum number of iterations'
      STDERR '            cfl=',real(cmax/itmax)
   end if

!  splitting loop
   do i=1,it

!     vertical loop
      do k=1,N-1

!        compute the slope ration
         if (ww(k) .gt. _ZERO_) then

!           compute Courant number
            c=ww(k)/float(it)*dt/(0.5*(h(k)+h(k+1)))

            if (k .gt. 1) then
               Yu=Y(k-1)                              ! upstream value
            else
               Yu=Y(k)
            end if
            Yc=Y(k  )                                 ! central value
            Yd=Y(k+1)                                 ! downstream value

!        negative speed
         else

!           compute Courant number
            c=-ww(k)/float(it)*dt/(0.5*(h(k)+h(k+1)))

            if (k .lt. N-1) then
               Yu=Y(k+2)                              ! upstream value
            else
               Yu=Y(k+1)
            end if
            Yc=Y(k+1)                                 ! central value
            Yd=Y(k  )                                 ! downstream value

         end if

!        compute the limited flux
         Yc = adv_reconstruct(method,c,Yu,Yc,Yd)
         cu(k) = ww(k) * Yc

      end do

!     do the upper boundary conditions
      select case (Bcup)
      case (flux)
         cu(N) = - Yup              ! flux into the domain is positive
      case (value)
         cu(N) =  ww(N)*Yup
      case (oneSided)
         if (ww(N).ge._ZERO_) then
            cu(N) =  ww(N)*Y(N)
         else
            cu(N) = _ZERO_
         end if
      case (zeroDivergence)
         cu(N) = cu(N-1)
      case default
         FATAL 'unkown upper boundary condition type in adv_center()'
         stop
      end select


!     do the lower boundary conditions
      select case (Bcdw)
      case (flux)
         cu(0) =   Ydw               ! flux into the domain is positive
      case (value)
         cu(0) =  ww(0)*Ydw
      case (oneSided)
         if(ww(0).le._ZERO_) then
            cu(0) =  ww(0)*Y(1)
         else
            cu(0) = _ZERO_
         end if
      case (zeroDivergence)
         cu(0) = cu(1)
      case default
         FATAL 'unkown lower boundary condition type in adv_center()'
         stop
      end select

!     do the vertical advection step which will be used for prescribed
!     vertical flow velocity and for settling of suspended matter.

      if (mode.eq.0) then ! non-conservative
         do k=1,N
            Y(k)=Y(k)-1./float(it)*dt*((cu(k)-cu(k-1))/        &
                 h(k)-Y(k)*(ww(k)-ww(k-1))/h(k))
         enddo
      else                ! conservative
         do k=1,N
            Y(k)=Y(k)-1./float(it)*dt*((cu(k)-cu(k-1))/h(k))
         enddo
      end if

   end do ! end of the iteration loop


#ifdef DEBUG
   write(*,*) 'Leaving adv_center()'
   write(*,*)
#endif

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: adv_reconstruct -
!
! !INTERFACE:
   REALTYPE function adv_reconstruct(scheme,cfl,fuu,fu,fd)
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,intent(in)  :: scheme
   REALTYPE,intent(in) :: cfl,fuu,fu,fd
!
! !DEFINED PARAMETERS:
#define _TWO_ 2.0d0
   REALTYPE,parameter :: one3rd=_ONE_/3
   REALTYPE,parameter :: one6th=_ONE_/6
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE           :: ratio,limiter,x,deltaf,deltafu
!
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(*,*) 'adv_reconstruct # ',Ncall
#endif

   deltaf  = fd - fu
   deltafu = fu - fuu

   if (deltaf*deltafu .gt. _ZERO_) then

      ratio = deltafu / deltaf   ! slope ratio

      select case (scheme)
         case (SUPERBEE)
            limiter = max(min(_TWO_*ratio,_ONE_),min(ratio,_TWO_))
         case (P2_PDM)
            x = one6th*(_ONE_-_TWO_*cfl)
            limiter = (_HALF_+x) + (_HALF_-x)*ratio
            limiter = min(_TWO_*ratio/(cfl+1.d-10),limiter,_TWO_/(_ONE_-cfl))
         case (SPLMAX13)
            limiter = min(_TWO_*ratio,one3rd*max(_ONE_+_TWO_*ratio,_TWO_+ratio),_TWO_)
         case (MUSCL)
            limiter = min(_TWO_*ratio,_HALF_*(_ONE_+ratio),_TWO_)
         case (P2)
            x = one6th*(_ONE_-_TWO_*cfl)
            limiter = (_HALF_+x) + (_HALF_-x)*ratio
         case (CENTRAL)
            limiter = _ONE_ / ( _ONE_ -cfl )
         case default
!           UPSTREAM
            limiter = _ZERO_
      end select

      adv_reconstruct = fu + _HALF_*limiter*(_ONE_-cfl)*deltaf

   else

      select case (scheme)
         case (P2)
            x = one6th*(_ONE_-_TWO_*cfl)
!           limiter formulation invalid for deltaf=0
            adv_reconstruct = &
                        fu + _HALF_*(_ONE_-cfl)*(  (_HALF_+x)*deltaf   &
                                                 + (_HALF_-x)*deltafu )
         case (CENTRAL)
            adv_reconstruct = _HALF_ * ( fu + fd )
         case default
!           UPSTREAM
            adv_reconstruct = fu
      end select

   end if

#ifdef DEBUG
   write(*,*) 'Leaving adv_reconstruct()'
   write(*,*)
#endif

   end function adv_reconstruct
!EOC
!-----------------------------------------------------------------------

   end subroutine adv_center
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

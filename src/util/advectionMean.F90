!$Id: advectionMean.F90,v 1.1 2004-01-13 08:30:53 lars Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: A collection of advection routines for meanflow variables \label{sec:advectionMean}
!
! !INTERFACE:
   subroutine advectionMean(N,dt,h,ww,Bcup,Bcdw,Yup,Ydw,method,Y)
!
! !DESCRIPTION:
!
! This subroutine solves a one-dimensional advection equation of the form
!  \begin{equation}
!   \label{Yadvection}
!    \partder{Y}{t} = \partder{F}{z}  
!    \comma
!  \end{equation}
! where $F=wY$ is the flux caused by the advective velocity $w$.
!
! The discretized form of \eq{Yadvection} is
!  \begin{equation}
!   \label{advDiscretized}
!   Y_i^{n+\frac{1}{2}} = Y_i^n 
!   - \dfrac{\Delta t}{\Delta z}
!    \left( F^n_{i+\frac{1}{2}} - F^n_{i-\frac{1}{2}} \right)
!   \comma
!  \end{equation}
! where the integers $n$ and $i$ correspond to the present time and space level, 
! respectively. The fluxes are computed in an upstream-biased way,
!  \begin{equation}
!   \label{upstream}
!   F^n_{i+\frac{1}{2}} = \dfrac{1}{\Delta t} 
!   \int_{x_{i+\frac{1}{2}} - w \Delta x}^{x_{i+\frac{1}{2}}} Y(z') dz'
!   \point
!  \end{equation}
!  For a third-order polynomial approximation of $Y$ (see \cite{Pietrzak98}), 
!  these fluxes can be written in so-called Lax-Wendroff form as
!  \begin{equation}
!   \label{fluxDiscretized}
!    \begin{array}{rcll}
!      F_{i+\frac{1}{2}} &=& w_{i+\frac{1}{2}} \left(Y_i +  \dfrac{1}{2} \Phi^+_{i+\frac{1}{2}}  
!      \left(1-\magn{c_{i+\frac{1}{2}}} \right) \left( Y_{i+1} - Y_i \right) \right) 
!      \quad & \text{for} \quad w_{i+\frac{1}{2}} > 0   
!      \comma  \\[5mm]
!      F_{i+\frac{1}{2}} &=& w_{i+\frac{1}{2}} \left(Y_{i+1} +  \dfrac{1}{2} \Phi^-_{i+\frac{1}{2}}  
!      \left(1-\magn{c_{i+\frac{1}{2}}} \right) \left( Y_i - Y_{i+1} \right) \right)
!      \quad & \text{for} \quad w_{i+\frac{1}{2}} < 0  
!      \comma
!    \end{array}
!  \end{equation}
!  where $c_{i+1/2} = w_{i+1/2} \Delta t / \Delta x$ is the Courant number. The factors appearing
!  in \eq{fluxDiscretized} are defined as
!  \begin{equation}
!   \label{phiDiscretized}
!  \Phi^+_{i+\frac{1}{2}} =  \alpha_{i+\frac{1}{2}} +  \beta_{i+\frac{1}{2}}  r^+_{i+\frac{1}{2}}   
!  \comma
!  \Phi^-_{i+\frac{1}{2}} =  \alpha_{i+\frac{1}{2}} +  \beta_{i+\frac{1}{2}}  r^-_{i+\frac{1}{2}}   
!  \comma
!  \end{equation}
!  where
!  \begin{equation}
!   \label{alphaDiscretized}
!   \alpha_{i+\frac{1}{2}} = \dfrac{1}{2} 
!    + \dfrac{1}{6} \left( 1- 2 \magn{c_{i+\frac{1}{2}}} \right) \comma   
!   \beta_{i+\frac{1}{2}} = \dfrac{1}{2} 
!    - \dfrac{1}{6} \left( 1- 2 \magn{c_{i+\frac{1}{2}}} \right) 
!  \point
!  \end{equation}
!  The upstream and downstream slope parameters are 
!  \begin{equation}
!   \label{slopeDiscretized}
!   r^+_{i+\frac{1}{2}} = \dfrac{Y_i - Y_{i-1}}{Y_{i+1}-Y_{i}}  \comma
!   r^-_{i+\frac{1}{2}} = \dfrac{Y_{i+2} - Y_{i+1}}{Y_{i+1}-Y_{i}}  
!  \point
!  \end{equation}
!
!  To obtain monotonic and postive schemes also in the presence of strong
!  gradients, so-called slope limiters are aplied for the factors $\Phi^+_{i+\frac{1}{2}}$ 
!  and $\Phi^-_{i+\frac{1}{2}}$. The two most obvious cases are 
!  the first-order upstream discretisation with $\Phi^+_{i+\frac{1}{2}}=\Phi^-_{i+\frac{1}{2}}=0$
!  and the Lax-Wendroff scheme with  $\Phi^+_{i+\frac{1}{2}}=\Phi^-_{i+\frac{1}{2}}=1$. 
! {\tt advectionMean.F90} provides six different slope-limiters, all discussed in detail in 
! \cite{Pietrzak98}:
!
! \begin{itemize}
!  \item first-order upstream ({\tt method=1})
!  \item second-order upstream-biased polynomial scheme ({\tt method=2}, not yet implemented)
!  \item third-order upstream-biased polynomial scheme ({\tt method=3})
!  \item third-order scheme (TVD) with Superbee limiter ({\tt method=4})
!  \item third-order scheme (TVD) with MUSCL limiter ({\tt method=5})
!  \item third-order scheme (TVD) with ULTIMATE QUICKEST limiter ({\tt method=6})
! \end{itemize}
!
!
! If during a certain time step the maximum Courant number is larger 
! than one, an iteration will be carried out which guarantees that the
! split step Courant numbers are just smaller than zero.
! For details, see the GETM report by \cite{BurchardBolding2002}.
!
! Several kinds of boundary conditions are implemented for the upper and lower boundaries.
! They are set by the values of {\tt Bcup} and {\tt Bcdw}, respectively. For a value of 1,
! the boundary values {\tt Yup} and {\tt Ydw} are interpreted as specified fluxes at the uppermost
! and lowest interface. Fluxes into the boundary cells are counted positive by convention.
! For a value of 2, {\tt Yup} and {\tt Ydw} specify the value 
! of $Y$ at the interfaces, and the flux is computed by multiplying with the (known) speed 
! at the interface. For a value of 3, {\tt Yup} and {\tt Ydw} are ignored and the flux is computed
! from a one-sided first-order upstream discretisation using the speed at the interface and the value
! of $Y$ at the center of the boundary cell. For a value of 4, the fluxes into and out of the
! boundary cells are set equal. This corresponds to a zero-gradient formulation, or to zero
! flux divergence in the boundary cells.
!
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                 :: N
   REALTYPE, intent(in)                 :: dt
   REALTYPE, intent(in)                 :: h(0:N)
   REALTYPE, intent(in)                 :: ww(0:N)
   integer,  intent(in)                 :: Bcup
   integer,  intent(in)                 :: Bcdw
   REALTYPE, intent(in)                 :: Yup
   REALTYPE, intent(in)                 :: Ydw
   integer,  intent(in)                 :: method
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE                             :: Y(0:N)
!
! !DEFINED PARAMETERS:
   integer,parameter                    :: UPSTREAM       = 1
   integer,parameter                    :: P1             = 2
   integer,parameter                    :: P2             = 3
   integer,parameter                    :: Superbee       = 4
   integer,parameter                    :: MUSCL          = 5
   integer,parameter                    :: P2_PDM         = 6


   integer,parameter                    :: flux           = 1
   integer,parameter                    :: value          = 2
   integer,parameter                    :: oneSided       = 3
   integer,parameter                    :: zeroDivergence = 4

   integer,parameter                    :: one6th=0.1666666666d0
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!  $Log: advectionMean.F90,v $
!  Revision 1.1  2004-01-13 08:30:53  lars
!  added new advection routine for meanflow variables
!
! 
!EOP
!
! !LOCAL VARIABLES:
   integer                              :: ii,k,it
   REALTYPE                             :: c,alpha,beta,x,r,Phi,limit,Yu,Yc,Yd,cmax
   REALTYPE                             :: cu(0:N)
   logical                              :: READY
!
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(*,*) 'advectionMean # ',Ncall
#endif

!  set interface fluxes to zero
   cu = _ZERO_

! Calculating w-interface fluxes 
   cmax= _ZERO_
   it=1
   ready=.false.
111  do ii=1,it    ! loop for iteration in case of high Courant numbers
      do k=1,N-1
         cu(k) = _ZERO_

!        compute the slope ration
         if (ww(k) .gt. _ZERO_) then

!           compute Courant number
            c=ww(k)/float(it)*dt/(0.5*(h(k)+h(k+1)))  
            if (c.gt.cmax) cmax=c

            if (k .gt. 1) then
               Yu=Y(k-1)                              ! upstream value
            else
               Yu=Y(k)
            end if
            Yc=Y(k  )                                 ! central value 
            Yd=Y(k+1)                                 ! downstream value

!           compute slope ration
            if (abs(Yd-Yc) .gt. 1e-10) then
               r=(Yc-Yu)/(Yd-Yc) 
            else
               r=(Yc-Yu)*1.e10
            end if

!        negative speed
         else

!           compute Courant number
            c=-ww(k)/float(it)*dt/(0.5*(h(k)+h(k+1)))
            if (c.gt.cmax) cmax=c


            if (k .lt. N-1) then
               Yu=Y(k+2)                              ! upstream value
            else
               Yu=Y(k+1)
            end if
            Yc=Y(k+1)                                 ! central value
            Yd=Y(k  )                                 ! downstream value


!           compute slope ratio
            if (abs(Yc-Yd) .gt. 1e-10) then
               r=(Yu-Yc)/(Yc-Yd)
            else
               r=(Yu-Yc)*1.e10
            end if

         end if

!        compute the flux-factor phi
         x    =  one6th*(1.-2.0*c)
         Phi  =  (0.5+x)+(0.5-x)*r

!       limit the flux according to different suggestions
         select case (method)
            case (UPSTREAM)
               limit=0.
            case (P1)
               FATAL "P1 advection method not yet implemented, choose other method"
               stop  "advectionMean.F90"
            case ((P2),(P2_PDM))
               if (method.eq.P2) then
                  limit=Phi 
               else
                  limit=max(_ZERO_,min(Phi,2./(1.-c),2.*r/(c+1.e-10)))
               end if
            case (Superbee)
               limit=max(_ZERO_, min(_ONE_, 2.0*r), min(r,2.*_ONE_) )
            case (MUSCL)
               limit=max(_ZERO_,min(2.*_ONE_,2.0*r,0.5*(1.0+r)))
            case default
               FATAL 'unkown advection method in advectionMean()'
               stop
          end select

!        compute the limited flux
         cu(k)=ww(k)*(Yc+0.5*limit*(1-c)*(Yd-Yc))

      end do

      if (.not.READY) then
         it=min(200,int(cmax)+1)
         if (it.gt.1)  &
            STDERR it,' iterations for advection, Courant number is',cmax
      end if
      if ((it.gt.1).and.(.not.READY)) then
         READY=.true.
         goto 111
      end if



!     do the boundary conditions
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
         FATAL 'unkown upper boundary condition type in advectionMean()'
         stop
      end select


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
         FATAL 'unkown lower boundary condition type in advectionMean()'
         stop
      end select

!     do the advection step
      do k=1,N                       
         Y(k)=(Y(k)*h(k)-1./float(it)*dt*(cu(k)-cu(k-1)))/h(k)
      end do

   end do ! end of the iteration loop


#ifdef DEBUG
   write(*,*) 'Leaving advectionMean()'
   write(*,*)
#endif

   return
   end subroutine advectionMean
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

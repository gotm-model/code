!$Id: diffusion.F90,v 1.1 2004-01-12 09:59:00 lars Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The tracer diffusion equation \label{sec:diffusion}
! 
! !INTERFACE:
   subroutine diffusion(N,dt,cnpar,h,Bcup,Bcdw,          &
                        Yup,Ydw,nuY,Qsour,Taur,Yobs,Y)
!
! !DESCRIPTION:
! This subroutine solves the one-dimensional diffusion equation 
! plus a source term, 
!  \begin{equation}
!   \label{YEq}
!    \partder{Y}{t}
!    = \partder{}{z} \left( \nu_Y \partder{Y}{z} \right) 
!    - \frac{1}{\tau_R}(Y-Y_{obs})
!    + Q_{\text{sour}}
!    \comma
!  \end{equation}
! for variables located at the centers of the grid cells and the 
! diffusion coefficient $\nu_Y$ at the faces. 
! Relaxation with time scale $\tau_R$ towards observed values 
! $Y_{\text{obs}}$ is possible. 
!  Central differences are used to discretize the problem 
! as discussed in \sect{SectionNumericsMean}. Since the subroutine applies
! the method of \cite{Patankar80} to ensure the positiveness of variables,
! it should only be used with variables whose differential equations 
! guaranty positiveness.
!
! The input parameters {\tt Bcup} and {\tt Bcdw} specify the type
! of the upper and lower boundary conditions and can have the values
! $0$ or $1$ for Dirichlet or Neumann boundary conditions, respectively.
! {\tt Yup} and {\tt Ydw} are the values of the boundary conditions at
! the surface and the bottom. Depending on the values of {\tt Bcup} and 
! {\tt Bcdw}, they represent either fluxes or prescribed values. Note that 
! fluxes entering a boundary cell are counted positive by convention.  
! 
!
! !USES:
   use mtridiagonal
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: N
   REALTYPE, intent(in)                :: dt,cnpar
   REALTYPE, intent(in)                :: h(0:N)
   integer,  intent(in)                :: Bcup,Bcdw
   REALTYPE, intent(in)                :: Yup,Ydw
   REALTYPE, intent(in)                :: nuY(0:N)
   REALTYPE, intent(in)                :: Qsour(0:N)
   REALTYPE, intent(in)                :: Taur(0:N)
   REALTYPE, intent(in)                :: Yobs(0:N)
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE                            :: Y(0:N)
!
! !DEFINED PARAMETERS:

!  boundary conditions
   integer,parameter                   :: Dirichlet  =  0 
   integer,parameter                   :: Neumann    =  1
!
! !REVISION HISTORY: 
!  Original author(s): Lars Umlauf
!
!  $Log: diffusion.F90,v $
!  Revision 1.1  2004-01-12 09:59:00  lars
!  new routine for diffusion of positive variables
!
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: a,c
!
!-----------------------------------------------------------------------
!BOC
!
!  Array of Diffusion/Advection terms + sources 
!  => [a b c].X=[d] where X=[2, ...,i,i+1,...N-1]
   do i=2,N-1
      c    =2*dt*nuY(i)  /(h(i)+h(i+1))/h(i) 
      a    =2*dt*nuY(i-1)/(h(i)+h(i-1))/h(i)
      cu(i)=-cnpar*c                                !i+1,n+1
      au(i)=-cnpar*a                                !i-1,n+1
      bu(i)=1-au(i)-cu(i)                           !i  ,n+1
      du(i)=Y(i)+dt*Qsour(i)               &        !i  ,n
            +(1-cnpar)*(a*Y(i-1)-(a+c)*Y(i)+c*Y(i+1))
    end do

!  Surface 
! => [a b /].X=[d] where X=[N-1,N,/]
    select case(Bcup)
    case(Neumann)
       a      =2*dt*nuY(N-1)/(h(N)+h(N-1))/h(N)
       au(N)=-cnpar*a
       if (Yup.lt.0.) then
          bu(N)=1.-au(N)
          du(N)=Y(N)+dt*(Qsour(N)-Yup/h(N))+(1.-cnpar)*a*(Y(N-1)-Y(N))
       else ! Patankar (1980) trick
          bu(N)=1.-au(N)+dt*Yup/Y(n)/h(N)
          du(N)=Y(N)+dt*Qsour(N)+(1.-cnpar)*a*(Y(N-1)-Y(N))
       end if
    case(Dirichlet) 
       au(N)=0.
       bu(N)=1.
       du(N)=Yup
    case default
       FATAL 'invalid boundary condition type for Bcup'
       stop  'diffusion.F90'
    end select

!  Bottom  
! => [/ b c].X=[d] where X=[/,1,2]
    select case(Bcdw)
    case(Neumann)
       c    =2*dt*nuY(1)/(h(1)+h(2))/h(1)
       cu(1)=-cnpar*c 
       if (Ydw.gt.0) then
          bu(1)=1.-cu(1)
          du(1)=Y(1)+dt*(Qsour(1)+Ydw/h(1))+(1-cnpar)*c*(Y(2)-Y(1))
       else ! Patankar (1980) trick
          bu(1)=1.-cu(1)-dt*Ydw/Y(1)/h(1)
          du(1)=Y(1)+dt*Qsour(1)+(1-cnpar)*c*(Y(2)-Y(1))
       end if
    case(Dirichlet)
       cu(1)=0.
       bu(1)=1.
       du(1)=Ydw
    case default
       FATAL 'invalid boundary condition type for Bcdw'
       stop  'diffusion.F90'
    end select

!  Implicit internal restoring
   do i=1,N
      if (Taur(i).lt.1.E10) then
         bu(i)=bu(i)+dt/Taur(i)
         du(i)=du(i)+dt/Taur(i)*Yobs(i)
      end if
   end do

!  Implicit vertical mixing
   call tridiagonal(N,1,N,Y)

   return
   end subroutine diffusion
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

!$Id: uequation.F90,v 1.3 2001-05-31 12:00:52 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The u-momentum equation. 
!
! !INTERFACE:
   subroutine uequation(nlev,dt,cnpar,tx,num,Method)
!
! !DESCRIPTION:
!  This subroutine calculates the diffusion equation for x-velocity $u$:
!
!  \begin{equation}\label{uEq}
!  \partial_tu
!  -\partial_z((\nu_t+\nu) \partial_z u)  =
!  -g\partial_x \zeta 
!  \end{equation}
!
!  Coriolis rotation is already carried out in coriolis().
!     
!  Pressure gradient is applied here only if surface slopes are
!  directly given. If velocities at a certain height or the depth
!  mean velocities are given, the pressure gradient is applied in
!  pressuregradient(). 
!
!  Bottom friction is implemented implicitely. Implicit friction may indeed be
!  applied on all levels in order to allow for inner friction terms such
!  as seagrass friction.
!
!  Diffusion is numerically treated implicitly.
!  The tri-diagonal matrix is solved then by a simplified Gauss elimination.
!
!  The $u$-contribution to shear frequency squared is now also
!  computed here, using a new scheme which guarantees energy conservation
!  of kinetic from mean to turbulent flow (see {\it Burchard} [1995]):
!
!  \begin{equation}\label{prod_diskret}
!  \begin{array}{l}
!  \!\!\!\!\!\!\!\!\!\!\!\!\!
!  P_{j+1/2}= \nu_{j+1/2}
!  \\ \\
!  \!\!\!\!\!\!\!\!\!\!\!\!\!
!  \times
!  \frac12\bigg[
!   \frac{\sigma(\hat u_{j+1}-\hat u_j)(\hat u_{j+1}-u_j)+(1-\sigma)
!   (u_{j+1}-u_j)(u_{j+1}-\hat u_j)}{(z_{j+1/2}-z_{j-1/2})(z_{j+1}-z_j)}
!  \\ \\
!  \!\!\!\!\!\!\!\!\!\!\!\!\!
!  \quad + \frac{\sigma(\hat u_{j+1}-\hat u_j)( u_{j+1}-\hat u_j)+(1-\sigma)
!   (u_{j+1}-u_j)(\hat u_{j+1}-u_j)}{(z_{j+3/2}-z_{j+1/2})(z_{j+1}-z_j)}
!  \bigg]
!  \end{array}
!  \end{equation}
!
!  where the indices $j-1/2=i-1$, $j+1/2=i$, $j+1=i+1$, $j+3/2=i+1$. This
!  schemes corrects the slightly erroneous scheme used before.
!
! !USES:
   use meanflow, only : gravity,avmolu
   use meanflow, only : h,u,v,avh,drag,SS
   use observations, only : idpdx,dpdx
#ifdef NUDGE_VEL
   use observations, only : uprof
#endif
   use mtridiagonal
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev,Method
   REALTYPE, intent(in)	:: dt
   REALTYPE, intent(in)	:: cnpar
   REALTYPE, intent(in)	:: tx
   REALTYPE, intent(in)	:: num(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: uequation.F90,v $
!  Revision 1.3  2001-05-31 12:00:52  gotm
!  Correction in the calculation of the shear squared calculation - now according
!  to Burchard 1995 (Ph.D. thesis).
!  Also some cosmetics and cleaning of Makefiles.
!
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE		:: a,c
   REALTYPE		:: uo(0:nlev)
#ifdef NUDGE_VEL
   REALTYPE		:: tau=900.
#endif
!
!EOP
!-----------------------------------------------------------------------
!BOC
   avh=num+avmolu
   do i=2,nlev-1
      c    =2*dt*avh(i)  /(h(i)+h(i+1))/h(i)
      a    =2*dt*avh(i-1)/(h(i)+h(i-1))/h(i)
      cu(i)=-cnpar*c				!i+1,n+1
      au(i)=-cnpar*a				!i-1,n+1
#ifndef SEAGRASS
      bu(i)=1-au(i)-cu(i)			!i  ,n+1
#else
      bu(i)=1-au(i)-cu(i)	&		!i  ,n+1
            + drag(i)*dt/h(i)*sqrt(u(i)*u(i)+v(i)*v(i))
#endif
      du(i)=u(i)+(1-cnpar)*(a*u(i-1)-(a+c)*u(i)+c*u(i+1))	!i  ,n
      du(i)=du(i)+dt*idpdx(i)
   end do


   c    =2*dt*avh(1)/(h(1)+h(2))/h(1)
   cu(1)=-cnpar*c
   bu(1)=1-cu(1)+drag(1)*dt/h(1)*sqrt(u(1)*u(1)+v(1)*v(1))
   du(1)=u(1)+(1-cnpar)*c*(u(2)-u(1))+dt*idpdx(1)
   
   a    =2*dt*avh(nlev-1)/(h(nlev)+h(nlev-1))/h(nlev)
   au(nlev)=-cnpar*a
#ifndef SEAGRASS
   bu(nlev)=1-au(nlev)
#else
   bu(nlev)=1-au(nlev)		&
            + drag(nlev)*dt/h(nlev)*sqrt(u(nlev)*u(nlev)+v(nlev)*v(nlev))
#endif
   du(nlev)=u(nlev)+tx*dt/h(nlev)+(1-cnpar)*a*(u(nlev-1)-u(nlev))
   du(nlev)=du(nlev)+dt*idpdx(nlev) 

   if (Method .eq. 0) then
      du=du-dt*gravity*dpdx
   end if 

   uo = u
#ifdef NUDGE_VEL
   do i=1,nlev
      bu(i)=bu(i)+dt/tau
      du(i)=du(i)+dt/tau*uprof(i)
   end do
#endif
   call tridiagonal(nlev,1,nlev,u)

!  Discretisation of vertiacal shear squared according to Burchard 1995
!  in order to guarantee conservation of kinetic energy when transformed
!  from mean kinetic energy to turbulent kinetic energy.
 
   do i=1,nlev-1
      SS(i)=SS(i)+0.5*(                                              &
                  (cnpar*(u(i+1)-u(i))*(u(i+1)-uo(i))+               &
                  (1.-cnpar)*(uo(i+1)-uo(i))*(uo(i+1)-u(i)))         &
                  /(0.5*(h(i+1)+h(i)))/h(i)                          &
                 +(cnpar*(u(i+1)-u(i))*(uo(i+1)-u(i))+               &
                  (1.-cnpar)*(uo(i+1)-uo(i))*(u(i+1)-uo(i)))         &
                  /(0.5*(h(i+1)+h(i)))/h(i+1)                        &
                  )
   end do 

   SS(0)=SS(1)
   SS(nlev)=SS(nlev-1)

   return
   end subroutine uequation 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

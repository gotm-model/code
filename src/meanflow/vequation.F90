!$Id: vequation.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The v-momentum equation. 
!
! !INTERFACE:
   subroutine vequation(nlev,dt,cnpar,ty,num,Method) 
!
! !DESCRIPTION:
!  This subroutine calculates the diffusion equation for y-velocity $v$:
!
!  \begin{equation}\label{vEq}
!  \partial_tv
!  -\partial_z((\nu_t+\nu) \partial_z v)  =
!  -g\partial_y \zeta
!  \end{equation}
!
!  Coriolis rotation is already carried out in coriolis().
!  
!  Pressure gradient is applied here only if surface slopes are
!  directly given. If velocities at a certain height or the depth
!  mean velocities are given, the pressure gradient is applied in
!  pressuregradient().
!
!  Bottom friction is implemented implicitely.
!
!  Diffusion is numerically treated implicitly.
!  The tri-diagonal matrix is solved then by a simplified Gauss elimination.
!
!  The $v$-contribution to shear frequency squared is now also
!  computed here, using a new scheme which guarantees energy conservation
!  of kinetic from mean to turbulent flow, for more details, see
!  subroutine {\tt uequation.f}.
!
! !USES:
   use meanflow, only:  gravity,avmolu
   use meanflow, only:  h,u,v,avh,drag,SS
   use observations, only : idpdy,dpdy
#ifdef NUDGE_VEL
   use observations, only : vprof
#endif
   use mtridiagonal
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev,Method
   REALTYPE, intent(in)	:: dt
   REALTYPE, intent(in)	:: cnpar
   REALTYPE, intent(in)	:: ty
   REALTYPE, intent(in)	:: num(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: vequation.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE		:: a,c
   REALTYPE		:: vo(0:nlev),vt(0:nlev),vm(0:nlev)
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
      cu(i)=-cnpar*c!i+1,n+1
      au(i)=-cnpar*a!i-1,n+1
#ifndef SEAGRASS
      bu(i)=1-au(i)-cu(i)			!i  ,n+1
#else
      bu(i)=1-au(i)-cu(i)	&		!i  ,n+1
            + drag(i)*dt/h(i)*sqrt(u(i)*u(i)+v(i)*v(i))
#endif

      du(i)=v(i)+(1-cnpar)*(a*v(i-1)-(a+c)*v(i)+c*v(i+1))  !i  ,n
      du(i)=du(i)+dt*idpdy(i)
   end do

   c    =2*dt*avh(1)/(h(1)+h(2))/h(1)
   cu(1)=-cnpar*c
   bu(1)=1-cu(1)+drag(1)*dt/h(1)*sqrt(u(1)*u(1)+v(1)*v(1))
   du(1)=v(1)+(1-cnpar)*c*(v(2)-v(1))+dt*idpdy(1)

   a     =2*dt*avh(nlev-1)/(h(nlev)+h(nlev-1))/h(nlev)
   au(nlev)=-cnpar*a
#ifndef SEAGRASS
   bu(nlev)=1-au(nlev)
#else
   bu(nlev)=1-au(nlev)		&
            +drag(nlev)*dt/h(nlev)*sqrt(u(nlev)*u(nlev)+v(nlev)*v(nlev))
#endif
   du(nlev)=v(nlev)+ty*dt/h(nlev)+(1-cnpar)*a*(v(nlev-1)-v(nlev))
   du(nlev)=du(nlev)+dt*idpdy(nlev)

   if (Method.eq.0) then 
      du=du-dt*gravity*dpdy 
   end if

   vo = v
#ifdef NUDGE_VEL
   do i=1,nlev
      bu(i)=bu(i)+dt/tau
      du(i)=du(i)+dt/tau*vprof(i)
   end do
#endif
   call tridiagonal(nlev,1,nlev,v)

   vm=cnpar*v +(1.-cnpar)*vo 
   vt=cnpar*vo+(1.-cnpar)*v 

   do i=1,nlev-1
      SS(i)=SS(i)+0.5*(vm(i+1)-vm(i))/(0.5*(h(i+1)+h(i))) *          &
                     ((vm(i+1)-vt(i))/h(i) + (vt(i+1)-vm(i))/h(i+1))
   end do

   SS(0)=SS(1)
   SS(nlev)=SS(nlev-1)

   return
   end subroutine vequation
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

!$Id: vequation.F90,v 1.4 2003-03-28 08:56:56 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The $v$--momentum equation \label{sec:vequation}
!
! !INTERFACE:
   subroutine vequation(nlev,dt,cnpar,ty,num,Method) 
!
! !DESCRIPTION:
!  This subroutine computes the transport of momentum in 
!  $y$--direction according to
!  \begin{equation}
!   \label{vEq}
!    \dot{v}
!    = {\cal D}_v
!    - g \partder{\zeta}{y} + \int_z^{\zeta} \partder{b}{y} \,dz' 
!    - \frac{1}{\tau_R(v)}(v-v_{obs})-C_f v \sqrt{u^2+v^2}
!    \comma
!  \end{equation}
!  where $\dot{v}$ denotes the material derivative of $v$, and
!  ${\cal D}_v$ is the sum of the turbulent and viscous transport
!  terms modelled according to
!  \begin{equation}
!   \label{Dv}
!    {\cal D}_v 
!    = \frstder{z} 
!     \left( 
!        \left( \nu_t + \nu \right) \partder{v}{z}
!      \right) 
!    \point
!  \end{equation}
!  In this equation, $\nu_t$ and $\nu$ are the turbulent and 
!  molecular diffusivities of momentum, respectively. The computation
!  of $\nu_t$ is discussed in \sect{sec:turbulenceIntro}.
!
!  Coriolis rotation is accounted for as described in 
!  \sect{sec:coriolis}. All other terms are completely analogous
!  to those described in \sect{sec:uequation}.
!
! !USES:
   use meanflow, only : gravity,avmolu
   use meanflow, only : h,ho,u,v,w,avh,drag,SS,grid_method,w_grid
   use observations, only : vel_relax_tau,vel_relax_ramp
   use observations, only : idpdy,dpdy,vprof
   use observations, only : w_adv_method,w_adv_discr
   use mtridiagonal
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev,Method
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: cnpar
   REALTYPE, intent(in)                :: ty
   REALTYPE, intent(in)                :: num(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!  $Log: vequation.F90,v $
!  Revision 1.4  2003-03-28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.3  2003/03/10 08:50:08  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.2  2001/05/31 12:00:52  gotm
!  Correction in the calculation of the shear squared calculation - now according
!  to Burchard (2001).
!  Also some cosmetics and cleaning of Makefiles.
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: a,c
   REALTYPE                  :: vo(0:nlev)
   REALTYPE                  :: tau=1.e15
   REALTYPE,save             :: runtime=0.
   logical                   :: surf_flux,bott_flux
!
!-----------------------------------------------------------------------
!BOC
   surf_flux=.false.
   bott_flux=.false.

!  Advection step:
   if (w_adv_method .ne. 0) then
      call w_split_it_adv(nlev,dt,h,ho,u,w,w_adv_discr,surf_flux,bott_flux,1)
   end if
   if (grid_method .ne. 0) then
      call w_split_it_adv(nlev,dt,h,ho,u,w_grid,w_adv_discr, &
                           surf_flux,bott_flux,2)
   end if

   avh=num+avmolu 
   do i=2,nlev-1
      c    =2*dt*avh(i)  /(h(i)+h(i+1))/h(i)
      a    =2*dt*avh(i-1)/(h(i)+h(i-1))/h(i)
      cu(i)=-cnpar*c!i+1,n+1
      au(i)=-cnpar*a!i-1,n+1
#ifndef SEAGRASS
      bu(i)=1-au(i)-cu(i)                                  !i  ,n+1
#else
      bu(i)=1-au(i)-cu(i)              &                   !i  ,n+1
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
   bu(nlev)=1-au(nlev)  &
            +drag(nlev)*dt/h(nlev)*sqrt(u(nlev)*u(nlev)+v(nlev)*v(nlev))
#endif
   du(nlev)=v(nlev)+ty*dt/h(nlev)+(1-cnpar)*a*(v(nlev-1)-v(nlev))
   du(nlev)=du(nlev)+dt*idpdy(nlev)

   if (Method.eq.0) then 
      du=du-dt*gravity*dpdy 
   end if

   vo = v

   if (vel_relax_tau.lt.1.e15) then
      runtime=runtime+dt
      if (runtime.lt.vel_relax_ramp) then
         if (vel_relax_ramp.ge.1.e15) then
            tau=vel_relax_tau*vel_relax_ramp/(vel_relax_ramp-runtime)
         else
            tau=vel_relax_tau
         end if
      else
         tau=1.e15
      end if
      do i=1,nlev
         bu(i)=bu(i)+dt/tau
         du(i)=du(i)+dt/tau*vprof(i)
      end do
   end if

   call tridiagonal(nlev,1,nlev,v)

!  Discretisation of vertiacal shear squared according to Burchard 1995
!  in order to guarantee conservation of kinetic energy when transformed
!  from mean kinetic energy to turbulent kinetic energy.

   do i=1,nlev-1
      SS(i)=SS(i)+0.5*( &
                  (cnpar*(v(i+1)-v(i))*(v(i+1)-vo(i))+       &
                  (1.-cnpar)*(vo(i+1)-vo(i))*(vo(i+1)-v(i))) &
                  /(0.5*(h(i+1)+h(i)))/h(i)                  &
                 +(cnpar*(v(i+1)-v(i))*(vo(i+1)-v(i))+       &
                  (1.-cnpar)*(vo(i+1)-vo(i))*(v(i+1)-vo(i))) &
                  /(0.5*(h(i+1)+h(i)))/h(i+1)                &  
                  )
   end do

   SS(0)=SS(1)
   SS(nlev)=SS(nlev-1)

   return
   end subroutine vequation
!EOC

!$Id: uequation.F90,v 1.6 2003-03-28 09:20:35 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The $u$--momentum equation \label{sec:uequation}
!
! !INTERFACE:
   subroutine uequation(nlev,dt,cnpar,tx,num,Method)
!
! !DESCRIPTION:
!  This subroutine computes the transport of momentum in 
!  $x$--direction according to
!  \begin{equation}
!   \label{uEq}
!    \dot{u}
!    = {\cal D}_u
!    - g \partder{\zeta}{x} + \int_z^{\zeta} \partder{b}{x} \,dz' 
!    - \frac{1}{\tau_R(u)}(u-u_{obs})-C_fu\sqrt{u^2+v^2}
!    \comma
!  \end{equation}
!  where $\dot{u}$ denotes the material derivative of $u$, and
!  ${\cal D}_u$ is the sum of the turbulent and viscous transport
!  terms modelled according to
!  \begin{equation}
!   \label{Du}
!    {\cal D}_u 
!    = \frstder{z} 
!     \left( 
!        \left( \nu_t + \nu \right) \partder{u}{z}
!      \right) 
!    \point
!  \end{equation}
!  In this equation, $\nu_t$ and $\nu$ are the turbulent and 
!  molecular diffusivities of momentum, respectively. The computation
!  of $\nu_t$ is discussed in \sect{sec:turbulenceIntro}.
!
!  Coriolis rotation is accounted for as described in 
!  \sect{sec:coriolis}.
!  The external pressure gradient (second term on right hand side)
!  is applied here only if surface slopes are
!  directly given. Otherwise, the gradient is computed as
!   described in \sect{sec:extpressure}, see \cite{Burchard99}.
!  The internal pressure gradient (third
!  term on right hand side) is calculated in {\tt intpressure.F90}, see
!  \sect{sec:intpressure}.
!  The fourth term on the right hand side allows for nudging velocity
!  to observed profiles with the relaxation time scale $\tau_R (u)$.
!  This is useful for initialising 
!  velocity profiles in case of significant inertial oscillations.
!  Bottom friction is implemented implicitely using the fourth term
!  on the right hand side. Implicit friction may  be
!  applied on all levels in order to allow for inner friction terms such
!  as seagrass friction (see \sect{sec:seagrass}).
!
!  Diffusion is numerically treated implicitly, see equations (\ref{sigmafirst})-
!  (\ref{sigmalast}).
!  The tri--diagonal matrix is solved then by a simplified Gauss elimination.
!  Vertical advection is included for accounting for adaptive grids,
!  see {\tt adaptivegrid.F90} in \sect{sec:adaptivegrid}.
!
!  The $u$--contribution to shear frequency squared $M^2$ is now also
!  computed here, using a new scheme which guarantees energy conservation
!  of kinetic energy from mean to turbulent flow, see \cite{Burchard2002}. 
! With this method, the discretisation of the
! $u$-contribution to shear squared $M^2$ is discretised as
! \begin{equation}\label{shearsquared}
! \left(\partial_z u\right)^2 \approx \frac{(\bar u_{j+1}-\bar u_j)
! (\tilde u_{j+1}-\tilde u_j)}{(z_{j+1}-z_j)^2}
! \end{equation}
! where $\tilde u_j=\frac12(\hat u_j+u_j)$. The shear obtained from (\ref{shearsquared})
! plus the $v$-contribution calculated in {\tt vequation.F90} is then used
! for the calculation of the turbulence shear production, see equation (\ref{computeP}). 
!
! !USES:
   use meanflow, only: gravity,avmolu
   use meanflow, only: h,ho,u,v,w,avh,drag,SS,grid_method,w_grid
   use observations, only: vel_relax_tau,vel_relax_ramp
   use observations, only: w_adv_method,w_adv_discr
   use observations, only: idpdx,dpdx,uprof
   use mtridiagonal
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev,Method
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: cnpar
   REALTYPE, intent(in)                :: tx
   REALTYPE, intent(in)                :: num(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: uequation.F90,v $
!  Revision 1.6  2003-03-28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.5  2003/03/28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.4  2003/03/10 08:50:07  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.3  2001/05/31 12:00:52  gotm
!  Correction in the calculation of the shear squared calculation
!  --- now according to Burchard 1995 (Ph.D. thesis).
!  Also some cosmetics and cleaning of Makefiles.
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: a,c
   REALTYPE                  :: uo(0:nlev)
   REALTYPE                  :: tau=1.e15
   REALTYPE,save             :: runtime= _ZERO_
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
   if (grid_method .eq. 3) then
      call w_split_it_adv(nlev,dt,h,ho,u,w_grid,w_adv_discr, &
                          surf_flux,bott_flux,2)
   end if

   avh=num+avmolu
   do i=2,nlev-1
      c    =2*dt*avh(i)  /(h(i)+h(i+1))/h(i)
      a    =2*dt*avh(i-1)/(h(i)+h(i-1))/h(i)
      cu(i)=-cnpar*c                                       !i+1,n+1
      au(i)=-cnpar*a                                       !i-1,n+1
#ifndef SEAGRASS
      bu(i)=1-au(i)-cu(i)                                  !i  ,n+1
#else
      bu(i)=1-au(i)-cu(i)                &                 !i  ,n+1
            + drag(i)*dt/h(i)*sqrt(u(i)*u(i)+v(i)*v(i))
#endif
      du(i)=u(i)+(1-cnpar)*(a*u(i-1)-(a+c)*u(i)+c*u(i+1))  !i  ,n
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
   bu(nlev)=1-au(nlev) &
            + drag(nlev)*dt/h(nlev)*sqrt(u(nlev)*u(nlev)+v(nlev)*v(nlev))
#endif
   du(nlev)=u(nlev)+tx*dt/h(nlev)+(1-cnpar)*a*(u(nlev-1)-u(nlev))
   du(nlev)=du(nlev)+dt*idpdx(nlev) 

   if (Method .eq. 0) then
      du=du-dt*gravity*dpdx
   end if 

   uo = u

   if (vel_relax_tau .lt. 1.e15) then 
      runtime=runtime+dt
      if (runtime .lt. vel_relax_ramp) then
         if (vel_relax_ramp .ge. 1.e15) then
            tau=vel_relax_tau*vel_relax_ramp/(vel_relax_ramp-runtime)
          else
             tau=vel_relax_tau
         end if   
      else
         tau=1.e15
      end if
      do i=1,nlev  
         bu(i)=bu(i)+dt/tau
         du(i)=du(i)+dt/tau*uprof(i)
      end do
   end if 

   call tridiagonal(nlev,1,nlev,u)

!  Discretisation of vertical shear squared according to Burchard 2002
!  in order to guarantee conservation of kinetic energy when transformed
!  from mean kinetic energy to turbulent kinetic energy.
 
   do i=1,nlev-1
      SS(i)=SS(i)+0.5*( &
                  (cnpar*(u(i+1)-u(i))*(u(i+1)-uo(i))+       &
                  (1.-cnpar)*(uo(i+1)-uo(i))*(uo(i+1)-u(i))) &
                  /(0.5*(h(i+1)+h(i)))/h(i)                  &
                 +(cnpar*(u(i+1)-u(i))*(uo(i+1)-u(i))+       &
                  (1.-cnpar)*(uo(i+1)-uo(i))*(u(i+1)-uo(i))) &
                  /(0.5*(h(i+1)+h(i)))/h(i+1)                &
                  )
   end do 

   SS(0)=SS(1)
   SS(nlev)=SS(nlev-1)

   return
   end subroutine uequation 
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

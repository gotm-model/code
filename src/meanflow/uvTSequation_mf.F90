#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Add mass flux term to the meanflow equations\label{sec:meanflow_massflux}
!
! !INTERFACE:
   subroutine uvTSequation_mf( nlev,dt,Fmass,u_p,v_p,T_p,S_p,Pmf,nsub,mf_dyn,mf_energy )
!
! !DESCRIPTION:
!  This subroutine computes the mass flux contribution to the tracer and momentum meanflow equations 
!  \begin{align*}
!    \partial_t\Theta &= {\cal F}_{\Theta} + \partial_z\left( F_M (\Theta_p - \Theta)  \right) \\
!    \partial_t S     &= {\cal F}_{S} + \partial_z\left( F_M (S_p - S)  \right) \\
!    \partial_t S     &= {\cal F}_{U} + \partial_z\left( F_M (U_p - U)  \right) \\
!    \partial_t S     &= {\cal F}_{V} + \partial_z\left( F_M (V_p - V)  \right) 
!    \comma
!  \end{align*}
!  where $F_M = -a_p w_p$, and ${\cal F}_{X}$ groups all the right-hand-side terms already added in  
!  the temperature, salinity, uequation, and vequation subroutines. 
!
! !USES:
   use meanflow,     only: h,u,v,T,S,uo,vo 

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)                 :: nlev

!  time step (s)
   REALTYPE, intent(in)                :: dt

!  mass flux (m/s)
   REALTYPE, intent(in)                :: Fmass(0:nlev)

!  convective plumes u-velocity (m/s)
   REALTYPE, intent(in)                :: u_p(0:nlev)

!  convective plumes v-velocity (m/s)
   REALTYPE, intent(in)                :: v_p(0:nlev)

!  convective plumes temperature (Celsius)
   REALTYPE, intent(in)                :: T_p(0:nlev)

!  convective plumes salinity (psu)
   REALTYPE, intent(in)                :: S_p(0:nlev)

!  TKE production term associated with convective plumes  
   REALTYPE, intent(out)               :: Pmf(0:nlev)

!  
   integer, intent(in)                :: nsub

!  apply mass flux to the dynamics 
   logical, intent(in)                :: mf_dyn, mf_energy

! !REVISION HISTORY:
!  Original author(s): Florian Lemarié 
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i,isub
   REALTYPE                  :: VFlux(0:nlev,2),dt_sub 
   REALTYPE                  :: u_ED(0:nlev), v_ED(0:nlev)
   REALTYPE                  :: u_star(0:nlev), v_star(0:nlev)
   REALTYPE                  :: T_star(0:nlev), S_star(0:nlev)
   REALTYPE                  :: idz,SSUnp12,SSVnp12,cff,normVel
!
!-----------------------------------------------------------------------
!BOC
!  
   if( mf_dyn .and. mf_energy) then 
      u_ED(:) = u(:)   ! Save the current value of u,v for which only ED has been applied 
      v_ED(:) = v(:)
   endif 

   dt_sub  = dt / real(nsub)

!  Substepping 

   do isub = 1, nsub
      !  save old value
      T_star = T 
      S_star = S
      !
      VFlux(0,:) = 0. !; VFlux(nlev) = 0.
      ! Compute fluxes associated to mass flux
      do i = 1,nlev 
         VFlux(i,1) = Fmass(i)*(t_p(i)-T_star(i)) 
         VFlux(i,2) = Fmass(i)*(s_p(i)-s_star(i)) 
      enddo 
      ! Apply flux divergence
      do i = 1,nlev
         T(i)=T_star(i)+dt_sub*(VFlux(i,1)-VFlux(i-1,1))/h(i) 
         S(i)=S_star(i)+dt_sub*(VFlux(i,2)-VFlux(i-1,2))/h(i) 
      enddo
      !
   enddo
   !
   if( mf_dyn ) then 
      do isub = 1, nsub
         !  save old value
         u_star = u
         v_star = v
         !
         VFlux(0,:) = 0. !; VFlux(nlev) = 0.
         ! Compute fluxes associated to mass flux
         do i = 1,nlev 
            VFlux(i,1) = Fmass(i)*(u_p(i)-u_star(i)) 
            VFlux(i,2) = Fmass(i)*(v_p(i)-v_star(i)) 
         enddo 
         ! Apply flux divergence
         do i = 1,nlev
            u(i)=u_star(i)+dt_sub*(VFlux(i,1)-VFlux(i-1,1))/h(i) 
            v(i)=v_star(i)+dt_sub*(VFlux(i,2)-VFlux(i-1,2))/h(i) 
         enddo
      !
      enddo
   endif

! Compute additional TKE production term for energetic consistency 

   if( mf_dyn .and. mf_energy ) then 
      do i = 1,nlev-1
         idz     = 2./(h(i+1)+h(i))
         SSUnp12 = 0.5*( u(i+1)-u(i) + uo(i+1)-uo(i) )
         SSVnp12 = 0.5*( v(i+1)-v(i) + vo(i+1)-vo(i) ) 
         Pmf(i)  =   idz*SSUnp12*Fmass(i)*( u_p(i)-u_ED(i) )     & 
                   + idz*SSVnp12*Fmass(i)*( v_p(i)-v_ED(i) )
      enddo  
   else 
      do i = 1,nlev-1
         Pmf(i)    =  0. 
      enddo 
   endif 
   !
   return
   end subroutine uvTSequation_mf
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

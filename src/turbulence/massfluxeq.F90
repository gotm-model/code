#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The convective plume nonlinear ODEs \label{sec:massfluxeq}
!
! !INTERFACE:
   subroutine massfluxeq(nlev,dt,u,v,T,S,rho,h)

! !DESCRIPTION:
! This subroutine solves a set of nonlinear ODEs to determine 
! the plume fractional area $a_p$, the plume temperature $\Theta_p$ and salinity $S_p$, 
! the plume horizontal momentum $U_p$ and $V_p$, and the plume vertical velocity $w_p$.  
! The plume equations are (Perrot and Lemarié, 2025):
! \begin{align*}
! \partial_z(a_ w_p) &= E-D \\
! a_p w_p \partial_z \Theta_p &= E( \Theta - \Theta_p ) \\
! a_p w_p \partial_z S_p &= E( S - S_p ) \\
! a_p w_p \partial_z U_p &= E( U - U_p ) + a_p w_p C_u \partial_z U \\
! a_p w_p \partial_z V_p &= E( V - V_p ) + a_p w_p C_u \partial_z V \\
! a_p w_p \partial_z w_p &= -b E w_p + a_p \left( a B_p + \frac{b'}{h} w_p^2 \right) \\
!                    B_p &= b_{\rm eos}( \Theta_p, S_p ) - b_{\rm eos}( \Theta, S ) \\
!                     E  &= a_p C_{ent} \max(0,\partial_z w_p)  \\
!                     D  &= -a_p C_{det} \min(0,\partial_z w_p) - a_p w_p \frac{\delta_0}{h} 
! \end{align*}
! where $a,b,C_u,b',C_{ent},C_{det},\delta_0$ are user provided parameters.
!
! !USES:
   use turbulence,  only: Fmass,a_p,w_p,u_p,v_p,T_p,S_p,EmD
   use turbulence,  only: mf_ap0,mf_wp0,mf_Cent,mf_Cdet, mf_d0
   use turbulence,  only: mf_aa,mf_bb,mf_bp,mf_uv,mf_dbkg
   use turbulence,  only: mf_zinv, Bmf, massflux_energy, mf_nsub
   use meanflow,    only: gravity, z, zi
   use density,     only: rho0, get_rho 
!   
   IMPLICIT NONE
!
! !INPUT PARAMETERS:         

!  number of vertical layers
   integer,  intent(in)                :: nlev

!  time step (s)
   REALTYPE, intent(in)                :: dt

!  horizontal velocity (m/s)
   REALTYPE, intent(in)                :: u(0:nlev),v(0:nlev)

!  temperature and salinity 
   REALTYPE, intent(in)                :: T(0:nlev),S(0:nlev)

!  
   REALTYPE, intent(in)                :: rho(0:nlev)

!  layer thickness (m)
   REALTYPE, intent(in)                :: h(0:nlev)

! !REVISION HISTORY:
!  Original author(s): Florian Lemarié 
!EOP
!------------------------------------------------------------------------
!
! !LOCAL VARIABLES:
   REALTYPE, parameter       :: wp_min  = 0.5E-08 ! min value for plume velocity
   REALTYPE                  :: mf_one_p_bCent, cff, cup, cdwn, cff1, Crt_number(0:nlev)  
   REALTYPE                  :: bp,d0,izinv,zinvMin, dpth, B_p, rhs_wp2, Crt_max
   REALTYPE                  :: app,wpp,wpp2,wpm2,wpm,temp_p,salt_p,rho_p, dz, nrj
   REALTYPE                  :: mf_EmD, hEnt, hDet, dwp, apm, ap, rhs, zinv, relax 
   REALTYPE                  :: totalDepth, penal, del
   integer                   :: i,im1
   logical                   :: found 
!
!------------------------------------------------------------------------
!BOC
!
   mf_one_p_bCent =  1. + mf_bb*mf_Cent
   zinvMin        = -1.0*h(nlev)
   mf_zinv        = MIN( mf_zinv,zinvMin )
   izinv          = -1./mf_zinv
   bp             = izinv*mf_bp 
   mf_d0          = izinv*mf_dbkg
   relax          = 0.6
   nrj            = 0.
   if(massflux_energy) nrj = 1.
   totalDepth     = -zi(0)
   del            = 1./(0.02*totalDepth)
!   
   a_p(0:nlev)    = 0.
   w_p(0:nlev)    = -wp_min
   Fmass(0:nlev)  = 0.

!  surface BC for plume quantities
   a_p(nlev) = mf_ap0
   w_p(nlev) = mf_wp0 
   Fmass(nlev) = 0.
   cff       = 1./( h(nlev-1)+h(nlev) )
   cup       = cff*(2.*h(nlev)+h(nlev-1))
   cdwn      = cff*h(nlev)
   t_p(nlev) = cup*T(nlev)-cdwn*T(nlev-1)    ! extrapolation toward the surface 
   s_p(nlev) = cup*S(nlev)-cdwn*S(nlev-1)
   u_p(nlev) = cup*u(nlev)-cdwn*u(nlev-1)
   v_p(nlev) = cup*v(nlev)-cdwn*v(nlev-1)   

!
!  Main loop for the computation of plume quantities (from top to bottom)
!   
!   found = .false. 

   do i=nlev,1,-1

!     1- Initialize values at the top of the current grid cell

      app   = a_p(i)
      wpp   = w_p(i)
      wpp2  = wpp*wpp
      penal = 0.5*( 1.+TANH( del*(zi(i-1)+0.9*totalDepth) ) )

!     2- Compute the buoyancy forcing term B_p  

      dpth = - z(i) 
      B_p  = - gravity*( get_rho(S_p(i),T_p(i),p=dpth) - rho(i) )/rho0

!     3- w_p equation 

!     >>> iteration 1
      rhs_wp2 = bp*(wpp2+wpp2)+2.*mf_aa*B_p
      if( rhs_wp2 < 0. ) then 
         cff = mf_one_p_bCent
      else 
         cff = 1.
      endif

      cff1  = 1./(cff+h(i)*bp)
      wpm2  = cff1*((cff-h(i)*bp)*wpp2-mf_aa*h(i)*2.*B_p)
      

!     >>> iteration 2
      rhs_wp2 = bp*(wpp2+wpm2)+2.*mf_aa*B_p
      if( rhs_wp2 < 0. ) then 
         cff = mf_one_p_bCent
      else 
         cff = 1.
         dz  = (wpp*wpp-wp_min*wp_min) / (2.*mf_aa*B_p+bp*(wpp*wpp+wp_min*wp_min))
         if( dz>0. .and. dz<h(i) ) then
            zinv    = MIN( zi(i)-dz, zinvMin )
            mf_zinv = (1.-relax)*mf_zinv + relax*zinv
         endif   
      endif

      cff1  = 1./(cff+h(i)*bp)
      wpm2  = cff1*((cff-h(i)*bp)*wpp2-mf_aa*h(i)*2.*B_p)
      wpm   = -SQRT( MAX(penal*wpm2,wp_min*wp_min) )

!     4- a_p equation

      dwp = wpp-wpm
      d0  = MIN( 0.5*h(i)*mf_d0*(wpp+wpm), -2.*wp_min)   
      if( dwp >= 0.) then
         hEnt  =  mf_Cent*dwp
         hDet  = -d0
      else
         hEnt  =  0.
         hDet  = -( mf_Cdet*dwp + d0 )
      endif

      mf_EmD  = hEnt-hDet
      cff     = 1. / (2.*wpm+mf_EmD)
      cff1    = app *(2.*wpp-mf_EmD)
      apm     = MAX(cff*cff1,0.) 

!     5- Plume tracer equations & Plume horizontal velocity equations

      ap  = 0.5*(app+apm)
      if( apm > 0. .and. wpm <-wp_min ) then 
         
         rhs      = ap*(hEnt*T(i)-hDet*T_p(i))
         t_p(i-1) = (app*wpp*T_p(i)-rhs)/(apm*wpm)
         rhs      = ap*(hEnt*S(i)-hDet*S_p(i))
         s_p(i-1) = (app*wpp*S_p(i)-rhs)/(apm*wpm)
         im1      = MAX(i-1,1)
         rhs      = ap*(hEnt*u(i)-hDet*u_p(i))
         u_p(i-1) = (app*wpp*u_p(i)-rhs)/(apm*wpm) - mf_uv*(u(i)-u(im1))
         rhs      = ap*(hEnt*v(i)-hDet*v_p(i))
         v_p(i-1) = (app*wpp*v_p(i)-rhs)/(apm*wpm) - mf_uv*(v(i)-v(im1))
         EmD(i  ) = - 2.*(app*wpp-apm*wpm)/(h(i)*(app*wpp+apm*wpm))
      else 
         t_p(i-1) = t_p(i)
         s_p(i-1) = s_p(i)
         u_p(i-1) = u_p(i)
         v_p(i-1) = v_p(i)
         EmD(i  ) = 0.
      endif 
      
      w_p(i-1)   = wpm
      a_p(i-1)   = apm
      Fmass(i-1) = -apm*wpm
      Bmf  (i  ) = -nrj*Fmass(i)*B_p

!      if(found) exit 

   end do

!  Compute Courant number to use substepping if necessary 

   do i = 1,nlev-1
      Crt_number(i) = Fmass(i)*dt/h(i)
   enddo 
   Crt_max = MAXVAL(Crt_number(:))
   mf_nsub = MAX( ceiling(Crt_max), 1 )

   return
   end subroutine massfluxeq
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Computation of mass flux contribution to the turbulent transport of TKE \label{sec:massfluxeq_Witek11}
!
! !INTERFACE:
   subroutine tke_transport_mf(nlev,u,v,h)
! !DESCRIPTION:
! This subroutine computes the term \eqref{TurbTransportTKE} to be added in the right hand side of the TKE equation.
!
! !USES:
   use turbulence,  only: Fmass,w_p,u_p,v_p
   use turbulence,  only: normVp,wk_mf
   use turbulence,  only: massflux_on_dynamics 
!   
   IMPLICIT NONE
!
! !INPUT PARAMETERS:         

!  number of vertical layers
   integer,  intent(in)                :: nlev

!  horizontal velocity (m/s)
   REALTYPE, intent(in)                :: u(0:nlev),v(0:nlev)

!  layer thickness (m)
   REALTYPE, intent(in)                :: h(0:nlev)

! !REVISION HISTORY:
!  Original author(s): Florian Lemarié 
!EOP
!------------------------------------------------------------------------
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: cff, FC(0:nlev)
!
!------------------------------------------------------------------------
!BOC
!
   FC(nlev)= 0.
   cff     = 0. 
   if( massflux_on_dynamics ) cff = 1.  
   do i = 1,nlev-1
      normVp(i) = 0.5*( w_p(i)*w_p(i) + cff*( u_p(i)-u(i) )**2     & 
                                      + cff*( v_p(i)-v(i) )**2 )
      FC(i)     = Fmass(i  )*normVp(i  )                                
   enddo  
   !
   do i=1,nlev
      wk_mf(i) = 2.*( FC(i+1) - FC(i  ))/(h(i+1)+h(i))  
   enddo 
   !
   return
   end subroutine tke_transport_mf
!EOC



!-----------------------------------------------------------------------
!BOP
!

! !ROUTINE: Computation of a mass flux term on TKE \label{sec:massfluxeq_mfTKE}
!
! !INTERFACE
   subroutine tkeeq_mf(nlev,dt,h)
! !DESCRIPTION:
! This subroutine computes the subplume TKE k_p from the equation 
! \[
! a_p w_p \partial_z k_p = E\left( (k-k_p) + \frac{1}{2} \|  \mathbf{u}_p - \mathbf{u} \|^2  \right) - a_p \epsilon_p 
! \]
! with $\epsilon_p = (c_\epsilon/l_\epsilon) k_p^{3/2}$. Then a mass flux term is added to the TKE equation 
! \[
! \partial_t k = - \partial_z \left( a_p w_p ( k_p - k )  \right) 
! \]
! This term is added as a correction to the already computed $k$ from tkeeq subroutine \ref{sec:tkeeq}.
!
!
! !USES:
   use turbulence,   only: normVp,Fmass,a_p,w_p 
   use turbulence,   only: tke,tkeo,tke_p,k_min,eps
   use turbulence,   only: mf_Cent,mf_Cdet, mf_d0, mf_nsub
!
!   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
!  number of vertical layers
   integer,  intent(in)                :: nlev
!
!  time step (s)
   REALTYPE, intent(in)                :: dt
!!
!  layer thickness (m)
   REALTYPE, intent(in)                :: h(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Florian Lemarié 
!EOP
!------------------------------------------------------------------------
!
! !LOCAL VARIABLES:
   REALTYPE, parameter       :: wp_min  = 0.5E-08 ! min value for plume velocity
   REALTYPE                  :: dt_sub, tke_star(0:nlev) 
   REALTYPE                  :: ap,dissp,kpp,kpm,dwp,k_env,d0,rhs,FC(0:nlev)
   integer                   :: i,isub
!
!------------------------------------------------------------------------
!BOC
!
! 1- Compute subplume TKE tke_p 

   tke_p(nlev-1) = tke(nlev-1) ! upper BC 

   do i = nlev-1,1,-1
      kpp     = a_p(i)*w_p(i)*tke_p(i)
      ap      = 0.5*( a_p(i  ) + a_p(i-1) )
      dissp   = eps(i)*tke_p(i)*SQRT(tke_p(i))   & 
                   / ( tkeo (i)*SQRT(tkeo (i)) )      ! (ceps/leps) tke_p**(3/2) 
      dwp     = w_p(i)-w_p(i-1)
      ! Compute entrainment and detrainment rates multiplied by h
      d0  = MIN( 0.5*h(i)*mf_d0*(w_p(i)+w_p(i-1)), -2.*wp_min )
      if( dwp >= 0.) then
         hEnt  =  ap*mf_Cent*dwp
         hDet  = -ap*d0
      else
         hEnt  =  0.
         hDet  = -ap*( mf_Cdet*dwp + d0 )
      endif
      !
      k_env   = tke(i-1)
      rhs     = hEnt*k_env-hDet*tke_p(i)
      rhs     = rhs + hEnt*normVp(i)-ap*dissp
      kpm     = kpp - rhs 
      !
      if( a_p(i-1)>0. .and. w_p(i-1) < -wpmin ) then 
         tke_p(i-1)  = MAX( kpm/(a_p(i-1)*w_p(i-1)), k_min )
      else 
         tke_p(i-1)  = k_min
      endif
   enddo 

   tke_p(nlev) = tke_p(nlev-1) 

! 2- Apply mass flux term to the TKE equation  
   dt_sub  = dt / real(mf_nsub) 
   
   do isub=1,mf_nsub
      !  save old value
      tke_star = tke  
      FC(1:nlev) = 0.

      do i = 1,nlev-1
         FC(i) = Fmass(i)*( tke_p(i)-tke_star(i-1) )
      enddo

      do i = 1,nlev-1
         tke(i) = tke_star(i) + dt_sub*(FC(i+1)-FC(i))*2./(h(i+1)+h(i))
      enddo
      !
   enddo 

!  clip at k_min
   do i=0,nlev
      tke(i) = max(tke(i),k_min)
   enddo
   
   return
   end subroutine tkeeq_mf
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

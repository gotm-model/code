!$Id: tkeeq.F90,v 1.2 2001-11-18 16:15:30 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Dynamic equation for TKE.
! 
! !INTERFACE:
   subroutine tkeeq(N,dt,u_taus,u_taub,z0s,h,P,B,numtke)
!
! !DESCRIPTION:
!  This subroutine calculates the turbulent kinetic energy as
!  needed for one- or two-equation models:
!
!  \begin{equation}\label{k_eq}
!  \partial_t k - \partial_z(\nu_k\partial_z k) =  P + B -\varepsilon,
!  \end{equation}
!
!  The diffusion coefficient depends on the type of model (k-epsilon
!  or Mellor-Yamada). 
!
!  As boundary conditions a choice between Dirichlet (flux-bdy=.false.)
!  and Neumann no-flux conditions (flux-bdy=.true.) has to be made.
!
!  Dirichlet condition:
!
!  \begin{equation}
!  k=\left(\frac{u_*}{c_{\mu}^0}\right)^2. 
!  \end{equation}
!
!  If flux conditions are chose, the {\it Craig and Banner} [1994] and
!  the {\it Craig} [1996] surface wave breaking theory can
!  be used. The boundarz condition is then:
!
!   \begin{equation}
!   -\nu_t \partial_zk =c_w (u_s^*)^3 \hfill z= 0. \qquad
!   \end{equation}
!
!   Since the flux is applied half a grid-box away from the boundary,
!   the {\it Craig} [1996] analytical solution is used for the
!   construction of this boundary condition:
!
!  \begin{equation}\label{tkeanalyt}
!  k=\frac{(u_s^*)^2}{c_{\mu}^{1/2}}
!  \left[a+
!  \left(\frac{3\sigma_k}{2}\right)^{1/2}
!  c_{\mu}^{1/4}c_w\left(\frac{z'+z_0^s}{z_0^s}\right)^{-m}
!  \right]^{2/3}. 
!  \end{equation}
! 
!   
!  The sink terms are treated quasi-implicitely in oder to guarantee
!  positivity. 
!
! !USES:
   use mTridiagonal
   use turbulence, ONLY: tkeo,tke,k_min,eps,num,cm0,cde,flux_bdy
   use turbulence, ONLY: tke_method
   use turbulence, ONLY: craig_banner,craig_m,cw,umlauf_burchard
   use turbulence, ONLY: sig_phi,l_gen,alpha_gen,cm_craig 
   IMPLICIT NONE
!
! !INPUT PARAMETERS: 
   integer, intent(in)	:: N
   REALTYPE, intent(in)	:: dt 
   REALTYPE, intent(in)	:: u_taus,u_taub,z0s
   REALTYPE, intent(in)	:: h(0:N)
   REALTYPE, intent(in)	:: P(0:N),B(0:N)
   REALTYPE, intent(in)	:: numtke(0:N)
!
! !INPUT/OUTPUT PARAMETERS: 
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: tkeeq.F90,v $
!  Revision 1.2  2001-11-18 16:15:30  gotm
!  New generic two-equation model
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
! 
! !LOCAL VARIABLES:
   REALTYPE 		:: avh(0:N)
   REALTYPE 		:: pminus(0:N),pplus(0:N)
   REALTYPE 		:: prod,buoyan,diss
   REALTYPE 		:: zz 
   logical, save	:: warning=.true. 
   integer 		:: i
!EOP
!-----------------------------------------------------------------------
!BOC


   tkeo=tke
 
   do i=2,N-1
      avh(i)=0.5*(numtke(i-1)+numtke(i))
   end do

   if (flux_bdy) then 
      avh(1)=0
      avh(N)=0
   else       ! Not needed any more for Dirichlet conditions, only 
              ! kept for "historical" reasons, see Burchard et al. [1998].
      if (tke_method .eq. 2) then  
         avh(1)=u_taub**4*2/(eps(0)+eps(1  ))
         avh(N)=u_taus**4*2/(eps(N)+eps(N-1))
      else   
         avh(1)= 0.5*(num(0)+numtke(1  ))
         avh(N)= 0.5*(num(N)+numtke(N-1))
      end if
   end if

   zz=0.
   do i=N-1,1,-1
      prod=P(i)
      buoyan=B(i)
      diss=eps(i)
      if (prod+buoyan.gt.0) then
         pplus(i)=prod+buoyan
         pminus(i)=diss
      else
         pplus(i)=prod
         pminus(i)=diss-buoyan
      end if
   end do
   i=N-1
 
   do i=1,N-1
      au(i)=-2.*dt*avh(i)/(h(i)+h(i+1))/h(i)
      cu(i)=-2.*dt*avh(i+1)/(h(i)+h(i+1))/h(i+1)
      bu(i)=1.-au(i)-cu(i)+pminus(i)*dt/tke(i)
      du(i)=(1+pplus(i)*dt/tke(i))*tke(i)
   end do

!  Surface flux of TKE due to surface wave breaking 
!  according to Craig and Banner 1994: 

   if (craig_banner) then 
      if (h(N) .gt. z0s .and.  warning) then
         STDERR 'WARNING: Surface roughness length smaller than'
         STDERR '         thickness of upper grid box. Calculations'
         STDERR '         might be inaccurate in this Craig and Banner'
         STDERR '         surface wave breaking parameterisation.'
         STDERR '         Computation is continued.' 
         warning=.false.   
      end if 
      du(N-1)=du(N-1)+cw*u_taus**3*dt/(0.5*(h(N)+h(N-1)))   & 
                 *((0.5*h(N)+z0s)/z0s)**(-craig_m)                      
   end if 

   if (flux_bdy) then
!  +-------------------------------------------------------------+
!  | No-flux conditions for TKE                                  | 
!  +-------------------------------------------------------------+
      call tridiagonal(N,1,N-1,tke)
      tke(0) = u_taub*u_taub/sqrt(cm0*cde) 
      tke(N) = u_taus*u_taus/sqrt(cm0*cde) 
      if (umlauf_burchard) then
         STDERR 'Flux boundary conditions for the Umlauf & Burchard model'
         STDERR 'are not coded yet. Please chose Dirichlet boundary'
         STDERR 'conditions (flux_bdy=     .true.)'
         STDERR 'in the keps namelist in the gotmturb.inp file.'  
         STDERR 'Program aborted in tkeeq.F90.'  
         stop
      end if 
   else
!  +-------------------------------------------------------------+
!  | Dirichlet conditions for TKE                                | 
!  +-------------------------------------------------------------+
      if (craig_banner) then
          STDERR 'For the Craig and Banner wave breaking condition,' 
          STDERR 'flux boundary conditions should be used.'
          STDERR 'Please, change namelist gotmturb.inp !'
          STDERR 'Program aborted ...'
          stop
       end if   
      cu(1)=0
      bu(1)=1.
      du(1)=u_taub*u_taub/sqrt(cm0*cde)
 
      bu(N-1)=1.
      au(N-1)=0.
      if (umlauf_burchard) then        ! from eqs. (4.14), (4.19) 
         du(N-1)=(cw*sig_phi/(-cm_craig*l_gen*alpha_gen))**(2./3.)   &
                  *u_taus**2/(z0s**alpha_gen)*(h(N)+z0s)**alpha_gen
         tke(N)=(cw*sig_phi/(-cm_craig*l_gen*alpha_gen))**(2./3.)   &
                  *u_taus**2/(z0s**alpha_gen)*z0s**alpha_gen
      else 
         du(N-1)=u_taus*u_taus/sqrt(cm0*cde)
         tke(N) =u_taus*u_taus/sqrt(cm0*cde)
      end if 
      call tridiagonal(N,1,N-1,tke)
   end if 

   where (tke .lt. k_min) tke = k_min

   return
   end subroutine tkeeq
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard & Karsten Bolding.

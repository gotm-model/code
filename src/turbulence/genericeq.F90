!$Id: genericeq.F90,v 1.1 2001-11-18 16:15:30 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Length scale from generic k^n eps^m equation.
!
! !INTERFACE:
   subroutine genericeq(nlev,dt,z0b,z0s,u_taus,u_taub,h,NN,P,B)
!
! !DESCRIPTION:
!  This subroutine calculates k^n eps^m in the framework
!  of the k-epsilon model:
!
!   \begin{equation}\label{knepsmeq}
!   \partial_t\left(k^n\eps^m\right)-
!   \partial_z \left( \frac{\nu_t}{\sigma_{nm}}\partial_z
!   \left(k^n\eps^m\right)\right)
!   =k^{n-1}\eps^m\left(c_{1nm}P+c_{3nm}B-c_{2nm}\eps\right).
!   \end{equation}
!
!  As boundary condtions a choice between Dirichlet (flux-bdy=.false.)
!  and Neumann flux conditions (flux-bdy=.true.) has to be made.
!
!  Dirichlet conditions:
!
!   \begin{equation}
!   k^n\eps^m =
!    \frac{u_*^{2n+3m}}{\left(c_{\mu}^0\right)^{n/2}\kappa^m(z'+z_0)^m}.
!   \end{equation}
!
!  Neumann flux conditions:
!
!   \begin{equation}
!   \frac{\nu_t}{\sigma_{nm}}\partial_z \left(k^n\eps^m \right)=
!   -\left(c_{\mu}^0\right)^{3/4}\frac{k^{3/2 m+n}}{\kappa^m(z'+z_0)^{m+1}},
!   \end{equation}
!
!  At the end of the subroutine, the Galperin et al. [1988] limitation
!  and the calculation of the macro length scale is carried out.
!
! !USES:
   use mtridiagonal
   use turbulence, ONLY: ce1,ce2,ce3plus,ce3minus,cde,kappa
   use turbulence, ONLY: sig_k
   use turbulence, ONLY: umlauf_burchard,d_gen,alpha_gen,l_gen
   use turbulence, ONLY: cm0,galp,flux_bdy,length_lim
   use turbulence, ONLY: num,eps,L,tkeo,tke,eps_min,L_min
   use turbulence, ONLY: cmue1,craig_banner,cw,nnn,mmm,craig_m,mx
   use turbulence, ONLY: sig_phi,c_phi1,c_phi2,nnx,mmx,nx,beta_gen,cm_craig
   use meanflow,   ONLY: avmolu
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: dt
   REALTYPE, intent(in)	:: z0b,z0s
   REALTYPE, intent(in)	:: u_taus,u_taub
   REALTYPE, intent(in)	:: h(0:nlev)
   REALTYPE, intent(in)	:: P(0:nlev),B(0:nlev)
   REALTYPE, intent(in)	:: NN(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: genericeq.F90,v $
!  Revision 1.1  2001-11-18 16:15:30  gotm
!  New generic two-equation model
!
!
! !LOCAL VARIABLES:
   REALTYPE 		:: avh(0:nlev),flux(0:nlev)
   REALTYPE 		:: pminus(0:nlev),pplus(0:nlev)
   REALTYPE 		:: prod,buoyan,diss
   REALTYPE 		:: cee3,epslim
   REALTYPE 		:: peps,X(0:nlev)
   REALTYPE 		:: kk,tke_surf,eps_surf
   integer 		:: i
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      flux(i)=0.
   end do

   do i=1,nlev
      avh(i)=0.5*(num(i-1)/sig_phi+num(i)/sig_phi)+avmolu
   end do

!  The following boundary conditions are derived from the law of the wall:
   if (flux_bdy) then
      flux(1  )=avh(1)*cde*tkeo(1  )**(1.5*mmx+nnx)/               &
                (kappa**mmx*(z0b+0.5*h(1))**(mmx+1.))
      kk=tkeo(nlev-1)
      flux(nlev-1)=cmue1(nlev-1)*sqrt(kk)*kappa*(0.5*h(nlev)+z0s)   &
             /sig_phi*cde*kk**(1.5*mmx+nnx)/                    &
                   (kappa**mmx*(z0s+0.5*h(nlev))**(mmx+1.))
      avh(1)=0
      avh(nlev)=0
   end if

   do i=1,nlev-1
      if (B(i).gt.0) then
         cee3=ce3plus
      else
         cee3=ce3minus
      end if
      prod=c_phi1*eps(i)**mmx*tkeo(i)**(nnx-1.)*P(i)
      buoyan=(cee3*mmx+nnx)*eps(i)**mmx*tkeo(i)**(nnx-1.)*B(i)
      diss=c_phi2*eps(i)*eps(i)**mmx*tkeo(i)**(nnx-1.)
      if (buoyan.gt.0) then
         pplus(i)=prod+buoyan
         pminus(i)=diss
      else
         pplus(i)=prod
         pminus(i)=diss-buoyan
      end if
   end do

   do i=1,nlev-1
      X(i)=tkeo(i)**nnx*eps(i)**mmx
   end do

   do i=1,nlev-1
      au(i)=-2.*dt*avh(i)/(h(i)+h(i+1))/h(i)
      cu(i)=-2.*dt*avh(i+1)/(h(i)+h(i+1))/h(i+1)
      bu(i)=1.-au(i)-cu(i)+pminus(i)*dt/X(i)
      du(i)=(1+pplus(i)*dt/X(i))*X(i)+flux(i)*dt/(0.5*(h(i)+h(i+1)))
   end do

   if (flux_bdy) then
      call Tridiagonal(nlev,1,nlev-1,X)

      do i=1,nlev-1
        eps(i)=(X(i)*tke(i)**(-nnx))**(1./mmx)
      end do

      eps(0) = cde*sqrt(tke(0)*tke(0)*tke(0))/kappa/z0b
      eps(nlev) = cde*sqrt(tke(nlev)*tke(nlev)*tke(nlev))/kappa/z0s
   else
      cu(1)=0.          ! log-law at lower boundary, one grid-box from bed
      bu(1)=1.
      du(1)=(cde*tke(1)**1.5/kappa/(z0b+h(1)))**mmx*tke(1)**nnx

      bu(nlev-1)=1.    ! upper boundary, one grid-box from surface
      au(nlev-1)=0.
      if (umlauf_burchard) then   ! from (4.20)
         tke_surf=tke(nlev-1)
         eps_surf=cm_craig**3*tke_surf**1.5/(l_gen*(h(nlev)+z0s))
         eps(nlev)=cm_craig**3*tke(nlev)**1.5/(l_gen*z0s)
         du(nlev-1)=tke_surf**nnx*eps_surf**mmx
      else
         tke_surf=u_taus*u_taus/sqrt(cm0*cde)*(1.+sqrt(1.5*sig_k)  &
                 *(cm0*cde)**0.25*cw*((h(nlev)+z0s)/z0s)**(-craig_m))**(2./3.)
         eps_surf=u_taus**3./kappa/(z0s+h(nlev))*(1.+sqrt(1.5*sig_k)  &
                 *(cm0*cde)**0.25*cw*((h(nlev)+z0s)/z0s)**(-craig_m))
         du(nlev-1)=tke_surf**nnx*eps_surf**mmx
         eps(nlev)=u_taus**3./kappa/z0s*(1.+sqrt(1.5*sig_k)  &
                 *(cm0*cde)**0.25*cw)
      end if

      call Tridiagonal(nlev,1,nlev-1,X)

      do i=1,nlev-1
        eps(i)=(X(i)*tke(i)**(-nnx))**(1./mmx)
      end do

   end if

   do i=0,nlev
      if ((NN(i).gt.0).and.(length_lim)) then
         epslim=cm0*cm0*cm0/sqrt(2.)/galp*tke(i)*sqrt(NN(i))
      else
         epslim=eps_min
      end if
      if (eps(i).lt.epslim) eps(i)=epslim
      L(i)=cde*sqrt(tke(i)*tke(i)*tke(i))/eps(i)
      if (L(i).lt.L_min) L(i)=L_min
   end do

   return
   end subroutine genericeq
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard.
!-----------------------------------------------------------------------

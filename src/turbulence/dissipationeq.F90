!$Id: dissipationeq.F90,v 1.2 2002-02-08 08:59:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Length scale from dissipation equation. 
! 
! !INTERFACE:
   subroutine dissipationeq(nlev,dt,z0b,z0s,u_taus,u_taub,h,NN,P,B)
!
! !DESCRIPTION:
!  This subroutine calculates the dissipation rate in the framework
!  of the k-epsilon model:
!
!  \begin{equation}\label{eps_eq}
!  \partial_t \varepsilon -
!  \partial_z(\nu_{\varepsilon}\partial_z \varepsilon) =
!  \frac{\varepsilon}{k} \left(c_{\varepsilon 1}P + c_{\varepsilon 3}B - c_{\varepsilon 2}\varepsilon \right).
!  \end{equation}
!
!  As boundary condtions a choice between Dirichlet (flux-bdy=.false.)
!  and Neumann flux conditions (flux-bdy=.true.) has to be made.   
!
!  Dirichlet conditions:
!
!  \begin{equation}\label{Standardeps}
!  \varepsilon =
!  \left( c_{\mu}^0 \right)^3 \frac{k^{3/2}}{\kappa (\tilde z + z_0)}.
!  \end{equation}
!
!  Neumann flux conditions:
!
!  \begin{equation}\label{Fluxeps}
!  \frac{\nu_t}{\sigma_{\varepsilon}} \partial_{\tilde z} \varepsilon =
!  -\left( c_{\mu}^0 \right)^3
!  \frac{\nu_t}{\sigma_{\varepsilon}}
!  \frac{k^{3/2}}{\kappa (\tilde z + z_0)^2}. 
!  \end{equation}
!
!  If the {\it Craig and Banner} [1994] and the {\it Craig} [1996]
! surface wave breaking theory is applied, then the following extended
! surface boundary condition for $\eps$ is used:
!  
!  
!  \begin{equation}\label{NewBC}
!  -\frac{\nu_t}{\sigma_{\eps}}\partial_z \eps=
!  -\frac{\nu_t}{\sigma_{\eps}}
!  (c_{\mu}^0)^{3/4}
!  \frac{\frac{3}{2}\frac{\sigma_k(c_{\mu}^0)^{3/4}}{c_{\mu}}c_w(u_*^s)^3
!  +\kappa k^{3/2}}
!  {\kappa^2(z'+z_0^s)^2}.
!  \end{equation}
!
!  This has been constructed with the aid of the analytical solution
!  for the dissipation rate $\eps$ as suggested by {\it Craig} [1996]:
!
!  \begin{equation}\label{epsanalyt}
!  \eps=\frac{(u_s^*)^3}{\kappa (z'+z_0^s)}
!  \left[a+\left(\frac{3\sigma_k}{2}\right)^{1/2}
!  c_{\mu}^{1/4}c_w\left(\frac{z'+z_0^s}{z_0^s}\right)^{-m}\right]. 
!  \end{equation}
!
!  The turbulent Schmidt number $\sigma_{\eps}$ for the dissipation
!  rate $\eps$ is a linear interpolation between
!
!  \begin{equation}\label{sigma0}
!  \sigma_{\eps}=
!  \sigma_{\eps 0}
!  =\left(\frac43m+1\right)(m+1)\frac{\kappa^2}{c_2c_{\mu}^{1/2}}
!  \approx 2.4
!  \end {equation}
!  
!  for $(P+B)/\eps=0$ and 
!  
!  \begin{equation}\label{sigma1}
!  \sigma_{\eps}=\sigma_{\eps 1}=\frac{\kappa^2}{c_{\mu}^{1/2}(c_2-c_1)}
!  \approx 1.111 
!  \end{equation}
!
!  for $(P+B)/\eps=1$. For more details, see {\it Burchard} [2000]. 
!
!  At the end of the subroutine, the Galperin et al. [1988] limitation
!  and the calculation of the macro length scale is carried out. 
!
! !USES:
   use mtridiagonal
   use turbulence, ONLY: ce1,ce2,ce3plus,ce3minus,cde,kappa
   use turbulence, ONLY: sig_e0,sig_e1,sig_k
   use turbulence, ONLY: cm0,galp,flux_bdy,length_lim
   use turbulence, ONLY: num,eps,L,tkeo,tke,eps_min,L_min
   use turbulence, ONLY: cmue1,craig_banner,cw
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
!  Original author(s): Hans Burchard, Karsten Bolding 
!                      & Manuel Ruiz Villarreal
!
!  $Log: dissipationeq.F90,v $
!  Revision 1.2  2002-02-08 08:59:58  gotm
!  Added Manuel as author and copyright holder
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
! !LOCAL VARIABLES:
   REALTYPE 		:: avh(0:nlev),flux(0:nlev)
   REALTYPE 		:: pminus(0:nlev),pplus(0:nlev)
   REALTYPE 		:: prod,buoyan,diss
   REALTYPE 		:: cee3,epslim 
   REALTYPE 		:: peps,sig_e(0:nlev) 
   REALTYPE 		:: kk
   integer 		:: i
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1 
      flux(i)=0 
   end do

!  Determination of the turbulent Schmidt number for the dissipation rate:

   if (craig_banner) then     ! With wave breaking
      sig_e(nlev)=sig_e0
      do i=1,nlev-1
         peps=(P(i)+B(i))/eps(i)
         if (peps .gt. 1.) peps=1.
         sig_e(i)=peps*sig_e1+(1.-peps)*sig_e0
      end do
      sig_e(0)=sig_e1
   else                        ! No wave breaking
      do i=0,nlev
         sig_e(i)=sig_e1
      end do
   end if 

   do i=1,nlev
      avh(i)=0.5*(num(i-1)/sig_e(i-1)+num(i)/sig_e(i))
   end do

   if (flux_bdy) then
      flux(1  )=avh(1)*cde*(tkeo(1  )**1.5)/(kappa*(z0b+0.5*h(1))**2.)
      kk=tkeo(nlev-1)
      if (craig_banner) then
         flux(nlev-1)=cmue1(nlev-1)*sqrt(kk)*kappa*(0.5*h(nlev)+z0s)   &
                /sig_e(nlev-1)*cde*(kk**1.5+                           &
                1.5/kappa/cmue1(nlev-1)*sig_k*cw*u_taus**3)/           &
                      (kappa*(z0s+0.5*h(nlev))**2.)
      else
         flux(nlev-1)=cmue1(nlev-1)*sqrt(kk)*kappa*(0.5*h(nlev)+z0s)   &
                /sig_e(nlev-1)*cde*kk**1.5/                            &
                      (kappa*(z0s+0.5*h(nlev))**2.)
! A bug in the previous two lines has been found 
! by Patrick Luyten, MUMM, Belgium. kappa had been squared as well before.
! See the GOTM report, 1999 for the correct mathematical formulation. 
      end if 
      avh(1)=0
      avh(nlev)=0
   else       ! Not needed any more for Dirichlet conditions, only 
              ! kept for "historical" reasons, see Burchard et al. [1998]. 
      avh(1)=u_taub**4*2/sig_e1/(eps(0)+eps(1))
      avh(nlev)=u_taus**4*2/sig_e1/(eps(nlev)+eps(nlev-1))
   end if 

   do i=1,nlev-1
      if (B(i).gt.0) then
         cee3=ce3plus 
      else
         cee3=ce3minus 
      end if
      prod=ce1*eps(i)/tkeo(i)*P(i)
      buoyan=cee3*eps(i)/tkeo(i)*B(i)
      diss=ce2*eps(i)*eps(i)/tkeo(i)
      if (prod+buoyan.gt.0) then
         pplus(i)=prod+buoyan
         pminus(i)=diss
      else
         pplus(i)=prod
         pminus(i)=diss-buoyan
      end if
   end do
 
   do i=1,nlev-1
      au(i)=-2.*dt*avh(i)/(h(i)+h(i+1))/h(i)
      cu(i)=-2.*dt*avh(i+1)/(h(i)+h(i+1))/h(i+1)
      bu(i)=1.-au(i)-cu(i)+pminus(i)*dt/eps(i)
      du(i)=(1+pplus(i)*dt/eps(i))*eps(i)+flux(i)*dt/(0.5*(h(i)+h(i+1)))
   end do

   if (flux_bdy) then
      call Tridiagonal(nlev,1,nlev-1,eps)
      eps(0) = cde*sqrt(tke(0)*tke(0)*tke(0))/kappa/z0b 
      eps(nlev) = cde*sqrt(tke(nlev)*tke(nlev)*tke(nlev))/kappa/z0s 
   else
      cu(1)=0          ! lower boundary, one grid-box from bed
      bu(1)=1.
      du(1)=cde*tke(1)**1.5/kappa/(z0b+h(1)) 

      bu(nlev-1)=1.    ! upper boundary, one grid-box from surface 
      au(nlev-1)=0
      du(nlev-1)=cde*tke(nlev-1)**1.5/kappa/(z0s+h(nlev)) 

      call Tridiagonal(nlev,1,nlev-1,eps)
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
   end subroutine dissipationeq
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard, Karsten Bolding 
!                     & Manuel Ruiz Villarreal.
!-----------------------------------------------------------------------

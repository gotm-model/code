!$Id: temperature.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The temperature equation. 
!
! !INTERFACE:
   subroutine temperature(nlev,dt,cnpar,I_0,heat,nuh,rad)
!
! !DESCRIPTION:
!  This subroutine calculates the diffusion equation for temperature $T$:
!
!  \begin{equation}\label{TEq}
!  \partial_tT
!  -\partial_z((\nu'_t(T)+\nu'(T)) \partial_z T)  =
!  - \frac{1}{\tau_R (T)} (T-T_d) + \frac{\partial_z I}{C_p\rho_0}
!  \end{equation}
!
!  Relaxation with $\tau_R (T)$ to a precribed (changing in time)
!  profile $Tobs$ is possible. 
!
!  The sum of latent, sensible, and longwave radiation is treated
!  as a boundary condition. Solar radiation is treated as an inner source.
!  Absorbtion of shortwave radiation is calculated following the
!  following exponential law. 
!      
!  \begin{equation}
!  I(z) = I_0 \bigg(Ae^{-\eta_1z}+(1-A)e^{-\eta_2z}\bigg).
!  \end{equation}
!
!  Absorbtion coefficients $\eta_1$ and $\eta_2$ depend on the water type
!  and have to be prescribed. 

!  Diffusion is numerically treated implicitly.
!  The tri-diagonal matrix is solved then by a simplified Gauss elimination.
!
! !USES:
   use meanflow, only:  avmolt,rho_0,cp
   use meanflow, only:  h,u,v,T,avh
   use observations, only:  dtdx,dtdy,t_adv,w_adv,w_adv_discr
   use observations, only:  tprof,TRelaxTau
   use observations, only:  A,g1,g2
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: dt,cnpar
   REALTYPE, intent(in)	:: I_0,heat
   REALTYPE, intent(in)	:: nuh(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE		:: rad(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: temperature.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: i,Meth,Bcup,Bcdw
   REALTYPE		:: w(0:nlev),Qsour(0:nlev) 
   REALTYPE		:: Tup,Tdw,z
   logical		:: surf_flux,bott_flux
!
!EOP
!-----------------------------------------------------------------------
!BOC
!  hard coding of parameters, to be included into namelist for gotm2.0 
   Bcup=1				!BC Neumann 
   Tup=-heat/(rho_0*cp)			!Heat flux (positive upward)
   Bcdw=1				!BC Neumann
   Tdw=0.				!No flux
   Meth=w_adv_discr	      		!Type of vertical advection scheme
   surf_flux=.true.                     
   bott_flux=.true.

   rad(nlev)=I_0/(rho_0*cp)
   z=0. 
   do i=nlev-1,0,-1 
      z=z+h(i+1)
      rad(i)=I_0/(rho_0*cp)*(A*exp(-z/g1)+(1-A)*exp(-z/g2))
      avh(i)=nuh(i)+avmolT 
      w(i)=w_adv
   end do

   do i=1,nlev
      Qsour(i)=(rad(i)-rad(i-1))/h(i) 
      if (t_adv) Qsour(i)=Qsour(i)-u(i)*dtdx(i)-v(i)*dtdy(i) 
   end do

   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Tup,Tdw,TRelaxTau,h,avh,w,   &
              Qsour,tprof,Meth,T,surf_flux,bott_flux)
   return
   end subroutine temperature
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

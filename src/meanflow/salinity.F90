!$Id: salinity.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Salinity equation. 
!
! !INTERFACE:
   subroutine salinity(nlev,dt,cnpar,nuh) 
!
! !DESCRIPTION:
!  This subroutine calculates the diffusion equation for salinity $S$:
!
!  \begin{equation}\label{SEq}
!  \partial_tS
!  -\partial_z((\nu'_t(S)+\nu'(S)) \partial_z S)  =
!  - \frac{1}{\tau_R (S)} (S-S_d) 
!  \end{equation}
! 
!  Relaxation with $\tau_R (S)$ to a precribed (changing in time) 
!  profile $Sobs$ is possible. Surface fluxes and inner sources or
!  sinks are not considered. Diffusion is numerically treated implicitly.   
!  The tri-diagonal matrix is solved then by a simplified Gauss elimination.
!
! !USES:
   use meanflow, only: avmols
   use meanflow, only: h,u,v,S,avh
   use observations, only: dsdx,dsdy,s_adv,w_adv,w_adv_discr
   use observations, only: sprof,SRelaxTau
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: dt,cnpar
   REALTYPE, intent(in)	:: nuh(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: salinity.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   REALTYPE		:: w(0:nlev),Qsour(0:nlev)
   REALTYPE		:: Sup,Sdw
   integer 		:: i,Meth,Bcup,Bcdw
   logical		:: surf_flux,bott_flux
!
!EOP
!-----------------------------------------------------------------------
!BOC
! hard coding of parameters, to be included into namelist for gotm2.0 
   Bcup=1                    !BC Neumann
   Sup=0.                    !No flux
   Bcdw=1                    !BC Neumann
   Sdw=0.                    !No flux
   Meth=w_adv_discr          !First order advection
   surf_flux=.true.                      
   bott_flux=.true.

   do i=1,nlev-1
      avh(i)=nuh(i)+avmolS 
      w(i)=w_adv
   end do
   do i=1,nlev
      Qsour(i)=0.
      if (s_adv) Qsour(i)=Qsour(i)-u(i)*dsdx(i)-v(i)*dsdy(i)
   end do
   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Sup,Sdw,SRelaxTau,h,avh,w,   &
              QSour,sprof,Meth,S,surf_flux,bott_flux)
   return
   end subroutine salinity 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

!$Id: variances.F90,v 1.1 2001-02-12 15:55:59 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculation of variances uu,vv,ww,tt according to the 
!           Canuto et al. [2000] version A non-equilibrium stability functions. 
! 
! !INTERFACE:
   subroutine variances(nlev,u,v,t,h,k,eps,num,nuh,B,uu,vv,ww,tt)
!
! !DESCRIPTION:
!  This subroutine computes Canuto et al. [2000] version A non-equilibrium
!  stability functions.
!
! !USES:
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: u(0:nlev)
   REALTYPE, intent(in)	:: v(0:nlev)
   REALTYPE, intent(in)	:: t(0:nlev)
   REALTYPE, intent(in)	:: h(0:nlev)
   REALTYPE, intent(in)	:: k(0:nlev)
   REALTYPE, intent(in)	:: eps(0:nlev)
   REALTYPE, intent(in)	:: num(0:nlev)
   REALTYPE, intent(in)	:: nuh(0:nlev)
   REALTYPE, intent(in)	:: B(0:nlev)
   REALTYPE, intent(in)	:: uu(0:nlev)
   REALTYPE, intent(in)	:: vv(0:nlev)
   REALTYPE, intent(in)	:: ww(0:nlev)
   REALTYPE, intent(in)	:: tt(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: variances.F90,v $
!  Revision 1.1  2001-02-12 15:55:59  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE     	:: dzu2,dzv2,dzt2
   REALTYPE, parameter	:: L1=0.1070
   REALTYPE, parameter	:: L2=0.0032
   REALTYPE, parameter	:: L3=0.0864
   REALTYPE, parameter	:: L4=0.1200
   REALTYPE, parameter	:: L5=11.9000
   REALTYPE, parameter	:: L6=0.4000
   REALTYPE, parameter	:: L7=0.0000
   REALTYPE, parameter	:: L8=0.4800
! 
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      dzu2=( (u(i+1)-u(i)) / (0.5*(h(i+1)+h(i))) )**2
      dzv2=( (v(i+1)-v(i)) / (0.5*(h(i+1)+h(i))) )**2
      dzt2=( (t(i+1)-t(i)) / (0.5*(h(i+1)+h(i))) )**2
      uu(i)=2./3.*k(i)+2./3.*k(i)/eps(i)*(                      &
            num(i)*((L2+3*L3)*dzu2-2*L2*dzv2)-2*L4*B(i))
      vv(i)=2./3.*k(i)+2./3.*k(i)/eps(i)*(                      &
            num(i)*((L2+3*L3)*dzv2-2*L2*dzu2)-2*L4*B(i))
      ww(i)=2./3.*k(i)+2./3.*k(i)/eps(i)*(                      &
            num(i)*((L2-3*L3)*(dzu2+dzv2))+4*L4*B(i))
      tt(i)=1.44*nuh(i)*k(i)/eps(i)*dzt2
      if (abs(uu(i)+vv(i)+ww(i)-2*k(i)) .gt. 1.e-8) then
         write(*,*) 'Velocity variances do not add up to 2k.'
         write(*,*) 'Program aborted.'
         write(*,*) i,uu(i),vv(i),ww(i)
         write(*,*) k(i),abs(uu(i)+vv(i)+ww(i)-2*k(i))
         stop
      end if
   end do 
   return
   end subroutine variances
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard & Karsten Bolding.
!-----------------------------------------------------------------------

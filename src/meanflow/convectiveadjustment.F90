!$Id: convectiveadjustment.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Convective adjustment for S,T and Buoy. 
!
! !INTERFACE:
   subroutine convectiveadjustment(nlev,num,nuh,const_num,const_nuh, &
                                   buoy_method,g,rho_0)
!
! !DESCRIPTION:
! 
! In this subroutine, convective adjustment is performed for temperature
! $T$ and salinity $S$, or alternatively for buoyancy $b$, if a dynamic
! equation is calculated for this quantity. Beginning from the first interface 
! below the surface, the water column is checked for static instability.
! If the Brunt Vaisala frequency squared ($N^2$) is negative, the two
! adjacent tracer boxes are completely mixed. The static stability for
! the interface below this homogenised upper part of the water column
! is then calculated, and, if needed, mixing performed again. By doing so,
! the water column is scanned as long, as a the first interface with
! statically stable or neutral stratification or the bottom is reached. 
!
! The constant values {\tt const-num} and {\tt const-nuh} are then imposed for
! eddy viscosity $\nu_t$ and eddy diffusivity $\nu'_t$, respectively. 
!
! !USES:
   use meanflow, only: h,t,s,buoy,NN
   use eqstate, only: eqstate1
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)		:: nlev,buoy_method
   REALTYPE, intent(in)		:: g,rho_0
   REALTYPE, intent(in)		:: const_num,const_nuh
!
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
   REALTYPE, intent(out)	:: num(1:nlev),nuh(1:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: convectiveadjustment.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer 			:: i,ii
   REALTYPE 			:: hint,Tint,Sint
   REALTYPE 			:: buoyupp,buoylow,buoyint
   REALTYPE 			:: zero=0.
!
!EOP
!-----------------------------------------------------------------------
!BOC
!  Imposing constant values for num and nuh.

   num=const_num
   nuh=const_nuh

!  Convective adjustment mixes down from the surface until the first stably
!  stratified interface is found or the bottom is reached. 

   if (buoy_method.eq.1) then  
      Tint=T(nlev)
      Sint=S(nlev)
      hint=h(nlev)
      i=nlev
111   i=i-1
      buoyupp=eqstate1(Sint,Tint,hint/10.,g,rho_0)
      buoylow=eqstate1(S(i),T(i),hint/10.,g,rho_0)
      if (buoyupp.lt.buoylow) then     ! instable stratification
         NN(i)=0.
         Tint=(Tint*hint+T(i)*h(i))/(hint+h(i))
         Sint=(Sint*hint+S(i)*h(i))/(hint+h(i))
         hint=hint+h(i)
         if (i.gt.1) then 
            goto 111
         else
            i=0        ! bottom reached 
         end if 
      end if 
      ii=i+1
      do i=ii,nlev
         T(i)=Tint     
         S(i)=Sint     
         buoy(i)=eqstate1(Sint,Tint,zero,g,rho_0)
      end do 
   else   ! if (buoy_method.eq.2) 
      buoyint=buoy(nlev)
      hint   =h(nlev)
      i=nlev
222   i=i-1
      if (buoyint.lt.buoy(i)) then     ! instable stratification 
         buoyint=(buoyint*hint+buoy(i)*h(i))/(hint+h(i))
         hint=hint+h(i)
         if (i.gt.1) then 
            goto 222 
         else          ! bottom reached 
            i=0
         end if 
      end if 
      ii=i+1
      do i=ii,nlev
         buoy(i)=buoyint     
      end do 
   end if 
   
   return
   end subroutine convectiveadjustment 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

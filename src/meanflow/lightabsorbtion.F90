!$Id: lightabsorbtion.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Light absorbtion in the water column.  
!
! !INTERFACE:
   subroutine light_absorbtion(nlev,I_0) 
!
! !DESCRIPTION:
!  The irradiance profile is used in the temperature equation as an
!  inner source term (however the sum of latent, sensible, and longwave
!  radiation is treated as a boundary condition) and used in the biological
!  equation to compute the depth-profile of PAR (Photosyntehic Active
!  Radiation). Absorbtion of radiation at interface levels is calculated
!  following various exponential law:

!  \begin{equation}
!   Rad(z)=Qlw+Qsw
!   Qlw=I_0*A*e^{-K1.zint}
!   Qsw=I_0*(1-A)*e^{-(K2+K3*Phy_av).zint} where Phy_av=Phyt/zint
!  \end{equation}

!  where "A"=the weighting function for spectral range
!  \& the extinction coefficients read:
!        "K1"=for long-wave radiation  "Qlw"    (red) [/m]
!        "K2"=for short-wave radiation "Qsw"    (visible blue-green) [/m]
!        "K3"=for biotic self-shading substance (Cholrophyll a) [m2/mmolN]
! !USES:
   use meanflow, only: h
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: I_0
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: lightabsorbtion.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   logical, save :: kbk_dummy = .true.
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if (kbk_dummy ) then
!kbk      STDERR 'light_absorption is not finished - we wait for PP'
      kbk_dummy = .false. 
   end if

#ifdef KAJ_KURT
   k1=one/g1
   k2=one/g2
   k3=zero
   if(A.lt.zero) k3=0.02 !Self-shading only when A<0
   A=abs(A)

   Rad(Nmx)=I_0
   z_int=zero
   Phy_t=zero
   do i=nlev-1,0,-1
      z_int=z_int+h(i+1)
      Phy_t=Phy_t+Phy(i)*h(i)         !Total Phy in mmolN/m3
      k4=k3*Phy_t/z_int               !Biotic Extinction
      k5=k2+k4                        !Total extinction
      Q_lw=I_0*(    A)*exp(-k1*z_int) !Long-wave radiation
      Q_sw=I_0*(one-A)*exp(-k5*z_int) !Short-wave radiation
      Par(i)=Q_sw                     !Photosynteticaly Active Radiation
      Rad(i)=Q_lw+Q_sw                !Total Irradiance
   end do
#endif
   return
   end subroutine light_absorbtion 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

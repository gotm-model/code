!$Id: lightabsorbtion.F90,v 1.3 2003-03-28 09:20:35 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The light absorbtion
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
!
!  \begin{equation}
!  \begin{array}{l}
!   Rad(z)=Qlw+Qsw \\ 
!   Qlw=I_0*A*e^{-K1.zint} \\
!   Qsw=I_0*(1-A)*e^{-(K2+K3*Phy_av).zint}, \mbox{where } Phy_av=Phyt/zint
!   \end{array}
!  \end{equation}
!
!  where "A"=the weighting function for spectral range
!  and the extinction coefficients read:
!
!        \begin{tabular}{ll}
!        "K1"=for long-wave radiation  "Qlw" &   (red) [/m] \\
!        "K2"=for short-wave radiation "Qsw" &   (visible blue-green) [/m] \\
!        "K3"=for biotic self-shading substance & (Cholrophyll a) [m2/mmolN]
!        \end{tabular}
!
! !USES:
   use meanflow, only: h
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	                :: nlev
   REALTYPE, intent(in)	                :: I_0
!
! !REVISION HISTORY:
!  Original author(s): Pierre-Phillipe Mathieu 
!
!  $Log: lightabsorbtion.F90,v $
!  Revision 1.3  2003-03-28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.2  2003/03/10 08:50:06  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
! !LOCAL VARIABLES:
   logical, save             :: kbk_dummy = .true.
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
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The NEMO stability function\label{sec:stabNEMO}
!
! !INTERFACE:
   subroutine cmue_nemo(nlev)
!
! !DESCRIPTION:
!  This subroutine computes stability functions according to
! \begin{equation}
! c_{\mu}=c_{\mu}^{0}=0.1,\qquad c'_{\mu}=c_{\mu}^0 Pr_t^{-1}
! \end{equation}
! with constant $c_{\mu}^0 = 0.1$ and $Pr_t$ the turbulent Prandtl number. 
! The empirical relation for the inverse turbulent Prandtl--number is 
! \begin{equation}
!   Pr_t^{-1} = \max\left(  0.1,  \mathrm{Ri}_c / \max( \mathrm{Ri}, \mathrm{Ri}_c ) \right)
!   \comma
! \end{equation}
! where $Ri$ is the gradient Richardson--number and \mathrm{Ri}_c its critical 
! value. This value is estimated from the local equilibrium $P+G = \epsilon$, 
! which translates into (considering $l_\epsilon=\sqrt{2k/N^2}$):
!\[
!\mathrm{num} S^2 - \mathrm{nuh} N^2 = c_\epsilon \frac{k \sqrt{N^2}}{\sqrt{2}}
!\]
!which amounts to
!\[
!c_{\mu}^0 l_k \sqrt{k} \left( S^2 - Pr_t^{-1} N^2 \right) = c_\epsilon \frac{k \sqrt{N^2}}{\sqrt{2}}
!\]
!Considering that $l_k=\sqrt{2k/N^2}$ and $\mathrm{Ri} = N^2 / S^2$ we get 
!\[
!1 - Pr_t^{-1} \mathrm{Ri} = \frac{c_\epsilon}{2 c_{\mu}^0} \mathrm{Ri} \qquad \longrightarrow \qquad \mathrm{Ri}_c = \frac{1}{1+\frac{1}{2}\frac{c_\epsilon}{c_{\mu}^0}} \approx 0.22
!\]
!
! !USES:

   use turbulence, only: cmue1,cmue2
   use turbulence, only: P,B  
   use turbulence, only: num,nuh
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Florian Lemarié 
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: Ri(0:nlev)
   REALTYPE                  :: Ri_loc,Ri_cri,Denom
   REALTYPE,parameter        :: limit=0.1
   REALTYPE,parameter        :: bshear=1.e-16
!
!-----------------------------------------------------------------------
!BOC
   cmue1(0:nlev) = 0.1
   Ri_cri = 2.*cmue1(1)/( 2.*cmue1(1) + 0.5*SQRT(2.) )
   
   do i=1,nlev-1
      if( B(i) >= 0.0 ) then
         Ri(i) = 0.
      else
         Denom = nuh(i)*P(i)
         if (Denom == 0.0) then
            Ri(i) = -B(i)*num(i) / bshear
         else
            Ri(i) = -B(i)*num(i) / Denom
         endif
      endif
   end do
   !
   Ri(0)=Ri(1); Ri(nlev)=Ri(nlev-1)
   !
   do i=1,nlev-1
      Ri_loc   = 0.25*(2.*Ri(i)+Ri(i+1)+Ri(i-1))
      cmue2(i) = cmue1(i)*MAX( limit, Ri_cri / MAX( Ri_cri , Ri_loc ) )
   enddo   
   !
   return
   end subroutine cmue_nemo
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

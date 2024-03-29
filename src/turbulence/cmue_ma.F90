#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The Munk and Anderson (1948) stability function\label{sec:cmueMA}
!
! !INTERFACE:
   subroutine cmue_ma(nlev)
!
! !DESCRIPTION:
!  This subroutine computes the stability functions
!  according to \cite{MunkAnderson48}. These are expressed
!  by the empirical relations
!  \begin{equation}
!    \begin{array}{ll}
!      c_{\mu} = c_\mu^0                          \comma             \\[3mm]
!      c_{\mu}'= \dfrac{c_{\mu}}{Pr_t^0} \,
!      \dfrac{(1+10 Ri)^{1/2}}{(1+3.33 Ri)^{3/2}} \comma &  Ri \geq 0 \\
!      c_{\mu}'= c_{\mu}                          \comma &  Ri  <   0
!      \comma
!    \end{array}
!  \end{equation}
!  where where $Ri$ is the gradient Richardson-number and $Pr_t^0$
! is the turbulent Prandtl-number for $Ri \rightarrow 0$. $Pr_t^0$
! and the fixed value $c_\mu^0$ have to be set in {\tt gotm.yaml}.
!
! !USES:
   use turbulence, only: cm0_fix,Prandtl0_fix
   use turbulence, only: cmue1,cmue2,as,an
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: Ri,Prandtl
!
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      Ri=an(i)/(as(i)+1e-8)   ! Gradient Richardson number
      if (Ri.ge.1e-10) then
         Prandtl=Prandtl0_fix*(1.+3.33*Ri)**1.5/sqrt(1.+10.0*Ri)
      else
         Prandtl=Prandtl0_fix
      end if
      cmue1(i)=cm0_fix
      cmue2(i)=cm0_fix/Prandtl
   end do

   return
   end subroutine cmue_ma
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

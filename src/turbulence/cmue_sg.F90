#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The Schumann and Gerz (1995) stability function\label{sec:sg}
!
! !INTERFACE:
   subroutine cmue_sg(nlev)
!
! !DESCRIPTION:
!  This subroutine computes stability functions according to
! \begin{equation}
! c_{\mu}=c_{\mu}^0,\qquad c'_{\mu}=\frac{c_{\mu}^0}{Pr_t}
! \end{equation}
! with constant $c_{\mu}^0$. Based simulation data on stratified homogeneous
! shear-flows, \cite{SchumannGerz95} proposed the empirical relation
! for the turbulent Prandtl--number,
! \begin{equation}
!   Pr_t = Pr_t^0 \exp\left(-\frac{Ri}{Pr_t^0 Ri^{\infty}}\right)
!   -\frac{Ri}{Ri^{\infty}}
!   \comma
! \end{equation}
! where where $Ri$ is the gradient Richardson--number and $Pr_t^0$
! is the turbulent Prandtl--number for $Ri \rightarrow 0$. $Pr_t^0$
! and the fixed value $c_\mu^0$ have to be set in {\tt gotmturb.nml}.
! \cite{SchumannGerz95}  suggested $Pr_t^0=0.74$ and $Ri^{\infty}=0.25$.
!
! !USES:
   use turbulence, only: Prandtl0_fix,cm0_fix
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
   REALTYPE,parameter        :: limit=3.
!
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      Ri=an(i)/(as(i)+1.e-8)   ! Gradient Richardson number
      if (Ri.ge.1e-10) then
         Prandtl=Prandtl0_fix*exp(-Ri/(Prandtl0_fix*0.25))+Ri/0.25
      else
         Prandtl=Prandtl0_fix
      end if

      cmue1(i)=cm0_fix
      cmue2(i)=cm0_fix/min(limit,Prandtl)

   end do
   return
   end subroutine cmue_sg
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

!$Id: tkealgebraic.F90,v 1.5 2003-03-28 09:20:35 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The algebraic $k$--equation \label{sec:tkealgebraic} 
! 
! !INTERFACE:
   subroutine tkealgebraic(nlev,u_taus,u_taub,NN,SS)
!
! !DESCRIPTION:
!  This subroutine calculates the turbulent kinetic energy based
!  on the local equilibrium assumption
!
!  \begin{equation}
!   \label{localEQa}  
!     P+B-\epsilon=0
!    \point
!  \end{equation}  
! This equation can be expressed in the form
!  \begin{equation}
!   \label{localEQb}  
!     k= (c_\mu^0)^{-3} \, l^2 ( c_\mu M^2 - c'_\mu N^2 )
!    \comma
!  \end{equation}  
!  were we used the expressions in \eq{PandG} together with 
!  \eq{fluxes} and \eq{nu}. The rate of dissipaton, $\epsilon$, 
!  has been expressed in terms of $l$ via \eq{epsilon}.
!  This equation has been implemented to update $k$ in a diagnostic
!  manner.
!
! !USES:
   use turbulence, only: tkeo,tke,L,k_min,cmue2,cde,cmue1,cm0
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: u_taus,u_taub
   REALTYPE, intent(in)                :: NN(0:nlev),SS(0:nlev)
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: tkealgebraic.F90,v $
!  Revision 1.5  2003-03-28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.4  2003/03/28 08:29:16  kbk
!  removed tabs
!
!  Revision 1.3  2003/03/10 09:02:05  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!  Revision 1.2  2002/02/08 08:59:58  gotm
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
!
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      tkeo(i)=tke(i)
      tke(i)=L(i)*L(i)/cde*(cmue1(i)*SS(i)-cmue2(i)*NN(i))
   end do 

   tke(0)=u_taub*u_taub/sqrt(cm0*cde) 
   tke(nlev)=u_taus*u_taus/sqrt(cm0*cde)

   do i=0,nlev
      if (tke(i).lt.k_min) tke(i)=k_min 
   end do 
 
   return
   end subroutine tkealgebraic
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

#include"cppdefs.h"
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: Diagnostic length-scale adapted from Gaspar with two master scales \label{sec:lengthscale_gaspar}
!
! !INTERFACE:               
   subroutine lengthscale_gaspar(nlev,z0s,z0b,h,NN)

! !DESCRIPTION:
!  This subroutine computes the length scale used in the NEMO mode, see Madec et al. (2025). 
!  Two master length scales $l_u$ and $l_d$ are defined and initialised with the Deardorff 
!  canonical value $l_{d80}=\sqrt(2k/N^2)$. The Deardorff length scale corresponds to a local 
!  approximation of the Bougeault and André (1986) length scales. 
!  Starting from $l_{\rm u}=l_{\rm d}=l_{\rm d80}$, the length scales are then limited not only 
!  by the distance to the surface and to the top but also by the distance to a strongly stratified portion of the air column. 
!  This limitation amounts to controlling the vertical gradients of $l_{\rm u}$ and $l_{\rm d}$ so that they do not exceed the depth variations. 
!  From $l_u$ and $l_d$ two length--scales are defined: $l_k$,
!  a characteristic mixing length, and $l_\epsilon$, a characteristic dissipation length.
!  They are computed according to Gaspar et al. (1990)
!   \begin{equation}
!   \begin{array}{l}
!   l_k(z)= \text{Min} ( l_d(z),l_u(z)) \comma \\[4mm]
!   l_{\epsilon}(z)=\left( l_d(z)l_u(z)\right)^\frac{1}{2}
!   \point
!   \end{array}
!   \end{equation}
!
!   $l_k$ is used in {\tt kolpran()} to compute eddy viscosity/difussivity.
!   $l_{\epsilon}$ is used to compute the dissipation rate $\epsilon$,
!    according to
!   \begin{equation}
!     \epsilon=C_{\epsilon} k^{3/2} l_{\epsilon}^{-1}
!     \comma
!     C_{\epsilon}=\frac{\sqrt{2}}{2}
!    \point
!   \end{equation}
!
! !USES:
   use turbulence, only: L,eps,tke,k_min,eps_min
   use turbulence, only: kappa

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: nlev

!  bottom and surface roughness (m)
   REALTYPE, intent(in)                :: z0b,z0s

!  layer thickness (m)
   REALTYPE, intent(in)                :: h(0:nlev)

!  buoyancy frequency (1/s^2)
   REALTYPE, intent(in)                :: NN(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s):  Florian Lemarié 
!
!EOP
!-------------------------------------------------------------------------
! !LOCAL VARIABLES:
   integer                   :: i,j
   REALTYPE                  :: lup (0:nlev),ldwn(0:nlev), ld80(0:nlev)
   REALTYPE                  :: rn2
   REALTYPE                  :: L_min, ceps, leps
   REALTYPE, parameter       :: NNmin =1.e-20
   REALTYPE, parameter       :: L_min0=0.04
!BOC
   ceps  = 0.5*SQRT(2.)
   L_min = ceps*k_min**1.5/eps_min
!-------------------------------------------------------------------------
!  Calculation of lu and ld based on Deardorff (1980) and further  
!  limitations following the NEMO methodology
!-------------------------------------------------------------------------
   do i=1,nlev-1
      rn2 = MAX( NN(i),NNmin )
      ld80(i)=SQRT( 2.*tke(i)/rn2 )
   enddo 
   !
   ld80(0   ) = MAX( L_min , kappa*z0b )
   ld80(nlev) = MAX( L_min0, kappa*z0s )
!-------------------------------------------------------------------------
! Physical limits for the mixing lengths 
!-------------------------------------------------------------------------
   ldwn(0  ) = ld80(0)    ! bottom boundary condition
   do i = 1, nlev
      ldwn(i) = MIN( ldwn(i-1) + h(i  ) , ld80(i) )
   enddo
   !
   lup(nlev) = ld80(nlev) ! surface boundary condition
   do i = nlev-1,0,-1
      lup (i) = MIN( lup (i+1) + h(i+1) , ld80(i) )
   enddo
!-------------------------------------------------------------------------
!   Calculation of L and eps
!-------------------------------------------------------------------------
   L(0   )=kappa*z0b
   L(nlev)=kappa*z0s
   !
   do i=1,nlev-1
      L(i) = MIN( lup(i), ldwn(i) ) ! used in kolpran   
   end do
   !
   do i=0,nlev 
      leps   = SQRT( lup(i)*ldwn(i) ) 
      eps(i) = (ceps/leps)*sqrt(tke(i)*tke(i)*tke(i)) ! used in tkeeq 
   enddo 

   return
   end subroutine lengthscale_gaspar
!EOC
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

!$Id: stratification.F90,v 1.4 2003-03-28 08:56:56 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculation of $N^2$ and $b$ \label{sec:stratification}
!
! !INTERFACE:
   subroutine stratification(nlev,buoy_method,dt,cnpar,g,rho_0,nuh)
!
! !DESCRIPTION:
!
!  The first part in this subroutine is devoted to the computation of
!  the (square of the ) Brunt--V\"ais\"al\"a frequency, $N^2=\partial_zb$,
!  with the buoyancy $b$ defined by \eq{DefBuoyancy}. The differencing
!  scheme for $N^2$ can be written as
!  \begin{equation}
!    \left(N^2\right)_{k+1/2}
!    = \frac{b(S_{k+1},\theta_{k+1},p_{k+1})
!           -b(S_k,\theta_k,p_k)}
!           {\frac12(h_{k+1}+h_k)}
!     \comma
!  \end{equation}
!  where the buoyancy is computed from an equation of state,
!  \begin{equation}
!    b_k=b(S_k,\theta_k,p_k)
!    \comma
!  \end{equation}
!  as a function of salinity, temperature and pressure (see 
!  \sect{sec:eqstate}). Note, that the local pressure (in bar) is
!  computed here from the relation $p \approx 0.1 z$, which is only
!  a coarse (but for this purpose sufficient) approximation of the 
!  hydrostatic limit. 
!  The Brunt--V\"ais\"al\"a frequency is
!  located at $z_{k+1/2}$, which is the vertical position of the interface 
!  between the boxes with indices $k$ and $k+1$. 
!
!  Alternatively, depending on the values of the parameter {\tt buoy\_method}, 
!  the buoyancy may be calculated by means of the transport equation \eq{bEq}.
!
! !USES:
   use meanflow, only: h,S,T,NN,buoy
   use eqstate, only: eqstate1
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev,buoy_method
   REALTYPE, intent(in)                :: dt,g,rho_0,cnpar
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: nuh(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: stratification.F90,v $
!  Revision 1.4  2003-03-28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.3  2003/03/10 08:50:07  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.2  2001/11/18 11:50:37  gotm
!  Cleaned
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: buoyp,buoym,z
   REALTYPE                  :: dz
   integer,parameter         :: USEEQSTATE=1
!
!-----------------------------------------------------------------------
!BOC
   if (buoy_method .EQ. USEEQSTATE) then
      z=0.0  
      do i=nlev-1,1,-1 
         z=z+h(i+1)
         dz=0.5*(h(i)+h(i+1)) 
         buoyp=eqstate1(S(i+1),T(i+1),z/10.,g,rho_0) 
         buoym=eqstate1(S(i  ),T(i  ),z/10.,g,rho_0) 
         NN(i)=(buoyp-buoym)/dz 
      end do
      z=0. 
      do i=nlev,1,-1 
         buoy(i)=eqstate1(S(i),T(i),z/10.,g,rho_0) 
      end do
   else
      call buoyancy(nlev,dt,cnpar,nuh) 
      do i=nlev-1,1,-1 
         dz=0.5*(h(i)+h(i+1)) 
         NN(i)=(buoy(i+1)-buoy(i))/dz 
      end do
   end if

   
   NN(0)=NN(1)
   NN(nlev)=NN(nlev-1)
 
   return
   end subroutine stratification
!EOC

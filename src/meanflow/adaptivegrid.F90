!$Id: adaptivegrid.F90,v 1.4 2003-12-11 09:58:22 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The vertically adaptive grid \label{sec:adaptivegrid}
!
! !INTERFACE:
   subroutine adaptivegrid(ga,NN,SS,h,depth,nlev,dt)
!
! !DESCRIPTION:
! 
! This routine creates a vertical grid dynamically adapting 
! to the internal flow dynamics. As described in detail 
! in \cite{BurchardBeckers2003}, an diffusion equation 
! of the form
! \begin{equation}\label{grid_eq}
!  \partder{z}{t} -
!  \frstder{\sigma} \left( k^{grid} \partder{z}{\sigma} \right)=0
! \end{equation}
! has to be solved for the vertical 
! coordinate $z$ in the normalised $z$--space (or $\sigma$--space)
! for $\sigma\in [-1,0]$. $k^{grid}$ is a grid--related diffusivity
! with physical unit s$^{-1}$. It is computed from
! \begin{equation}
! k^{grid}=\frac{cD}{T^{grid}}
! \left(c_{\rho}K^{grid}_{\rho}+c_uK^{grid}_u+c_dK^{grid}_d+c_bK^{grid}_b\right)
! \end{equation}
! with the stratification--related component
! \begin{equation}\label{Krho}
!   K^{grid}_{\rho} = \frac{\max(0,\partial_z\rho)}{\Delta \rho}
!   \comma
! \end{equation}
! the shear--related component
! \begin{equation}\label{Ku}
!   K^{grid}_u = \frac{M}{\Delta u}
!  \comma
! \end{equation}
! the near--surface component
! \begin{equation}\label{Kd}
!   K^{grid}_d = \frac{1}{d+d_0}
!   \comma
! \end{equation}
! and the background component
! \begin{equation}\label{comp4}
!   K^{grid}_b = \frac{1}{D}
!  \point
! \end{equation}
! 
! Here, $\Delta \rho$ is a reference density difference and
! $\Delta u$ a reference velocity difference.
! The grid diffusion time scale is denoted by $T^{grid}$.
! The parameter $d$ is the distance from the surface and
! $d_0$ is a quantity determining the intensity of
! the near--surface grid zooming.
!
! \eq{grid_eq} is discretised according to
! \begin{equation}\label{iterate_grid}
!    z_i^{n+1}=z_i^{n}+N_i^2\Delta t^{grid}
!   \left(k^{grid}_{i+1}\left(z_{i+1}^{n+1}-z_i^{n+1}\right)
!   -k^{grid}_{i}\left(z_i^{n+1}-z_{i-1}^{n+1}\right)\right)
! \end{equation}
! for $i=1,\dots,N_i-1$ with $N_i$ being the number of spatial grid
! intervals. For details, see \cite{BurchardBeckers2003}.
!
! !USES:
   use meanflow, only: c1ad,c2ad,c3ad,c4ad,Tgrid,NNnorm,SSnorm
   use meanflow, only: dsurf,dtgrid
   use mTridiagonal
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: NN(0:nlev),SS(0:nlev),h(0:nlev)
   REALTYPE, intent(in)                :: depth,dt
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE                            :: ga(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Jean-Marie Beckers
!
!  $Log: adaptivegrid.F90,v $
!  Revision 1.4  2003-12-11 09:58:22  kbk
!  now compiles with FORTRAN_COMPILER=IFORT - removed TABS
!
!  Revision 1.3  2003/03/28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.2  2003/03/28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.1  2003/03/10 08:49:52  gotm
!  Added support for adaptive vertical grid
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i,j,split
   REALTYPE                  :: ganew(0:nlev),gaold(0:nlev)
   REALTYPE                  :: NNloc(0:nlev),SSloc(0:nlev)
   REALTYPE                  :: rat,center,g=9.81
   logical,save              :: first=.true.
   REALTYPE,dimension(0:nlev):: av1,av2,av3,av
!
!-----------------------------------------------------------------------
!BOC
  if (abs(dt/dtgrid-nint(dt/dtgrid)).gt.1.e-10) then
     STDERR 'dt/dtgrid must be integer number.' 
     STDERR 'Please adjust dt or dtgrid accordingly.'
     STDERR ' Program aborted in adaptivegrid.'
     stop 
  end if

  split=nint(dt/dtgrid)

   gaold=ga
   NNloc=NN
   SSloc=SS
   ga(nlev)=0.
   ganew(nlev)=0.
   ganew(0)=-1.
   do j=1,split
      call gridinterpol(nlev-1,1,gaold,NN,nlev-1,ga,NNloc)
      call gridinterpol(nlev-1,1,gaold,SS,nlev-1,ga,SSloc)

!     Stratification 
      do i=1,nlev
         av1(i)=min(_ONE_,max(_ZERO_,0.5*(NNloc(i)+NNloc(i-1)))/g*1024./NNnorm) 
      end do 

!     Shear
      do i=1,nlev
         av2(i)=min(_ONE_,sqrt(max(_ZERO_,0.5*(SSloc(i)+SSloc(i-1))))/SSnorm) 
      end do

!     Distance from surface
      do i=1,nlev
         av3(i)=_ONE_/(dsurf-0.5*(ga(i-1)+ga(i))*depth) 
      end do 

!     Calculation of grid diffusivity
      do i=1,nlev
         av(i)=depth/Tgrid*(c1ad*av1(i)+c2ad*av2(i)+c3ad*av3(i)+c4ad/depth) 
         av(i)=av(i)*dtgrid*nlev**2/100.
!        Minimum layer thickness
         if ((ga(i)-ga(i-1)).lt.0.001/float(nlev)) av(i)=0.
      end do 

      do i=1,nlev-1
         au(i)=-av(i)
         cu(i)=-av(i+1)
         bu(i)=1.-au(i)-cu(i)
         du(i)=ga(i)
      end do 
      cu(0)=0
      bu(0)=1.
      du(0)=-1.

      bu(nlev)=1.
      au(nlev)=0.
      du(nlev)=0.

      call tridiagonal(nlev,0,nlev,ga)
   end do  

   if (first) then
      first=.false.
   end if 

   end subroutine adaptivegrid 
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

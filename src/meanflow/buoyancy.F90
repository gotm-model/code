!$Id: buoyancy.F90,v 1.4 2003-03-28 08:56:56 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The buoyancy equation 
! 
! !INTERFACE:
   subroutine buoyancy(nlev,dt,cnpar,nuh)
!
! !DESCRIPTION:
!  This subroutine computes the transport of the buoyancy,
!  \begin{equation}
!    \label{DefBuoyancy}
!    b=-g\frac{\rho-\rho_0}{\rho_0}
!    \comma
!  \end{equation}
!  where $g$ is the accelaration of gravity, and $\rho$ and $\rho_0$ 
!  are the actual and the reference densitiy. 
!  A simplified transport equation for $b$ can be written as
!  \begin{equation}
!   \label{bEq}
!    \dot{b}
!    = {\cal D}_b
!    \comma
!  \end{equation}
!  where $\dot{b}$ denotes the material derivative of $b$, and
!  ${\cal D}_b$ is the sum of the turbulent and viscous transport
!  terms modelled according to
!  \begin{equation}
!   \label{Db}
!    {\cal D}_b 
!    = \frstder{z} 
!     \left( 
!        \nu'_t\partder{b}{z}
!      \right) 
!    \point
!  \end{equation}
!  In this equation, $\nu'_t$ is the turbulent diffusivity
!  of buoyancy. 
!  The computation
!  of $\nu'_t$ is discussed in \sect{sec:turbulenceIntro}. Note, 
!  that the model \eq{DS} assumes that turbulent transport of heat
!  and salt is identical.  Source and sink
! terms are completely disregarded, and thus \eq{bEq} serves
! mainly as a convenient tool for some idealised test cases in 
! GOTM.
!
!  Diffusion is treated implicitly in space (see equations (\ref{sigmafirst})-
!  (\ref{sigmalast})), and then solved by a 
!  simplified Gauss elimination. 
!   Vertical advection is included for accounting for adaptive grids,
!  see {\tt adaptivegrid.F90}.
!
! !USES:
   use mtridiagonal
   use meanflow,     only:   h,ho,buoy,avh,w,w_grid,grid_method
   use observations, ONLY:   b_obs_NN,b_obs_surf,b_obs_sbf
   use observations, ONLY:   w_adv_discr,w_adv_method
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS: 
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt,cnpar
   REALTYPE, intent(in)                :: nuh(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: buoyancy.F90,v $
!  Revision 1.4  2003-03-28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.3  2003/03/10 08:50:06  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                      :: i,Meth,Bcup,Bcdw,flag 
   REALTYPE                     :: zz,tot
   logical, save                :: first=.true.
   REALTYPE                     :: Qsour(0:nlev),RelaxT(0:nlev)
   REALTYPE                     :: Tup,Tdw,z
   logical                      :: surf_flux,bott_flux
!
!-----------------------------------------------------------------------
!BOC
!  Construct initial linear profile from information in namelist 
   if (first) then
      zz=0.0
      do i=nlev,1,-1
         zz=zz+0.5*h(i)
         buoy(i)  = b_obs_surf - zz*b_obs_NN
         zz=zz+0.5*h(i)
      end do
      first=.false.
   end if

!  hard coding of parameters, to be included into namelist for gotm2.0
   Bcup=1                               !BC Neumann
   Tup=b_obs_sbf                        !Buoyancy flux 
   Bcdw=1                               !BC Neumann
   Tdw=0.                               !No flux
   surf_flux=.false.
   bott_flux=.false.

   avh=nuh
   Qsour=0.
   RelaxT=1.e15

   flag=1  ! divergence correction for vertical advection

   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Tup,Tdw,RelaxT,h,ho,avh,w,        &
              Qsour,buoy,w_adv_method,w_adv_discr,buoy,surf_flux,  &
              bott_flux,grid_method,w_grid,flag)

   return
   end subroutine buoyancy
!EOC

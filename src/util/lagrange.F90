#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Lagrangian particle random walk \label{sec:lagrange}
!
! !INTERFACE:
   subroutine lagrange(nlev,dt,zlev,nuh,w,npar,active,zi,zp)
!
! !DESCRIPTION:
!
! Here a Lagrangian particle random walk for spatially
! inhomogeneous turbulence according to \cite{Visser1997} is implemented.
! With the random walk, the particle $i$ is moved from the vertical
! position $z_i^n$ to $z_i^{n+1}$ according to the following algorithm:
! \begin{equation}
! \begin{array}{rcl}
! z_i^{n+1} &=&
! z^n_i + \partial_z \nu_t (z^n_i)\Delta t \\ \\
! &+&
! R \left\{2 r^{-1} \nu_t (z^n_i + \frac12  \partial_z \nu_t (z^n_i)\Delta t)
! \Delta t\right\}^{1/2},
! \end{array}
! \end{equation}
! where $R$ is a random process with $\langle R \rangle =0$ (zero mean) and
! and the variance $\langle R^2 \rangle=r$.
! Set {\tt visc\_corr=.true.} for
! evaluating eddy viscosity in a semi-implicit way. A background viscosity
! ({\tt visc\_back}) may be set. The variance $r$ of the random walk scheme
! ({\tt rnd\_var}) has to be set manually as well here.
!
! !USES:
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: zlev(0:nlev)
   REALTYPE, intent(in)                :: nuh(0:nlev)
   REALTYPE, intent(in)                :: w
   integer, intent(in)                 :: npar
   logical, intent(in)                 :: active(npar)
!
! !INPUT/OUTPUT PARAMETERS:
   integer, intent(inout)              :: zi(npar)
   REALTYPE, intent(inout)             :: zp(npar)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   integer            :: i,n
   REALTYPE           :: rnd(npar),rnd_var_inv
   REALTYPE,parameter :: visc_back=0.e-6,rnd_var=0.333333333
   REALTYPE           :: depth,dz(nlev),dzn(nlev),step,zp_old
   REALTYPE           :: visc,rat,dt_inv,zloc
   logical,parameter  :: visc_corr=.false.
!EOP
!-----------------------------------------------------------------------
!BOC

   dt_inv=1./dt
   rnd_var_inv=1./rnd_var

   call random_number(rnd)
   rnd=(2.*rnd-1.)

   do i=1,nlev
      dz(i)=zlev(i)-zlev(i-1)
      dzn(i)=(nuh(i)-nuh(i-1))/dz(i)
   end do

   depth=-zlev(0)
   do n=1,npar
!     local viscosity calculation
      if (visc_corr) then ! correction suggested by Visser [1997]
         zloc=zp(n)+0.5*(dzn(zi(n))+w)*dt
         do while (zloc .lt. -depth .or. zloc .gt. _ZERO_)
            if (zloc .lt. -depth) then
               zloc=-depth+(-depth-zloc)
            else
               zloc=-zloc
            end if
         end do
         step=zloc-zp(n)
         if (step.gt.0) then ! search new index above old index
            do i=zi(n),nlev
               if (zlev(i) .gt. zloc) EXIT
            end do
         else                ! search new index below old index
            do i=zi(n),1,-1
               if (zlev(i-1) .lt. zloc) EXIT
            end do
         end if
      else
         i=zi(n)
         zloc=zp(n)
      end if
      rat=(zloc-zlev(i-1))/dz(i)
      visc=rat*nuh(i)+(1.-rat)*nuh(i-1)
      if (visc.lt.visc_back) visc=visc_back
      zp_old=zp(n)
      step=dt*(sqrt(2.*rnd_var_inv*dt_inv*visc)*rnd(n)+w+dzn(i))
      zp(n)=zp(n)+step
      do while (zp(n) .lt. -depth .or. zp(n) .gt. _ZERO_)
         if (zp(n) .lt. -depth) then
            zp(n)=-depth+(-depth-zp(n))
         else
            zp(n)=-zp(n)
         end if
      end do
      step=zp(n)-zp_old
      if (step.gt.0) then ! search new index above old index
         do i=zi(n),nlev
            if (zlev(i) .gt. zp(n)) EXIT
         end do
      else                ! search new index below old index
         do i=zi(n),1,-1
            if (zlev(i-1) .lt. zp(n)) EXIT
         end do
      end if
      zi(n)=i
   end do

   return
   end subroutine lagrange
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

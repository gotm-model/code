!$Id: salinity.F90,v 1.14 2008-04-09 11:56:31 kb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The salinity equation \label{sec:salinity}
!
! !INTERFACE:
   subroutine salinity(nlev,dt,cnpar,nus,gams)
!
! !DESCRIPTION:
! This subroutine computes the balance of salinity in the form
!  \begin{equation}
!   \label{SEq}
!    \dot{S}
!    = {\cal D}_S
!    - \frac{1}{\tau^S_R}(S-S_{obs})
!    \comma
!  \end{equation}
!  where $\dot{S}$ denotes the material derivative of the salinity $S$, and
!  ${\cal D}_S$ is the sum of the turbulent and viscous transport
!  terms modelled according to
!  \begin{equation}
!   \label{DS}
!    {\cal D}_S
!    = \frstder{z}
!     \left(
!        \left( \nu^S_t + \nu^S \right) \partder{S}{z} - \tilde{\Gamma}_S
!        \right)
!    \point
!  \end{equation}
!  In this equation, $\nu^S_t$ and $\nu^S$ are the turbulent and
!  molecular diffusivities of salinity, respectively,
!  and $\tilde{\Gamma}_S$
!  denotes the non-local flux of salinity, see
!  \sect{sec:turbulenceIntro}. In the current version of GOTM,
!  we set $\nu^S_t = \nu^\Theta_t$ for simplicity.
!
!  Horizontal advection is optionally
!  included  (see {\tt obs.nml}) by means of prescribed
!  horizontal gradients $\partial_xS$ and $\partial_yS$ and
!  calculated horizontal mean velocities $U$ and $V$.
!  Relaxation with the time scale $\tau^S_R$
!  towards a precribed (changing in time)
!  profile $S_{obs}$ is possible.

!  Inner sources or sinks are not considered.
!  The surface freshwater flux is given by means of the precipitation
!  - evaporation data read in as $P-E$ through the {\tt airsea.nml} namelist:
!  \begin{equation}
!     \label{S_sbc}
!    {\cal D}_S =  S (P-E),
!    \qquad \mbox{at } z=\zeta,
!  \end{equation}
!  with $P-E$ given as a velocity (note that ${\cal D}_S$ is the flux in the
!  direction of $z$, and thus positive for a \emph{loss} of salinity) .
!  Diffusion is numerically treated implicitly,
!  see equations (\ref{sigmafirst})-(\ref{sigmalast}).
!  The tri-diagonal matrix is solved then by a simplified Gauss elimination.
!  Vertical advection is included, and it must be non-conservative,
!  which is ensured by setting the local variable {\tt adv\_mode=0},
!  see section \ref{sec:advectionMean} on page \pageref{sec:advectionMean}.
!
! !USES:
   use meanflow,     only: avmols
   use meanflow,     only: h,u,v,w,S,avh
   use observations, only: dsdx,dsdy,s_adv
   use observations, only: w_adv_discr,w_adv_method
   use observations, only: sprof,SRelaxTau
   use airsea,       only: precip,evap
   use util,         only: Dirichlet,Neumann
   use util,         only: oneSided,zeroDivergence
!#ifdef _LAKE_
   use meanflow,     only: hypsography_file
   use meanflow,     only: hypsography,hypsography_slope
   use meanflow,     only: idealised
   use util,         only: flux
   use meanflow,     only: adv_error
!#endif

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)                 :: nlev

!   time step (s)
   REALTYPE, intent(in)                :: dt

!  numerical "implicitness" parameter
   REALTYPE, intent(in)                :: cnpar

!  diffusivity of salinity (m^2/s)
   REALTYPE, intent(in)                :: nus(0:nlev)

!  non-local salinity flux (psu m/s)
   REALTYPE, intent(in)                :: gams(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: salinity.F90,v $
!  Revision 1.14  2008-04-09 11:56:31  kb
!  GOTM/GETM concensus on signs for precip and evap - both positive into the ocean
!
!  Revision 1.13  2008-03-07 17:57:49  hb
!  AdvBcup changed to oneSided
!
!  Revision 1.12  2007-12-07 10:12:20  kb
!  replaced p_e with precip and included evap
!
!  Revision 1.11  2007-01-06 11:49:15  kbk
!  namelist file extension changed .inp --> .nml
!
!  Revision 1.10  2006-11-06 13:36:45  hb
!  Option for conservative vertical advection added to adv_center
!
!  Revision 1.9  2005-11-17 09:58:20  hb
!  explicit argument for positive definite variables in diff_center()
!
!  Revision 1.8  2005/06/27 13:44:07  kbk
!  modified + removed traling blanks
!
!  Revision 1.7  2004/08/18 11:43:10  lars
!  updated documentation
!
!  Revision 1.6  2004/01/07 12:17:47  lars
!  Removed latex bug
!
!  Revision 1.5  2003/06/13 09:27:15  hb
!  Implemented freshwater fluxes
!
!  Revision 1.4  2003/03/28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.3  2003/03/28 08:56:56  kbk
!  removed tabs
!
!  Revision 1.2  2003/03/10 08:50:07  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: adv_mode=0
   integer                   :: posconc=1
   integer                   :: i
   integer                   :: DiffBcup,DiffBcdw
   integer                   :: AdvBcup,AdvBcdw
   REALTYPE                  :: DiffSup,DiffSdw
   REALTYPE                  :: AdvSup,AdvSdw
   REALTYPE                  :: Lsour(0:nlev)
   REALTYPE                  :: Qsour(0:nlev)
!#ifdef _LAKE_
   REALTYPE                  :: AdvSpeed(0:nlev)
!#endif
!
!-----------------------------------------------------------------------
!BOC
!
!  set boundary conditions
   DiffBcup       = Neumann
   DiffBcdw       = Neumann
   DiffSup        = -S(nlev)*(precip+evap)
   DiffSdw        = _ZERO_

!#ifdef _LAKE_
   if (hypsography_file /= '') then
      AdvBcup       = flux
      AdvBcdw       = flux
   else
!#else
      AdvBcup       = oneSided
      AdvBcdw       = oneSided
   end if
!#endif
   AdvSup        = _ZERO_
   AdvSdw        = _ZERO_

!  compute total diffusivity
   do i=0,nlev
!#ifdef _IDEALISED_
      if (idealised) then
         avh(i)=nus(i)
      else
!#else
         avh(i)=nus(i)+avmolS
      end if
!#endif
   end do

!  add contributions to source term
   Lsour=_ZERO_
   Qsour=_ZERO_

   do i=1,nlev
!     from non-local turbulence
#ifdef NONLOCAL
      Qsour(i) = Qsour(i) - ( gams(i) - gams(i-1) )/h(i)
#endif
   end do

!  ... and from lateral advection
   if (s_adv) then
      do i=1,nlev
         Qsour(i) = Qsour(i) - u(i)*dsdx(i) - v(i)*dsdy(i)
      end do
   end if

!#ifdef _LAKE_
   if (hypsography_file /= '') then
!     transform to new variables
!     watch out for different boundary types they can be defined at
!     grid center and at grid face
      select case(DiffBcup)
      case(Neumann)
         DiffSup = DiffSup * hypsography(nlev)
      case(Dirichlet)
         DiffSup = DiffSup * (hypsography(nlev) + hypsography(nlev-1))/2
      case default
         FATAL 'invalid boundary condition type for upper boundary'
         stop 'salinity.F90'
      end select

      select case(DiffBcdw)
      case(Neumann)
         DiffSdw = DiffSdw * hypsography(0)
      case(Dirichlet)
         DiffSdw = DiffSdw * (hypsography(1) + hypsography(0))/2
      case default
         FATAL 'invalid boundary condition type for lower boundary'
         stop 'salinity.F90'
      end select
!     only one boundary type is applicable to advection routine
!     and these are always zero, so no transformation is needed

!     compute all neccessary things at the grid centers
      do i = 1, nlev
         S(i) =  S(i) * (hypsography(i) + hypsography(i-1))/2
         sprof(i) = sprof(i) * (hypsography(i) + hypsography(i-1))/2
         Qsour(i) = Qsour(i) * (hypsography(i) + hypsography(i-1))/2
      end do
!     compute all neccessary things at the grid interfaces
!     set up the advection speed
!     with "normal" advection (w) too
      if (w_adv_method.ne.0) then
         do i = 0, nlev
            AdvSpeed(i) = w(i) + avh(i) * hypsography_slope(i) / hypsography(i)
         end do
      else
         do i = 0, nlev
            AdvSpeed(i) = avh(i) * hypsography_slope(i) / hypsography(i)
            write(*,*) i, ",", hypsography_slope(i)
         end do
      end if
      stop

!     do advection step for lake model
      call adv_center(nlev,dt,h,h,AdvSpeed,AdvBcup,AdvBcdw,                &
                           AdvSup,AdvSdw,w_adv_discr,1,S,adv_error)
      if (adv_error) then
         write(*,*) "Courant number greater than 1 for salinity"
         do i = 0, nlev
            if (AdvSpeed(i) .gt. 0.0) then
            write(*,*) "i = ", i, " | AdvSpeed(i) = ", AdvSpeed(i), " | mu = ",&
               AdvSpeed(i) * dt / h(i)
            end if
         end do
         write(*,*) " "
      end if
   else
!#else
!     do advection step
      if (w_adv_method .ne. 0) then
         call adv_center(nlev,dt,h,h,w,AdvBcup,AdvBcdw,                    &
                             AdvSup,AdvSdw,w_adv_discr,adv_mode,S,adv_error)
      end if
   end if
!#endif

!  do diffusion step
   call diff_center(nlev,dt,cnpar,posconc,h,DiffBcup,DiffBcdw,          &
                    DiffSup,DiffSdw,avh,LSour,Qsour,SRelaxTau,sProf,S)

!#ifdef _LAKE_
   if (hypsography_file /= '') then
!     transform everything back
!     compute all neccessary things at the grid centers
      do i = 1, nlev
         S(i) = S(i) / ((hypsography(i) + hypsography(i-1))/2)
         sprof(i) = sprof(i) / ((hypsography(i) + hypsography(i-1))/2)
         Qsour(i) = Qsour(i) / ((hypsography(i) + hypsography(i-1))/2)
      end do

!     transform bc's back, you never know...
      select case(DiffBcup)
      case(Neumann)
         DiffSup = DiffSup / hypsography(nlev)
      case(Dirichlet)
         DiffSup = DiffSup / ((hypsography(nlev) + hypsography(nlev-1))/2)
      case default
         FATAL 'invalid boundary condition type for upper boundary'
         stop 'salinity.F90'
      end select

      select case(DiffBcdw)
      case(Neumann)
         DiffSdw = DiffSdw / hypsography(0)
      case(Dirichlet)
         DiffSdw = DiffSdw / ((hypsography(1) + hypsography(0))/2)
      case default
         FATAL 'invalid boundary condition type for lower boundary'
         stop 'salinity.F90'
      end select
   end if
!#endif

   return
   end subroutine salinity
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

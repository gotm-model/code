#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: density --- the equation of state \label{sec:eqstate}
!
! !INTERFACE:
   MODULE density
!
! !DESCRIPTION:
!  Computes the density, $\mean{\rho}$, and buoyancy from the
!  salinity, $S$, the temperature, $\Theta$, and the thermodynamic
!  pressure, $P$, according to an \emph{equation of state},
!  \begin{equation}
!    \label{DefEOS}
!     \mean{\rho} = \hat{\rho} (S,\Theta,P)
!     \point
!  \end{equation}
!
!  The following remark on the thermodynamic interpretation of
!  density, temperature, and pressure is useful here.  If $\Theta$ is
!  identified with the in-situ temperature, and $P$ with the in-situ
!  pressure, then $\mean{\rho}$ will be the in-situ density.  On the
!  other hand, if $P$ is identified with the surface pressure, and
!  $\Theta$ with the potential temperature, the same equation of
!  state, \eq{DefEOS}, will yield $\mean{\rho}$ as the potential
!  density. Note that the quantity {\tt sigma\_t} found in the GOTM output
!  is simply computed from  $\mean{\rho}$ - 1000 kg m$^{-3}$, and may
!  therefore adopt different meanings.
!
!
!  At present, two different models for the equation of state ("modes"),
!  and four different "methods" how to evalute the equation of state
!  are implemented.
!
!  Modes:
!  \begin{enumerate}
!     \item The UNESCO equation of state according to \cite{FofonoffMillard83}
!     \item The \cite{JACKETTea05} equation of state
!  \end{enumerate}
!  Methods:
!  \begin{enumerate}
!     \item the full equation of state --- including pressure effects
!     \item the full equation of state --- without pressure effects
!     \item the linearised equation of state
!     \item a general linear form of the equation of state
!  \end{enumerate}

!USES:
   use gsw_mod_teos10_constants, only: gsw_cp0     
   use gsw_mod_toolbox, only: gsw_rho,gsw_sigma0,gsw_alpha,gsw_beta
   IMPLICIT NONE
!  default: all is private.

   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_density, do_density, get_rho, get_alpha, get_beta
   integer, public :: density_method
   REALTYPE, public :: T0,S0,p0,rho0=1027.,alpha0,beta0,cp
   REALTYPE, public, allocatable :: alpha(:), beta(:)
   REALTYPE, public, allocatable :: rho(:), rho_p(:)
!
   REALTYPE :: rhob
   integer, parameter :: rk = kind(_ONE_)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Configuring {\tt eqstate}
!
! !INTERFACE:
   subroutine init_density(nlev)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
   integer, intent(in) :: nlev

! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Jorn Bruggeman
!
! !LOCAL VARIABLES:
   integer :: rc
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (density_method)
      case(1) ! use gsw_rho(S,T,p) - default p=0
         cp      =  gsw_cp0
         LEVEL3 'rho0=  ',rho0
      case(2) ! S0, T0, p0 are provided - rho0, alpha, beta, cp are calculated
         rhob     =  gsw_sigma0(S0,T0) + 1000._rk
         alpha0   =  gsw_alpha(S0,T0,p0)
         beta0    =  gsw_beta(S0,T0,p0)                  
         cp       =  gsw_cp0
         LEVEL2 'Linearized - TEOS10 - using S0, T0, p0'
         LEVEL3 'S0=    ',S0
         LEVEL3 'T0=    ',T0
         LEVEL3 'p0=    ',p0        
         LEVEL3 'rho0=  ',rho0
         LEVEL3 'alpha= ',alpha0
         LEVEL3 'beta=  ',beta0
         LEVEL3 'cp=    ',cp         
      case(3) ! S0, T0, rho0, alpha, beta are all provided
         rhob = rho0
         LEVEL2 'Linearized - custom - using S0, T0, rho0, alpha, beta, cp'
         LEVEL3 'S0=    ',S0
         LEVEL3 'T0=    ',T0
         LEVEL3 'rho0=  ',rho0
         LEVEL3 'alpha= ',alpha0
         LEVEL3 'beta=  ',beta0
         LEVEL3 'cp=    ',cp                  
       case default
   end select

   allocate(alpha(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (alpha)'  
   alpha = alpha0
   allocate(beta(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (beta)'  
   beta =  beta0
   allocate(rho(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (rho)'  
   rho = _ZERO_
   allocate(rho_p(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (rho_p)'  
   rho_p = _ZERO_

   LEVEL2 'done.'
   end subroutine init_density
!EOC

!-----------------------------------------------------------------------

   subroutine do_density(nlev,S,T,p,pi)

!  Subroutine arguments
   integer, intent(in) :: nlev
      !! number of layers []
   REALTYPE, intent(in), dimension(0:nlev) :: S
      !! absolute salinity [g/kg]
   REALTYPE, intent(in), dimension(0:nlev) :: T
      !! conservative temperature [C]
   REALTYPE, intent(in), dimension(0:nlev) :: p
      !! pressure at cell centers [dbar]
   REALTYPE, intent(in), dimension(0:nlev) :: pi
      !! pressure at cell interfaces [dbar]


   REALTYPE, dimension(0:nlev)             :: Si, Ti
      !! interface salnity and temperature
   
!-----------------------------------------------------------------------

   ! compute interface salinity and temperature
   Si(1:nlev-1)         = 0.5*(S(1:nlev-1) + S(2:nlev))
   Ti(1:nlev-1)         = 0.5*(T(1:nlev-1) + T(2:nlev))   

   select case (density_method)
      case(1)
         rho(1:nlev)     =  gsw_rho(S(1:nlev),T(1:nlev),p(1:nlev))
         rho_p(1:nlev)   =  gsw_sigma0(S(1:nlev),T(1:nlev)) + 1000._rk
         alpha(1:nlev-1) =  gsw_alpha(Si(1:nlev-1),Ti(1:nlev-1),pi(1:nlev-1))
         beta(1:nlev-1)  =  gsw_beta(Si(1:nlev-1),Ti(1:nlev-1),pi(1:nlev-1))
      case(2,3)
         rho_p(1:nlev)   = rhob*(_ONE_ - alpha0*(T(1:nlev)-T0) + beta0*(S(1:nlev)-S0) )
         rho             = rho_p   ! Lars: here, we should implement some sort of pressure dependency
      case default
   end select

   end subroutine do_density

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute in-situ or potential density
!
! !INTERFACE:
   elemental REALTYPE function get_rho(S,T,p)
!
! !DESCRIPTION:
! Compute in-situ density $\rho$ or potential density $\rho_p$  based on absolute salinity, conservative temperature, 
! and in case - in-situ - the optional parameter pressure.
! There are three options at the moment
!   \begin{enumerate}
!     \item Compute $\rho$ from the full TEOS-10 equation of state
!     \item Compute $\rho$ from a linear equation of state: 
!           \begin{equation}
!              \label{eosLinear}
!              \frac{\rho}{\rho_0} = 1 - \text{\tt alpha} (T - T_0) + \text{\tt beta} (S - S_0)
!               \comma
!           \end{equation}
!           where  {\tt rho0}, {\tt alpha}, {\tt beta} are computed from TEOS-10 for user-spezified values of
!           {\tt T0},  {\tt S0},  and {\tt p0}.
!     \item  Compute $rho$ from the above linear equation of state for user-specified  values of {\tt rho0},
!     {\tt alpha}, and  {\tt beta}.
!   \end{enumerate}
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in)                 :: S,T
   REALTYPE,intent(in), optional       :: p
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
! !LOCAL VARIABLES:
!
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (density_method)
      case(1)
         if (present(p)) then
            get_rho = gsw_rho(S,T,p)
         else
            get_rho = gsw_sigma0(S,T) + 1000.
         end if
      case (2,3)
         get_rho = rhob*( _ONE_ - alpha0*(T-T0) + beta0*(S-S0) )
         ! Lars: here, we should implement some pressure dependency 
      case default
   end select

   end function get_rho
!EOC   

   
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute thermal expansion coefficient
!
! !INTERFACE:
   elemental REALTYPE function get_alpha(S,T,p)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in)                 :: S,T,p
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
! !LOCAL VARIABLES:

!EOP
!-----------------------------------------------------------------------
!BOC
   select case (density_method)
      case(1)
         get_alpha = gsw_alpha(S,T,p)
       case (2,3)
         get_alpha = alpha0
      case default
   end select

   end function get_alpha
!EOC   


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute haline contraction coefficient
!
! !INTERFACE:
   elemental REALTYPE function get_beta(S,T,p)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in)                 :: S,T,p
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
! !LOCAL VARIABLES:

!EOP
!-----------------------------------------------------------------------
!BOC
   select case (density_method)
      case(1)
         get_beta = gsw_beta(S,T,p)
       case (2,3)
         get_beta = beta0
      case default
   end select

   end function get_beta
!EOC   

!-----------------------------------------------------------------------
!BOP
   subroutine clean_density()
!EOP
!-----------------------------------------------------------------------
!BOC

   if (allocated(alpha)) deallocate(alpha)
   if (allocated(beta)) deallocate(beta)
   if (allocated(rho)) deallocate(rho)
   if (allocated(rho_p)) deallocate(rho_p)

   end subroutine clean_density
!EOC   

!-----------------------------------------------------------------------

   end module density

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

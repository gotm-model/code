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
   use gsw_mod_toolbox, only: gsw_rho,gsw_sigma0,gsw_rho_alpha_beta,gsw_alpha,gsw_beta
   IMPLICIT NONE
!  default: all is private.

   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_density, calculate_density, do_density_1, get_alpha, get_beta
   integer, public :: density_method
   REALTYPE, public :: T0,S0,p0,rho0=1027.,alpha0,beta0
   REALTYPE, public, allocatable :: alpha(:), beta(:)
   REALTYPE, public, allocatable :: rho(:), rho_p(:)
!
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
         LEVEL3 'rho0=  ',rho0
      case(2) ! S0, T0, p0 are provided - rho0, alpha, beta are calculated
         call gsw_rho_alpha_beta(S0,T0,p0,rho0,alpha0,beta0) 
         LEVEL2 'Linearized - using gsw_rho_alpha_beta()'
         LEVEL3 'S0=    ',S0
         LEVEL3 'T0=    ',T0
         LEVEL3 'p0=    ',p0        
         LEVEL3 'rho0=  ',rho0
         LEVEL3 'alpha= ',alpha0
         LEVEL3 'beta=  ',beta0
      case(3) ! S0, T0, rho0, alpha, beta are all provided
         LEVEL2 'Linearized - custom - using S0, T0, rho0, alpha, beta'
         LEVEL3 'S0=    ',S0
         LEVEL3 'T0=    ',T0
         LEVEL3 'rho0=  ',rho0
         LEVEL3 'alpha= ',alpha0
         LEVEL3 'beta=  ',beta0
       case default
   end select

   allocate(alpha(nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (alpha)'  
   alpha = alpha0
   allocate(beta(nlev),stat=rc)
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
!BOP
!
! !IROUTINE: Select an equation of state
!
! !INTERFACE:
   elemental REALTYPE function calculate_density(S,T,p,g)
!
! !DESCRIPTION:
!  Calculates the in-situ buoyancy according to the selected method.
!  {\tt S} is salinity $S$ in psu, {\tt T} is
!  potential temperature $\theta$ in $^{\circ}$C (ITS-90), {\tt p} is
!  gauge pressure (absolute pressure - 10.1325 bar), {\tt g} is the
!  gravitational acceleration in m\,s$^{-2}$ and {\tt rho\_0} the reference
!  density in kg\,m$^{-3}$. {\tt calculate_density} is the in-situ-density
!  in kg\,m$^{-3}$.
!  For {\tt eq\_state\_method}=1, the UNESCO equation of state is used,
!  for {\tt eq\_state\_method}=2, the \cite{JACKETTea05} equation
!  of state is used. Here, some care is needed, since the UNESCO equation
!  used bar for pressure and the \cite{JACKETTea05} uses dbar for pressure.
!  For values of
!  {\tt eq\_state\_method} ranging from 1 to 4, one of the following methods
!  will be used.
!
!   \begin{enumerate}
!     \item the full equation of state for sea water
!           including pressure dependence.
!     \item the equation of state for sea water
!           with the pressure evaluated at the sea surface as
!           reference level. This is the choice
!           for computations based on potential temperature and density.
!     \item a linearised equation of state.
!           The parameters {\tt T0},
!           {\tt S0} and {\tt p0} have to be specified in the namelist.
!     \item a linear equation of state with prescribed {\tt rho0}, {\tt T0},
!           {\tt S0}, {\tt alpha}, {\tt beta} according to
!           \begin{equation}
!              \label{eosLinear}
!              \rho = \rho_0 + \text{\tt alpha} (T - T_0)
!                            + \text{\tt beta} (S - S_0)
!               \point
!           \end{equation}
!   \end{enumerate}
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in)                 :: S,T,p
   REALTYPE,optional,intent(in)        :: g
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   REALTYPE :: x
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (density_method)
      case(1)
         x=gsw_sigma0(S,T) + 1000.
         !x=gsw_rho(S,T,p)
      case (2, 3)
         x=rho0*(_ONE_-alpha0*(T-T0)+beta0*(S-S0))
      case default
   end select

   if (present(g)) then
      calculate_density=-g*(x-rho0)/rho0
   else
      calculate_density=x
   end if
   end function calculate_density
!EOC

!-----------------------------------------------------------------------

   subroutine do_density_1(nlev,S,T,p)

!  Subroutine arguments
   integer, intent(in) :: nlev
      !! number of layers []
   REALTYPE, intent(in), dimension(0:nlev) :: S
      !! absolute salinity [kg/g]
   REALTYPE, intent(in), dimension(0:nlev) :: T
      !! conservative temperature profile [C]
   REALTYPE, intent(in), dimension(0:nlev) :: p
      !! conservative temperature profile [dbar]
!-----------------------------------------------------------------------

      select case (density_method)
         case(1)
            call gsw_rho_alpha_beta(S(1:),T(1:),p(1:), &
                                    rho=rho(1:),alpha=alpha,beta=beta)
            rho_p(1:nlev)=gsw_sigma0(S(1:nlev),T(1:nlev)) + 1000._rk
         case(2,3)
            rho_p(1:nlev)=rho0-alpha*(T(1:nlev)-T0)+beta*(S(1:nlev)-S0)
!KB            call gsw_rho_alpha_beta(S(1:),T(1:),p(1:),rho=rho(1:))
         case default
      end select

! Lars suggests to put calculation of buoyancy here

   end subroutine do_density_1

!-----------------------------------------------------------------------

   subroutine get_alpha(nlev,gravity,S_const,NN,z,zi,T)

!  Subroutine arguments
   integer, intent(in) :: nlev
      !! number of layers []
   REALTYPE, intent(in) :: gravity
      !! gravitationl acceleration [m/s^2]
   REALTYPE, intent(in) :: S_const
      !! constant salinity [kg/g]
   REALTYPE, intent(in) :: NN
      !! Brunt Vaisalla frequency squared [s^-2]
   REALTYPE, intent(in), dimension(0:nlev) :: z
      !! z-ccordinate of cell centers [m]
   REALTYPE, intent(in), dimension(0:nlev) :: zi
      !! z-ccordinate of cell faces [m]
   REALTYPE, intent(inout), dimension(0:nlev) :: T
      !! conservative temperature profile [C]

!  Local variables
   integer :: n
!-----------------------------------------------------------------------

   select case (density_method)
      case(1)
         do n=nlev-1,1,-1
            ! estimate beta based on T above the interface
            alpha(n) = gsw_alpha(T(n+1),S_const,-zi(n))
            ! use this to estimate S below the interface
            T(n) = T(n+1) - _ONE_/(gravity*alpha(n))*NN*(z(n+1)-z(n))
            ! compute improved beta                                  
            alpha(n)=gsw_alpha(0.5*(T(n+1)+T(n)),S_const,-zi(n))
         end do
      case default
   end select

   end subroutine get_alpha

!-----------------------------------------------------------------------

   subroutine get_beta(nlev,gravity,T_const,NN,z,zi,S)

   integer, intent(in) :: nlev
      !! number of layers []
   REALTYPE, intent(in) :: gravity
      !! gravitationl acceleration [m/s^2]
   REALTYPE, intent(in) :: T_const
      !! constant temperature [C]
   REALTYPE, intent(in) :: NN
      !! Brunt Vaisalla frequency squared [s^-2]
   REALTYPE, intent(in), dimension(0:nlev) :: z
      !! z-ccordinate of cell centers [m]
   REALTYPE, intent(in), dimension(0:nlev) :: zi
      !! z-ccordinate of cell faces [m]
   REALTYPE, intent(inout), dimension(0:nlev) :: S
      !! salinity profile [kg/g]

!  Local variables
   integer :: n
!-----------------------------------------------------------------------

   select case (density_method)
      case(1)
         do n=nlev-1,1,-1
            ! estimate beta based on T above the interface
            beta(n) = gsw_beta(S(n+1),T_const,-zi(n))
            ! use this to estimate S below the interface
            S(n) = S(n+1) + _ONE_/(gravity*beta(n))*NN*(z(n+1)-z(n))
            ! compute improved beta                                  
            beta(n)=gsw_beta(0.5*(S(n+1)+S(n)),T_const,-zi(n))
         end do
      case default
   end select

   end subroutine get_beta

!-----------------------------------------------------------------------

   subroutine clean_density()

      if (allocated(alpha)) deallocate(alpha)
      if (allocated(beta)) deallocate(beta)
      if (allocated(rho)) deallocate(rho)
      if (allocated(rho_p)) deallocate(rho_p)

   end subroutine clean_density

   end module density

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

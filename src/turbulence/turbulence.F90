!$Id: turbulence.F90,v 1.8 2004-01-27 08:31:00 lars Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: turbulence: its all in here \ldots \label{sec:turbulence}
!
! !INTERFACE:
   module turbulence
!
! !DESCRIPTION:
! In this module, variables of the turbulence model and some 
! member functions to manipulate them are defined. The key-functions
! are {\tt init\_turbulence()}, which initialises the model, and
! {\tt do\_turbulence()}, which manages the time step for the 
! whole procedure. There are many more internal functions, for 
! which further descriptions are given seperately.
!
!
! !USES:
   use meanflow, only : z0b,z0s
   IMPLICIT NONE

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_turbulence
   public stabilityfunctions,do_tke,lengthscale
   public kolpran,do_turbulence,internal_wave
   public k_bc,q2over2_bc,epsilon_bc,psi_bc,q2l_bc 
   public c3
 
! !PUBLIC DATA MEMBERS:
!  TKE, rate of dissipation, 
!  turbulent length-scale
   REALTYPE, public, dimension(:), allocatable   :: tke,eps,L

!  the turbulent diffusivities
   REALTYPE, public, dimension(:), allocatable   :: num,nuh

!  the stability functions
   REALTYPE, public, dimension(:), allocatable   :: cmue1,cmue2

!  long. Reynolds-stresses, temp.  variance, 
!  its rate of dissipation
   REALTYPE, public, dimension(:), allocatable   :: uu,vv,ww,tt,chi

!  TKE at the old timestep
   REALTYPE, public, dimension(:), allocatable   :: tkeo

!  alpha_M and alpha_N
   REALTYPE, public, dimension(:), allocatable   :: as,an
 
!  the flux Richardson number
   REALTYPE, public, dimension(:), allocatable   :: xRf 

!  some additional constants
   REALTYPE, public          :: cm0,cmsf,cde,rcm, b1
  
!  Prandtl-number in neutrally stratified flow
   REALTYPE, public          :: Prandtl0

!  parameters for wave-breaking 
   REALTYPE, public          :: craig_m,sig_e0

!  the 'turbulence' namelist
   integer, public           :: turb_method=2 
   integer, public           :: tke_method=2
   integer, public           :: len_scale_method=8
   integer                   :: stab_method=3

!  the 'bc' namelist
   integer, public           :: k_ubc=1
   integer, public           :: k_lbc=1
   integer, public           :: psi_ubc=1
   integer, public           :: psi_lbc=1
   integer, public           :: ubc_type=1
   integer, public           :: lbc_type=1

!  the 'turb_param' namelist 
   REALTYPE, public          :: cm0_fix=0.5477
   REALTYPE, public          :: Prandtl0_fix=0.74
   REALTYPE, public          :: cw=100.0
   logical                   :: compute_kappa=.false.
   REALTYPE, public          :: kappa=0.4
   logical                   :: compute_c3=.false.
   REALTYPE                  :: ri_st=0.25
   logical,  public          :: length_lim=.false.
   REALTYPE, public          :: galp=0.53
   REALTYPE, public          :: const_num=5.0e-4
   REALTYPE, public          :: const_nuh=5.0e-4
   REALTYPE, public          :: k_min=1.0e-8
   REALTYPE, public          :: eps_min=1.0e-12

!  the 'generic' namelist 
   logical                   :: compute_param=.false.
   REALTYPE, public          :: gen_m=1.5
   REALTYPE, public          :: gen_n=-1.0
   REALTYPE, public          :: gen_p=3.0
   REALTYPE, public          :: cpsi1=1.44
   REALTYPE, public          :: cpsi2=1.92
   REALTYPE, public          :: cpsi3minus=0.0
   REALTYPE, public          :: cpsi3plus=1.0
   REALTYPE                  :: sig_kpsi=1.0
   REALTYPE, public          :: sig_psi=1.3
   REALTYPE                  :: gen_d=-1.2
   REALTYPE                  :: gen_alpha=-2.0
   REALTYPE                  :: gen_l=0.2

!  the 'keps' namelist
   REALTYPE, public          :: ce1=1.44
   REALTYPE, public          :: ce2=1.92
   REALTYPE, public          :: ce3minus=0.0
   REALTYPE, public          :: ce3plus=1.0
   REALTYPE, public          :: sig_k=1.0
   REALTYPE, public          :: sig_e=1.3
   logical,  public          :: sig_peps=.false.

!  the 'my' namelist
   REALTYPE, public          :: e1=1.8
   REALTYPE, public          :: e2=1.33
   REALTYPE, public          :: e3=1.8
   REALTYPE, public          :: sq=0.2
   REALTYPE, public          :: sl=0.2
   integer,  public          :: my_length=1
   logical,  public          :: new_constr=.false.
 
!  the 'iw' namelist
   integer                   :: iw_model=0
   REALTYPE, public          :: alpha=0.0
   REALTYPE                  :: klimiw=1e-6
   REALTYPE                  :: rich_cr  = 0.7
   REALTYPE                  :: numiw    = 1e-4
   REALTYPE                  :: nuhiw    = 5e-5
   REALTYPE                  :: numshear = 5e-3
!
! !DEFINED PARAMETERS:

!  method to update TKE
   integer, parameter, public :: tke_local_eq=1
   integer, parameter, public :: tke_keps=2 
   integer, parameter, public :: tke_MY=3

!  stability functions
   integer, parameter        :: genASM=0
   integer, parameter        :: MellorYamada=1
   integer, parameter        :: KanClay=2
   integer, parameter        :: BurBaum=3
   integer, parameter        :: CanutoA=4
   integer, parameter        :: CanutoB=5
   integer, parameter        :: GalperinQe=6
   integer, parameter        :: KanClayQe=7 
   integer, parameter        :: BurBaumQe=8
   integer, parameter        :: CanutoAQe=9
   integer, parameter        :: CanutoBQe=10
   integer, parameter        :: Constant=11
   integer, parameter        :: MunkAnderson=12
   integer, parameter        :: SchumGerz=13
   integer, parameter        :: FluxRich=14

!  method to update length scale
   integer, parameter        :: Parabola=1
   integer, parameter        :: Triangle=2
   integer, parameter        :: Xing=3
   integer, parameter        :: RobertOuellet=4
   integer, parameter        :: Blackadar=5
   integer, parameter        :: BougeaultAndre=6
   integer, parameter        :: ispra_length=7
   integer, parameter, public          :: diss_eq=8
   integer, parameter, public          :: length_eq=9
   integer, parameter, public          :: generic_eq=10

!  boundary conditions 
   integer, parameter        :: Dirichlet=0
   integer, parameter        :: Neumann=1
   integer, parameter        :: viscous=0
   integer, parameter        :: logarithmic=1
   integer, parameter        :: injection=2

!
! !BUGS: 
!        The algebraic equation for the TKE is not save
!        to use at the moment. Use it only in conncection
!        with the prescribed length-scale profiles. The 
!        functions report_model() will report wrong things
!        for the algebraic TKE equation. To be fixed with 
!        the next version.
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard, 
!                      Manuel Ruiz Villarreal,
!                      Lars Umlauf

!
!  $Log: turbulence.F90,v $
!  Revision 1.8  2004-01-27 08:31:00  lars
!  support for gen. stability function
!
!  Revision 1.7  2003/03/28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.6  2003/03/28 08:20:01  kbk
!  removed tabs
!
!  Revision 1.5  2003/03/10 09:02:06  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!
!  Revision 1.3  2001/11/27 19:42:58  gotm
!  Cleaned
!
!  Revision 1.2  2001/11/18 16:15:30  gotm
!  New generic two-equation model
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the turbulence module
!
! !INTERFACE:
   subroutine init_turbulence(namlst,fn,nlev)
!
! !DESCRIPTION:
! Initialises all turbulence related stuff. This routine reads a number 
! of namelists and allocates memory for turbulence related vectors.
! The core consists of calls to the the internal functions 
! {\tt generate\_model()} and {\tt analyse\_model()}, discussed in 
! great detail in \sect{sec:generate} and \sect{sec:analyse}, respectively.
! The former function computes the model coefficients for the generic two-equation
! model (see \cite{Umlaufetal2003}) from physically motivated quantities
! like the von K{\'a}rm{\'a}n constant, $\kappa$, the decay rate in homogeneous
! turbulence, $d$, the steady-state Richardson number, $Ri_{st}$, 
! and many others. The latter function does the inverse: 
! it computes the physically motivated quantities from the model constants
! of any model currently available in GOTM. 
! After the call to either function, all relevant model parameters and
! and their physical implications are known to GOTM.  
! Then, the function {\tt report\_model()} is called, which displays all 
! results on the screen.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: namlst
   character(len=*), intent(in)        :: fn
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard, 
!                      Manuel Ruiz Villarreal,
!                      Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   integer                       :: rc
   REALTYPE                      :: L_min
!
   namelist /turbulence/    turb_method,tke_method,            &
                            len_scale_method,stab_method

   namelist /bc/            k_ubc,k_lbc,psi_ubc,psi_lbc,       &
                            ubc_type,lbc_type

   namelist /turb_param/    cm0_fix,Prandtl0_fix,cw,           &
                            compute_kappa,kappa,               &
                            compute_c3,ri_st,length_lim,galp,  &
                            const_num,const_nuh,k_min,eps_min
            
   namelist /generic/       compute_param,gen_m,gen_n,gen_p,   &
                            cpsi1,cpsi2,cpsi3minus,cpsi3plus,  &
                            sig_kpsi,sig_psi,                  &
                            gen_d,gen_alpha,gen_l

   namelist /keps/          ce1,ce2,ce3minus,ce3plus,sig_k,    &
                            sig_e,sig_peps

   namelist /my/            e1,e2,e3,sq,sl,my_length,new_constr

   namelist /iw/            iw_model,alpha,klimiw,rich_cr,     &
                            numiw,nuhiw,numshear
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_turbulence'

   ! read the variables from the namelist file

   open(namlst,file=fn,status='old',action='read',err=80)

   LEVEL2 'reading turbulence namelists..'

   read(namlst,nml=turbulence,err=81)
   read(namlst,nml=bc,err=82)
   read(namlst,nml=turb_param,err=83)
   read(namlst,nml=generic,err=84)
   read(namlst,nml=keps,err=85)
   read(namlst,nml=my,err=86)
   read(namlst,nml=iw,err=87)
   close (namlst)

   LEVEL2 'done.'

!  allocate memory
   
   LEVEL2 'allocation memory..'
   allocate(tke(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (tke)'
   tke = k_min

   allocate(eps(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (eps)'
   eps = eps_min

   allocate(L(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (L)'
   L = 0.

   allocate(num(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (num)'
   num = 0.

   allocate(nuh(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (nuh)'
   nuh = 0.

   allocate(tkeo(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (tkeo)'
   tkeo = 0.

   allocate(uu(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (uu)'
   uu = 0.

   allocate(vv(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (vv)'
   vv = 0.

   allocate(ww(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (ww)'
   ww = 0.

   allocate(tt(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (tt)'
   tt = 0.

   allocate(chi(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (chi)'
   chi = 0.

   allocate(cmue1(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (cmue1)'
   cmue1 = 0.

   allocate(cmue2(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (cmue2)'
   cmue2 = 0.

   allocate(an(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (an)'
   an = 0.

   allocate(as(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (as)'
   as = 0.

   allocate(xRF(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (xRF)'
   xRF = 0.

   LEVEL2 'done.'

!  cm0 (the value in the log-layer) and cmsf (the shear-free value)
!  are set by the stability functions   
   call compute_cm0(nlev,stab_method)

!  compute auxiliary constants
   cde  = cm0**3.
   rcm  = cm0/cmsf

!  cm0 is related to B1 of Mellor and Yamada (1982)
   b1   = 2.**1.5/cde   

! initialise some things
   L_min=cde*k_min**1.5/eps_min
   L=L_min

!  generate or analyse the model constants
   if ((len_scale_method.eq.generic_eq).and.compute_param) then
      call generate_model
   else
      call analyse_model
   endif

!  report on parameters and properties of the model
   call report_model

   return

80 FATAL 'I could not open "gotmturb.inp"'
   stop 'init_turbulence'
81 FATAL 'I could not read "turbulence" namelist'
   stop 'init_turbulence'
82 FATAL 'I could not read "bc" namelist'
   stop 'init_turbulence'
83 FATAL 'I could not read "turb_param" namelist'
   stop 'init_turbulence'
84 FATAL 'I could not read "generic" namelist'
   stop 'init_turbulence'
85 FATAL 'I could not read "keps" namelist'
   stop 'init_turbulence'
86 FATAL 'I could not read "my" namelist'
   stop 'init_turbulence'
87 FATAL 'I could not read "iw" namelist'
   stop 'init_turbulence'

 end subroutine init_turbulence
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Generate a two--equation model \label{sec:generate}

! !INTERFACE:
   subroutine generate_model
!
! !DESCRIPTION:
! Computes the parameters of an instance of the `generic' two--equation 
! model according to the specifications set in {\tt gotmturb.inp}. This model
! solves \eq{tkeA} for the $k$ and \eq{generic} for the generic length--scale
! defined in \sect{sec:genericeq} together with an Algebraic Stress Model. For several
! simple turbulent flows, analytical solutions of this models exist and 
! can be used to calibrate the model coefficients. The method is described
! in great detail in \cite{UmlaufBurchard2003}. Even users that are
! not interested in using the generic part of GOTM should have a look at 
! this section, because the solutions derived here referenced in other
! parts of this manual.  
!
! After the call to {\tt generate\_model()}, all parameters of the generic
! two--equation model are known. The resulting model behaves in all situations
! discussed below exactly as desired. This is the major advantage of using 
! the generic equation (also see \sect{sec:genericeq}).

! \vspace{10mm}
! {\bf The logarithmic boundary layer}
! \vspace{5mm}
!
! In the logarithmic boundary layer one has $P=\epsilon$ and 
! $k\propto u^2_*$ by defintion. Under these conditions it is easy 
! to show that a solution of \eq{tkeA} is
! \begin{equation}
!   \label{kLog}
!   k = \dfrac{u_*^2}{(c_\mu^0)^2}
!   \comma
! \end{equation}
! and a solution of \eq{generic} can only be obtained if the condition
! \begin{equation}
!   \label{psiLog}
!   \sigma_\psi = \dfrac{n^2 \kappa^2}{(c_\mu^0)^2 (c_{\psi 2}-c_{\psi 1})}
! \end{equation}
! is satisfied. \eq{kLog} can be conveniently used to obtain boundary conditions
! for $k$, whereas \eq{psiLog} yields for example the value for the turbulent
! Schmidt--number $\sigma_\psi$ as a function of the von K{\'a}rm{\'a}n constant 
! (provided, of course, that the other constants are assumed to be known). The
! value of the von K{\'a}rm{\'a}n constant is usually assumed to be 
! $\kappa \approx 0.4$.
! 
! \vspace{10mm}
! {\bf Decay of homogeneous turbulence}
! \vspace{5mm}
!
! Another example of a simple but fundamental turbulent situation is the
! temporal decay of isotropic, homogeneous turbulence (approximated by
! the spatial decay of turbulence behind grids in laboratory settings).
! At large times, $t$, data from many experiments are well described by
! a power law of the form
! \begin{equation}
!   \label{decay}
!   \frac{k}{k_0} = A \left( \frac{t}{\tau_0} \right)^d
!   \comma
! \end{equation}
! with constant $A$ and initial values of the kinetic energy, $k_0$, and
! the {\it eddy turnover time}, $\tau_0$. The decay rates, $d$, have
! been thoroughly documented. Experiments (\cite{Bradshaw75},
! \cite{Townsend76}, \cite{DomaradzkiMellor84}, \cite{Mohamed90})
! suggest that $d$ is in the range $-1.3 < d < -1$.  DNS, generally
! conducted at low Reynolds numbers, produce consistently higher values.
! For example, \cite{Briggsetal96} obtain a value near $-1.5$ from their
! DNS.
! 
! In homogeneous decaying turbulence, \eq{tkeA} and \eq{generic} reduce
! to a balance between the rate and dissipation terms, respectively. The
! coupled system of ordinary differential equations can be solved for
! given initial values $k_0$ and $\psi_0$.
! The solution can be shown to reduce to \eq{decay} at large times.
! Then, the decay exponent, $d$, is determined by
! \begin{equation}
!   \label{d}
!   d = - \frac{2n}{2m+n-2c_{\psi 2}}
!   \comma
! \end{equation}
! and thus depends only on the exponents $m$ and $n$ defined in \eq{psi_l} and
! the model constant $c_{\psi 2}$. For given exponents $m$ and $n$, the
! experimental values of $d$ can be used to derive model the value of the
! model constant $c_{\psi 2}$. 
! Note, that the predicted decay rate, $d$, is
! completely independent of the ASM (or the stability functions in other
! words). 
! 
! \vspace{10mm}
! {\bf Homogeneous turbulent shear--flows}
! \vspace{5mm}
!
! A natural extension of decaying homogeneous turbulence is the
! inclusion of a homogeneous shear and an aligned homogeneous
! stratification.  Since turbulence is still assumed to be homogeneous,
! the divergence of any turbulent transport term vanishes and the
! intricate interplay between the stabilizing effects of stratification
! and the destabilizing action of shear can be isolated. Thus, it is not
! surprising that this highly interesting special case of turbulence has
! been explored extensively by laboratory experiments
! (\cite{TavoularisCorrsin81a,TavoularisCorrsin81b},
! \cite{TavoularisKarnik89}, \cite{Rohretal88}), by Direct Numerical
! Simulation (\cite{Gerzetal89}, \cite{Holtetal92},
! \cite{Jacobitzetal97}, \cite{Shihetal2000}) and by Large--Eddy
! Simulation (\cite{Kaltenbachetal94}).  That flows of this kind are
! crucial also in many oceanographic flows has recently been pointed out
! by \cite{BaumertPeters2000}.
! 
! In the context of the generic two--equation model, this turbulent flow
! is mathematically established by neglecting the turbulent transport
! terms and the advective part of the material time derivative. Then,
! \eq{tkeA} and \eq{generic} reduce to a set of ordinary differential
! equations. Using the chain rule of differentiation, the relation
! \begin{equation}
!   \label{GE_l_chain_rule}
!   \frac{1}{l} \deriv{l}{t} = 
!   \frac{1}{n} \frac{1}{\psi} \deriv{\psi}{t} - 
!   \frac{m}{n} \frac{1}{k}    \deriv{k}{t}  
! \end{equation}
! for the mixing length, $l$, follows immediately from \eq{psi_l}. With
! \eq{GE_l_chain_rule}, the generic model expressed by \eq{tkeA} and
! \eq{generic} can be used to derive an evolution equation for the
! integral length scale, $l$,
! \begin{equation}
!   \label{GE_length_homogeneous}
!   \begin{array}{rcl}
!     \dfrac{1}{l} \deriv{l}{t} 
!     &=&
!     - \left( \dfrac{1}{n} c_{\psi_2} - \dfrac{m}{n}  \right) \dfrac{\epsilon}{k}
!     \\[5mm]
!     &+&
!     \dfrac{1}{k} 
!     \left(
!       \left( \dfrac{1}{n} c_{\psi_1} - \dfrac{m}{n}  \right) P + 
!       \left( \dfrac{1}{n} c_{\psi_3} - \dfrac{m}{n}  \right) G
!     \right) 
!     \point
!   \end{array}
! \end{equation}
! 
! \cite{Tennekes89} derived an equation similar to
! \eq{GE_length_homogeneous}, however only for the special case of the
! $k$--$\epsilon$ model applied to unstratified flows, and stated that
! \emph{`on dimensional grounds, $l$ cannot depend upon the shear because the
! shear is homogeneous and cannot impose a length scale'}. This argument
! requires immediately
! \begin{equation}
!   \label{c_psi1}
!   c_{\psi 1} = m
!   \comma
! \end{equation}
! which is used in the subroutine to determine the model parameter 
! $c_{\psi 1}$. Are much more detailed discussion of this method is given
! in \cite{UmlaufBurchard2003}.
!
! \vspace{10mm}
! {\bf Shear--free turbulence, wave--breaking}
! \vspace{5mm}
! 
! The first step in understanding the behaviour of two--equation models
! in the surface layer affected by breaking gravity waves is the
! investigation of a special case, in which turbulence decays spatially
! away from a planar source {\em without mean shear}.  Turbulence generated by
! an oscillating grid in a water tank has been used in various
! laboratory settings to study the spatial decay of velocity
! fluctuations in this basic turbulent flow, where turbulent transport
! and dissipation balance exactly. For a summary of these results,
! see \cite{Umlaufetal2003}.
!
! All grid stirring experiments confirmed a power
! law for the decay of $k$ and a linear increase of the length scale, $l$,
! according to
! \begin{equation}
!   \label{power_law}
!   k = K (z+z_0)^\alpha \comma l = L(z+z_0)
!   \comma
! \end{equation}
! where $K$, $L$, and $z_0$ are constants, and the source of turbulence
! has been assumed to be at $z=0$.  In these experiments, $z_0 = l / L$
! at $z=0$ is not related to any kind of surface roughness length.
! Rather, it is connected to the length scale of injected turbulence
! which is determined uniquely by the spectral properties of turbulence
! at the source. Experiments suggest that the decay rate for the
! turbulent kinetic energy is likely to be between $-3<\alpha<-2$. The
! values of $L$, i.e.\ the slope of the turbulent length scale, $l$,
! indicate that in all cases $L < \kappa \approx 0.4$, see
! \cite{Umlaufetal2003}. 
! 
! In stationary, shear--free, unstratified turbulence, the generic model
! simplifies to a balance between the turbulent transport terms and the
! dissipative terms in \eq{tkeA} and \eq{generic}. Using the definition
! of $\psi$ in \eq{psi_l} and the scaling for the rate of
! dissipation, \eq{epsilon}, the transport and dissipation of $k$ and
! $\psi$ are balanced according to
! \begin{equation}
!   \begin{array}{rcl}
!   \label{GE_shear_free}
!     \frstderiv{z} \left( \dfrac{c_\mu}{\sigma_k^{\psi}}
!       k^{\frac{1}{2}} l  \deriv{k}{z} \right)
!   &=&
!     (c_\mu^0)^3 \dfrac{k^{\frac{3}{2}}}{l}
!   \comma  \\[5mm]
!   \frstderiv{z} \left( \dfrac{c_\mu}{\sigma_{\psi}}
!     k^{\frac{1}{2}} l  \frstderiv{z} \left(  (c_\mu^0)^p k^m l^n \right) \right)
!   &=&
!   c_{\psi 2} (c_\mu^0)^{p+3} k^{m+\frac{1}{2}} l^{n-1} 
!   \point
!   \end{array}
! \end{equation}
! Note, that in shear--free turbulence, the shear--number defined in 
! \eq{alphaMN} is $\alpha_M=0$ by
! definition, and stability functions always reduce to a constant which
! is, however, different from the constant $c_\mu^0$ approached in the
! logarithmic boundary layer (see \eq{kLog}).
! 
! For the solution of this non--linear system , we inserted the
! expressions \eq{power_law} in \eq{GE_shear_free}. From \eq{epsilon} and
! \eq{nu}, power--laws follow then also for $\epsilon = E(z+z_0)^\beta$
! and $\nu_t = N (z+z_0)^\gamma$.
!
! Inserting \eq{power_law} into \eq{GE_shear_free}$_1$ yields the
! equation
! \begin{equation}
!   \label{GE_alphaL_1}
!   (\alpha L)^2 = \frac{2}{3}   (c_\mu^0)^2 R \, \sigma_k^\psi 
!   \comma
! \end{equation}
! where the constant ratio $R=c_\mu^0/c_\mu$ follows uniquely from the
! respective ASM.  The power--law \eq{power_law} can also be inserted in
! \eq{GE_shear_free}$_2$ to yield
! \begin{equation}
!   \label{GE_alphaL_2}
!   \left( \alpha m + n \right) \left( \left( \dfrac{1}{2}
!     + m \right) \alpha + n \right) L^2
!   = 
!   \left(c_\mu^0 \right)^2 R \, \sigma_\psi  c_{\psi 2} 
!   \point
! \end{equation}
! 
! We note that with the help of \eq{d} and \eq{c_psi1}, the
! relation \eq{psiLog} can be rewritten as
! \begin{equation}
!   \label{psiLog2}
!   \sigma_\psi = \frac{2 \kappa^2 d}{(c_\mu^0)^2 (d+2)} n
!   \point
! \end{equation}
! Expressing now $\sigma_\psi$ with \eq{psiLog2} and $c_{\psi 2}$ with the help
! of \eq{d} on the right hand side of \eq{GE_alphaL_2},
! an equation expressing the exponent $m$ in terms of
! $n$ (or vice--versa) can be obtained. The result for $n$ can be written
! as
! \begin{equation}
!   \label{GE_n}
!   \begin{array}{rcl}
!     n &=& - \dfrac{1}{4 (2+d) (\kappa^2 R - L^2)} 
!     \Bigg(
!       4 d \kappa^2 R \,  m - (1+4m) (2+d) \alpha L^2   
!       \\ 
!     &+& 
!       \left.  \sqrt{ 8 m (1+2m) (2+d)^2 (\kappa^2 R - L^2) \alpha^2 L^2  
!     + \Big( - 4 d \kappa^2 R \ m + (2+d) (1+4m) \alpha L^2   \Big)^2
!        } 
!     \right)
!     \point
!   \end{array}
! \end{equation}
!
! After assigning appropriate values for the von K{\'a}rm{\'a}n
! constant, $\kappa$, the decay coefficient of homogeneous turbulence,
! $d$, the spatial decay rate, $\alpha$, and the slope, $L$, an infinite
! number of pairs of $m$ and $n$ satisfying \eq{GE_n} can be derived.
! Each corresponds to a different two--equation model.  Some example are
! given in \tab{tab:alphaL}. 
! 
! \begin{table}[ht]
!   \begin{center}
!     \begin{tabular}{ccccccc}
!       $\alpha$   &   $L$    &    $m$    &    $n$    &    $c_{\psi 2}$   &   $\sigma_k^\psi$   &     $\sigma_\psi$     \\[2mm] \hline
!       $-2.0$    &  $0.20$   &  $1.00$   &  $-0.67$  &      $1.22$       &       $0.80$        &          $1.07$       \\ 
!       $-2.0$    &  $0.20$   &  $2.00$   &  $-1.09$  &      $2.36$       &       $0.80$        &          $1.75$       \\[2mm] 
! %      $-2.5$    &  $0.20$   &  $0.92$   &  $-1.00$  &      $1.27$       &       $1.25$        &          $1.60$       \\
!       $-2.5$    &  $0.20$   &  $1.00$   &  $-1.05$  &      $1.35$       &       $1.25$        &          $1.68$       \\ 
!       $-2.5$    &  $0.20$   &  $2.00$   &  $-1.74$  &      $2.58$       &       $1.25$        &          $2.78$       \\ 
! %      $-2.5$    &  $0.25$   &  $0.19$   &  $-1.00$  &      $0.52$       &       $1.95$        &          $1.60$       \\
!     \end{tabular}
!     \caption{\label{tab:alphaL} Some parameter sets for the generic model with $\kappa = 0.4$,
!       $d = -1.2$, $(c_\mu^0)^2=0.3$, $c_{\psi_1}=m$ and obeying the log--layer
!       compatibility relation \eq{psiLog2}.}
!   \end{center}
! \end{table}
! Even though each line in this table represents a different
! two-equation model with completely different model constants, each of
! the two groups of models (with $\alpha=-2.0$ and $\alpha=-2.5$,
! respectively) {\em performs completely identical in all situations
!   discussed until here.} Thus, the generic model allows for the
! formulation of groups of two--equation models with fully controlled
! properties from the outset. As discussed by \cite{UmlaufBurchard2003},
! one more constraint is necessary to obtain the final values of all 
! parameters, including the exponents $m$ and $n$. These authors suggested
! that the first line in \tab{tab:alphaL} yields a model with excellent
! properties in all flows they considered.
!
! \vspace{10mm}
! {\bf Mixed layer deepending}
! \vspace{5mm}
!  
! The correct prediction of mixed layer deepening into a stratified
! fluid due to a wind stress at the surface is one of the most crucial
! requirements for an oceanic turbulence model. This situation has been
! frequently interpreted by analogy with the classical experiment of
! \cite{KatoPhillips69} and its re--interpretation by \cite{Price79}, in
! which the entrainment in a linearly stratified fluid subject to a
! constant surface stress was investigated. The results of this
! experiment have been used by numerous authors to calibrate their
! turbulence models.
! 
! In particular, it has been shown by \cite{BurchardBolding2001} for the
! $k$--$\epsilon$ model of \cite{Rodi87}, by \cite{Burchard2001c} for the
! $q^2 l$ model of \cite{MellorYamada82}, and by \cite{Umlaufetal2003} 
! for the $k$--$\omega$ model of \cite{Wilcox88} that, remarkably, the mixed
! layer depth predicted by these models depends almost exclusively on
! the value of the Richardson number, $Ri=N^2/M^2$, computed in a {\em
!   homogeneous}, stratified shear--flow in steady--state. This value is
! usually referred to as the steady--state Richardson number, $Ri_{st}$
! (\cite{Rohretal88}, \cite{Kaltenbachetal94}, \cite{Jacobitzetal97},
! \cite{Shihetal2000}).
! 
! \cite{Umlaufetal2003} showed that in the context of models considered
! in GOTM, the steady--state Richardson number is determined by the 
! relation
! \begin{equation}
!   \label{Ri_st}
!   Ri_{st}=\dfrac{c_\mu}{{c_\mu}'} \dfrac{c_{\psi 2} - c_{\psi 1}}{c_{\psi 2} - c_{\psi 3}}
!   \point
! \end{equation}
! Since it is well--known that, with the equilibrium assumption $P+B=\epsilon$,
! stability functions reduce to functions of $Ri$ only
! (\cite{MellorYamada74}, \cite{Galperinetal88}), \eq{Ri_st} is a
! non--linear equation for the model constant $c_{\psi 3}$ for given
! $Ri_{st}$. Note, that the structure parameters, $m$ and $n$, do not
! appear in \eq{Ri_st}. This implies that the type of the two--equation
! model is irrelevant for the prediction of the mixed layer depth, as
! long as \eq{Ri_st} is fulfilled for identical $Ri_{st}$. Numerical
! examples with very different values of $m$ and $n$ confirmed indeed
! that the mixed layer depth depends only on $Ri_{st}$.  This extends
! the findings of the authors mentioned above, who demonstrated that for
! all traditional models the mixed layer depth in the experiment of
! \cite{KatoPhillips69} could almost perfectly be reproduced, provided
! the parameter $c_{\psi 3}$ was chosen to correspond to $Ri_{st}\approx0.25$
! (see \cite{Umlaufetal2003}).

! Note, that in instable
! situations, a different value of the parameter $c_{\psi 3}$ needs to
! be used. This does not cause a discontinuity in the model because the
! buoyancy term in \eq{generic} is zero at the transition.  An
! evaluation of the length--scale equations in convective flows, however,
! is intimately related to the third--order modelling of the triple
! correlation terms, a topic outside the scope of this documentation.
! 
! !USES:
  IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                   ::  rad
!   
!-----------------------------------------------------------------------
!BOC
! do some checks

   if (tke_method.ne.tke_keps) then
      STDERR 'The generic scale equation should be used only'
      STDERR 'in connection with the dynamic equation for'
      STDERR 'the tke (use tke_method = 2).'
      STDERR 'Please change gotmturb.inp accordingly'
      STDERR 'Program aborts now in turbulence.F90'
      stop
   endif

   if (gen_d.gt.0.) then
      STDERR 'Temporal decay rate d in homogeneous'
      STDERR 'turbulence has to be negative.'
      STDERR 'Measured values are: -1.0 < d < -1.3'
      STDERR 'Please change gotmturb.inp accordingly'
      STDERR 'Program aborts now in turbulence.F90'
      stop
   endif

   if (gen_alpha.gt.0.) then
      STDERR 'Decay exponent alpha has to be negative.'
      STDERR 'Measured values are: -3 < alpha < -2' 
      STDERR 'Please change gotmturb.inp accordingly'
      STDERR 'Program aborts now in turbulence.F90'
      stop
   endif

   if (gen_l.lt.0.) then
      STDERR 'Slope L of the length scale in shear-free'
      STDERR 'turbulence has to be positive.'
      STDERR 'Measured values are: 0.15 < L < 0.25' 
      STDERR 'Please change gotmturb.inp accordingly'
      STDERR 'Program aborts now in turbulence.F90'
      stop
   endif

! compute the parameters as decribed in 
! Umlauf and Burchard, J. Mar. Res., 2003
   cpsi1    = gen_m
   sig_kpsi = 1.5*(gen_alpha*gen_l)**2/(rcm*cm0**2) 
   sig_k    = sig_kpsi

   rad      = 8.*gen_alpha**2.*gen_l**2.*(2.+gen_d)**2.* &
                   (rcm*kappa**2.-gen_l**2.)*gen_m*(1.+2.*gen_m) + &
                   ( -4.*gen_d*rcm*kappa**2.*gen_m &
                     +gen_alpha*gen_l**2.*(2.+gen_d)*(1.+4.*gen_m) )**2.
         
! check for negative radicand and compute n
   if (rad.gt.0) then
      gen_n = -1./( 4.*(2.+gen_d)*(rcm*kappa**2.-gen_l**2.) )* &
           ( 4.*gen_d*rcm*kappa**2.*gen_m &
             -gen_alpha*gen_l**2.*(2.+gen_d)*(1.+4.*gen_m) + sqrt(rad) )
      
   else
      STDERR 'Negative radicand discovered in computing'
      STDERR 'exponent n of the generic model.'
      STDERR 'Please choose other value for gen_m in gotmturb.inp'
      STDERR 'Program aborts now in turbulence.F90'
      stop
   endif

! check for positive exponent n   
   if (gen_n.gt.0.) then
      STDERR 'A positive exponent n of the lengt scale l'
      STDERR 'has been computed. This indicates that the model'
      STDERR 'would require a wall-function.'
      STDERR 'Please change gotmturb.inp accordingly'
      STDERR 'Program aborts now in turbulence.F90'
      stop
   endif

! compute Schmidt-number for psi
   sig_psi  = 2.*kappa**2.*gen_d*gen_n/(cm0**2*(gen_d+2.)) 

! check for negative Schmidt-number sig_psi
   if (sig_psi.lt.0.) then
      STDERR 'A negative Schmidt-number sig_psi has been'
      STDERR 'computed. Physically, that does not make sense.'
      STDERR 'Possible reason: You set gen_d < -2 in gotmturb.inp?'
      STDERR 'Please change gotmturb.inp accordingly'
      STDERR 'Program aborts now in turbulence.F90'
      stop
   endif
   cpsi2    = gen_n**2*kappa**2/(cm0**2*sig_psi)+cpsi1     

! compute c3 from given steady-state Richardson-number, or vice-versa
   if (compute_c3)  then
      cpsi3minus=c3(cpsi1,cpsi2,Ri_st)
   else
      ri_st=Rist(cpsi1,cpsi2,cpsi3minus)
   endif

! compute c3 for unstable stratification corresponding to a certain 
! ce3 in the k-epsilon model

   cpsi3plus=(1.5-ce3plus)*gen_n+gen_m

   return
   end subroutine generate_model
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Analyse the turbulence models \label{sec:analyse} 
!
! !INTERFACE:
   subroutine analyse_model 
!
! !DESCRIPTION:
! This routine analyses all models in GOTM for their physical properties
! implied by chosen model parameters. These results can be displayed by
! calling the internal routine {\tt report\_model()}, also defined in the
! {\tt turbulence} module (see \sect{sec:report}).
!
! In most cases, the relations connecting model parameters and physical
! properties have already been derived in \sect{sec:generate}:
! the von K{\'a}rm{\'a}n constant, $\kappa$, follows from \eq{psiLog}, 
! the decay rate in homogeneous turbulence , $d$, from \eq{d}, and
! the steady-state Richardson-number from \eq{Ri_st}. These relations have
!  been obtained in `generic' form (see \sect{sec:genericeq}), but relations
! for specific models, like the $k$-$\epsilon$ model or the $k$-$\omega$ model,
! can be derived by simply adopting the parameters compiled in \tab{tab:psi} and
! \tab{tab:constants} in \sect{sec:genericeq}.
! 
! The decay rates $\alpha$ and $L$ in shear-free turbulence follow from 
! the physically meaningful roots of \eq{GE_alphaL_1} and \eq{GE_alphaL_2},
! which are
! \begin{equation}
!   \label{GE_alphaL}
!   \begin{array}{rcl}
!     \alpha 
!     &=& - 
!     \dfrac{4 n {(\sigma_k^\psi)}^\frac{1}{2}}
!     { (1+4m) {(\sigma_k^\psi)}^\frac{1}{2} - {(\sigma_k^\psi + 24 \sigma_\psi c_{\psi 2} )}^\frac{1}{2}} \comma \\[8mm]
!     L 
!     &=&
!     c_\mu^0 R^{\frac{1}{2}} \left( 
!       \dfrac{ (1+4m+8m^2) \sigma_k^\psi 
!         + 12 \sigma_\psi c_{\psi 2} 
!          - (1+4m) ( {\sigma_k^\psi} ( \sigma_k^\psi 
!           + 24 \sigma_\psi c_{\psi 2} ))^\frac{1}{2}  }{12 n^2}
!   \right)^\frac{1}{2}
!   \comma
!   \end{array}
! \end{equation}
! where it should be recalled that $R=c_\mu^0/c_\mu$.
! For the standard models (without ASM), $R=1$ may be assumed. Then,
! with the values from \tab{tab:psi} and \tab{tab:constants}, 
! solutions for the $k$-$\epsilon$ model of \cite{Rodi87},
!  and the $k$-$\omega$ model of \cite{Umlaufetal2003} can be directly
! recovered as special cases of this equation. 
! 
! Due to its wall-functions, the model of \cite{MellorYamada82} described
! in \sect{sec:lengthscaleeq} requires a slightly more complicated
! analysis. For this model, the von K{\'a}rm{\'a}n constant is computed
! according to
! \begin{equation}
!    \label{kappaMY}
!    \kappa = \sqrt{\dfrac{E_2 - E_1 + 1}{S_l B_1}}
! \point
! \end{equation}
! 
! The decay rates in shear-free turbulence can be shown to be
! \begin{equation}
!   \label{MY_alphaL}
!   \begin{array}{rcl}
!     \alpha 
!     &=& \dfrac{5 \kappa B_1^{\frac{1}{2}} S_l 
!       + \left( 12 E_2 \left( 2 S_l - S_q \right) 
!       +  B_1 \kappa^2 S_l \left(S_l + 12 S_q \right) \right)^{\frac{1}{2}}}{
!       3 \kappa B_1^{\frac{1}{2}} (S_q - 2 S_l)  }  \\
!     L 
!     &=&
!     \kappa \left( 
!       \dfrac{\cal N }{
!        6 S_q (E_2 - B_1 \kappa^2 S_l )^2}
!     \right)^\frac{1}{2}
!   \comma
!   \end{array}
! \end{equation}
! where we introduced the abbreviation
! \begin{equation}
!   \begin{array}{rcl}
!     {\cal N}
!     &=&
!     6 E_2 \left( 2 S_l - S_q \right) 
!    + B_1 \kappa^2 S_l \left( 13 S_l + 6 S_q \right) \\[2mm]
!    &-& 5 B_1^\frac{1}{2} \kappa S_l \left( 12 E_2 (2 S_l-S_q) 
!    + B_1 \kappa^2 S_l (S_l + 12 S_q )\right)^\frac{1}{2}
!   \point
!   \end{array}
! \end{equation}
! These equations replace \eq{GE_alphaL} for the model of \cite{MellorYamada82}.
! Decay-rates for this model do not at all depend on the stability
! functions. However, they depend on the parameter $E_2$ of the
! wall-functions. This implies that far from the surface ($E_2=0$) decay
! is different from that close to the surface, which is physically not
! very reasonable (see \cite{Umlaufetal2003}).
!
! The routine {\tt analyse\_model()} works also for one-equation models,
! where the length-scale, $l$, is prescribed by an analytical expression 
! (see \sect{sec:algebraiclength}).  However, some attention has to be paid
! in interpreting the results. First, it is clear these models cannot
! predict homogeneous turbulence, simply because all formulations rely on
! some type of modified boundary layer expressions for the length-scale.
! This impies that a well--defined decay rate, $d$, and a steady--state 
! Richardson-number, $Ri_{st}$, cannot be computed. Second, the von 
! K{\'a}rm{\'a}n constant, $\kappa$, does not follow from \eq{psiLog} or 
! \eq{kappaMY}, because $\kappa$ now relates directly to 
! the prescribed slope of the length-scale close to the bottom or the surface.
! Third, in shear-free flows, \eq{GE_alphaL}$_1$ or \eq{MY_alphaL}$_1$ remain
! valid, provided the planar source of the spatially decaying turbulence
! is located at the surface. Only then, the slope of the length-scale, $L$,
! defined in \eq{power_law} can be identified with the prescribed slope, 
! $\kappa$.
! \eq{GE_alphaL}$_1$ or \eq{MY_alphaL}$_1$ are then identical 
! to the solutions suggested by \cite{CraigBanner94}.
!
! In this context, it should be pointed out that the 
! shear--free solutions also have a 
! direct relation to an important oceanic situation. If the planar
! source of turbulence is assumed to be located at the surface $z=0$, 
! and if the injected turbulence is identified with turbulence caused
! by breaking surface--waves, then it can be shown that \eq{GE_alphaL} 
! or \eq{MY_alphaL} are valid in a thin boundary layer adjacent to the 
! suface. Further below, to classical law of the wall determines the 
! flow, see \cite{Umlaufetal2003}.
! 
! !USES:
   use meanflow, only:       depth
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE              :: rad,one=1.0
!
!-----------------------------------------------------------------------
!BOC
!  compute the basic properties of all models available
  
   select case(len_scale_method)
   case(Parabola)
      
      if (compute_kappa) kappa=0.4
      gen_l     = kappa
      gen_alpha = -sqrt(2./3.*cm0**2.*rcm*sig_k/gen_l**2.)
      
   case(Triangle)
      
      if (compute_kappa) kappa=0.4
      gen_l     = kappa
      gen_alpha = -sqrt(2./3.*cm0**2.*rcm*sig_k/gen_l**2.)
      
   case(Xing)
      
      if (compute_kappa) kappa=0.4
      gen_l     = kappa
      gen_alpha = -sqrt(2./3.*cm0**2.*rcm*sig_k/gen_l**2.)
      
   case(RobertOuellet)

      if (compute_kappa) kappa=0.4
      ! Slope at the surface computed from the analytical profile
      ! of Robert and Ouellet (1987) (see algebraiclength.F90)

      gen_l     = kappa*( (depth+z0b)/depth/(2.*sqrt(z0s/depth)) &
                         - sqrt(z0s/depth) )  
      gen_alpha = -sqrt(2./3.*cm0**2.*rcm*sig_k/gen_l**2.)
      
   case(Blackadar)
      
      if (compute_kappa) kappa=0.4
      gen_l     = kappa
      gen_alpha = -sqrt(2./3.*cm0**2.*rcm*sig_k/gen_l**2.)
      
   case(BougeaultAndre)
      
      !     This needs a check
      if (compute_kappa) kappa=0.4
      gen_l     = kappa
      gen_alpha = -sqrt(2./3.*cm0**2.*rcm*sig_k/gen_l**2.)
      
   case(ispra_length)
      
      if (compute_kappa) kappa=0.4
      gen_l     = kappa
      gen_alpha = -sqrt(2./3.*cm0**2.*rcm*sig_k/gen_l**2.)
      
      
   case(diss_eq)

      ! compute kappa from the parameters
      if (compute_kappa)  then
         rad=sig_e*(ce2-ce1)
         if (rad .gt. 0) then
            kappa=cm0*sqrt(rad)
         else
            STDERR 'Negative radicand discovered in computing'
            STDERR 'kappa for the k-epsilon model.'
            STDERR 'Possible reason: you took ce2 < ce1 '
            STDERR 'Please change gotmturb.inp accordingly.'
            STDERR 'Program aborts now in turbulence.F90'
            stop
         endif
         if (sig_peps) then
            STDERR 'For using the Craig & Banner 1994 parameterisation'
            STDERR 'by Burchard (2001) kappa must be prescribed.'
            STDERR 'For doing so, compute_kappa=.false. must be set.'
            STDERR 'Please change gotmturb.inp accordingly.'
            STDERR 'Program aborts now in turbulence.F90'
            stop
         end if

         sig_e0=sig_e  ! use constant Schmidt-number always

      ! or compute the Schmidt-number for given kappa
      else

         sig_e= kappa**2/(ce2-ce1)/cm0**2

         ! compute Schmidt-number for Burchard (2001) wave-breaking
         if (sig_peps) then
            craig_m=sqrt(1.5*cmsf**2*sig_k/kappa**2)
            sig_e0=(4./3.*craig_m+1.)*(craig_m+1.)*kappa**2/(ce2*cmsf**2)
         else
            sig_e0=sig_e
         endif
      endif


      ! compute model propeties
      gen_d      = 1./(1.-ce2)
      gen_alpha  = 4.*sqrt(sig_k)/(7.*sqrt(sig_k)-sqrt(sig_k+24.*sig_e0*ce2))
      gen_l      = cm0*sqrt(rcm)*sqrt( (25.*sig_k+12.*sig_e0*ce2 -  &
                   7.*sqrt(sig_k*(sig_k+24.*sig_e0*ce2 ) ) ) / 12. )

      ! compute c3 from given steady-state Richardson-number, or vice-versa
      if (compute_c3)  then
         ce3minus=c3(ce1,ce2,Ri_st)
      else
         ri_st=Rist(ce1,ce2,ce3minus)
      endif
   case(generic_eq)
      rad=sig_psi*(cpsi2-cpsi1)/gen_n**2.
      if (rad.gt.0) then
         kappa=cm0*sqrt(rad)
      else
         STDERR 'Negative radicand discovered in computing'
         STDERR 'kappa for the generic model.'
         STDERR 'Possible reason: you took cpsi2 < cpsi1'
         STDERR 'Please change gotmturb.inp accordingly.'
         STDERR 'Program aborts now in turbulence.F90'
         stop
      endif
      sig_k      = sig_kpsi
      gen_d      = -2.*gen_n/(2.*gen_m + gen_n - 2.*cpsi2)   
      gen_alpha  = -4.*gen_n*sqrt(sig_k) / &
                    ( (1.+4.*gen_m)*sqrt(sig_k) &
                     - sqrt(sig_k + 24.*sig_psi*cpsi2 ) )
      gen_l      = cm0*sqrt(rcm)* &
                  sqrt( ( (1.+4.*gen_m+8.*gen_m**2)*sig_k &
                         + 12.*sig_psi*cpsi2 &
                         - (1.+4.*gen_m) &
                            *sqrt(sig_k*(sig_k+24.*sig_psi*cpsi2)) ) &
                       /(12.*gen_n**2.) ) 

      ! compute c3 from given steady-state Richardson-number, or vice-versa
      if (compute_c3)  then
         cpsi3minus=c3(cpsi1,cpsi2,Ri_st)
      else
         ri_st=Rist(cpsi1,cpsi2,cpsi3minus)
      endif

      ! compute c3 for unstable stratification from corresponding value of 
      ! ce3 for the k-epsilon model
      cpsi3plus=(1.5-ce3plus)*gen_n+gen_m
      

   case(length_eq)
      ! compute kappa from the parameters or vice-versa
      if (compute_kappa)  then
         rad = (e2-e1+1.)/(sl*b1)
         if (rad.gt.0) then
            kappa=sqrt(rad)
         else
            STDERR 'Negative radicand discovered in computing'
            STDERR 'kappa for the Mellor-Yamada model.'
            STDERR 'Possible reason: you took E2 < E1-1 '
            STDERR 'Please change gotmturb.inp accordingly.'
            STDERR 'Program aborts now in turbulence.F90'
            stop
         endif
      else
         ! E_2 instead of S_l is used here to tune kappa
         e2=kappa**2*b1*sl+e1-1.  
      endif
      ! d=-1 always for the Mellor-Yamada (1982) model
      gen_d      = -1.           

      ! spatial decay rates of turbulence from a planar source are 
      ! given here for the Mellor-Yamada model with (!) function.
      ! This corresponds to the wave-breaking case.
      gen_alpha  = ( 5.*kappa*sqrt(b1)*sl &
                    +sqrt(12.*e2*(2.*sl-sq)+b1*kappa**2.*sl*(sl+12.*sq) ) ) &
                  /( 3.*kappa*sqrt(b1)*(sq-2*sl) )
      gen_l = kappa*sqrt( ( 6.*e2*(2.*sl-sq)+b1*kappa**2.*sl*(13.*sl+6.*sq) &
                           -5.*sqrt(b1)*kappa*sl*sqrt( 12.*e2*(2.*sl-sq) &
                           +b1*kappa**2.*sl*(sl+12.*sq) ) ) &
                              /(6.*sq*(e2-b1*kappa**2.*sl)**2.) )           
               
      ! compute E3 from given steady-state Richardson-number, or vice-versa
      if (compute_c3)  then
         e3=c3(e1,one,Ri_st)
      else
         ri_st=Rist(e1,one,e3)
      endif

   case default
   end select
  
   return
   end subroutine analyse_model 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Report turbulence model \label{sec:report}
!
! !INTERFACE:
   subroutine report_model
!
! !DESCRIPTION:
! This routine reports on the parameters and the propeties 
! of all turbulence models implemented in GOTM. Results are
! written to the screen.

! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!-----------------------------------------------------------------------
!BOC
! Report on the properties of each model 
   select case(len_scale_method)
      case(Parabola)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using a one-equation model'
         LEVEL2 'with a parabolic prescribed length-scale.'
         LEVEL2 'The properties of this model are:'
         LEVEL2 ' '
         LEVEL3 'Schmidt-number for k,          sig_k =', sig_k
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL2 ' '

         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'At the surface:'
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear), L     =', gen_l
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case(Triangle)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using a one-equation model'
         LEVEL2 'with a triangular prescribed length-scale.'
         LEVEL2 'The properties of this model are:'
         LEVEL2 ' '
         LEVEL3 'Schmidt-number for k,          sig_k =', sig_k
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL2 ' '
         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'At the surface:'
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear),    L =', gen_l
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case(Xing)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using a one-equation model'
         LEVEL2 'with the prescribed length-scale of Xing and Davies (1995)'
         LEVEL2 'The properties of this model are:'
         LEVEL2 ' '
         LEVEL3 'Schmidt-number for k,          sig_k =', sig_k
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL2 ' '
         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'At the surface:'
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear), L     =', gen_l
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case(RobertOuellet)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using a one-equation model'
         LEVEL2 'with the prescribed length-scale of Robert and Ouellet (1987)'
         LEVEL2 ' '
         LEVEL2 'The properties of this model are:'
         LEVEL3 'Schmidt-number for k,          sig_k =', sig_k
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL2 ' '
         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'At the surface:'
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear),     L =', gen_l
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case(Blackadar)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using a one-equation model'
         LEVEL2 'with the length-scale of Blackadar (1962)'
         LEVEL2 'The properties of this model are:'
         LEVEL2 ' '
         LEVEL3 'Schmidt-number for k,          sig_k =', sig_k
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL2 ' '
         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'At the surface:'
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear),     L =', gen_l
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case(BougeaultAndre)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using a one-equation model'
         LEVEL2 'with the length-scale of Bougeault and Andre (1986)'
         LEVEL2 'The properties of this model are:'
         LEVEL2 ' '
         LEVEL3 'Schmidt-number for k,          sig_k =', sig_k
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL2 ' '
      case(ispra_length)
         LEVEL2 ' '
         LEVEL2 'You are using a one-equation model'
         LEVEL2 'with the ISPRAMIX length-scale (see GOTM-report)'
         LEVEL2 'The properties of this model are:'
         LEVEL2 ' '
         LEVEL3 'Schmidt-number for k,          sig_k =', sig_k
         LEVEL3 'von Karman constant,          kappa  =', kappa
         LEVEL2 ' '
         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'At the surface:'
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear),     L =', gen_l
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case(diss_eq)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using the k-epsilon model'
         LEVEL2 'with the following properties:'
         LEVEL2 ' '
         LEVEL3 'ce1                                  =', ce1
         LEVEL3 'ce2                                  =', ce2
         LEVEL3 'ce3minus                             =', ce3minus
         LEVEL3 'ce3plus                              =', ce3plus
         LEVEL3 'sig_k                                =', sig_k
         LEVEL3 'sig_e                                =', sig_e
         LEVEL2 ' '
         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL3 'homogeneous decay rate,            d =', gen_d
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear),     L =', gen_l
         LEVEL3 'steady-state Richardson-number, Ri_st=', ri_st
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case(length_eq)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using the Mellor-Yamada model'
         LEVEL2 'with the following properties:'
         LEVEL2 ' '
         LEVEL3 'B1                                   =', b1
         LEVEL3 'E1                                   =', e1
         LEVEL3 'E2                                   =', e2
         LEVEL3 'E3                                   =', e3
         LEVEL3 'Sq                                   =', sq
         LEVEL3 'Sl                                   =', sl
         LEVEL2 ' '
         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL3 'homogeneous decay rate,            d =', gen_d
         LEVEL3 'steady-state Richardson-number, Ri_st=', ri_st
         LEVEL2 ' '
         LEVEL3 'At the surface (i.e. with wall-function):'
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear),     L =', gen_l
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case(generic_eq)
         LEVEL2 ' '
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 'You are using the generic two-equation model'
         LEVEL2 'with the following properties:'
         LEVEL2 ' '
         LEVEL3 'exponent of k in psi-equation,    m  =', gen_m
         LEVEL3 'exponent of l in psi-equation,    n  =', gen_n
         LEVEL3 'exponent of cm0 in psi-equation,  p  =', gen_p
         LEVEL3 'cpsi1                                =', cpsi1
         LEVEL3 'cpsi2                                =', cpsi2
         LEVEL3 'cpsi3minus                           =', cpsi3minus
         LEVEL3 'cpsi3plus                            =', cpsi3plus
         LEVEL3 'sig_k                                =', sig_kpsi
         LEVEL3 'sig_psi                              =', sig_psi
         LEVEL2 ' '
         LEVEL3 'Value of the stability function'       
         LEVEL3 'in the log-law,                   cm0 =', cm0
         LEVEL3 'in shear-free turbulence,        cmsf =', cmsf
         LEVEL2 ' '
         LEVEL3 'von Karman constant,           kappa =', kappa
         LEVEL3 'homogeneous decay rate,            d =', gen_d
         LEVEL3 'spatial decay rate (no shear), alpha =', gen_alpha
         LEVEL3 'length-scale slope (no shear),     L =', gen_l
         LEVEL3 'steady-state Richardson-number, Ri_st=', ri_st
         LEVEL2 '--------------------------------------------------------'
         LEVEL2 ' '
      case default
   end select  

   return
   end subroutine report_model 
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Manage turbulence time-stepping
!
! !INTERFACE:
   subroutine do_turbulence(nlev,dt,depth,u_taus,u_taub,  &
                            z0s,z0b,h,NN,SS,P,B)
!
! !DESCRIPTION: This routine is the central point of the 
! turbulence scheme. It determines the order, in which
! turbulence variables are updated. It calls for example
! other member functions which update the stability functions,
! the TKE, the length-scale, the dissipation rate, and the 
! turbulent diffusivities.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt,depth,u_taus,u_taub,z0s,z0b
   REALTYPE, intent(in)                :: h(0:nlev)
   REALTYPE, intent(in)                :: NN(0:nlev),SS(0:nlev)
   REALTYPE, intent(in)                :: P(0:nlev),B(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard, 
!                      Manuel Ruiz Villarreal
!
!EOP
!-------------------------------------------------------------------------
!BOC
   call stabilityfunctions(nlev,NN,SS,1)
   call do_tke(nlev,dt,u_taus,u_taub,z0s,z0b,h,SS,NN,P,B)
   call lengthscale(nlev,dt,z0b,z0s,u_taus,u_taub,depth,h,NN,P,B)
   call kolpran(nlev)

   return
 end subroutine do_turbulence
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the turbulent kinetic energy \label{sec:updateTKE}
!
! !INTERFACE:
   subroutine do_tke(nlev,dt,u_taus,u_taub,z0s,z0b,h,SS,NN,P,B)
!
! !DESCRIPTION:
! Based on user input, this routine calls the appropriate routines for
! calculating the turbulent kinetic energy. The user has the choice
! between an algebraic equation described in \sect{sec:tkealgebraic}, and two
! versions of the dynamic transport equation of the TKE described
! in \sect{sec:tkeeq} and \sect{sec:q2over2eq}. The former uses
! $k$-$\epsilon$ notation, the latter the notation of 
! \cite{MellorYamada82} and the slightly different model for
! the turbulent transport used by the latter authors. Apart from this, 
! they are identical and update the vectors {\tt tke} and {\tt tkeo}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: u_taus,u_taub,z0s,z0b
   REALTYPE, intent(in)                :: h(0:nlev)
   REALTYPE, intent(in)                :: P(0:nlev),B(0:nlev)
   REALTYPE, intent(in)                :: NN(0:nlev),SS(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard, 
!                      Manuel Ruiz Villarreal
!
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (tke_method)
      case(tke_local_eq)
         ! use algebraic length scale equation
          call tkealgebraic(nlev,u_taus,u_taub,NN,SS)      
      case(tke_keps)
         ! use differential equation for tke (k-epsilon style)
         call tkeeq(nlev,dt,u_taus,u_taub,z0s,z0b,h,P,B,num)
      case(tke_MY)
         ! use differential equation for q^2/2 (Mellor-Yamada style)
         call q2over2eq(nlev,dt,u_taus,u_taub,z0s,z0b,h,P,B)
      case default
   end select
      
   return
 end subroutine do_tke
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Update the length-scale \label{sec:updateLength}
!
! !INTERFACE:
   subroutine lengthscale(nlev,dt,z0b,z0s,u_taus,u_taub,   &
                          depth,h,NN,P,B)
!
! !DESCRIPTION:
! Based on user input, this routine calls the appropriate routines for
! calculating the turbulent length-scale, $l$, and the rate of 
! dissipation, $\epsilon$.  The user has the choice
! between several algebraic equations described in \sect{sec:algebraiclength},
! and several differential transport equations for a length-scale
! determining variable. At the moment, GOTM implements equations 
! for the rate of dissipation, described in \sect{sec:dissipationeq}, 
! for the Mellor-Yamada model described in \sect{sec:lengthscaleeq}, and 
! for the generic scale formulated by \cite{UmlaufBurchard2003} and described
! in \sect{sec:genericeq}. This last transport equation generalises all of the 
! previously mentioned models. For example, the $k$-$\epsilon$ model 
! and the $k$-$\omega$ model  can be recovered as special cases of the
! generic equation, see \cite{UmlaufBurchard2003}.  
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: z0b,z0s
   REALTYPE, intent(in)                :: u_taus,u_taub
   REALTYPE, intent(in)                :: depth
   REALTYPE, intent(in)                :: h(0:nlev),NN(0:nlev)
   REALTYPE, intent(in)                :: P(0:nlev),B(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard, 
!                      Manuel Ruiz Villarreal,
!                      Lars Umlauf
!
!EOP
!-----------------------------------------------------------------------
!BOC
   select case(len_scale_method)
      case(diss_eq)
         call dissipationeq(nlev,dt,u_taus,u_taub,z0s,z0b,h,P,B,NN,num)
      case(generic_eq)
         call genericeq(nlev,dt,u_taus,u_taub,z0s,z0b,h,P,B,NN,num)
      case(length_eq)
         call lengthscaleeq(nlev,dt,u_taus,u_taub,z0s,z0b,h,depth,P,B,NN)
      case(BougeaultAndre)
         call potentialml(nlev,z0b,z0s,h,depth,NN)
      case default
         call algebraiclength(len_scale_method,nlev,z0b,z0s,depth,h,NN)
   end select
 
   return
   end subroutine lengthscale
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update diffusivities (Kolmogorov-Prandtl relation) \label{sec:kolpran}
!
! !INTERFACE:
   subroutine kolpran(nlev)
!
! !DESCRIPTION:
! Eddy viscosity and diffusivity are calculated by means of the relation of 
! Kolmogorov and Prandtl from the updated values of $k$, $l$ and the
! stability functions according to \eq{nu}. 
! 
! Note, that this routine relies on the fact that the lowest and
! uppermost values of the stability functions and of $k$ and $l$
! have been computed using the correct boundary conditions. No 
! special treatment of $\nu_t$ and $\nu'_t$ at the boundaries is processed.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard, 
!                      Manuel Ruiz Villarreal
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: x
!
!-----------------------------------------------------------------------
!BOC
   do i=0,nlev
      x=sqrt(tke(i))*L(i)
      num(i)=cmue1(i)*x
      nuh(i)=cmue2(i)*x
   end do

   return
   end subroutine kolpran 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Boundary conditons for the $k$--equation ($k$-$\epsilon$ style) \label{sec:kBC} 
!
! !INTERFACE:

   REALTYPE function k_bc(bc,type,zi,z0,u_tau)
!
!
! !DESCRIPTION:
! Computes prescribed and flux boundary conditions for  the transport
! equation \eq{tkeA}. The formal parameter {\tt bc} determines 
! whether {\tt Dirchlet} or {\tt Neumann}--type boundary conditions
! are computed. Depending on the physical properties of the 
! boundary--layer, the parameter {\tt type} relates either to a {\tt visous},
! a {\tt logarithmic}, or an {\tt injection}--type boundary--layer.
! In the latter case, the flux of TKE caused by breaking surface waves
! has to be specified. Presently, there is only one possibility 
! to do so implemented in GOTM. It is described in \sect{sec:fkCraig}.
! All parameters that determine the boundary layer have to be 
! set in {\tt gotmturb.inp}.
!
! Note that  in this section, for brevity, $z$ denotes the distance 
! from the wall (or the surface), and \emph{not} the standard 
! coordinate of the same name used in GOTM.
!
! \vspace{6mm}
! {\bf Viscous boundary-layers}
! \vspace{4mm}
!
! This type is not implemented yet in GOTM.
!
! \vspace{6mm}
! {\bf Logarithmic boundary-layers}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows from \eq{kLog} as
! \begin{equation}
!   \label{KE_k_Dirichlet}
!   k= \dfrac{u_*^2}{(c_\mu^0)^2}
!   \point
!  \end{equation}
!
! The Neumann (flux) boundary condition can be derived from
! the constancy of  $k$ in the logarithmic region. This fact
! can be written as
! \begin{equation}
!   \label{KE_k_Neumann}
!   F_k = - \dfrac{\nu_t}{\sigma_k} \partder{k}{z} = 0
!   \point
! \end{equation}
!
! \vspace{6mm}
! {\bf Shear-free boundary-layers with injection of TKE}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows simply
! from the power-law in \eq{power_law},
! \begin{equation}
!   \label{KE_k_wave_Dirichlet}
!   k= K (z+z_0)^\alpha
!   \point
! \end{equation}
! 
! The Neumann (flux) boundary condition can be written as
! \begin{equation}
!   \label{KE_k_wave_Neumann}
!   F_k = - \dfrac{\nu_t}{\sigma_k} \partder{k}{z} = 
!   - \dfrac{c_\mu}{\sigma_k} K^\frac{3}{2} L \alpha (z+z_0)^{\frac{3}{2} \alpha} 
!   \comma
! \end{equation}
! which follows immediately from \eq{power_law} and the expression for
! the turbulent diffusivity, \eq{nu}. The parameter $K$ can be determined
! from an evaluation of \eq{KE_k_wave_Neumann} at $z=0$. The result is
! \begin{equation}
!   \label{K}
!   K = \left( -\dfrac{\sigma_k}{c_\mu \alpha L} F_k \right)^\frac{2}{3} \dfrac{1}{z_0^\alpha}
!  \comma
! \end{equation}
! where the specification of the flux $F_k$ and the value of $z_0$ have to be
! determined from a suitable model of the wave breaking process.
!
! !USES:
     IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: bc,type
   REALTYPE, intent(in)                :: zi,z0,u_tau
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!     
! !LOCAL VARIABLES:
   REALTYPE                  :: K
   REALTYPE                  :: f_k
   REALTYPE, external        :: fk_craig
!
!-----------------------------------------------------------------------
!BOC
! check for correct function call

   select case(type)
      
      case(viscous)
         STDERR 'Sorry, viscous boundary layers not yet implemented.'
         STDERR 'Please change gotmturb.inp accordingly'
         STDERR 'Program aborts now in turbulence.F90'
         stop
         
      case(logarithmic)
         if (bc.eq.Dirichlet) then
            k_bc = u_tau**2./cm0**2. 
         else
            k_bc = 0.
         endif
         
      case(injection)
         ! compute the capital K from the analytical solution 
         ! for shear-free flows

         ! compute the flux of k from the wave-breaking model
         f_k  = fk_craig(u_tau)

         K    = (-sig_k*f_k/(cmsf*gen_alpha*gen_l) )**(2./3.) / z0**gen_alpha

         if (bc.eq.Dirichlet) then
            k_bc = K*(zi+z0)**gen_alpha
         else
            k_bc = -cmsf/sig_k*K**1.5*gen_alpha*gen_l*(zi+z0)**(1.5*gen_alpha)
         endif
      
      case default
   end select
      
   return  
   end function k_bc
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Boundary conditons for the $k$--equation (Mellor--Yamada style) \label{sec:q2over2BC}
!
! !INTERFACE:
   REALTYPE function  q2over2_bc(bc,type,zi,z0,u_tau)
!
!
! !DESCRIPTION:
! Computes prescribed and flux boundary conditions for  the transport
! equation \eq{tkeB}. The formal parameter {\tt bc} determines 
! whether {\tt Dirchlet} or {\tt Neumann}--type boundary conditions
! are computed. Depending on the physical properties of the 
! boundary--layer, the parameter {\tt type} relates either to a {\tt visous},
! a {\tt logarithmic}, or an {\tt injection}--type boundary--layer.
! In the latter case, the flux of TKE caused by breaking surface waves
! has to be specified. Presently, there is only one possibility 
! to do so implemented in GOTM. It is described in \sect{sec:fkCraig}.
! All parameters that determine the boundary layer have to be 
! set in {\tt gotmturb.inp}.
!
! Note that  in this section, for brevity, $z$ denotes the distance 
! from the wall (or the surface), and \emph{not} the standard 
! coordinate of the same name used in GOTM.
!
! \vspace{6mm}
! {\bf Viscous boundary-layers}
! \vspace{4mm}
!
! This type is not implemented yet in GOTM.
!
! \vspace{6mm}
! {\bf Logarithmic boundary-layers}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows from \eq{kLog} 
! and \eq{B1} as
! \begin{equation}
!   \label{MY_k_Dirichlet}
!   q^2/2= \dfrac{u_*^2 B_1^\frac{2}{3}}{2}
!   \point
! \end{equation}
! 
! The Neumann (flux) boundary condition can be derived from
! the constancy of  $q^2/2$ in the logarithmic region. This fact
! can be written as
! \begin{equation}
!   \label{MY_k_Neumann}
!   F_q = - S_q q l \partder{k}{z} = 0
!   \point
! \end{equation}
! 
! \vspace{6mm}
! {\bf Shear-free boundary-layers with injection of TKE}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows simply
! from the power-law in \eq{power_law},
! \begin{equation}
!   \label{MY_k_wave_Dirichlet}
!   \frac{q^2}{2} = k = K (z+z_0)^\alpha
!  \point
! \end{equation}
! 
! The Neumann (flux) boundary condition can be written as
! \begin{equation}
!   \label{MY_k_wave_Neumann}
!   F_q = - S_q q l \partder{k}{z} = 
!   - \sqrt{2} S_q  K^\frac{3}{2} \alpha L (z+ z_0)^{\frac{3}{2} \alpha} 
!   \comma
! \end{equation}
! which follows immediately from \eq{power_law}. 
! The parameter $K$ can be determined
! from an evaluation of \eq{MY_k_wave_Neumann} at $z=0$. The result is
! \begin{equation}
!   \label{MY_K}
!   K = \left( - \dfrac{F_q}{\sqrt{2} S_q \alpha L} \right)^\frac{2}{3} 
!        \dfrac{1}{z_0^\alpha}
!    \comma
! \end{equation}
! where the specification of the flux $F_q$ and the value of $z_0$ have to be
! determined from a suitable model of the wave breaking process.
!
! !USES:
     IMPLICIT NONE
!
! !INPUT PARAMETERS:
  integer, intent(in)                  :: bc,type
  REALTYPE, intent(in)                 :: zi,z0,u_tau
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
      REALTYPE               :: K
      REALTYPE               :: f_k
      REALTYPE, external     :: fk_craig
! 
!-----------------------------------------------------------------------
!BOC
! Compute the boundary conditions
   select case(type)
      
      case(viscous)
         STDERR 'Sorry, viscous boundary layers not yet implemented.'
         STDERR 'Please change gotmturb.inp accordingly'
         STDERR 'Program aborts now in turbulence.F90'
         stop
         
      case(logarithmic)
         if (bc.eq.Dirichlet) then
            q2over2_bc = u_tau**2.*b1**(2./3.)/2. 
         else
            q2over2_bc = 0.
         endif
         
      case(injection)
         ! compute the capital K from the analytical solution 
         ! of shear-free flows and from f_k given by the wave-breaking model

         ! compute the flux of k from the wave-breaking model
         f_k  = fk_craig(u_tau)
         
         K = ( -f_k/(sqrt(2.)*sq*gen_alpha*gen_l) )**(2./3.) / z0**gen_alpha
         
         if (bc.eq.Dirichlet) then
            q2over2_bc = K*(zi+z0)**gen_alpha
         else
            q2over2_bc = -sqrt(2.)*sq*K**1.5*gen_alpha*gen_l* &
                         (zi+z0)**(1.5*gen_alpha)
         endif
      
      case default
   end select
   end function q2over2_bc
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Boundary conditons for the $\epsilon$--equation  \label{sec:epsilonBC}
!
! !INTERFACE:
   REALTYPE function  epsilon_bc(bc,type,zi,ki,z0,u_tau)
!
!
!DESCRIPTION:
! Computes prescribed and flux boundary conditions for  the transport
! equation \eq{dissipation}. The formal parameter {\tt bc} determines 
! whether {\tt Dirchlet} or {\tt Neumann}--type boundary conditions
! are computed. Depending on the physical properties of the 
! boundary--layer, the parameter {\tt type} relates either to a {\tt visous},
! a {\tt logarithmic}, or an {\tt injection}--type boundary--layer.
! In the latter case, the flux of TKE caused by breaking surface waves
! has to be specified. Presently, there is only one possibility 
! to do so implemented in GOTM. It is described in \sect{sec:fkCraig}.
! All parameters that determine the boundary layer have to be 
! set in {\tt gotmturb.inp}.
!
! Note that  in this section, for brevity, $z$ denotes the distance 
! from the wall (or the surface), and \emph{not} the standard 
! coordinate of the same name used in GOTM.
!
! \vspace{6mm}
! {\bf Viscous boundary-layers}
! \vspace{4mm}
!
! This type is not implemented yet in GOTM.
!
! \vspace{6mm}
! {\bf Logarithmic boundary-layers}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows from \eq{epsilon} as
! \begin{equation}
!   \label{KE_e_Dirichlet}
!   \epsilon= \dfrac{(c_\mu^0)^3 k^\frac{3}{2}}{\kappa (z+z_0)}
!   \comma
! \end{equation}
! where we used the law--of--the--wall relation $l=\kappa(z+z_0)$.
! 
! The Neumann (flux) boundary condition can be expressed as 
! \begin{equation}
!   \label{KE_e_Neumann}
!   F_\epsilon = - \dfrac{\nu_t}{\sigma_\epsilon} \partder{\epsilon}{z} 
!   = \dfrac{(c_\mu^0)^4}{\sigma_\epsilon}  \dfrac{k^2}{z+z_0}
!   \comma
! \end{equation}
! by inserting $l=\kappa(z+z_0)$ into the expression for the 
! diffusivity in \eq{nu}. Note, that in \eq{KE_e_Dirichlet} and
! \eq{KE_e_Neumann}, we use {\tt ki}, the value of $k$ at the current time
! step, to compute the boundary conditions. By means of 
! \eq{kLog}, it would have been also possible to express the boundary
! conditions in terms of the friction velocity, $u_*$. This, however,
! causes numerical difficulties in case of a stress--free surface
! boundary--layer as for example in the pressure--driven open channel
! flow. 
! 
! \vspace{6mm}
! {\bf Shear-free boundary-layers with injection of TKE}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows simply
! from the power-law \eq{power_law} inserted in \eq{epsilon}. This
! yields
! \begin{equation}
!   \label{KE_e_wave_Dirichlet}
!   \epsilon= (c_\mu^0)^3 K^\frac{3}{2} L^{-1} (z+z_0)^{\frac{3}{2}\alpha - 1}
!   \point
! \end{equation}
! 
! The Neumann (flux) boundary condition is
! \begin{equation}
!   \label{KE_e_wave_Neumann}
!   F_\epsilon = - \dfrac{\nu_t}{\sigma_\epsilon} \partder{\epsilon}{z} = 
!   - \dfrac{c_\mu (c_\mu^0)^3}{\sigma_\epsilon} K^2 \left( \dfrac{3}{2} 
!     \alpha -1  \right) (z+z_0)^{2\alpha-1}
!     \comma
! \end{equation}
! which follows from \eq{power_law} and \eq{nu}. The parameter $K$ is 
! computed as described in the context of \eq{K}.
! 
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: bc,type
   REALTYPE, intent(in)                :: zi,ki,z0,u_tau
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
     REALTYPE                :: K
     REALTYPE                :: f_k
     REALTYPE, external      :: fk_craig
!
!-----------------------------------------------------------------------
!BOC
! Compute the boundary conditions
   select case(type)
      case(viscous)
         STDERR 'Sorry, viscous boundary layers not yet implemented.'
         STDERR 'Please change gotmturb.inp accordingly'
         STDERR 'Program aborts now in turbulence.F90'
         stop
      case(logarithmic)
         if (bc.eq.Dirichlet) then
            epsilon_bc = cde*ki**1.5/(kappa*(zi+z0))
         else
            epsilon_bc = cm0**4.*ki**2./(sig_e*(zi+z0))
         endif
      case(injection)
         ! compute the capital K from the analytical solution 
         ! for shear-free flows
         ! compute the flux of k from the wave-breaking model
         f_k  = fk_craig(u_tau)

         K    = (-sig_k*f_k/(cmsf*gen_alpha*gen_l) )**(2./3.) / z0**gen_alpha

         if (bc.eq.Dirichlet) then
            epsilon_bc = cde*K**1.5/gen_l*(zi+z0)**(1.5*gen_alpha-1.)
         else
            epsilon_bc = -cmsf*cde/sig_e0*K**2. &
                         *(1.5*gen_alpha-1.)*(zi+z0)**(2.*gen_alpha-1.)
         endif
      case default
   end select
    end function epsilon_bc
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Boundary conditons for the $\psi$--equation  \label{sec:psiBC}
!
! !INTERFACE:
   REALTYPE function  psi_bc(bc,type,zi,ki,z0,u_tau)
!
!DESCRIPTION:
! Computes prescribed and flux boundary conditions for  the transport
! equation \eq{generic}. The formal parameter {\tt bc} determines 
! whether {\tt Dirchlet} or {\tt Neumann}--type boundary conditions
! are computed. Depending on the physical properties of the 
! boundary--layer, the parameter {\tt type} relates either to a {\tt visous},
! a {\tt logarithmic}, or an {\tt injection}--type boundary--layer.
! In the latter case, the flux of TKE caused by breaking surface waves
! has to be specified. Presently, there is only one possibility 
! to do so implemented in GOTM. It is described in \sect{sec:fkCraig}.
! All parameters that determine the boundary layer have to be 
! set in {\tt gotmturb.inp}.
!
! Note that  in this section, for brevity, $z$ denotes the distance 
! from the wall (or the surface), and \emph{not} the standard 
! coordinate of the same name used in GOTM.
!
! \vspace{6mm}
! {\bf Viscous boundary-layers}
! \vspace{4mm}
!
! This type is not implemented yet in GOTM.
!
! \vspace{6mm}
! {\bf Logarithmic boundary-layers}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows from \eq{psi_l} as
! \begin{equation}
!   \label{GE_psi_Dirichlet}
!   \psi = (c_\mu^0)^p \kappa^n k^m \left( z+z_0 \right)^n
!   \comma
! \end{equation}
! where we used the law--of--the--wall relation $l=\kappa(z+z_0)$.
! 
! Neumann (flux) boundary condition can be written as
! \begin{equation}
!   \label{GE_psi_Neumann}
!   F_\psi = - \dfrac{\nu_t}{\sigma_\psi} \partder{\psi}{z} = 
!   - \dfrac{ n (c_\mu^0)^{p+1} \kappa^{n+1}}{\sigma_\psi} k^{m+\frac{1}{2}} (z+z_0)^n
! \end{equation}
! by inserting $l=\kappa(z+z_0)$ into the expression for the 
! diffusivity in \eq{nu}. Note, that in \eq{GE_psi_Dirichlet} and
! \eq{GE_psi_Neumann}, we use {\tt ki}, the value of $k$ at the current time
! step, to compute the boundary conditions. By means of 
! \eq{kLog}, it would have been also possible to express the boundary
! conditions in terms of the friction velocity, $u_*$. This, however,
! causes numerical difficulties in case of a stress--free surface
! boundary--layer as for example in the pressure--driven open channel
! flow. 
! 
! \vspace{6mm}
! {\bf Shear-free boundary-layers with injection of TKE}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows simply
! from the power-law \eq{power_law} inserted in \eq{psi_l}. This
! yields
! \begin{equation}
!   \label{GE_psi_wave_Dirichlet}
!   \psi= (c_\mu^0)^p K^m  L^n (z+z_0)^{m \alpha + n}
!   \point
! \end{equation}
! 
! The Neumann (flux) boundary condition is
! \begin{equation}
!   \label{GE_psi_wave_Neumann}
!   F_\psi = - \dfrac{\nu_t}{\sigma_\psi} \partder{\psi}{z} = 
!   - \dfrac{c_\mu (c_\mu^0)^p}{\sigma_\psi} \left(m\alpha + n  \right)
!      K^{m+\frac{1}{2}} L^{n+1} 
!  (z+z_0)^{(m+\frac{1}{2})\alpha+n}
!  \comma
! \end{equation}
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: bc,type
   REALTYPE, intent(in)                :: zi,ki,z0,u_tau
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
     REALTYPE                            :: K
     REALTYPE                            :: f_k
     REALTYPE, EXTERNAL                  :: fk_craig
!             
!-----------------------------------------------------------------------
!BOC
! Compute the boundary conditions
   select case(type)
      case(viscous)
         STDERR 'Sorry, viscous boundary layers not yet implemented.'
         STDERR 'Please change gotmturb.inp accordingly'
         STDERR 'Program aborts now in turbulence.F90'
         stop
      case(logarithmic)
         if (bc.eq.Dirichlet) then
            psi_bc = cm0**gen_p*kappa**gen_n*ki**gen_m*(zi+z0)**gen_n
         else
            psi_bc = - gen_n*cm0**(gen_p+1.)*kappa**(gen_n+1.)/sig_psi      &
                       *ki**(gen_m+0.5)*(zi+z0)**gen_n
         endif
      case(injection)
         ! compute the capital K from the analytical solution 
         ! of shear-free flows and from f_k given by the wave-breaking model
         
         ! compute the flux of k from the wave-breaking model
         f_k  = fk_craig(u_tau)

         K    = (-sig_k*f_k/(cmsf*gen_alpha*gen_l) )**(2./3.) / z0**gen_alpha
         
         if (bc.eq.Dirichlet) then
            psi_bc = cm0**gen_p*K**gen_m*gen_l**gen_n &
                     *(zi+z0)**(gen_m*gen_alpha+gen_n)
         else
            psi_bc = - (gen_m*gen_alpha+gen_n)*cmsf*cm0**gen_p/sig_psi &
                         *K**(gen_m+0.5)*gen_l**(gen_n+1.) &
                         *(zi+z0)**((gen_m+0.5)*gen_alpha+gen_n)                        
         endif
      case default
   end select
   end function psi_bc
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Boundary conditons for the $q^2 l$--equation  \label{sec:q2lBC}
!
! !INTERFACE:
   REALTYPE function  q2l_bc(bc,type,zi,ki,z0,u_tau)
!
!
! !DESCRIPTION:
! Computes prescribed and flux boundary conditions for  the transport
! equation \eq{MY}. The formal parameter {\tt bc} determines 
! whether {\tt Dirchlet} or {\tt Neumann}--type boundary conditions
! are computed. Depending on the physical properties of the 
! boundary--layer, the parameter {\tt type} relates either to a {\tt visous},
! a {\tt logarithmic}, or an {\tt injection}--type boundary--layer.
! In the latter case, the flux of TKE caused by breaking surface waves
! has to be specified. Presently, there is only one possibility 
! to do so implemented in GOTM. It is described in \sect{sec:fkCraig}.
! All parameters that determine the boundary layer have to be 
! set in {\tt gotmturb.inp}.
!
! Note that  in this section, for brevity, $z$ denotes the distance 
! from the wall (or the surface), and \emph{not} the standard 
! coordinate of the same name used in GOTM.
!
! \vspace{6mm}
! {\bf Viscous boundary-layers}
! \vspace{4mm}
!
! This type is not implemented yet in GOTM.
!
! \vspace{6mm}
! {\bf Logarithmic boundary-layers}
! \vspace{4mm}
!
! The Dirchlet (prescribed) boundary conditions can be written as
! \begin{equation}
!   \label{MY_q2l_Dirichlet}
!   q^2 l = 2 \kappa k (z+z_0) 
!  \comma
! \end{equation}
! where we used the law--of--the--wall relation $l=\kappa(z+z_0)$.
! 
! Neumann (flux) boundary condition can be written as
! \begin{equation}
!   \label{MY_q2l_Neumann}
!   F_l = - S_l q l \partder{q^2 l}{z} 
!      = - 2 \sqrt{2} S_l \kappa^2 k^\frac{3}{2} (z+z_0)
! \end{equation}
! by inserting $l=\kappa(z+z_0)$ ($q$ is constant in the log--layer). 
! Note, that in \eq{MY_q2l_Dirichlet} and
! \eq{MY_q2l_Neumann}, we use {\tt ki}, the value of $k$ at the current time
! step, to compute the boundary conditions. By means of 
! \eq{kLog}, it would have been also possible to express the boundary
! conditions in terms of the friction velocity, $u_*$. This, however,
! causes numerical difficulties in case of a stress--free surface
! boundary--layer as for example in the pressure--driven open channel
! flow. 
!
! \vspace{6mm}
! {\bf Shear-free boundary-layers with injection of TKE}
! \vspace{4mm}
!
! The Dirichlet (prescribed) boundary condition follows simply
! from the power-law \eq{power_law}, yielding
! \begin{equation}
!   \label{MY_q2l_wave_Dirichlet}
!   q^2 l = 2 K  L (z+z_0)^{\alpha + 1}
!   \point
! \end{equation}
! 
! Neumann (flux) boundary condition is
! \begin{equation}
!   \label{MY_q2l_wave_Neumann}
!   F_l = - S_l q l \partder{q^2 l}{z} = 
!   - 2 \sqrt{2} S_l  (\alpha+1) K^\frac{3}{2}  L^2 (z+ z_0)^{\frac{3}{2} \alpha + 1} 
!   \comma
! \end{equation}
! which follows from \eq{power_law}. The parameter $K$ is 
! computed as described in the context of \eq{MY_K}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: bc,type
   REALTYPE, intent(in)                :: zi,ki,z0,u_tau
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
     REALTYPE                            :: K
     REALTYPE                            :: f_k
     REALTYPE, EXTERNAL                  :: fk_craig
!
!-----------------------------------------------------------------------
!BOC
! Compute the boundary conditions
   select case(type)
      case(viscous)
         STDERR 'Sorry, viscous boundary layers not yet implemented.'
         STDERR 'Please change gotmturb.inp accordingly'
         STDERR 'Program aborts now in turbulence.F90'
         stop
      case(logarithmic)
         if (bc.eq.Dirichlet) then
            q2l_bc = 2.*kappa*ki*(zi+z0)
         else
            q2l_bc = -2.*sqrt(2.)*sl*kappa**2.*ki**1.5*(zi+z0)
         endif
      case(injection)
         ! compute the capital K from the analytical solution 
         ! of shear-free flows and from f_k given by the wave-breaking model
         
         ! compute the flux of k from the wave-breaking model
         f_k  = fk_craig(u_tau) 

         K = ( -f_k/(sqrt(2.)*sq*gen_alpha*gen_l) )**(2./3.) / z0**gen_alpha
         
         if (bc.eq.Dirichlet) then
            q2l_bc = 2.*K*gen_l*(zi+z0)**(gen_alpha+1.)
         else
            q2l_bc = -2.*sqrt(2.)*sl*(gen_alpha+1.)*K**1.5 &
                        *gen_l**2.*(zi+z0)**(1.5*gen_alpha+1.)
         endif
      case default
   end select
   end function q2l_bc
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Stability Functions: 
!
! !INTERFACE:
   subroutine stabilityfunctions(nlev,NN,SS,calc_asan)
!
! !DESCRIPTION:
! Based on the user's specifications in {\tt gotmtub.inp}, this internal 
! routine selects the desired stability functions defined in \eq{nu}.
! The parameter {\tt calc\_asan} determines if the stability parameters
! $\alpha_M$ and $\alpha_N$ defined in \eq{alphaMN} are updated or not.
!
! Note that the so--called quasi--equilibrium stability functions result
! from the full functions by invoking the equilibrium condition \eq{localEQa}
! which can be expressed as
!  \begin{equation}\label{quasieq}
!     c_\mu \frac{k^2}{\epsilon^2} M^2 
!   - c'_\mu \frac{k^2}{\epsilon^2} N^2 = 1
!   \point
!  \end{equation}
!  This equation allows to express $\alpha_M$ in terms of  $\alpha_N$ 
! inside the stability functions, see \cite{BurchardBolding2001}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev,calc_asan
   REALTYPE, intent(in)                :: NN(0:nlev),SS(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard, 
!                      Manuel Ruiz Villarreal
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i 
   REALTYPE                  :: LLk
!
!-----------------------------------------------------------------------
!BOC
   if (calc_asan.eq.1) then
      do i=0,nlev
         LLk=L(i)*L(i)/tke(i)
         as(i)=LLk*SS(i)
         an(i)=LLk*NN(i)
      end do
   end if

   select case(stab_method)
      case(genASM)
         call cmue_gen(nlev)
      case(MellorYamada)
         call cmue_my(nlev)
      case(KanClay)
         call cmue_kc(nlev)
      case(BurBaum)
         call cmue_bb(nlev)
      case(CanutoA)
         call cmue_ca(nlev)
      case(CanutoB)
         call cmue_cb(nlev)
      case(GalperinQe)
         call cmue_gpqe(nlev)
      case(KanClayQe)
         call cmue_kcqe(nlev)
      case(BurBaumQe)
         call cmue_bbqe(nlev)
      case(CanutoAQe)
         call cmue_caqe(nlev)
      case(CanutoBQe)
         call cmue_cbqe(nlev)
      case(Constant)
         cmue1=cm0_fix
         cmue2=cm0_fix/Prandtl0_fix
      case(MunkAnderson)
         call cmue_ma(nlev)
      case(SchumGerz)
         call cmue_sg(nlev)
      case(FluxRich)
         call cmue_rf(nlev)
      case default
   end select

! formally set the values at the boundaries
   cmue1(0)=cmue1(1)
   cmue1(nlev)=cmue1(nlev-1)
   cmue2(0)=cmue2(1)
   cmue2(nlev)=cmue2(nlev-1)

   return
   end subroutine stabilityfunctions 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compute some special values of the stability functions \label{sec:computeCmu} 
!
! !INTERFACE:
   subroutine compute_cm0(nlev,stab_method)
!
! !DESCRIPTION:
! Computes the values of the stability functions 
! in the logarithmic boundary--layer, $c_\mu^0$, and in
! shear--free, spatially decaying turbulence, {\tt cmsf} (see 
! \sect{sec:analyse}). A numerical method is used.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   integer, intent(in)                 :: stab_method
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf, Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
     integer                 :: i
     REALTYPE                :: NN(0:2),SS(0:2)
     REALTYPE                :: fc,fp,step,e=1.e-10,cme
!
!-----------------------------------------------------------------------
!BOC
   an=0.
   NN=0.
   SS=0.
   cm0  = 0.5
   do i=1,10000
      as=cm0**2
      call stabilityfunctions(2,NN,SS,0)
      fc=cmue1(1)-cm0
      cme=cm0+e
      as=cme**2
      call stabilityfunctions(2,NN,SS,0)
      fp=cmue1(1)-(cm0+e)
      if (fp.eq.fc) then
         step=0.
      else   
         step=-fc/((fp-fc)/e)
      endif   
      cm0=cm0+0.3*step
      if (abs(step).lt.1.e-8) goto 111
   end do
111 as=0.
    call stabilityfunctions(2,NN,SS,0)
    cmsf     = cmue1(1)

   return
   end subroutine compute_cm0
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate c3 from steady--state Richardson number\label{sec:c3}
!
! !INTERFACE:
   REALTYPE function c3(c1,c2,Ri)
!
! !DESCRIPTION:
! Computes $c_{\psi 3}$ for two--equations models from the given
! steady--state Richardson--number $Ri_{st}$ and the parameters
! $c_{\psi 1}$ and $c_{\psi 2}$ according to \eq{Ri_st}.
! A Newton--iteration is used to solve the resulting
! implicit non--linear equation.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: c1,c2,Ri
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf, Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
     integer                       :: i,imax=100
     REALTYPE                      :: fc,fp,e=1.e-8,step,ann
     REALTYPE                      :: NN(0:2),SS(0:2)
!
!-----------------------------------------------------------------------
!BOC
   NN=0. 
   SS=0.
   ann=0.1
   do i=0,imax
      an(1)=ann
      as(1)=an(1)/Ri
      call stabilityfunctions(2,NN,SS,0)
      fc=cmue1(1)*as(1)-cmue2(1)*an(1)-cm0**3
      an(1)=ann+e
      as(1)=an(1)/Ri
      call stabilityfunctions(2,NN,SS,0)
      fp=cmue1(1)*as(1)-cmue2(1)*an(1)-cm0**3
      step=-fc/((fp-fc)/e)
      ann=ann+0.5*step
      if (abs(step).gt.100.) then
         STDERR 'Method for calculating c3 does not converge.'
         STDERR 'Probably, the prescribed steady-state Richardson number' 
         STDERR 'is outside the range of the chosen stability function.'
         STDERR 'Please change gotmturb.inp accordingly.'
         STDERR 'You have chosen the stability function no.',stab_method
         STDERR 'If the problem persists, then use another' 
         STDERR 'stability function.' 
         STDERR 'Program aborts now in turbulence.F90.'
         stop
      endif

      if (abs(step).lt.1.e-10) goto 111
   end do 
111   an(1)=ann
   as(1)=an(1)/Ri
   call stabilityfunctions(2,NN,SS,0)
   c3=c2+(c1-c2)/Ri*cmue1(1)/cmue2(1)

   end function c3
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate steady--state Richardson number from c3 \label{sec:Rist}
!
! !INTERFACE:
   REALTYPE function  Rist(c1,c2,c3)
!
! !DESCRIPTION:
! Computes the steady--state Richardson--number $Ri_{st}$ 
! for two-equations models from the given
! $c_{\psi 3}$ and the parameters
! $c_{\psi 1}$ and $c_{\psi 2}$ according to \eq{Ri_st}.
! A (very tricky) double Newton--iteration is used to solve the resulting
! implicit non--linear equation.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: c1,c2,c3
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf, Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
     integer                      :: i,j,imax=100
     REALTYPE                     :: cc3,fc,fp,e=1.e-9,step,ann
     REALTYPE                     :: ffc,ffp,Ri,Rii,ee=1.e-4
     REALTYPE                     :: NN(0:2),SS(0:2)
     logical                      :: converged=.true.
!
!-----------------------------------------------------------------------
!BOC
!!$   NN=0. 
!!$   SS=0.
!!$   Ri=0.18  ! Initial guess for Rist
!!$      
!!$   do j=0,imax
!!$      Rii=Ri
!!$      ann=0.1
!!$      do i=0,imax
!!$         an(1)=ann
!!$         as(1)=an(1)/Rii
!!$         call stabilityfunctions(2,NN,SS,0)
!!$         fc=cmue1(1)*as(1)-cmue2(1)*an(1)-cm0**3
!!$         an(1)=ann+e
!!$         as(1)=an(1)/Rii
!!$         call stabilityfunctions(2,NN,SS,0)
!!$         fp=cmue1(1)*as(1)-cmue2(1)*an(1)-cm0**3
!!$         step=-fc/((fp-fc)/e)
!!$         ann=ann+0.5*step
!!$
!!$         if (abs(step).gt.100.) then
!!$            STDERR '                                                 '
!!$            STDERR 'Method for calculating the steady-state Richardson'
!!$            STDERR 'number Rist does not converge.'
!!$            STDERR 'Probably, the prescribed value for c3' 
!!$            STDERR 'is outside the range of the chosen stability function.'
!!$            STDERR 'Please change gotmturb.inp accordingly.'
!!$            STDERR 'You have chosen the stability function no.',stab_method
!!$            STDERR 'If the problem persists, then use another' 
!!$            STDERR 'stability function.' 
!!$            STDERR 'I will continue anyway. Good luck!' 
!!$            STDERR '                                                 '
!!$            converged=.false.
!!$            goto 333
!!$         endif
!!$
!!$         if (abs(step).lt.1.e-10) goto 111
!!$      end do 
!!$111   an(1)=ann
!!$      as(1)=an(1)/Rii
!!$      call stabilityfunctions(2,NN,SS,0)
!!$      cc3=c2+(c1-c2)/Rii*cmue1(1)/cmue2(1)
!!$      ffc=cc3-c3
!!$
!!$      Rii=Ri+ee
!!$      ann=0.1
!!$      do i=0,imax
!!$         an(1)=ann
!!$         as(1)=an(1)/Rii
!!$         call stabilityfunctions(2,NN,SS,0)
!!$         fc=cmue1(1)*as(1)-cmue2(1)*an(1)-cm0**3
!!$         an(1)=ann+e
!!$         as(1)=an(1)/Rii
!!$         call stabilityfunctions(2,NN,SS,0)
!!$         fp=cmue1(1)*as(1)-cmue2(1)*an(1)-cm0**3
!!$         step=-fc/((fp-fc)/e)
!!$         ann=ann+0.5*step
!!$         if (abs(step).gt.100.) then
!!$            STDERR 'Method for calculating the steady-state Richardson'
!!$            STDERR 'number Rist does not converge.'
!!$            STDERR 'Probably, the prescribed value for c3' 
!!$            STDERR 'is outside the range of the chosen stability function.'
!!$            STDERR 'Please change gotmturb.inp accordingly.'
!!$            STDERR 'You have chosen the stability function no.',stab_method
!!$            STDERR 'If the problem persists, then use another' 
!!$            STDERR 'stability function.' 
!!$            STDERR 'I will continue anyway. Good luck!' 
!!$            STDERR '                                                 '
!!$            converged=.false.
!!$            goto 333
!!$         endif
!!$         if (abs(step).lt.1.e-10) goto 222
!!$      end do 
!!$222   an(1)=ann
!!$      as(1)=an(1)/Rii
!!$      call stabilityfunctions(2,NN,SS,0)
!!$      cc3=c2+(c1-c2)/Rii*cmue1(1)/cmue2(1)
!!$      ffp=cc3-c3
!!$
!!$      step=-ffc/((ffp-ffc)/ee)
!!$      Ri=Ri+0.25*step
!!$      if (abs(step).gt.100.) then
!!$         STDERR '                                                 '
!!$         STDERR 'Method for calculating the steady-state Richardson'
!!$         STDERR 'number Rist does not converge.'
!!$         STDERR 'Probably, the prescribed value for c3' 
!!$         STDERR 'is outside the range of the chosen stability function.'
!!$         STDERR 'Please change gotmturb.inp accordingly.'
!!$         STDERR 'You have chosen the stability function no.',stab_method
!!$         STDERR 'If the problem persists, then use another' 
!!$         STDERR 'stability function.' 
!!$         STDERR 'I will continue anyway. Good luck!'
!!$         STDERR '                                                 '
!!$         converged=.false.
!!$         goto 333
!!$    endif
!!$      if (abs(step).lt.1.e-10) goto 333
!!$   end do
!!$   stop
!!$
!!$333   Rist=Ri
!!$
!!$   if (.not.converged) Rist=-999.

     Rist = -999.
   end function Rist
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Internal Waves. 
!
! !INTERFACE:
   subroutine internal_wave(nlev,NN,SS) 
!
! !DESCRIPTION:
!  Imposes eddy viscosity and diffusivity characteristic 
!  of internal wave activity and shear instability when there is extinction 
!  of turbulence as suggested by \cite{KanthaClayson94}. 
!  In this case, these new {\tt num} and {\tt nuh} 
!  are used instead of those computed with the model.
!
!  When k is small (extinction of turbulence, diagnosed by 
!  $k<${\tt klimiw}), 
!  $\nu_t$ and $\nu'_t$ are set to empirical values typical 
!  in the presence of internal wave activity (IW) and shear 
!  instability (SI). This model is described by
!  \begin{equation}
!    \nu_t = \nu_t^{IW}  + \nu_t^{SI}      \comma
!    \nu'_t= \nu'^{IW}_t + \nu'^{SI}_t     \comma
!  \end{equation}
!  where
!  \begin{equation}
!    \nu_t^{IW}  =         10^{-4}          \comma             
!    \nu'^{IW}_t = 5 \cdot 10^{-5}          \point
!  \end{equation} 
!  The `SI' parts are functions of the Richardson number according to
!  \begin{eqnarray}
!  \nu_t^{SI} = \nu'^{SI}_t = 0              \comma
!     & R_i>0.7 \comma \\[4mm]
!  \nu_t^{SI} = \nu'^{SI}_t = 5 \cdot 10^{-3} \left( 1-\left(\frac {R_i} 
!  {0.7}\right)^2\right)^3                    \comma
!     & 0<R_i<0.7 \comma \\[4mm]
!  \nu_t^{SI} = \nu'^{SI}_t = 5 \cdot 10^{-3} \comma
!     & R_i < 0  
!     \point
!  \end{eqnarray}
!  The unit of all diffusivities is m$^2$s$^{-1}$.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: NN(0:nlev),SS(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard, 
!                      Manuel Ruiz Villarreal
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE              :: rich(0:nlev)
   REALTYPE              :: rich2,pot,x      
   integer               :: i
!
!-----------------------------------------------------------------------
!BOC
   select case (iw_model)
      case (0)
      case (1)
      case (2)
         rich2 = rich_cr*rich_cr
         do i=1,nlev-1 
            if (tke(i).le.klimiw) then
               rich(i)=NN(i)/(SS(i)+1.e-10)
               if (rich(i).lt.rich_cr) then
                  if (rich(i).gt.0) then
                     pot=1-rich(i)*rich(i)/rich2 
                     x=numshear*pot*pot*pot
                     num(i)=numiw+x 
                     nuh(i)=nuhiw+x  
                  else
                     num(i)=numiw+numshear
                     nuh(i)=nuhiw+numshear
                  end if          
               else
                  num(i)=numiw
                  nuh(i)=nuhiw
               end if
            end if   
         end do
      case default
   end select

   return
   end subroutine internal_wave 
!EOC

!-----------------------------------------------------------------------

 end module turbulence

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

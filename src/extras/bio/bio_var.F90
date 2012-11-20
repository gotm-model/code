#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_var --- declaration of biological variables
!
! !INTERFACE:
   module bio_var
!
! !DESCRIPTION:
!  Here all variables necessary for the biogeochemical models are
!  declared, mostly as allocatable variables.
!
! !USES:
!  default: all is public.
   public
!
! !PUBLIC DATA MEMBERS:

!  model types
   integer                                      :: bio_model
   logical                                      :: bio_eulerian

!  available models
   integer, parameter                           :: TEMPLATE=-1
   integer, parameter                           :: NPZD=0
   integer, parameter                           :: ERGOM=2
   integer, parameter                           :: SED=3
   integer, parameter                           :: FASHAM=4
   integer, parameter                           :: ERGOM_MAB=5
   integer, parameter                           :: ROLM=6
   integer, parameter                           :: CL=8
   integer, parameter                           :: MANGAN=10
   integer, parameter                           :: PHOTO=20

!  time parameters
   REALTYPE, parameter                          :: secs_pr_day =86400.
   REALTYPE, parameter                          :: secs_pr_hour=3600.

!  general model variables
   integer                                      :: numc
   REALTYPE, dimension(:)         , allocatable :: zlev
   REALTYPE, dimension(:)         , allocatable :: par
   REALTYPE, dimension(:,:)       , allocatable :: cc,ws

!  surface fluxes
   integer                                      :: surface_flux_method
   integer                                      :: n_surface_fluxes
   REALTYPE, dimension(:)         , allocatable :: sfl_read
   REALTYPE, dimension(:)         , allocatable :: sfl,bfl
   integer , dimension(:)         , allocatable :: posconc
   logical , dimension(:)         , allocatable :: mussels_inhale

!  decription of variables in cc array
   integer          , dimension(:), allocatable :: var_ids
   character(len=64), dimension(:), allocatable :: var_names
   character(len=64), dimension(:), allocatable :: var_units
   character(len=64), dimension(:), allocatable :: var_long


!  external variables - i.e. provided by the calling program but
!  made available via this module to the different biological models
!  the variables are copied via set_env_spm() in bio.F90
   integer                                      :: nmax
   integer                                      :: nlev
   REALTYPE                                     :: dt
   REALTYPE                                     :: zbot
   REALTYPE                                     :: u_taub
   REALTYPE                                     :: ztop
   REALTYPE, dimension(:)         , allocatable :: h
   REALTYPE, dimension(:)         , allocatable :: t
   REALTYPE, dimension(:)         , allocatable :: s
   REALTYPE, dimension(:)         , allocatable :: rho
   REALTYPE, dimension(:)         , allocatable :: nuh
   REALTYPE, dimension(:)         , allocatable :: w
   REALTYPE, dimension(:)         , allocatable :: rad
   REALTYPE                                     :: wind
   REALTYPE                                     :: I_0
   REALTYPE                                     :: secondsofday
   integer                                      :: w_adv_ctr


!  external variables updated by the biological models
!  the variables are copied back to the calling program using
!  get_bio_updates()
   REALTYPE, dimension(:)         , allocatable :: bioshade_
   REALTYPE, dimension(:)         , allocatable :: abioshade_


!  Lagrangian particles
!  (also passed over to and from external routines)
   integer                                      :: npar
   integer                                      :: ntype
   integer                                      :: nprop
   logical                                      :: par_allocation
   logical , dimension(:,:)       , allocatable :: par_act
   integer , dimension(:,:)       , allocatable :: par_ind
   REALTYPE, dimension(:,:)       , allocatable :: par_z
   REALTYPE, dimension(:,:,:)     , allocatable :: par_prop

   logical                                      :: init_saved_vars
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Lars Umlauf, Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------

   end module bio_var

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include"cppdefs.h"

!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_0d --- Interface between 1D GOTM and 0D biogeochemical models
!
! !INTERFACE:
   module bio_0d
!
! !DESCRIPTION:
! TODO
!
! !USES:
!  default: all is private.
   use bio_var
   use bio_0d_base
   use bio_npzd_0d
   use observations, only: A,g2
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio_0d, init_var_0d,                 &
          light_0d, surface_fluxes_0d, do_bio_0d, end_bio_0d
!
! !PRIVATE DATA MEMBERS:
   type (type_model_info) :: modelinfo
   type (type_variable_info), allocatable :: varinfo(:)
   integer, parameter :: npzd_0d_id = 1001

!
! !REVISION HISTORY:!
!  Original author(s): Jorn Bruggeman
!
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
   
   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the 0d biological framework
!
! !INTERFACE:
   subroutine init_bio_0d(namlst,unit)
!
! !DESCRIPTION:
!  TODO
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,          intent(in)   :: namlst
   integer,          intent(in)   :: unit
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
   integer :: i

!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'init_bio_0d'
   
   ! Initialize model information with defaults
   modelinfo%numc = 0
   modelinfo%par_fraction = _ONE_-A
   modelinfo%par_background_extinction = _ONE_/g2
   modelinfo%par_bio_background_extinction = _ZERO_
   
   select case (bio_model)
      case (npzd_0d_id)  ! NPZD
         call init_bio_npzd_0d(namlst,'bio_npzd.nml',unit,modelinfo)
      case default
         stop "bio_0d: no valid biomodel specified in bio.nml !"
   end select

   if (modelinfo%numc.eq.0) stop "bio_0d: bio model does not specify any state variables!"
   
   numc = modelinfo%numc
   allocate(varinfo(numc))
   
   ! Initialize variable information with defaults
   varinfo%name = ''
   varinfo%unit = ''
   varinfo%longname = ''
   varinfo%initial_value = _ZERO_
   varinfo%light_extinction = _ZERO_
   varinfo%sinking_rate = _ZERO_
   varinfo%positive_definite = .false.
#if 0
   varinfo%mussels_inhale = .false.
#endif

   select case (bio_model)
      case (npzd_0d_id)  ! NPZD
         call get_var_info_npzd_0d(numc,varinfo)
   end select
   
   call bio_alloc_info

   do i=1,numc
      var_names(i) = varinfo(i)%name
      var_units(i) = varinfo(i)%unit
      var_long (i) = varinfo(i)%longname
   end do

   end subroutine init_bio_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the biological variables
!
! !INTERFACE:
   subroutine init_var_0d
!
! !DESCRIPTION:
!  TODO
!
! !USES:
   IMPLICIT NONE

! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman

! !LOCAL VARIABLES:
   integer :: i,j

!EOP
!-----------------------------------------------------------------------
!BOC

   do j=1,numc
      ws(j,0:nlev) = -varinfo(j)%sinking_rate
      cc(j,1:nlev) = varinfo(j)%initial_value
      posconc(j) = varinfo(j)%positive_definite
#if 0
      mussels_inhale(j) = varinfo(j)%mussels_inhale
#endif
   end do

   end subroutine init_var_0d
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Surface fluxes
!
! !INTERFACE:
   subroutine surface_fluxes_0d(nlev,t,s)
!
! !DESCRIPTION:
! TODO
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
  integer                              :: nlev
  REALTYPE, intent(in)                 :: t,s
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC

   select case (bio_model)
   end select

   end subroutine surface_fluxes_0d
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Light properties for the 0d biological framework
!
! !INTERFACE:
   subroutine light_0d(nlev,bioshade_feedback)
!
! !DESCRIPTION:
! TODO
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   logical, intent(in)                 :: bioshade_feedback
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
   integer :: i,j
   REALTYPE :: zz,bioext

!EOP
!-----------------------------------------------------------------------
!BOC
   zz = _ZERO_
   bioext = _ZERO_
   do i=nlev,1,-1
      ! Add the extinction of the first half of the grid box.
      bioext = bioext+modelinfo%par_bio_background_extinction*0.5*h(i)
      do j=1,numc
         bioext = bioext+varinfo(j)%light_extinction*cc(j,i)*0.5*h(i)
      end do

      zz=zz+0.5*h(i)
      par(i)=rad(nlev)*modelinfo%par_fraction*exp(-zz*modelinfo%par_background_extinction-bioext)

      ! Add the extinction of the second half of the grid box.
      bioext = bioext+modelinfo%par_bio_background_extinction*0.5*h(i)
      do j=1,numc
         bioext = bioext+varinfo(j)%light_extinction*cc(j,i)*0.5*h(i)
      end do
      
      zz=zz+0.5*h(i)
      if (bioshade_feedback) bioshade_(i)=exp(-bioext)
   end do

   end subroutine light_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Right hand sides of NPZD model
!
! !INTERFACE:
   subroutine do_bio_0d(first,numc,nlev,cc,pp,dd)
!
! !DESCRIPTION:
! TODO
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   logical, intent(in)                  :: first
   integer, intent(in)                  :: numc,nlev
   REALTYPE, intent(in)                 :: cc(1:numc,0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)                :: pp(1:numc,1:numc,0:nlev)
   REALTYPE, intent(out)                :: dd(1:numc,1:numc,0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
   integer                    :: ci
   type (type_environment)    :: env
!EOP
!-----------------------------------------------------------------------
!BOC

!KBK - is it necessary to initialise every time - expensive in a 3D model
   pp = _ZERO_
   dd = _ZERO_

   env%I_0 = I_0
   env%wind = wind
   do ci=1,nlev
      env%par  = par(ci)
      env%t    = t(ci)
      env%s    = s(ci)
      env%nuh  = nuh(ci)
      env%rho  = rho(ci)
      select case (bio_model)
         case (npzd_0d_id)
            call do_bio_npzd_0d(first,numc,cc(1:numc,ci),env,pp(1:numc,1:numc,ci),dd(1:numc,1:numc,ci))
      end select
   end do
   
   end subroutine do_bio_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the bio calculations
!
! !INTERFACE:
   subroutine end_bio_0d
!
! !DESCRIPTION:
!  Nothing done yet --- supplied for completeness.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!BOC

   select case (bio_model)
   end select

   end subroutine end_bio_0d
!EOC

!-----------------------------------------------------------------------

   end module bio_0d

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

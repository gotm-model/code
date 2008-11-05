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
          light_0d, surface_fluxes_0d, do_bio_0d_eul, &
          light_0d_par, do_bio_0d_par, end_bio_0d
!
! !PRIVATE DATA MEMBERS:
   type (type_model_info) :: modelinfo
   type (type_variable_info), allocatable :: varinfo(:)
   integer, parameter :: npzd_0d_id = 1001
   
   ! Lagrangian model
   integer :: np,nt
   REALTYPE, allocatable :: shade(:),extinction(:),cc_loc(:,:)
   type (type_environment) :: env_par

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
   integer :: i,ioffset

!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'init_bio_0d'
   
   ! Initialize model information with defaults
   call init_model_info(modelinfo)
   modelinfo%par_fraction = _ONE_-A
   modelinfo%par_background_extinction = _ONE_/g2
   
   ! Get actual model info based on the model selected.
   select case (bio_model)
      case (npzd_0d_id)  ! NPZD
         call init_bio_npzd_0d(namlst,'bio_npzd.nml',unit,modelinfo)
      case default
         stop "bio_0d: no valid biomodel specified in bio.nml !"
   end select
   
   ! Initialize variable information with defaults
   allocate(varinfo(modelinfo%numc))
   do i=1,modelinfo%numc
      call init_variable_info(varinfo(i))
   end do

   ! Get actual variable info from the selected biogeochemical model
   select case (bio_model)
      case (npzd_0d_id)  ! NPZD
         call get_var_info_npzd_0d(modelinfo%numc,varinfo)
   end select
   
   ! Allocate global arrays for info on biogeochemical model
   ! Add a variable for particle densities if using Lagragian model
   numc = modelinfo%numc
   if (.not. bio_eulerian) numc = numc+1
   call bio_alloc_info

   ! If using Lagrangian model, the first variable will describe the number
   ! particles per unit of volume.
   if (.not. bio_eulerian) then
      var_names(1) = 'Np'
      var_units(1) = 'counts/volume'
      var_long (1) = 'number of particles per volume'
      ioffset = 1
      
      ntype = 1
      nprop = modelinfo%numc
   else
      ioffset = 0
   end if

   ! Register the variables used by the biogeochemical model in the global arrays.
   do i=1,modelinfo%numc
      var_names(i+ioffset) = varinfo(i)%name
      var_units(i+ioffset) = varinfo(i)%unit
      var_long (i+ioffset) = varinfo(i)%longname
   end do

   end subroutine init_bio_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Fill initial particle distribution
!
! !INTERFACE:
   subroutine init_par_bio_0d()
!
! !DESCRIPTION:
! Particles are distributed homogeneously over the whole water column. 
! Indices of vertical grid cells are assigend to all particles, and the
! particles are marked active.  
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf, Hans Burchard, Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!
! !LOCAL VARIABLES:
   integer                   :: i,j,n

!-----------------------------------------------------------------------
!BOC


!  create homogeneous particle distribution
   do j=1,ntype
      do n=1,npar
         par_z(n,j) = zbot + n/float(npar+1)*(ztop-zbot)
      end do
   end do

!  assign cell indices to particles
   do j=1,ntype
      do n=1,npar
         do i=1,nlev
            if (zlev(i) .gt. par_z(n,j)) exit
         end do
         par_ind(n,j) = i
         par_act(n,j)  = .true.
      end do
   end do


   return
   end subroutine init_par_bio_0d
!EOC
!-----------------------------------------------------------------------

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
   integer :: i,j,ioffset

!EOP
!-----------------------------------------------------------------------
!BOC

   ioffset = 0
   if (.not. bio_eulerian) ioffset = 1
   
   do j=1,modelinfo%numc
      ws(j+ioffset,0:nlev) = -varinfo(j)%sinking_rate
      posconc(j+ioffset) = varinfo(j)%positive_definite
#if 0
      mussels_inhale(j+ioffset) = varinfo(j)%mussels_inhale
#endif
   end do

   if (.not. bio_eulerian) then
      nt = 1

      do j=1,modelinfo%numc
         par_prop(:,j,nt) = (ztop-zbot)*varinfo(j)%initial_value/npar
      end do
      
      ! Configure particle variable
      cc = _ZERO_
      ws(1,0:nlev) = _ZERO_
      posconc(1) = 1
#if 0
      mussels_inhale(1) = .false.
#endif

      call init_par_bio_0d

      allocate(cc_loc(1:modelinfo%numc,0:1))
   else
      do j=1,numc
         cc(j,1:nlev) = varinfo(j)%initial_value
      end do
   end if

   ! Arrays for particle model
   allocate(shade(0:nlev))
   allocate(extinction(1:nlev))

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
! !IROUTINE: Light properties for particle version of the 0d biological framework
!
! !INTERFACE:
   subroutine light_0d_par(nlev,bioshade_feedback)
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
   integer :: ind,i,j
   REALTYPE :: bioext

!EOP
!-----------------------------------------------------------------------
!BOC
   nt = 1

   ! Get bio-related extinction in each grid box
   extinction = _ZERO_
   do np=1,npar
      if (par_act(np,nt)) then

         ! Get depth index of the current particle
         ind = par_ind(np,nt)

         ! Get the light extinction coefficient combined over all state variables.
         do j=1,modelinfo%numc
            extinction(ind) = extinction(ind) + varinfo(j)%light_extinction*par_prop(np,j,nt)
         end do
      end if
   end do
   extinction(1:nlev) = extinction(1:nlev)/h(1:nlev) + modelinfo%par_bio_background_extinction

   ! Calculate shade factor at each depth
   bioext = _ZERO_
   shade(nlev) = _ZERO_
   do i=nlev,1,-1
      bioext = bioext+extinction(i)*h(i)
      shade(i-1) = exp(-bioext) ! Note: this is the shade factor at the top of the grid box
   end do
   
   ! Set feedback of bioturbidity to physics if needed
   if (bioshade_feedback) bioshade_(1:nlev)=shade(0:nlev-1)
   end subroutine light_0d_par
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Right hand sides of eulerian 0D biogeochemical model
!
! !INTERFACE:
   subroutine do_bio_0d_eul(first,numc,nlev,cc,pp,dd)
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
   
   ! Set up abiotic environment
   call init_environment(env)

   ! Set non-local properties of the environment
   env%I_0 = I_0
   env%wind = wind

   do ci=1,nlev
      env%z    = zlev(ci-1)+0.5*h(ci)
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
   
   end subroutine do_bio_0d_eul
!EOC

!BOP
!
! !IROUTINE: Update particle model described by 0D biogeochemical model.
!
! !INTERFACE:
   subroutine do_bio_0d_par(ode_method,dt)
!
! !DESCRIPTION:
! TODO
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,intent(in) :: ode_method
   REALTYPE,intent(in) :: dt
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
! !LOCAL VARIABLES:
   integer                    :: i,j,ind
   REALTYPE                   :: bioext
!EOP
!-----------------------------------------------------------------------
!BOC

   nt = 1
   
   ! First column of state variable array is not used by ode solvers.
   cc_loc(1:modelinfo%numc,0) = _ZERO_
   
   ! Set up abiotic environment
   call init_environment(env_par)
   
   ! Set non-local properties of the environment
   env_par%I_0  = I_0
   env_par%wind = wind

   ! For each particle: integrate over the time step
   do np=1,npar
      ! Get the current state
      cc_loc(1:modelinfo%numc,1) = par_prop(np,1:modelinfo%numc,nt)
      
      ! Call ode solver to get updated state
      call ode_solver(ode_method,modelinfo%numc,1,dt,cc_loc,get_bio_0d_par_rhs)
      
      ! Store updated state as particle properties
      par_prop(np,1:modelinfo%numc,nt) = cc_loc(1:modelinfo%numc,1)
   end do
   
   ! Initialize array with [Eulerian] summary statistics
   cc = _ZERO_

   do np=1,npar
      if (par_act(np,nt)) then

         ! Get depth index of the current particle
         ind = par_ind(np,nt) 

         ! Count particles per grid volume
         cc(1,ind) = cc(1,ind) + _ONE_
         do j=1,modelinfo%numc
            cc(j+1,ind) = cc(j+1,ind) + par_prop(np,j,nt)
         end do

      end if

   end do

   ! Compute volume averages
   do i=1,nlev
      cc(:,i) = cc(:,i)/h(i)
   end do
   
   end subroutine do_bio_0d_par
!EOC

!BOP
!
! !IROUTINE: Get the right-hand side of the ODE system for the current particle
!
! !INTERFACE:
   subroutine get_bio_0d_par_rhs(first,numc,nlev,cc,pp,dd)
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
   REALTYPE                   :: rat, localshade
   integer                    :: i
!EOP
!-----------------------------------------------------------------------
!BOC

   i=par_ind(np,nt)
   rat=(par_z(np,nt)-zlev(i-1))/h(i)

   localshade = rat*shade(i)+(1.-rat)*shade(i-1)
   env_par%par  = rad(nlev)*modelinfo%par_fraction*exp(par_z(np,nt)*modelinfo%par_background_extinction)*localshade

   ! Linearly interpolate environmental conditions
   env_par%z    = par_z(np,nt)
   env_par%t    = rat*  t(i)+(1.-rat)*  t(i-1)
   env_par%s    = rat*  s(i)+(1.-rat)*  s(i-1)
   env_par%nuh  = rat*nuh(i)+(1.-rat)*nuh(i-1)
   env_par%rho  = rat*rho(i)+(1.-rat)*rho(i-1)
   
   select case (bio_model)
      case (npzd_0d_id)
         call do_bio_npzd_0d(first,numc,par_prop(np,1:modelinfo%numc,nt),env_par,pp(:,:,1),dd(:,:,1))
   end select
   
   end subroutine get_bio_0d_par_rhs
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

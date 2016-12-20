#include"cppdefs.h"

#ifdef _FABM_
#  define _FABM_PARTICLES_
#endif

module particle_class

   use field_manager, only: type_field, type_field_manager, id_dim_z

#ifdef _FABM_PARTICLES_
   use fabm_particle_driver
#endif

   implicit none

   private

   public type_particle_class
   public rk

   integer, parameter :: rk = kind(_ZERO_)

   type type_interpolated_variable
      real(rk), pointer                          :: source(:) => null()
      real(rk), allocatable                      :: target(:)
      type (type_interpolated_variable), pointer :: next      => null()
   end type

   type type_particle_class
      integer               :: npar
      integer,  allocatable :: k(:)
      real(rk), allocatable :: z(:)

#ifdef _FABM_PARTICLES_
      integer                         :: nstate
      type (type_fabm_particle_state) :: state
#else
      logical,  allocatable :: active(:)
#endif

      real(rk), allocatable :: state_eul(:,:)
      integer :: count_index = -1
      type (type_interpolated_variable), pointer :: first_interpolated_variable => null()
   contains
      procedure :: initialize
      procedure :: link_eulerian_data
      procedure :: start
      procedure :: advance
      procedure :: interpolate_state_to_grid
      procedure :: finalize
   end type

   contains

   subroutine initialize(self, npar, nlev, field_manager)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: npar
      integer,                     intent(in)    :: nlev
      class (type_field_manager),  intent(inout) :: field_manager

      integer :: ivar
      integer :: nstate_eul

      self%npar = npar
      allocate(self%k(self%npar))
      allocate(self%z(self%npar))

#ifdef _FABM_PARTICLES_
      call self%state%initialize(npar, 'fabm_particles.yaml')
      self%nstate = self%state%nstate
      self%count_index = 1
      nstate_eul = self%nstate
#else
      allocate(self%active(self%npar))
      self%active = .true.
      nstate_eul = 0
#endif

      if (self%count_index == -1) nstate_eul = nstate_eul + 1
      allocate(self%state_eul(nlev, nstate_eul))
      if (self%count_index == -1) call field_manager%register('particle_concentration', '# m-3', 'particle concentration', dimensions=(/id_dim_z/), data1d=self%state_eul(:,1))

#ifdef _FABM_PARTICLES_
      do ivar=1,self%nstate
         call field_manager%register('particle_'//trim(self%state%model%state_variables(ivar)%name), &
            trim(self%state%model%state_variables(ivar)%units), &
            'particle '//trim(self%state%model%state_variables(ivar)%long_name), dimensions=(/id_dim_z/), data1d=self%state_eul(:,ivar))
      end do
#endif
   end subroutine initialize

   subroutine link_eulerian_data(self, name, dat)
      class (type_particle_class), intent(inout) :: self
      character(len=*),            intent(in)    :: name
      real(rk), target,            intent(in)    :: dat(:)

#ifdef _FABM_PARTICLES_
      type (type_interpolated_variable), pointer :: interpolated_variable

      if (.not.self%state%is_variable_used(name)) return

      allocate(interpolated_variable)
      interpolated_variable%source => dat
      allocate(interpolated_variable%target(self%npar))
      call self%state%send_data(name, interpolated_variable%target)

      ! Prepend to list
      interpolated_variable%next => self%first_interpolated_variable
      self%first_interpolated_variable => interpolated_variable
#endif
   end subroutine link_eulerian_data

   subroutine start(self, nlev, z_if)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: nlev
      real(rk),                    intent(in)    :: z_if(0:nlev)

      integer :: ipar
      integer :: k

      ! Random initialization of vertical position.
      call random_number(self%z)
      self%z = z_if(0) + self%z*(z_if(nlev)-z_if(0))

      ! Find depth index for each particle
      do ipar=1,self%npar
         do k=1,nlev-1
            if (self%z(ipar) < z_if(k)) exit
         end do
         self%k(ipar) = k
      end do

#ifdef _FABM_PARTICLES_
      call self%state%start()
#endif
   end subroutine start

   subroutine advance(self, nlev, dt, z_if, nuh)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: nlev
      real(rk),                    intent(in)    :: dt
      real(rk),                    intent(in)    :: z_if(0:nlev)
      real(rk),                    intent(in)    :: nuh(0:nlev)

      type (type_interpolated_variable), pointer :: interpolated_variable
      integer                                    :: ipar
      real(rk)                                   :: w(self%npar)
#ifdef _FABM_PARTICLES_
      real(rk)                                   :: dy(self%npar,self%nstate)
#endif

      ! Interpolate Eulerian fields to particle positions (center values from associated layers).
      interpolated_variable => self%first_interpolated_variable
      do while (associated(interpolated_variable))
         do ipar=1,self%npar
            interpolated_variable%target(ipar) = interpolated_variable%source(self%k(ipar))
         end do
         interpolated_variable => interpolated_variable%next
      end do

#ifdef _FABM_PARTICLES_
#  define _ACTIVE_ self%state%active
      call self%state%get_vertical_movement(w)
      call self%state%get_sources(dy)

      ! Time-integrate source terms (Forward Euler)
      self%state%y = self%state%y + dy*dt
#else
#  define _ACTIVE_ self%active
      w = 0.0_rk
#endif

      ! Transport particles
      call lagrange(nlev, dt, z_if, nuh, w, self%npar, _ACTIVE_, self%k, self%z)
   end subroutine advance

   subroutine interpolate_state_to_grid(self, nlev, h)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: nlev
      real(rk),                    intent(in)    :: h(1:nlev)

      integer :: istate

      real(rk) :: particle_count(self%npar)

#ifdef _FABM_PARTICLES_
      ! Create Eulerian particle fields.
      if (self%count_index == -1) then
         particle_count(:) = 1.0_rk
      else
         ! Obtain particle counts from FABM driver
         particle_count(:) = self%state%y(:,self%count_index)
      end if
#else
      particle_count(:) = 1.0_rk
#endif

      ! Interpolate particle counts to Eulerian grid
      call to_eul(particle_count, self%state_eul(:,1))

      ! Interpolate auxiliary particles states and normalize by number of particles
      do istate=2,size(self%state_eul,2)
         !call to_eul(state, particle_count*self%state_eul(:,istate))
         self%state_eul(:,istate) = self%state_eul(:,istate)/self%state_eul(:,1)
      end do

      ! Divide particle counts (# m-2) by layer heights to get concentration (# m-3)
      self%state_eul(:,1) = self%state_eul(:,1)/h(:)

   contains

      subroutine to_eul(par_in, eul_out)
         real(rk), intent(in)  :: par_in(self%npar)
         real(rk), intent(out) :: eul_out(nlev)

         integer :: ipar

         eul_out = 0.0_rk
         do ipar=1,self%npar
            eul_out(self%k(ipar)) = eul_out(self%k(ipar)) + par_in(ipar)
         end do
      end subroutine

   end subroutine interpolate_state_to_grid

   subroutine finalize(self)
      class (type_particle_class), intent(inout) :: self
      type (type_interpolated_variable), pointer :: interpolated_variable, next_interpolated_variable

      interpolated_variable => self%first_interpolated_variable
      do while (associated(interpolated_variable))
         next_interpolated_variable => interpolated_variable%next
         deallocate(interpolated_variable)
         interpolated_variable => next_interpolated_variable
      end do
      self%first_interpolated_variable => null()
   end subroutine finalize

end module
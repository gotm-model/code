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
      character(len=256)                         :: name      = ''
      real(rk), pointer                          :: source(:) => null()
      integer                                    :: itarget   = -1
      type (type_interpolated_variable), pointer :: next      => null()
   end type

   type type_interpolated_variable_set
      type (type_interpolated_variable), pointer :: first => null()
   contains
      procedure :: add      => interpolated_variable_set_add
      procedure :: finalize => interpolated_variable_set_finalize
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

      real(rk), allocatable :: interpolated_eul(:,:)
      real(rk), allocatable :: interpolated_par(:,:)

      integer :: count_index = -1
      type (type_interpolated_variable_set) :: eulerian_variables
      type (type_interpolated_variable_set) :: particle_variables

      type (type_particle_class), pointer :: next => null()
   contains
      procedure :: initialize
      procedure :: link_eulerian_data
      procedure :: interpolate_to_grid
      procedure :: start
      procedure :: advance
      procedure :: finalize
   end type

   contains

   subroutine initialize(self, npar, nlev, field_manager)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: npar
      integer,                     intent(in)    :: nlev
      class (type_field_manager),  intent(inout) :: field_manager

      integer                    :: n
      type (type_output),pointer :: output

      self%npar = npar
      allocate(self%k(self%npar))
      allocate(self%z(self%npar))

      n = 0
#ifdef _FABM_PARTICLES_
      call self%state%configure(npar, 'fabm_particles.yaml')
      self%nstate = self%state%nstate
      self%count_index = 1
      output => self%state%first_output
      do while (associated(output))
         call field_manager%register('particle_'//trim(output%name), trim(output%units), &
            'particle '//trim(output%long_name), dimensions=(/id_dim_z/), used=output%save)
         if (output%save) n = n + 1
         output => output%next
      end do
#else
      allocate(self%active(self%npar))
      self%active = .true.
#endif

      if (self%count_index == -1) n = n + 1
      allocate(self%interpolated_par(nlev, n))

      n = 1
#ifdef _FABM_PARTICLES_
      ! Complete initialization (must happen after setting the "save" attribute on outputs)
      ! After this is done, additional data fields may be sent to the FABM particle manager
      ! by calling link_eulerian_data.
      call self%state%initialize()

      output => self%state%first_output
      do while (associated(output))
         if (output%save) then
            call self%particle_variables%add(trim(output%name), output%data, n)
            call field_manager%send_data('particle_'//trim(output%name), self%interpolated_par(:,n))
            n = n + 1
         end if
         output => output%next
      end do
#endif
      if (self%count_index == -1) call field_manager%register('particle_concentration', '# m-3', 'particle concentration', dimensions=(/id_dim_z/), data1d=self%interpolated_par(:,n))
   end subroutine initialize

   subroutine link_eulerian_data(self, name, dat)
      class (type_particle_class), intent(inout) :: self
      character(len=*),            intent(in)    :: name
      real(rk), target,            intent(in)    :: dat(:)

#ifdef _FABM_PARTICLES_
      if (self%state%is_variable_used(name)) call self%eulerian_variables%add(name, dat)
#endif
   end subroutine link_eulerian_data

   subroutine start(self, nlev, z_if)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: nlev
      real(rk),                    intent(in)    :: z_if(0:nlev)

      integer :: ipar
      integer :: k
      integer :: n
      type (type_interpolated_variable), pointer :: eulerian_variable

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

      ! Determine how many Eulerian variables need to be mapped to the particles,
      ! and then allocate the memory for these interploted fields.
      n = 0
      eulerian_variable => self%eulerian_variables%first
      do while (associated(eulerian_variable))
         n = n + 1
         eulerian_variable%itarget = n
         eulerian_variable => eulerian_variable%next
      end do
      allocate(self%interpolated_eul(self%npar,n))
      eulerian_variable => self%eulerian_variables%first
      do while (associated(eulerian_variable))
         call self%state%send_data(trim(eulerian_variable%name), self%interpolated_eul(:,eulerian_variable%itarget))
         eulerian_variable => eulerian_variable%next
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

      type (type_interpolated_variable), pointer :: eulerian_variable
      integer                                    :: ipar
      real(rk)                                   :: w(self%npar)
#ifdef _FABM_PARTICLES_
      real(rk)                                   :: dy(self%npar,self%nstate)
#endif

      ! Interpolate Eulerian fields to particle positions (center values from associated layers).
      eulerian_variable => self%eulerian_variables%first
      do while (associated(eulerian_variable))
         do ipar=1,self%npar
            self%interpolated_eul(ipar,eulerian_variable%itarget) = eulerian_variable%source(self%k(ipar))
         end do
         eulerian_variable => eulerian_variable%next
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

   subroutine interpolate_to_grid(self, nlev, h)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: nlev
      real(rk),                    intent(in)    :: h(1:nlev)

      type (type_interpolated_variable), pointer :: particle_variable
      integer                                    :: ipar

      ! Accumulate particle states per layer
      particle_variable => self%particle_variables%first
      do while (associated(particle_variable))
         self%interpolated_par(:,particle_variable%itarget) = 0.0_rk
         do ipar=1,self%npar
            self%interpolated_par(self%k(ipar),particle_variable%itarget) = self%interpolated_par(self%k(ipar),particle_variable%itarget) + particle_variable%source(ipar)
         end do
         particle_variable => particle_variable%next
      end do

      ! Divide by layer heights to obtain concentration
      particle_variable => self%particle_variables%first
      do while (associated(particle_variable))
         self%interpolated_par(:,particle_variable%itarget) = self%interpolated_par(:,particle_variable%itarget)/h(:)
         particle_variable => particle_variable%next
      end do
   end subroutine interpolate_to_grid

   subroutine finalize(self)
      class (type_particle_class), intent(inout) :: self

      call self%eulerian_variables%finalize()
      call self%particle_variables%finalize()

      deallocate(self%interpolated_eul)
      deallocate(self%interpolated_par)

      self%next => null()
   end subroutine finalize

   subroutine interpolated_variable_set_add(self, name, source, itarget)
      class (type_interpolated_variable_set), intent(inout) :: self
      character(len=*),                       intent(in)    :: name
      real(rk), target,                       intent(in)    :: source(:)
      integer, optional,                      intent(in)    :: itarget

      type (type_interpolated_variable), pointer :: variable

      allocate(variable)
      variable%name = trim(name)
      variable%source => source
      if (present(itarget)) variable%itarget = itarget
      variable%next => self%first
      self%first => variable
   end subroutine interpolated_variable_set_add

   subroutine interpolated_variable_set_finalize(self)
      class (type_interpolated_variable_set), intent(inout) :: self

      type (type_interpolated_variable), pointer :: variable, variable_next

      variable => self%first
      do while (associated(variable))
         variable_next => variable%next
         deallocate(variable)
         variable => variable_next
      end do
      self%first => null()
   end subroutine interpolated_variable_set_finalize

end module

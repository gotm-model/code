#include"cppdefs.h"

#ifdef _FABM_
#  define _FABM_PARTICLES_
#endif

module particle_class

   use field_manager, only: type_field, type_field_manager, id_dim_z, output_level_debug, output_level_default

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
      type (type_interpolated_variable), pointer :: linked    => null()
      type (type_interpolated_variable), pointer :: next      => null()
   end type

   type type_interpolated_variable_set
      type (type_interpolated_variable), pointer :: first => null()
   contains
      procedure :: add      => interpolated_variable_set_add
      procedure :: find     => interpolated_variable_set_find
      procedure :: finalize => interpolated_variable_set_finalize
   end type

   type type_particle_class
      integer               :: npar
      integer,  allocatable :: k(:)
      real(rk), allocatable :: z(:)
      real(rk), allocatable :: depth(:)
      real(rk), allocatable :: w(:)

#ifdef _FABM_PARTICLES_
      integer                         :: nstate
      type (type_fabm_particle_state) :: state
      real(rk), allocatable           :: dy(:,:)
#endif

      real(rk), allocatable :: interpolated_eul(:,:)
      real(rk), allocatable :: interpolated_par(:,:)

      type (type_interpolated_variable_set) :: eulerian_variables
      type (type_interpolated_variable_set) :: particle_variables

      type (type_particle_class), pointer :: next => null()
   contains
      procedure :: initialize
      procedure :: link_eulerian_data
      procedure :: link_horizontal_data
      procedure :: link_scalar
      procedure :: interpolate_to_grid
      procedure :: start
      procedure :: advance
      procedure :: finalize
   end type

   contains

   subroutine initialize(self, name, dictionary, nlev, field_manager)
      use yaml_types, only: type_dictionary

      class (type_particle_class), intent(inout) :: self
      character(len=*),            intent(in)    :: name
      class (type_dictionary),     intent(inout) :: dictionary
      integer,                     intent(in)    :: nlev
      class (type_field_manager),  intent(inout) :: field_manager

      integer                                    :: n
      type (type_output),                pointer :: output
      integer                                    :: output_level
      type (type_interpolated_variable), pointer :: particle_variable

#ifdef _FABM_PARTICLES_
      ! Configure the (biogeochemical) properties associated with the particles.
      ! This creates the linked list with properties (head: self%state%first_output),
      ! with metadata per property. The "save" attribute of these properties can
      ! be changed to communicate to FABM-particles that the property will be needed.
      ! After calling self%state%initialize, each property with "save" set will have
      ! its associated data array available throught its "data" attribute.
      call self%state%configure(dictionary)
      self%npar = self%state%npar
      self%nstate = self%state%nstate

      ! Allocate work arrays (allocatable rather than automatic to avoid excessive consumption of stack memory)
      allocate(self%w(self%npar))
      allocate(self%dy(self%npar, self%nstate))

      n = 0
      output => self%state%first_output
      do while (associated(output))
         output_level = output_level_debug
         if (output%save) output_level = output_level_default
         call field_manager%register(trim(name)//'_'//trim(output%name), trim(output%units), &
            trim(name)//' '//trim(output%long_name), dimensions=(/id_dim_z/), output_level=output_level, used=output%save)
         if (output%save) n = n + 1
         output => output%next
      end do
#else
      n = 1
#endif

      allocate(self%interpolated_par(nlev, n))
      allocate(self%k(self%npar))
      allocate(self%z(self%npar))

#ifdef _FABM_PARTICLES_
      ! Complete initialization (must happen after setting the "save" attribute on outputs)
      ! After this is done, additional data fields may be sent to the FABM particle manager
      ! by calling link_eulerian_data.
      call self%state%initialize()

      n = 0
      output => self%state%first_output
      do while (associated(output))
         if (output%save) then
            n = n + 1
            particle_variable => self%particle_variables%add(trim(output%name), output%data, n)
            call field_manager%send_data(trim(name)//'_'//trim(output%name), self%interpolated_par(:,n))
         end if
         output => output%next
      end do

      output => self%state%first_output
      do while (associated(output))
         if (associated(output%specific_to)) then
            particle_variable => self%particle_variables%find(output%name)
            particle_variable%linked => self%particle_variables%find(output%specific_to%name)
            if (.not.associated(particle_variable%linked)) stop 'particle_variable%linked'
         end if
         output => output%next
      end do
#else
      call field_manager%register(trim(name)//'_concentration', '# m-3', trim(name)//' concentration', dimensions=(/id_dim_z/), data1d=self%interpolated_par(:,1))
#endif
   end subroutine initialize

   subroutine link_eulerian_data(self, name, dat)
      class (type_particle_class), intent(inout) :: self
      character(len=*),            intent(in)    :: name
      real(rk), target,            intent(in)    :: dat(:)

      type (type_interpolated_variable), pointer :: eulerian_variable

      if (name == 'cell_thickness' .or. name == 'depth') return
#ifdef _FABM_PARTICLES_
      if (self%state%is_variable_used(name)) eulerian_variable => self%eulerian_variables%add(name, dat)
#endif
   end subroutine link_eulerian_data

   subroutine link_horizontal_data(self, name, dat)
      class (type_particle_class), intent(inout) :: self
      character(len=*),            intent(in)    :: name
      real(rk), target,            intent(in)    :: dat

#ifdef _FABM_PARTICLES_
      call self%state%send_horizontal_data(name, dat)
#endif
   end subroutine link_horizontal_data

   subroutine link_scalar(self, name, dat)
      class (type_particle_class), intent(inout) :: self
      character(len=*),            intent(in)    :: name
      real(rk), target,            intent(in)    :: dat

#ifdef _FABM_PARTICLES_
      call self%state%send_scalar(name, dat)
#endif
   end subroutine link_scalar

   subroutine start(self, nlev, z_if)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: nlev
      real(rk),                    intent(in)    :: z_if(0:nlev)

      integer                                    :: ipar
      integer                                    :: k
      integer                                    :: n
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

      if (self%state%is_variable_used('depth')) then
         allocate(self%depth(self%npar))
         self%depth = -self%z
         call self%state%send_data('depth', self%depth)
      end if

#ifdef _FABM_PARTICLES_
      ! Allow biogeochemical state to perform final initialization steps.
      ! This checks whether all dependencies have been fulfilled, and also initializes the state.
      call self%state%start((z_if(nlev)-z_if(0))/self%npar)
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
      logical                                    :: valid
#ifndef _FABM_PARTICLES_
      logical                                    :: active(self%npar)
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
      call self%state%get_vertical_movement(self%w)
      call self%state%get_sources(self%dy)

      ! Time-integrate source terms (Forward Euler)
      self%state%y = self%state%y + self%dy*dt
#else
#  define _ACTIVE_ self%active
      active = .true.
      self%w = 0.0_rk
#endif

      ! Transport particles
      call lagrange(nlev, dt, z_if, nuh, self%w, self%npar, _ACTIVE_, self%k, self%z)

      if (allocated(self%depth)) self%depth = -self%z

      valid = self%state%check_state(.false.)
      if (.not.valid) stop 'particle_class::advance'
   end subroutine advance

   subroutine interpolate_to_grid(self, nlev, h)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: nlev
      real(rk),                    intent(in)    :: h(1:nlev)

      type (type_interpolated_variable), pointer :: particle_variable
      integer                                    :: ipar
      integer                                    :: k

      ! Accumulate particle states per layer
      particle_variable => self%particle_variables%first
      do while (associated(particle_variable))
         self%interpolated_par(:,particle_variable%itarget) = 0.0_rk
         if (.not.associated(particle_variable%linked)) then
            ! Concentration variable: accumulate value
            do ipar=1,self%npar
               self%interpolated_par(self%k(ipar),particle_variable%itarget) = self%interpolated_par(self%k(ipar),particle_variable%itarget) + particle_variable%source(ipar)
            end do
         else
            ! Property variable (e.g., age): accumulate property*concentration
            do ipar=1,self%npar
               self%interpolated_par(self%k(ipar),particle_variable%itarget) = self%interpolated_par(self%k(ipar),particle_variable%itarget) + particle_variable%source(ipar)*particle_variable%linked%source(ipar)
            end do
         end if
         particle_variable => particle_variable%next
      end do

      ! For all property variables (e.g., age): divide by linked field (concentration) to obtain mean.
      ! Note: take care to avoid division by zero for layers without particles!
      particle_variable => self%particle_variables%first
      do while (associated(particle_variable))
         if (associated(particle_variable%linked)) then
            do k=1,nlev
               if (self%interpolated_par(k,particle_variable%linked%itarget) > 0) &
                  self%interpolated_par(k,particle_variable%itarget) = self%interpolated_par(k,particle_variable%itarget)/self%interpolated_par(k,particle_variable%linked%itarget)
            end do
         end if
         particle_variable => particle_variable%next
      end do

      ! For all concentration variables: divide by layer heights to obtain concentration
      particle_variable => self%particle_variables%first
      do while (associated(particle_variable))
         if (.not.associated(particle_variable%linked)) &
            self%interpolated_par(:,particle_variable%itarget) = self%interpolated_par(:,particle_variable%itarget)/h(:)
         particle_variable => particle_variable%next
      end do
   end subroutine interpolate_to_grid

   subroutine finalize(self)
      class (type_particle_class), intent(inout) :: self

      call self%eulerian_variables%finalize()
      call self%particle_variables%finalize()

      if (allocated(self%k)) deallocate(self%k)
      if (allocated(self%z)) deallocate(self%z)
      if (allocated(self%depth)) deallocate(self%depth)
      if (allocated(self%interpolated_eul)) deallocate(self%interpolated_eul)
      if (allocated(self%interpolated_par)) deallocate(self%interpolated_par)

      self%next => null()
   end subroutine finalize

   function interpolated_variable_set_add(self, name, source, itarget) result(variable)
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
   end function interpolated_variable_set_add

   function interpolated_variable_set_find(self, name) result(variable)
      class (type_interpolated_variable_set), intent(inout) :: self
      character(len=*),                       intent(in)    :: name

      type (type_interpolated_variable), pointer :: variable

      variable => self%first
      do while (associated(variable))
         if (variable%name==name) return
         variable => variable%next
      end do
   end function interpolated_variable_set_find

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

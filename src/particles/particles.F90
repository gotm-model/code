#include"cppdefs.h"

module particles

   use field_manager

   implicit none

   public particles_initialize, particles_start, particles_prepare_output, particles_advance, particles_clean

   integer, parameter :: rk = kind(_ZERO_)

   type type_interpolated_variable
      type (type_field), pointer :: field => null()
      real(rk), pointer :: source(:) => null()
      real(rk), allocatable :: target(:)
      type (type_interpolated_variable), pointer :: next => null()
   end type

   type type_particle_class
      integer :: npar
      integer,  allocatable :: k(:)
      real(rk), allocatable :: z(:)
      real(rk), allocatable :: w(:)
      real(rk), allocatable :: state_eul(:,:)
      integer :: count_index = -1
      logical, allocatable :: active(:)
      type (type_interpolated_variable), pointer :: first_interpolated_variable => null()
   contains
      procedure :: initialize => particle_class_initialize
      procedure :: start      => particle_class_start
      procedure :: finalize   => particle_class_finalize
   end type

   type (type_particle_class) :: particle_class
   
   contains

   subroutine particles_initialize(field_manager, nlev)
      class (type_field_manager), intent(inout) :: field_manager
      integer,                    intent(in)    :: nlev

      call particle_class%initialize(1000, nlev, field_manager)
   end subroutine particles_initialize

   subroutine particles_start(field_manager, zmin, nlev, h)
      class (type_field_manager), intent(in) :: field_manager
      real(rk),                   intent(in)    :: zmin
      integer,                    intent(in)    :: nlev
      real(rk),                   intent(in)    :: h(1:nlev)

      integer :: ibin
      type (type_field), pointer :: field
      type (type_interpolated_variable), pointer :: interpolated_variable
      real(rk) :: z_if(0:nlev)
      integer :: k

      do ibin=1,size(field_manager%field_table)
         field => field_manager%field_table(ibin)%first_field
         do while (associated(field))
            if (associated(field%data_1d)) then
               if (.false.) then
                  ! Create object to hold interpolation data
                  allocate(interpolated_variable)
                  interpolated_variable%field => field
                  interpolated_variable%source => field%data_1d
                  allocate(interpolated_variable%target(particle_class%npar))

                  ! Prepend to list
                  interpolated_variable%next => particle_class%first_interpolated_variable
                  particle_class%first_interpolated_variable => interpolated_variable
               end if
            end if
            field => field%next
         end do
      end do

      ! Compute depth coordinate of interfaces
      z_if(0) = zmin
      do k=1,nlev
         z_if(k) = z_if(k-1) + h(k)
      end do

      ! Initialize particle positions
      call particle_class%start(nlev, z_if)

      call particles_prepare_output(nlev,h)
   end subroutine particles_start

   subroutine particles_advance(nlev, dt, zmin, h, nuh)
      integer,  intent(in) :: nlev
      real(rk), intent(in) :: dt
      real(rk), intent(in) :: h(1:nlev)
      real(rk), intent(in) :: zmin
      real(rk), intent(in) :: nuh(0:nlev)

      type (type_interpolated_variable), pointer :: interpolated_variable
      integer :: ipar
      real(rk) :: z_if(0:nlev)
      integer :: k

      ! Interpolate Eulerian fields to particle positions.
      interpolated_variable => particle_class%first_interpolated_variable
      do while (associated(interpolated_variable))
         do ipar=1,particle_class%npar
            interpolated_variable%target(ipar) = interpolated_variable%source(particle_class%k(ipar))
         end do
         interpolated_variable => interpolated_variable%next
      end do

      z_if(0) = zmin
      do k=1,nlev
         z_if(k) = z_if(k-1) + h(k)
      end do

      call lagrange(nlev, dt, z_if, nuh, particle_class%w, particle_class%npar, particle_class%active, particle_class%k, particle_class%z)
   end subroutine particles_advance

   subroutine particles_prepare_output(nlev, h)
      integer,  intent(in) :: nlev
      real(rk), intent(in) :: h(1:nlev)

      integer :: istate

      real(rk) :: particle_count(particle_class%npar)

      ! Create Eulerian particle fields.
      if (particle_class%count_index == -1) then
         particle_count(:) = 1.0_rk
      else
         ! Obtain particle counts from FABM driver
      end if

      ! Interpolate particle counts to Eulerian grid
      call to_eul(particle_count, particle_class%state_eul(:,1))

      ! Interpolate auxiliary particles states and normalize by number of particles
      do istate=2,size(particle_class%state_eul,2)
         !call to_eul(state, particle_count*self%state_eul(:,istate))
         particle_class%state_eul(:,istate) = particle_class%state_eul(:,istate)/particle_class%state_eul(:,1)
      end do

      ! Divide particle counts (# m-2) by layer heights to get concentration (# m-3)
      particle_class%state_eul(:,1) = particle_class%state_eul(:,1)/h(:)

   contains

      subroutine to_eul(par_in, eul_out)
         real(rk), intent(in)  :: par_in(particle_class%npar)
         real(rk), intent(out) :: eul_out(nlev)

         integer :: ipar

         eul_out = 0.0_rk
         do ipar=1,particle_class%npar
            eul_out(particle_class%k(ipar)) = eul_out(particle_class%k(ipar)) + par_in(ipar)
         end do
      end subroutine

   end subroutine particles_prepare_output

   subroutine particles_clean()
      call particle_class%finalize()
   end subroutine particles_clean

   subroutine particle_class_start(self, nlev, z_if)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: nlev
      real(rk),                    intent(in)    :: z_if(0:nlev)

      integer :: ipar
      integer :: k

      call random_number(self%z)
      self%z = z_if(0) + self%z*(z_if(nlev)-z_if(0))

      ! Find depth index for each particle
      do ipar=1,self%npar
         do k=1,nlev-1
            if (self%z(ipar) < z_if(k)) exit
         end do
         self%k(ipar) = k
      end do
   end subroutine particle_class_start

   subroutine particle_class_initialize(self, npar, nlev, field_manager)
      class (type_particle_class), intent(inout) :: self
      integer,                     intent(in)    :: npar
      integer,                     intent(in)    :: nlev
      class (type_field_manager), intent(inout)  :: field_manager

      integer :: nstate_eul

      self%npar = npar
      allocate(self%k(self%npar))
      allocate(self%z(self%npar))
      allocate(self%w(self%npar))
      allocate(self%active(self%npar))
      self%active = .true.
      self%w = 0.0_rk

      nstate_eul = 0
      if (self%count_index == -1) nstate_eul = nstate_eul + 1
      allocate(self%state_eul(nlev, nstate_eul))
      if (self%count_index == -1) call field_manager%register('particle_concentration', '# m-3', 'concentration of particles', dimensions=(/id_dim_z/), data1d=self%state_eul(:,1))
   end subroutine particle_class_initialize
   
   subroutine particle_class_finalize(self)
      class (type_particle_class), intent(inout) :: self
      type (type_interpolated_variable), pointer :: interpolated_variable, next_interpolated_variable

      interpolated_variable => self%first_interpolated_variable
      do while (associated(interpolated_variable))
         next_interpolated_variable => interpolated_variable%next
         deallocate(interpolated_variable)
         interpolated_variable => next_interpolated_variable
      end do
      self%first_interpolated_variable => null()
   end subroutine particle_class_finalize

end module
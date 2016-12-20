#include"cppdefs.h"

module particles

   use field_manager
   use particle_class

   implicit none

   public particles_initialize, particles_start, particles_advance, particles_clean

   type (type_particle_class) :: particle
   
   contains

   subroutine particles_initialize(field_manager, nlev)
      class (type_field_manager), intent(inout) :: field_manager
      integer,                    intent(in)    :: nlev

      call particle%initialize(1000, nlev, field_manager)
   end subroutine particles_initialize

   subroutine particles_start(field_manager, zmin, nlev, h)
      class (type_field_manager), intent(in) :: field_manager
      real(rk),                   intent(in) :: zmin
      integer,                    intent(in) :: nlev
      real(rk),                   intent(in) :: h(1:nlev)

      integer                    :: ibin
      type (type_field), pointer :: field
      real(rk)                   :: z_if(0:nlev)
      integer                    :: k

      do ibin=1,size(field_manager%field_table)
         field => field_manager%field_table(ibin)%first_field
         do while (associated(field))
            if (associated(field%data_1d)) then
               call particle%link_eulerian_data(trim(field%name), field%data_1d)
               if (field%standard_name /= '') call particle%link_eulerian_data(trim(field%standard_name), field%data_1d)
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
      call particle%start(nlev, z_if)

      call particle%interpolate_state_to_grid(nlev, h)
   end subroutine particles_start

   subroutine particles_advance(nlev, dt, zmin, h, nuh)
      integer,  intent(in) :: nlev
      real(rk), intent(in) :: dt
      real(rk), intent(in) :: h(1:nlev)
      real(rk), intent(in) :: zmin
      real(rk), intent(in) :: nuh(0:nlev)

      real(rk) :: z_if(0:nlev)
      integer  :: k

      ! Compute depth coordinates of interfaces (needed for transport)
      z_if(0) = zmin
      do k=1,nlev
         z_if(k) = z_if(k-1) + h(k)
      end do

      ! Get particle source terms and vertical velocities
      call particle%advance(nlev, dt, z_if, nuh)

      ! Compute the gridded particle fields (concentration per layer) that are sent to output.
      call particle%interpolate_state_to_grid(nlev, h)
   end subroutine particles_advance

   subroutine particles_clean()
      call particle%finalize()
   end subroutine particles_clean

end module
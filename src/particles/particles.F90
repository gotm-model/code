#include"cppdefs.h"

module particles

   use field_manager
   use particle_class

   implicit none

   private

   public particles_initialize, particles_start, particles_advance, particles_clean

   type (type_particle_class), pointer, save :: first_particle_class => null()

   contains

   subroutine particles_initialize(field_manager, nlev)
      use yaml, only: yaml_parse => parse, yaml_error_length => error_length
      use yaml_types, only: type_node, type_key_value_pair, type_dictionary, type_error

      class (type_field_manager), intent(inout) :: field_manager
      integer,                    intent(in)    :: nlev

      logical                             :: file_exists
      integer,parameter                   :: yaml_unit = 100
      character(len=yaml_error_length)    :: parse_error
      class (type_node),          pointer :: node
      type(type_error),           pointer :: yaml_error
      class (type_dictionary),    pointer :: dictionary
      type (type_key_value_pair), pointer :: pair, pair2
      type (type_particle_class), pointer :: particle_class
      integer                             :: npar

      first_particle_class => null()

      ! Check whether particles.yaml exists - if not, particles will not be used.
      inquire(file='particles.yaml', exist=file_exists)
      if (.not. file_exists) then
         LEVEL3 'WARNING: particles will not be used because particles.yaml is not present.'
         return
      end if

      ! Parse particles.yaml.
      node => yaml_parse('particles.yaml', yaml_unit, parse_error)
      if (parse_error /= '') then
         FATAL trim(parse_error)
         stop 'particles_initialize'
      end if
      if (.not. associated(node)) then
         LEVEL3 'WARNING: particles will not be used because particles.yaml is empty.'
         return
      end if

      ! Obtain class dictionary from root-level dictionary.
      select type (node)
      class is (type_dictionary)
         dictionary => node%get_dictionary('classes', .true., yaml_error)
         if (associated(yaml_error)) then
            FATAL trim(yaml_error%message)
            stop 'particles_initialize'
         end if

         ! Check for any remaining [unknown] keys at root level.
         pair => node%first
         do while (associated(pair))
            if (.not.pair%accessed) then
               FATAL 'particles.yaml: key '//trim(pair%key)//' at root level was not recognized.'
               stop 'particles_initialize'
            end if
            pair => pair%next
         end do
      class default
         FATAL 'particles.yaml must contain a dictionary with (variable name : information) pairs.'
         stop 'particles_initialize'
      end select

      ! Process classes.
      pair => dictionary%first
      do while (associated(pair))
         if (pair%key == '') then
            FATAL 'Error parsing particles.yaml: empty class name.'
            stop 'particles_initialize'
         end if
         select type (class_dictionary => pair%value)
         class is (type_dictionary)
            ! Allocate new particle class.
            allocate(particle_class)

            ! Initialize particle class.
            call particle_class%initialize(trim(pair%key), class_dictionary, nlev, field_manager)

            ! Check for any remaining [unknown] class attributes.
            pair2 => class_dictionary%first
            do while (associated(pair2))
               if (.not.pair2%accessed) then
                  FATAL 'particles.yaml: key '//trim(pair2%key)//' below classes/'//trim(pair%key)//' was not recognized.'
                  stop 'particles_initialize'
               end if
               pair2 => pair2%next
            end do

            ! Prepend to particle class list.
            particle_class%next => first_particle_class
            first_particle_class => particle_class
         class default
            FATAL 'Error parsing particles.yaml: contents of '//trim(class_dictionary%path)//' must be a dictionary, not a single value.'
            stop 'particles_initialize'
         end select
         pair => pair%next
      end do
   end subroutine particles_initialize

   subroutine particles_start(field_manager, zmin, nlev, h)
      class (type_field_manager), intent(in) :: field_manager
      real(rk),                   intent(in) :: zmin
      integer,                    intent(in) :: nlev
      real(rk),                   intent(in) :: h(1:nlev)

      integer                             :: ibin
      type (type_field), pointer          :: field
      type (type_particle_class), pointer :: particle_class
      real(rk)                            :: z_if(0:nlev)
      integer                             :: k

      if (.not. associated(first_particle_class)) return

      ! Offer all fields registered with the field manager to the active particle classes.
      do ibin=1,size(field_manager%field_table)
         field => field_manager%field_table(ibin)%first_field
         do while (associated(field))
            if (associated(field%data_1d)) then
               ! 1D variable (defined throughout the water column)
               particle_class => first_particle_class
               do while (associated(particle_class))
                  call particle_class%link_eulerian_data(trim(field%name), field%data_1d)
                  if (field%standard_name /= '') call particle_class%link_eulerian_data(trim(field%standard_name), field%data_1d)
                  particle_class => particle_class%next
               end do
            end if
            if (associated(field%data_0d)) then
               ! 0D variable (horizontal-only)
               particle_class => first_particle_class
               do while (associated(particle_class))
                  call particle_class%link_horizontal_data(trim(field%name), field%data_0d)
                  if (field%standard_name /= '') call particle_class%link_horizontal_data(trim(field%standard_name), field%data_0d)
                  particle_class => particle_class%next
               end do
            end if
            field => field%next
         end do
      end do

      ! Compute depth coordinate of interfaces (needed for initialization of particle positions)
      z_if(0) = zmin
      do k=1,nlev
         z_if(k) = z_if(k-1) + h(k)
      end do

      particle_class => first_particle_class
      do while (associated(particle_class))
         ! Initialize particle positions
         call particle_class%start(nlev, z_if)

         ! Compute the gridded particle fields (concentration per layer) that are sent to output.
         ! This ensures all outputs have an initial value.
         call particle_class%interpolate_to_grid(nlev, h)

         particle_class => particle_class%next
      end do
   end subroutine particles_start

   subroutine particles_advance(nlev, dt, zmin, h, nuh)
      integer,  intent(in) :: nlev
      real(rk), intent(in) :: dt
      real(rk), intent(in) :: h(1:nlev)
      real(rk), intent(in) :: zmin
      real(rk), intent(in) :: nuh(0:nlev)

      real(rk)                            :: z_if(0:nlev)
      integer                             :: k
      type (type_particle_class), pointer :: particle_class

      if (.not. associated(first_particle_class)) return

      ! Compute depth coordinates of interfaces (needed for transport)
      z_if(0) = zmin
      do k=1,nlev
         z_if(k) = z_if(k-1) + h(k)
      end do

      particle_class => first_particle_class
      do while (associated(particle_class))
         ! Integrate over one time step (include sources and transport)
         call particle_class%advance(nlev, dt, z_if, nuh)

         ! Compute the gridded particle fields (concentration per layer) that are sent to output.
         call particle_class%interpolate_to_grid(nlev, h)

         particle_class => particle_class%next
      end do
   end subroutine particles_advance

   subroutine particles_clean()
      type (type_particle_class), pointer :: particle_class, particle_class_next

      particle_class => first_particle_class
      do while (associated(particle_class))
         particle_class_next => particle_class%next
         call particle_class%finalize()
         particle_class => particle_class_next
      end do
   end subroutine particles_clean

end module

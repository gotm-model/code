module output_operators_base

   use output_manager_core
   use field_manager
   use yaml_types

   implicit none

   private

   public type_base_operator

   type, extends(type_base_output_field) :: type_base_operator
      class (type_base_output_field), pointer :: source => null()
      real(rk)              :: result_0d
      real(rk), allocatable :: result_1d(:)
      real(rk), allocatable :: result_2d(:,:)
      real(rk), allocatable :: result_3d(:,:,:)
   contains
      procedure :: initialize
      procedure :: new_data
      procedure :: before_save
      procedure :: get_metadata
      procedure :: fill
   end type

contains

   recursive subroutine initialize(self, field_manager)
      class (type_base_operator), intent(inout), target :: self
      type (type_field_manager),  intent(in)            :: field_manager

      if (.not. associated(self%source)) call host%fatal_error('type_base_operator%initialize', 'BUG: source has not been set.')
      call self%source%initialize(field_manager)
   end subroutine

   recursive subroutine new_data(self)
      class (type_base_operator), intent(inout) :: self
      call self%source%new_data()
   end subroutine

   recursive subroutine before_save(self)
      class (type_base_operator), intent(inout) :: self
      call self%source%before_save()
   end subroutine

   recursive subroutine get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_base_operator), intent(in) :: self
      character(len=:), allocatable, optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), optional :: attributes
      call self%source%get_metadata(long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
   end subroutine

   subroutine fill(self, value)
      class (type_base_operator), intent(inout) :: self
      real(rk),                   intent(in)    :: value

      if (allocated(self%result_3d)) then
         self%result_3d(:,:,:) = value
      elseif (allocated(self%result_2d)) then
         self%result_2d(:,:) = value
      elseif (allocated(self%result_1d)) then
         self%result_1d(:) = value
      else
         self%result_0d = value
      end if
   end subroutine

end module
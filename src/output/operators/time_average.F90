module output_operators_time_average

   use output_manager_core
   use field_manager
   use yaml_types
   use output_operators_base

   implicit none

   private

   public type_time_average_operator

   type, extends(type_base_operator) :: type_time_average_operator
      integer :: method = time_method_mean
      integer :: n = 0
   contains
      procedure :: initialize
      procedure :: flag_as_required
      procedure :: new_data
      procedure :: before_save
   end type

contains

   recursive subroutine initialize(self, field_manager)
      class (type_time_average_operator), intent(inout), target :: self
      type (type_field_manager),          intent(in)            :: field_manager

      real(rk) :: fill_value

      call self%type_base_operator%initialize(field_manager)
      if (associated(self%source%data_3d)) then
         allocate(self%result_3d(size(self%source%data_3d,1), size(self%source%data_3d,2), size(self%source%data_3d,3)))
         self%data_3d => self%result_3d
      elseif (associated(self%source%data_2d)) then
         allocate(self%result_2d(size(self%source%data_2d,1), size(self%source%data_2d,2)))
         self%data_2d => self%result_2d
      elseif (associated(self%source%data_1d)) then
         allocate(self%result_1d(size(self%source%data_1d)))
         self%data_1d => self%result_1d
      elseif (associated(self%source%data_0d)) then
         self%data_0d => self%result_0d
      end if
      call self%get_metadata(fill_value=fill_value)
      if (self%method == time_method_mean) call self%fill(fill_value)
   end subroutine

   recursive subroutine flag_as_required(self, required)
      class (type_time_average_operator), intent(inout) :: self
      logical,                            intent(in) :: required

      call self%source%flag_as_required(.true.)
   end subroutine

   recursive subroutine new_data(self)
      class (type_time_average_operator), intent(inout) :: self

      call self%source%before_save()
      if (self%n == 0) call self%fill(0.0_rk)
      if (allocated(self%result_3d)) then
         self%result_3d(:,:,:) = self%result_3d + self%source%data_3d
      elseif (allocated(self%result_2d)) then
         self%result_2d(:,:) = self%result_2d + self%source%data_2d
      elseif (allocated(self%result_1d)) then
         self%result_1d(:) = self%result_1d + self%source%data_1d
      else
         self%result_0d = self%result_0d + self%source%data_0d
      end if
      self%n = self%n + 1
   end subroutine

   recursive subroutine before_save(self)
      class (type_time_average_operator), intent(inout) :: self

      if (self%method == time_method_mean) then
         if (allocated(self%result_3d)) then
            self%result_3d(:,:,:) = self%result_3d/self%n
         elseif (allocated(self%result_2d)) then
            self%result_2d(:,:) = self%result_2d/self%n
         elseif (allocated(self%result_1d)) then
            self%result_1d(:) = self%result_1d/self%n
         else
            self%result_0d = self%result_0d/self%n
         end if
      end if
      self%n = 0
   end subroutine

end module
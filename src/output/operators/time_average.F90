module output_operators_time_average

   use output_manager_core
   use field_manager
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
      procedure :: get_field
      procedure :: get_metadata
   end type

contains

   recursive subroutine initialize(self, field_manager)
      class (type_time_average_operator), intent(inout), target :: self
      type (type_field_manager),          intent(in)            :: field_manager

      real(rk) :: fill_value

      call self%type_base_operator%initialize(field_manager)

      if (associated(self%source%data%p3d)) then
         allocate(self%result_3d(size(self%source%data%p3d,1), size(self%source%data%p3d,2), size(self%source%data%p3d,3)))
         self%data%p3d => self%result_3d
      elseif (associated(self%source%data%p2d)) then
         allocate(self%result_2d(size(self%source%data%p2d,1), size(self%source%data%p2d,2)))
         self%data%p2d => self%result_2d
      elseif (associated(self%source%data%p1d)) then
         allocate(self%result_1d(size(self%source%data%p1d)))
         self%data%p1d => self%result_1d
      elseif (associated(self%source%data%p0d)) then
         self%data%p0d => self%result_0d
      end if
      call self%get_metadata(fill_value=fill_value)
      if (self%method == time_method_mean) call self%fill(fill_value)
   end subroutine

   recursive subroutine flag_as_required(self, required)
      class (type_time_average_operator), intent(inout) :: self
      logical,                            intent(in)    :: required

      call self%source%flag_as_required(.true.)
   end subroutine

   recursive subroutine new_data(self)
      class (type_time_average_operator), intent(inout) :: self

      call self%source%before_save()
      if (self%n == 0) call self%fill(0.0_rk)
      if (allocated(self%result_3d)) then
         self%result_3d(:,:,:) = self%result_3d + self%source%data%p3d
      elseif (allocated(self%result_2d)) then
         self%result_2d(:,:) = self%result_2d + self%source%data%p2d
      elseif (allocated(self%result_1d)) then
         self%result_1d(:) = self%result_1d + self%source%data%p1d
      else
         self%result_0d = self%result_0d + self%source%data%p0d
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

   recursive function get_field(self, field) result(output_field)
      class (type_time_average_operator), intent(inout) :: self
      type (type_field), target                         :: field
      class (type_base_output_field), pointer           :: output_field
      class (type_time_average_operator), pointer :: time_average
      allocate(time_average)
      time_average%method = self%method
      time_average%source => self%source%get_field(field)
      time_average%output_name = time_average%source%output_name
      output_field => time_average
   end function

   recursive subroutine get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_time_average_operator), intent(in) :: self
      character(len=:), allocatable, optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), optional :: attributes
      call self%type_base_operator%get_metadata(long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      if (present(attributes)) then
         select case (self%method)
         case (time_method_mean)
            call attributes%set('cell_methods', 'time: mean')
         case default
            call attributes%set('cell_methods', 'time: sum')
         end select
      end if
   end subroutine

end module
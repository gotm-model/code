module output_operators_time_average

   use output_manager_core
   use field_manager
   use output_operators_base

   implicit none

   private

   public type_time_average_operator

   type, extends(type_base_operator) :: type_time_average_operator
      integer :: method = time_method_mean
   contains
      procedure :: apply
   end type

   type, extends(type_universal_operator_result) :: type_result
      integer :: method = time_method_mean
      integer :: n = 0
   contains
      procedure :: flag_as_required
      procedure :: new_data
      procedure :: before_save
      procedure :: get_metadata
   end type
   
contains

   function apply(self, source) result(output_field)
      class (type_time_average_operator), intent(inout), target :: self
      class (type_base_output_field), target                    :: source
      class (type_base_output_field), pointer                   :: output_field

      real(rk)                                   :: fill_value
      type (type_dimension_pointer), allocatable :: dimensions(:)
      integer                                    :: itimedim
      class (type_result), pointer               :: result

      call source%get_metadata(dimensions=dimensions, fill_value=fill_value)
      do itimedim=1,size(dimensions)
         if (dimensions(itimedim)%p%id == id_dim_time) exit
      end do
      if (itimedim > size(dimensions)) then
         output_field => source
         return
      end if

      allocate(result)
      result%operator => self
      result%source => source
      result%output_name = 'time_average('//trim(result%source%output_name)//')'
      output_field => result
      result%method = self%method

      if (associated(result%source%data%p3d)) then
         allocate(result%result_3d(size(result%source%data%p3d,1), size(result%source%data%p3d,2), size(result%source%data%p3d,3)))
         result%data%p3d => result%result_3d
      elseif (associated(result%source%data%p2d)) then
         allocate(result%result_2d(size(result%source%data%p2d,1), size(result%source%data%p2d,2)))
         result%data%p2d => result%result_2d
      elseif (associated(result%source%data%p1d)) then
         allocate(result%result_1d(size(result%source%data%p1d)))
         result%data%p1d => result%result_1d
      elseif (associated(result%source%data%p0d)) then
         result%data%p0d => result%result_0d
      end if
      if (self%method == time_method_mean) call result%fill(fill_value)
   end function

   recursive subroutine flag_as_required(self, required)
      class (type_result), intent(inout) :: self
      logical,             intent(in)    :: required

      call self%source%flag_as_required(.true.)
   end subroutine

   recursive subroutine new_data(self)
      class (type_result), intent(inout) :: self

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
      class (type_result), intent(inout) :: self

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

   recursive subroutine get_metadata(self, long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
      class (type_result), intent(in) :: self
      character(len=:), allocatable, optional :: long_name, units, standard_name, path
      type (type_dimension_pointer), allocatable, intent(out), optional :: dimensions(:)
      real(rk), intent(out), optional :: minimum, maximum, fill_value
      type (type_attributes), intent(out), optional :: attributes
      call self%type_universal_operator_result%get_metadata(long_name, units, dimensions, minimum, maximum, fill_value, standard_name, path, attributes)
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
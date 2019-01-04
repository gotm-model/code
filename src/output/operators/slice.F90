module output_operators_slice

   use output_manager_core
   use field_manager
   use yaml_types
   use output_operators_base

   implicit none

   private

   public type_slice_operator

   type, extends(type_base_operator) :: type_slice_operator
      character(len=string_length) :: dimension
      integer :: start = 1
      integer :: stop = -1
      integer :: stride = 1
   contains
      procedure :: initialize
   end type

contains

   recursive subroutine initialize(self, field_manager)
      class (type_slice_operator), intent(inout), target :: self
      type (type_field_manager),   intent(in)            :: field_manager

      type (type_dimension_pointer), allocatable :: dimensions(:)
      integer,                       allocatable :: starts(:), stops(:), strides(:)
      integer                                    :: i, j, idim

      call self%type_base_operator%initialize(field_manager)

      call self%source%get_metadata(dimensions=dimensions)
      allocate(starts(1:size(dimensions)))
      allocate(stops(1:size(dimensions)))
      allocate(strides(1:size(dimensions)))
      starts(:) = 1
      strides(:) = 1
      j = 0
      do i=1,size(dimensions)
         if (dimensions(i)%p%length > 1) then
            j = j + 1
            stops(j) = dimensions(i)%p%length
         end if
         if (dimensions(i)%p%name == self%dimension) idim = i
      end do
      starts(idim) = self%start
      stops(idim) = self%stop
      strides(idim) = self%stride

      if (all(stops(1:j) >= starts(1:j))) then
         if (associated(self%source%data_3d)) then
            if (j/=3) call host%fatal_error('type_slice_operator%initialize','BUG: data of '//trim(self%output_name)//' contains one or more singleton dimensions.')
            self%data_3d => self%source%data_3d(starts(1):stops(1):strides(1), starts(2):stops(2):strides(2), starts(3):stops(3):strides(3))
         elseif (associated(self%source%data_2d)) then
            if (j/=2) call host%fatal_error('type_slice_operator%initialize','BUG: data of '//trim(self%output_name)//' contains one or more singleton dimensions.')
            self%data_2d => self%source%data_2d(starts(1):stops(1):strides(1), starts(2):stops(2):strides(2))
         elseif (associated(self%source%data_1d)) then
            if (j/=1) call host%fatal_error('type_slice_operator%initialize','BUG: data of '//trim(self%output_name)//' contains one or more singleton dimensions.')
            self%data_1d => self%source%data_1d(starts(1):stops(1):strides(1))
         else
            if (j/=0) call host%fatal_error('type_slice_operator%initialize','BUG: data of '//trim(self%output_name)//' contains one or more singleton dimensions.')
            self%data_0d => self%source%data_0d
         end if
      end if
   end subroutine

end module
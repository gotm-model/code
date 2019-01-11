module output_operators_slice

   use output_manager_core
   use field_manager
   use output_operators_base

   implicit none

   private

   public type_slice_operator

   type type_single_dimension_slice
      character(len=string_length) :: dimension
      integer :: global_start = 1
      integer :: global_stop = -1
      integer :: stride = 1
      type (type_single_dimension_slice), pointer :: next => null()
   end type

   type, extends(type_base_operator) :: type_slice_operator
      type (type_single_dimension_slice), pointer :: first => null()
   contains
      procedure :: apply
      procedure :: add
   end type

   contains

   subroutine add(self, dimension, start, stop, stride)
      class (type_slice_operator), intent(inout) :: self
      character(len=*),            intent(in)    :: dimension
      integer,                     intent(in)    :: start, stop, stride

      type (type_single_dimension_slice), pointer :: single_dimension_slice

      if (start < 0) call host%fatal_error('type_slice_operator%initialize', &
         'Start index of dimension ' // dimension // ' must equal or exceed 0.')
      if (stop < 1) call host%fatal_error('type_slice_operator%initialize', &
         'Stop index of dimension ' // dimension // ' must equal or exceed 1.')

      allocate(single_dimension_slice)
      single_dimension_slice%dimension = dimension
      single_dimension_slice%global_start = start
      single_dimension_slice%global_stop = stop - mod(stop - start, stride)
      single_dimension_slice%stride = stride
      single_dimension_slice%next => self%first
      self%first => single_dimension_slice
   end subroutine

   recursive function apply(self, source) result(output_field)
      class (type_slice_operator), intent(inout), target :: self
      class (type_base_output_field), target             :: source
      class (type_base_output_field), pointer            :: output_field

      class (type_universal_operator_result), pointer :: result
      type (type_dimension_pointer),    allocatable :: dimensions(:)
      type (type_dimension), pointer                :: new_dimension
      integer                                       :: local_start, local_stop, stride
      integer,                          allocatable :: starts(:), stops(:), strides(:)
      type (type_single_dimension_slice), pointer   :: single_dimension_slice
      integer                                       :: i, j, global_start, global_stop

      output_field => self%type_base_operator%apply(source)
      if (.not. associated(output_field)) return

      call output_field%get_metadata(dimensions=dimensions)

      allocate(starts(1:size(dimensions)))
      allocate(stops(1:size(dimensions)))
      allocate(strides(1:size(dimensions)))
      starts(:) = 1
      strides(:) = 1
      j = 0
      do i=1,size(dimensions)
         if (dimensions(i)%p%id == id_dim_time) cycle
         global_start = 1
         global_stop = dimensions(i)%p%global_length
         stride = 1
         single_dimension_slice => self%first
         do while (associated(single_dimension_slice))
            if (dimensions(i)%p%name == single_dimension_slice%dimension) exit
            single_dimension_slice => single_dimension_slice%next
         end do
         if (associated(single_dimension_slice)) then
            if (single_dimension_slice%global_start > dimensions(i)%p%global_length) call host%fatal_error('type_slice_operator%initialize', &
               'Start index of dimension ' // trim(single_dimension_slice%dimension) // ' exceeds length.')
            if (single_dimension_slice%global_stop > dimensions(i)%p%global_length) call host%fatal_error('type_slice_operator%initialize', &
               'Stop index of dimension ' // trim(single_dimension_slice%dimension) // ' exceeds length.')
            global_start = single_dimension_slice%global_start
            if (single_dimension_slice%global_stop /= -1) global_stop = single_dimension_slice%global_stop
            stride = single_dimension_slice%stride
         end if
         call find_local_range(global_start, global_stop, dimensions(i)%p%offset, dimensions(i)%p%length, stride, local_start, local_stop)            
         if (local_stop < local_start) then
            output_field => null()
            return
         end if
         if (dimensions(i)%p%length > 1) then
            j = j + 1
            starts(j) = local_start
            stops(j) = local_stop
            strides(j) = stride
         end if
         if (local_start /= 1 .or. local_stop /= dimensions(i)%p%length .or. stride /= 1) then
            ! This dimension is being modified by the slicing
            allocate(new_dimension)
            new_dimension%name = dimensions(i)%p%name
            new_dimension%length = 1 + (local_stop - local_start) / stride
            new_dimension%global_length = new_dimension%length
            new_dimension%coordinate => dimensions(i)%p%coordinate
            dimensions(i)%p => new_dimension
         end if
      end do

      allocate(result)
      result%operator => self
      result%source => output_field
      result%output_name = 'slice('//trim(result%source%output_name)//')'
      output_field => result
      allocate(result%dimensions(1:size(dimensions)))
      result%dimensions(:) = dimensions

      if (associated(result%source%data%p3d)) then
         if (j/=3) call host%fatal_error('type_slice_operator%initialize','BUG: data of '//trim(result%output_name)//' contains one or more singleton dimensions.')
         call result%data%set(result%source%data%p3d(starts(1):stops(1):strides(1), starts(2):stops(2):strides(2), starts(3):stops(3):strides(3)))
      elseif (associated(result%source%data%p2d)) then
         if (j/=2) call host%fatal_error('type_slice_operator%initialize','BUG: data of '//trim(result%output_name)//' contains one or more singleton dimensions.')
         call result%data%set(result%source%data%p2d(starts(1):stops(1):strides(1), starts(2):stops(2):strides(2)))
      elseif (associated(result%source%data%p1d)) then
         if (j/=1) call host%fatal_error('type_slice_operator%initialize','BUG: data of '//trim(result%output_name)//' contains one or more singleton dimensions.')
         call result%data%set(result%source%data%p1d(starts(1):stops(1):strides(1)))
      else
         if (j/=0) call host%fatal_error('type_slice_operator%initialize','BUG: data of '//trim(result%output_name)//' contains one or more singleton dimensions.')
         call result%data%set(result%source%data%p0d)
      end if
   end function

   subroutine find_local_range(global_start, global_stop, local_offset, local_length, stride, local_start, local_stop)
      integer, intent(in)  :: global_start, global_stop, stride, local_offset, local_length      
      integer, intent(out) :: local_start, local_stop

      integer :: distance

      ! Compute local [i.e., within-subdomain] start and stop positons from global positions and local offset.
      if (global_start > local_offset + local_length) then
         ! Start point lies beyond our subdomain
         local_start = 1
         local_stop = local_start - stride
      else
         if (global_start > local_offset) then
            ! Starting point lies within our subdomain
            local_start = global_start - local_offset
         else
            ! Starting point lies before our subdomain: we start immediately but have to account for stride

            ! Determine distance between subdomain start and nearest included point outside the domain.
            distance = mod(local_offset + 1 - global_start, stride)

            ! Convert to distance to next point within the domain
            if (distance > 0) distance = stride - distance
            local_start = 1 + distance
         end if

         ! Determine local stop by subtracting subdomain offset [maximum is subdomain length)
         local_stop = min(global_stop - local_offset, local_length)

         if (local_stop < local_start) then
            ! stop precedes start, so we have 0 length, i.e.,
            ! length = (local_stop-local_start)/stride + 1 = 0
            local_stop = local_start - stride
         else
            ! Reduce stop to last point that is actually included (due to stride>1)
            local_stop = local_stop - mod(local_stop - local_start, stride)
         end if
      end if
   end subroutine

end module
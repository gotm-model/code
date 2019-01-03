module output_operators_library

   use output_operators_interp
   use output_operators_time_average
   use output_manager_core
   use field_manager
   use yaml_types

   implicit none

   private

   public apply_operators

contains

   subroutine apply_operators(field, list, field_manager)
      class (type_base_output_field), pointer  :: field
      class (type_list),         intent(in)    :: list
      type (type_field_manager), intent(inout) :: field_manager

      type (type_list_item),      pointer :: item
      character(len=string_length)        :: operator_type
      type (type_error),          pointer :: config_error
      class (type_interp_operator), pointer :: interp

      item => list%first
      do while (associated(item))
         select type (mapping=>item%node)
         class is (type_dictionary)
            operator_type = mapping%get_string('type', error=config_error)
            if (associated(config_error)) call host%fatal_error('process_operator', config_error%message)
            select case (operator_type)
            case ('interp')
               allocate(interp)
               call interp%configure(mapping, field_manager)
               interp%source => field
               field => interp
            case default
               call host%fatal_error('apply_operators', trim(mapping%path)//': operator type '//trim(operator_type)//' not recognized.')
            end select
         class default
            call host%fatal_error('apply_operators','Elements below '//trim(list%path)//' must be dictionaries.')
         end select
         item => item%next
      end do
   end subroutine

end module
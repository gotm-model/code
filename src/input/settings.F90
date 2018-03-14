module settings
   use yaml_settings

   implicit none

   private

   public type_settings, type_option, settings_store

   type,extends(type_settings) :: type_gotm_settings
   contains
      procedure :: create_child
   end type
   
   type (type_gotm_settings) :: settings_store

contains

   function create_child(self) result(child)
      class (type_gotm_settings), intent(in) :: self
      class (type_settings),  pointer   :: child
      allocate(type_gotm_settings::child)
   end function create_child
   
end module
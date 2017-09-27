#include"cppdefs.h"

module field_manager

   implicit none

   ! Public subroutine and functions
   public type_field_manager

   ! Public data types and variables
   public type_field
   public type_category_node
   public type_dimension
   public type_attribute, type_real_attribute, type_integer_attribute, type_string_attribute
   public type_field_set, type_field_set_member

   ! Public parameters
   public string_length,default_fill_value,default_minimum,default_maximum

   private

   integer,parameter :: string_length = 256
   integer,parameter :: nmaxdims = 10
   integer,parameter :: rk = kind(_ONE_)

   integer, parameter, public :: id_dim_lon  = 1
   integer, parameter, public :: id_dim_lat  = 2
   integer, parameter, public :: id_dim_z    = 3
   integer, parameter, public :: id_dim_zi   = 4
   integer, parameter, public :: id_dim_time = 5
   integer, parameter, public :: id_dim_unused = 20   ! First free id for user-specified dimensions

   integer, parameter, public :: status_not_registered       = 0
   integer, parameter, public :: status_registered_no_data   = 1
   integer, parameter, public :: status_registered_with_data = 2

   integer, parameter, public :: output_level_none     = 0
   integer, parameter, public :: output_level_required = 2
   integer, parameter, public :: output_level_default  = 8
   integer, parameter, public :: output_level_debug    = 32

   real(rk),parameter :: default_fill_value = -huge(_ONE_)
   real(rk),parameter :: default_minimum = default_fill_value + spacing(default_fill_value)
   real(rk),parameter :: default_maximum = huge(_ONE_)

   type type_dimension
      character(len=string_length)   :: name = ''
      character(len=string_length)   :: iterator = ''
      integer                        :: length = -1
      integer                        :: global_length = -1
      integer                        :: offset = 0
      integer                        :: id = -1
      type (type_field), pointer     :: coordinate => null()
      type (type_dimension), pointer :: next => null()
   end type

   type type_dimension_pointer
      type (type_dimension), pointer :: p => null()
   end type

   type type_attribute
      character(len=string_length)   :: name = ''
      class (type_attribute),pointer :: next => null()
   end type

   type,extends(type_attribute) :: type_real_attribute
      real(rk) :: value = 0.0_rk
   end type

   type,extends(type_attribute) :: type_integer_attribute
      integer :: value = 0
   end type

   type,extends(type_attribute) :: type_string_attribute
      character(len=string_length) :: value = ''
   end type

   type type_field
      integer                      :: id             = 0
      character(len=string_length) :: name           = ''
      character(len=string_length) :: units          = ''
      character(len=string_length) :: long_name      = ''
      character(len=string_length) :: standard_name  = ''
      real(rk)                     :: fill_value     = default_fill_value
      real(rk)                     :: minimum        = default_minimum
      real(rk)                     :: maximum        = default_maximum
      integer                      :: output_level   = output_level_default
      logical                      :: in_output      = .false.
      integer                      :: status         = status_not_registered
      type (type_dimension_pointer),allocatable :: dimensions(:)
      class (type_attribute), pointer :: first_attribute => null()
      integer,allocatable          :: extents(:)
      real(rk),pointer             :: data_0d        => null()
      real(rk),pointer             :: data_1d(:)     => null()
      real(rk),pointer             :: data_2d(:,:)   => null()
      real(rk),pointer             :: data_3d(:,:,:) => null()
      class (type_category_node),pointer :: category => null()
      type (type_field),pointer    :: next           => null()
   contains
      procedure :: has_dimension         => field_has_dimension
      procedure :: set_real_attribute    => field_set_real_attribute
      procedure :: set_integer_attribute => field_set_integer_attribute
      procedure :: set_string_attribute  => field_set_string_attribute
      procedure :: delete_attribute      => field_delete_attribute
      generic :: set_attribute           => set_real_attribute, set_integer_attribute, set_string_attribute
      procedure :: finalize              => field_finalize
   end type type_field

   type,abstract :: type_node
      class (type_node),pointer :: parent       => null()
      class (type_node),pointer :: first_child  => null()
      class (type_node),pointer :: next_sibling => null()
   contains
      procedure :: finalize => node_finalize
   end type

   type,extends(type_node) :: type_field_node
      type (type_field), pointer :: field => null()
   end type

   type,extends(type_node) :: type_category_node
      character(len=string_length) :: name = ''
      integer                      :: output_level = output_level_none
   contains
      procedure :: get_all_fields
      procedure :: has_fields
      procedure :: get_path => category_get_path
   end type

   integer, parameter :: hash_table_size = 256
   type type_dictionary_bin
      type (type_field), pointer :: first_field => null()
   end type

   type type_field_set_member
      type (type_field),            pointer :: field => null()
      type (type_field_set_member), pointer :: next  => null()
   end type

   type type_field_set
      type (type_field_set_member), pointer :: first => null()
   contains
      procedure :: add      => field_set_add
      procedure :: finalize => field_set_finalize
   end type

   type type_field_manager
      type (type_dimension), pointer :: first_dimension => null()

      type (type_dimension_pointer),allocatable :: prepend_dimensions(:)
      type (type_dimension_pointer),allocatable :: append_dimensions(:)

      type (type_dictionary_bin) :: field_table(hash_table_size)
      type (type_category_node) :: root
      integer                   :: nregistered = 0
   contains
      procedure :: initialize
      procedure :: finalize
      procedure :: register
      procedure :: find
      procedure :: list
      procedure :: send_data_0d
      procedure :: send_data_1d
      procedure :: send_data_2d
      procedure :: send_data_3d
      procedure :: send_data_by_name_0d
      procedure :: send_data_by_name_1d
      procedure :: send_data_by_name_2d
      procedure :: send_data_by_name_3d
      procedure :: select_for_output
      procedure :: select_category_for_output
      procedure :: register_dimension
      procedure :: find_dimension
      procedure :: find_category
      generic :: send_data => send_data_0d,send_data_1d,send_data_2d,send_data_3d,send_data_by_name_0d,send_data_by_name_1d,send_data_by_name_2d,send_data_by_name_3d
   end type type_field_manager

contains

   subroutine register_dimension(self,name,length,global_length,offset,id,newid)
      class (type_field_manager), intent(inout) :: self
      character(len=*),           intent(in)    :: name
      integer, optional,          intent(in)    :: length
      integer, optional,          intent(in)    :: global_length
      integer, optional,          intent(in)    :: offset
      integer, optional,          intent(in)    :: id
      integer, optional,          intent(out)   :: newid

      type (type_dimension), pointer :: dim

      if (name=='') call fatal_error('register_dimension','dimension name cannot be empty')

      ! Check whether dimension has already been registered.
      dim => self%first_dimension
      do while (associated(dim))
         if (dim%name==name) call fatal_error('register_dimension','dimension "'//trim(name)//'" has already been registered.')
         if (present(id)) then
            if (dim%id==id) call fatal_error('register_dimension','id specified for dimension '//trim(name)//' has already been assigned to '//trim(dim%name)//'.')
         end if
         dim => dim%next
      end do

      ! Create dimension object
      allocate(dim)
      dim%name = name
      if (present(length)) dim%length = length
      if (present(offset)) dim%offset = offset
      if (present(id)) then
         dim%id = id
      elseif (present(newid)) then
         newid = next_free_dimension_id(self)
         dim%id = newid
      end if
      dim%global_length = dim%length
      if (present(global_length)) dim%global_length = global_length

      select case (dim%id)
      case (id_dim_lon)
         dim%iterator = 'i'
      case (id_dim_lat)
         dim%iterator = 'j'
      case (id_dim_z)
         dim%iterator = 'k'
      case (id_dim_zi)
         dim%iterator = 'k1'
      end select

      ! Basic consistency checks
      if (dim%length<-1) call fatal_error('register_dimension','length for dimension '//trim(dim%name)//' must be -1 (unlimited) or more')
#if 0
      if (dim%offset<0)  call fatal_error('register_dimension','offset for dimension '//trim(dim%name)//' must be 0 or more')
#endif

      ! Prepend to dimension list.
      dim%next => self%first_dimension
      self%first_dimension => dim
   end subroutine register_dimension

   integer function next_free_dimension_id(self)
      class (type_field_manager), intent(in) :: self

      type (type_dimension), pointer :: dim

      next_free_dimension_id = id_dim_unused
      do
         dim => self%first_dimension
         do while (associated(dim))
            if (dim%id==next_free_dimension_id) exit
            dim => dim%next
         end do
         if (.not.associated(dim)) return
         next_free_dimension_id = next_free_dimension_id + 1
      end do
   end function next_free_dimension_id

   subroutine initialize(self,prepend_by_default,append_by_default)
      class (type_field_manager), intent(inout) :: self
      integer,optional,           intent(in)    :: prepend_by_default(:),append_by_default(:)

      integer :: i

      if (present(prepend_by_default)) then
         allocate(self%prepend_dimensions(size(prepend_by_default)))
         do i=1,size(prepend_by_default)
            self%prepend_dimensions(i)%p => find_dimension(self,prepend_by_default(i))
            if (.not.associated(self%prepend_dimensions(i)%p)) call fatal_error('initialize','Auto-prepend dimension has not been registered yet.')
         end do
      else
         allocate(self%prepend_dimensions(0))
      end if
      if (present(append_by_default)) then
         allocate(self%append_dimensions(size(append_by_default)))
         do i=1,size(append_by_default)
            self%append_dimensions(i)%p => find_dimension(self,append_by_default(i))
            if (.not.associated(self%append_dimensions(i)%p)) call fatal_error('initialize','Auto-append dimension has not been registered yet.')
         end do
      else
         allocate(self%append_dimensions(0))
      end if
   end subroutine initialize

   subroutine list(self)
      class (type_field_manager), intent(in) :: self

      character(256)             :: line
      integer                    :: ibin
      type (type_field), pointer :: field

      write(line,'(A8,4x,A12,4x,A40)') 'name','unit',adjustl('long_name')
      write(*,*) trim(line)
      write(line,'(A68)') '----------------------------------------------------------------'
      write(*,*) trim(line)
      do ibin=1,hash_table_size
         field => self%field_table(ibin)%first_field
         do while (associated(field))
            write(line,'(I2,2x,A15,2x,A15,2x,A45)') field%id,adjustl(field%name),adjustl(field%units),adjustl(field%long_name)
            write(*,*) trim(line)
!KB         write(*,*) field%dimensions
            field => field%next
         end do
      end do
      write (*,*) 'field tree:'
      call list_node(self%root,1)

      stop 'field_manager::list()'
   end subroutine list

   recursive subroutine list_node(category,depth)
      type (type_category_node), intent(in) :: category
      integer,                   intent(in) :: depth

      class (type_node), pointer :: node

      node => category%first_child
      do while (associated(node))
         select type (node)
         class is (type_category_node)
            write (*,*) repeat('  ',depth)//trim(node%name)
            call list_node(node,depth+1)
         class is (type_field_node)
            write (*,*) repeat('  ',depth)//trim(node%field%name)
         end select
         node => node%next_sibling
      end do
   end subroutine list_node

   subroutine finalize(self)
      class (type_field_manager), intent(inout) :: self

      integer                        :: ibin
      type (type_field),     pointer :: field, next_field
      type (type_dimension), pointer :: dim, next_dim

      do ibin=1,hash_table_size
         field => self%field_table(ibin)%first_field
         do while (associated(field))
            next_field => field%next
            call field%finalize()
            deallocate(field)
            field => next_field
         end do
         self%field_table(ibin)%first_field => null()
      end do

      dim => self%first_dimension
      do while (associated(dim))
         next_dim => dim%next
         deallocate(dim)
         dim => next_dim
      end do
      self%first_dimension => null()

      call self%root%finalize()

      if (allocated(self%prepend_dimensions)) deallocate(self%prepend_dimensions)
      if (allocated(self%append_dimensions )) deallocate(self%append_dimensions)

      self%nregistered = 0
   end subroutine finalize

   function find_dimension(self,dimid) result(dim)
      class (type_field_manager), intent(in) :: self
      integer,                    intent(in) :: dimid
      type (type_dimension), pointer         :: dim

      dim => self%first_dimension
      do while (associated(dim))
         if (dim%id==dimid) return
         dim => dim%next
      end do
   end function find_dimension

   function select_for_output(self,name) result(field)
      class (type_field_manager),intent(inout) :: self
      character(len=*), intent(in) :: name
      type (type_field), pointer :: field

      field => self%find(name,create=.true.)
      field%in_output = .true.
   end function select_for_output

   function select_category_for_output(self,name,output_level) result(category)
      class (type_field_manager),intent(inout) :: self
      character(len=*),          intent(in)    :: name
      integer,                   intent(in)    :: output_level
      class (type_category_node), pointer       :: category

      category => self%find_category(name,create=.true.)
      call activate(category)
   contains
      recursive subroutine activate(category)
         type (type_category_node), intent(inout) :: category
         class (type_node), pointer :: child
         category%output_level = max(category%output_level,output_level)
         child => category%first_child
         do while (associated(child))
            select type (child)
            class is (type_category_node)
               call activate(child)
            end select
            child => child%next_sibling
         end do
      end subroutine activate
   end function select_category_for_output

   subroutine field_set_add(self,field)
      class (type_field_set), intent(inout) :: self
      type (type_field), target             :: field

      type (type_field_set_member),pointer :: member
      type (type_field_set_member),pointer :: last

      last => null()
      member => self%first
      do while (associated(member))
         if (associated(member%field,field)) return
         last => member
         member => member%next
      end do
      allocate(member)
      member%field => field
      if (associated(last)) then
         last%next => member
      else
         self%first => member
      end if
   end subroutine field_set_add

   subroutine field_set_finalize(self)
      class (type_field_set), intent(inout) :: self

      type (type_field_set_member),pointer :: member,next_member

      member => self%first
      do while (associated(member))
         next_member => member%next
         deallocate(member)
         member => next_member
      end do
      self%first => null()
   end subroutine field_set_finalize

   recursive subroutine get_all_fields(self,set,output_level)
      class (type_category_node), intent(inout) :: self
      type (type_field_set),      intent(inout) :: set
      integer,                    intent(in)    :: output_level
      class (type_node), pointer :: child

      child => self%first_child
      do while (associated(child))
         select type (child)
         class is (type_category_node)
            call get_all_fields(child,set,output_level)
         class is (type_field_node)
            if (child%field%output_level<=output_level) call set%add(child%field)
         end select
         child => child%next_sibling
      end do
   end subroutine get_all_fields

   recursive logical function has_fields(self)
      class (type_category_node), intent(inout) :: self

      class (type_node), pointer :: child

      has_fields = .true.
      child => self%first_child
      do while (associated(child))
         select type (child)
         class is (type_category_node)
            if (child%has_fields()) return
         class is (type_field_node)
            return
         end select
         child => child%next_sibling
      end do
      has_fields = .false.
   end function has_fields

   function find(self,name,create) result(field)
      class (type_field_manager),intent(inout) :: self
      character(len=*),intent(in) :: name
      logical,optional,intent(in) :: create
      type (type_field), pointer :: field

      integer :: ibin
      logical :: create_eff

      ibin = mod(hash(trim(name)),hash_table_size)+1
      field => self%field_table(ibin)%first_field
      do while (associated(field))
         if (field%name==name) return
         field => field%next
      end do

      create_eff = .false.
      if (present(create)) create_eff = create
      if (create_eff) then
         allocate(field)
         field%name = name
         field%next => self%field_table(ibin)%first_field
         self%field_table(ibin)%first_field => field
      end if
   end function find

   subroutine register(self, name, units, long_name, standard_name, fill_value, minimum, maximum, dimensions, data0d, data1d, data2d, data3d, no_default_dimensions, category, output_level, coordinate_dimension, used, field)
      class (type_field_manager),intent(inout) :: self
      character(len=*),          intent(in)    :: name, units, long_name
      character(len=*),optional, intent(in)    :: standard_name
      real(rk),        optional, intent(in)    :: fill_value, minimum, maximum
      integer,         optional, intent(in)    :: dimensions(:)
      real(rk),        optional, target        :: data0d,data1d(:),data2d(:,:),data3d(:,:,:)
      logical,         optional, intent(in)    :: no_default_dimensions
      character(len=*),optional, intent(in)    :: category
      integer,         optional, intent(in)    :: output_level
      integer,         optional, intent(in)    :: coordinate_dimension
      logical,         optional, intent(out)   :: used
      type (type_field),optional,pointer       :: field

      type (type_field),     pointer :: field_
      type (type_dimension), pointer :: dim
      logical :: no_default_dimensions_
      integer :: i,n

      if (name=='') call fatal_error('field_manager%register','name cannot be empty.')
      if (long_name=='') call fatal_error('field_manager%register','long_name cannot be empty.')

      ! Find existing field_ (a placeholder with status_not_registered may have been created by a call to select_for_output) or create new one.
      field_ => self%find(name,create=.true.)
      if (field_%status>status_not_registered) call fatal_error('field_manager%register','field with name "'//trim(name)//'" has already been registered.')
      field_%status = status_registered_no_data

      ! Increment number of registered fields
      self%nregistered = self%nregistered + 1

      ! Copy field_ configuration
      field_%id = self%nregistered
      field_%name = name
      field_%units = units
      field_%long_name = long_name
      if (present(standard_name)) field_%standard_name = standard_name
      if (present(fill_value)) field_%fill_value = fill_value
      if (present(minimum)) field_%minimum = minimum
      if (present(maximum)) field_%maximum = maximum
      if (present(output_level)) field_%output_level = output_level
      if (present(coordinate_dimension)) then
         dim => find_dimension(self,coordinate_dimension)
         if (.not.associated(dim)) call fatal_error('field_manager%register','coordinate dimension of variable '//trim(field_%name)//' has not been registered yet.')
         dim%coordinate => field_
      end if

      no_default_dimensions_ = .false.
      if (present(no_default_dimensions)) no_default_dimensions_ = no_default_dimensions
      if (no_default_dimensions_) then
         ! Use actual provided dimensions only (no prepend/append)
         if (present(dimensions)) then
            allocate(field_%dimensions(size(dimensions)))
            do i=1,size(dimensions)
               field_%dimensions(i)%p => find_dimension(self,dimensions(i))
               if (.not.associated(field_%dimensions(i)%p)) &
                  call fatal_error('field_manager%register','Dimension of variable '//trim(field_%name)//' has not been registered yet.')
            end do
         else
            allocate(field_%dimensions(0))
         end if
      else
         ! Also prepend/append implicit dimensions
         if (present(dimensions)) then
            allocate(field_%dimensions(size(self%prepend_dimensions)+size(dimensions)+size(self%append_dimensions)))
            do i=1,size(dimensions)
               field_%dimensions(size(self%prepend_dimensions)+i)%p => find_dimension(self,dimensions(i))
               if (.not.associated(field_%dimensions(size(self%prepend_dimensions)+i)%p)) &
                  call fatal_error('field_manager%register','Dimension of variable '//trim(field_%name)//' has not been registered yet.')
            end do
         else
            allocate(field_%dimensions(size(self%prepend_dimensions)+size(self%append_dimensions)))
         end if
         field_%dimensions(:size(self%prepend_dimensions)) = self%prepend_dimensions
         field_%dimensions(size(field_%dimensions)-size(self%append_dimensions)+1:) = self%append_dimensions
      end if

      ! Determine extents of field_ (excluding singleton dimensions)
      n = 0
      do i=1,size(field_%dimensions)
         if (field_%dimensions(i)%p%length>1) n = n + 1
      end do
      allocate(field_%extents(n))
      n = 0
      do i=1,size(field_%dimensions)
         if (field_%dimensions(i)%p%length>1) then
            n = n + 1
            field_%extents(n) = field_%dimensions(i)%p%length
         end if
      end do

      call add_field_to_tree(self,field_,category)

      ! Note: the "in_output" flag can have been set by a call to select_for_output (typically from the output manager),
      ! even before the actual variable is registered with the field_ manager.
      if (present(used)) used = field_%in_output

      if (present(data0d)) call self%send_data_0d(field_,data0d)
      if (present(data1d)) call self%send_data_1d(field_,data1d)
      if (present(data2d)) call self%send_data_2d(field_,data2d)
      if (present(data3d)) call self%send_data_3d(field_,data3d)

      if (present(field)) field => field_
   end subroutine register

   logical function field_has_dimension(self,id)
      class (type_field),intent(in) :: self
      integer,           intent(in) :: id

      integer :: i

      field_has_dimension = .true.
      do i=1,size(self%dimensions)
         if (self%dimensions(i)%p%id==id) return
      end do
      field_has_dimension = .false.
   end function field_has_dimension

   subroutine field_delete_attribute(self,name)
      class (type_field),intent(inout) :: self
      character(len=*),  intent(in)    :: name

      class (type_attribute),pointer :: attribute, previous_attribute

      previous_attribute => null()
      attribute => self%first_attribute
      do while (associated(attribute))
         if (attribute%name==name) then
            if (associated(previous_attribute)) then
               previous_attribute%next => attribute%next
            else
               self%first_attribute => attribute%next
            end if
            deallocate(attribute)
            return
         end if
         previous_attribute => attribute
         attribute => attribute%next
      end do
   end subroutine field_delete_attribute

   subroutine field_set_attribute(self,name,attribute)
      class (type_field),    intent(inout)        :: self
      character(len=*),      intent(in)           :: name
      class (type_attribute),intent(inout),target :: attribute

      call self%delete_attribute(name)
      attribute%name = name
      attribute%next => self%first_attribute
      self%first_attribute => attribute
   end subroutine field_set_attribute

   subroutine field_set_real_attribute(self,name,value)
      class (type_field),intent(inout) :: self
      character(len=*),  intent(in)    :: name
      real(rk),          intent(in)    :: value

      class (type_real_attribute),pointer :: attribute

      allocate(attribute)
      attribute%value = value
      call field_set_attribute(self,name,attribute)
   end subroutine field_set_real_attribute

   subroutine field_set_integer_attribute(self,name,value)
      class (type_field),intent(inout) :: self
      character(len=*),  intent(in)    :: name
      integer,           intent(in)    :: value

      class (type_integer_attribute),pointer :: attribute

      allocate(attribute)
      attribute%value = value
      call field_set_attribute(self,name,attribute)
   end subroutine field_set_integer_attribute

   subroutine field_set_string_attribute(self,name,value)
      class (type_field),intent(inout) :: self
      character(len=*),  intent(in)    :: name
      character(len=*),  intent(in)    :: value

      class (type_string_attribute),pointer :: attribute

      allocate(attribute)
      attribute%value = value
      call field_set_attribute(self,name,attribute)
   end subroutine field_set_string_attribute

   subroutine field_finalize(self)
      class (type_field),intent(inout) :: self

      class (type_attribute),pointer :: attribute, next_attribute

      deallocate(self%dimensions)
      deallocate(self%extents)

      attribute => self%first_attribute
      do while (associated(attribute))
         next_attribute => attribute%next
         deallocate(attribute)
         attribute => next_attribute
      end do
      self%first_attribute => null()
   end subroutine field_finalize

   subroutine add_field_to_tree(self,field,category)
      class (type_field_manager),intent(inout),target :: self
      type (type_field), target :: field
      character(len=*),intent(in),optional :: category

      class (type_category_node),pointer :: parent
      class (type_node),         pointer :: node

      ! Find parent node
      parent => self%root
      if (present(category)) parent => self%find_category(category,create=.true.)

      ! If field has not been selected for output yet, do so if its output_level does not exceed that the parent category.
      if (.not.field%in_output) field%in_output = field%output_level<=parent%output_level
      if (.not.associated(field%category)) field%category => parent

      ! Create node with field pointer and add to children of parent.
      allocate(type_field_node::node)
      select type (node)
      class is (type_field_node)
         node%field => field
      end select
      call add_to_category(parent,node)
   end subroutine add_field_to_tree

   function find_category(self,name,create) result(category)
      class (type_field_manager),intent(inout),target :: self
      character(len=*),          intent(in) :: name
      logical,optional,          intent(in) :: create
      class (type_category_node), pointer :: category

      class (type_node),         pointer :: node
      character(len=string_length)       :: remaining_path
      integer                            :: istop
      logical                            :: done
      logical                            :: create_

      category => self%root
      remaining_path = name
      do
         istop = index(remaining_path,'/')-1
         done = istop==-1
         if (done) istop = len_trim(remaining_path)

         if (istop>0) then
            ! First try to find existing parent
            node => category%first_child
            do while (associated(node))
               select type (node)
               class is (type_category_node)
                  if (node%name==remaining_path(:istop)) exit
               end select
               node => node%next_sibling
            end do

            ! If parent does not exist yet, create it if allowed to do so.
            if (.not.associated(node)) then
               create_ = .false.
               if (present(create)) create_ = create
               if (.not.create_) return

               allocate(type_category_node::node)
               select type (node)
               class is (type_category_node)
                  node%name = remaining_path(:istop)
                  node%output_level = category%output_level
               end select
               call add_to_category(category,node)
            end if

            ! Update current path position.
            select type (node)
            class is (type_category_node)
               category => node
            end select
         end if

         ! If no more path components, we're done. Otherwise, strip the component we processed and continue.
         if (done) return
         remaining_path = remaining_path(istop+2:)
      end do
   end function find_category

   subroutine add_to_category(parent,node)
      type (type_category_node), target, intent(inout) :: parent
      class (type_node),         target                :: node

      class (type_node),         pointer       :: previous_sibling

      if (associated(parent%first_child)) then
         previous_sibling => parent%first_child
         do while (associated(previous_sibling%next_sibling))
            previous_sibling => previous_sibling%next_sibling
         end do
         previous_sibling%next_sibling => node
      else
         parent%first_child => node
      end if
      node%parent => parent
   end subroutine add_to_category

   subroutine send_data_by_name_0d(self, name, data)
      class (type_field_manager),intent(inout) :: self
      character(len=*),          intent(in)    :: name
      real(rk),target                          :: data

      type (type_field), pointer :: field

      field => self%find(name)
      if (.not.associated(field)) call fatal_error('send_data_by_name_0d','Field "'//trim(name)//'" has not been registered.')
      call self%send_data_0d(field,data)
   end subroutine send_data_by_name_0d

   subroutine send_data_by_name_1d(self, name, data)
      class (type_field_manager),intent(inout) :: self
      character(len=*),          intent(in)    :: name
      real(rk),target                          :: data(:)

      type (type_field), pointer :: field

      field => self%find(name)
      if (.not.associated(field)) call fatal_error('send_data_by_name_1d','Field "'//trim(name)//'" has not been registered.')
      call self%send_data_1d(field,data)
   end subroutine send_data_by_name_1d

   subroutine send_data_by_name_2d(self, name, data)
      class (type_field_manager),intent(inout) :: self
      character(len=*),          intent(in)    :: name
      real(rk),target                          :: data(:,:)

      type (type_field), pointer :: field

      field => self%find(name)
      if (.not.associated(field)) call fatal_error('send_data_by_name_2d','Field "'//trim(name)//'" has not been registered.')
      call self%send_data_2d(field,data)
   end subroutine send_data_by_name_2d

   subroutine send_data_by_name_3d(self, name, data)
      class (type_field_manager),intent(inout) :: self
      character(len=*),          intent(in)    :: name
      real(rk),target                          :: data(:,:,:)

      type (type_field), pointer :: field

      field => self%find(name)
      if (.not.associated(field)) call fatal_error('send_data_by_name_3d','Field "'//trim(name)//'" has not been registered.')
      call self%send_data_3d(field,data)
   end subroutine send_data_by_name_3d

   subroutine check_sent_data(field,extents)
      type (type_field), intent(inout) :: field
      integer,           intent(in)    :: extents(:)

      integer                          :: i
      character(len=8)                 :: str1,str2,str3

      ! Check array rank
      if (size(extents)/=size(field%extents)) then
         write (str1,'(i0)') size(extents)
         write (str2,'(i0)') size(field%extents)
         call fatal_error('check_sent_data',trim(str1)//'D data provided for '//trim(field%name)//', but this field should have '//trim(str2)//' non-singleton dimensions.')
      end if

      ! Check array extents
      do i=1,size(extents)
         if (extents(i)/=field%extents(i)) then
            write (str1,'(i0)') i
            write (str2,'(i0)') extents(i)
            write (str3,'(i0)') field%extents(i)
            call fatal_error('check_sent_data', 'Field '//trim(field%name)//', dimension '//trim(str1)//': &
               &extents of provided data ('//trim(str2)//') does not match expected value '//trim(str3)//'.')
         end if
      end do

      if (field%status==status_registered_with_data) call fatal_error('check_sent_data','Data for field "'//trim(field%name)//'" have already been provided.')
      field%status = status_registered_with_data
   end subroutine check_sent_data

   subroutine send_data_0d(self, field, data)
      class (type_field_manager),intent(inout) :: self
      type (type_field),         intent(inout) :: field
      real(rk),target                          :: data
      call check_sent_data(field,shape(data))
      field%data_0d => data
   end subroutine send_data_0d

   subroutine send_data_1d(self, field, data)
      class (type_field_manager),intent(inout) :: self
      type (type_field),         intent(inout) :: field
      real(rk),target                          :: data(:)
      if (size(data,1)==1) then
         ! Singleton dimension - send scalar value instead
         call send_data_0d(self,field,data(1))
      else
         call check_sent_data(field,shape(data))
         field%data_1d => data
      end if
   end subroutine send_data_1d

   subroutine send_data_2d(self, field, data)
      class (type_field_manager),intent(inout) :: self
      type (type_field),         intent(inout) :: field
      real(rk),target                          :: data(:,:)
      if (size(data,1)==1) then
         call send_data_1d(self,field,data(1,:))
      elseif (size(data,2)==1) then
         call send_data_1d(self,field,data(:,1))
      else
         call check_sent_data(field,shape(data))
         field%data_2d => data
      end if
   end subroutine send_data_2d

   subroutine send_data_3d(self, field, data)
      class (type_field_manager),intent(inout) :: self
      type (type_field),         intent(inout) :: field
      real(rk),target                          :: data(:,:,:)
      if (size(data,1)==1) then
         call send_data_2d(self,field,data(1,:,:))
      elseif (size(data,2)==1) then
         call send_data_2d(self,field,data(:,1,:))
      elseif (size(data,3)==1) then
         call send_data_2d(self,field,data(:,:,1))
      else
         call check_sent_data(field,shape(data))
         field%data_3d => data
      end if
   end subroutine send_data_3d

   subroutine fatal_error(location,error)
      character(len=*),intent(in) :: location,error

      FATAL trim(location)//': '//trim(error)
      stop 'field_manager::fatal_error'
   end subroutine

   recursive subroutine node_finalize(self)
      class (type_node), intent(inout) :: self

      class (type_node), pointer :: child, next_child

      child => self%first_child
      do while (associated(child))
         next_child => child%next_sibling
         call child%finalize()
         deallocate(child)
         child => next_child
      end do
      self%first_child => null()
   end subroutine node_finalize

   function category_get_path(self) result(path)
      class (type_category_node), target, intent(in)  :: self
      character(len=256) :: path

      class (type_node), pointer :: current

      path = trim(self%name)
      current => self%parent
      do while (associated(current))
         select type (current)
         class is (type_category_node)
            path = trim(current%name)//'/'//trim(path)
         end select
         current => current%parent
      end do
   end function category_get_path

   integer function hash(str)
      character(len=*), intent(in) :: str

      integer :: i
      character, dimension(len(str)) :: tmp

      do i=1,len(str)
       tmp(i) = str(i:i)
      end do
      hash = sum(ichar(tmp))
   end function

end module field_manager

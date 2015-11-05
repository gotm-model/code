#include"cppdefs.h"

module field_manager

   implicit none

   ! Public subroutine and functions
   public type_field_manager

   ! Public data types and variables
   public type_node, type_field, type_field_node, type_category_node, type_dimension

   ! Public parameters
   public string_length,default_fill_value,default_minimum,default_maximum

   private

   integer,parameter :: string_length = 256
   integer,parameter :: nmaxdims = 10
   integer,parameter :: rk = kind(_ONE_)

   integer, parameter, public :: id_dim_lon  = 1
   integer, parameter, public :: id_dim_lat  = 2
   integer, parameter, public :: id_dim_z    = 3
   integer, parameter, public :: id_dim_z1   = 4
   integer, parameter, public :: id_dim_time = 5

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

   integer            :: counter=0

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
      integer,allocatable          :: extents(:)
      real(rk),pointer             :: data_0d        => null()
      real(rk),pointer             :: data_1d(:)     => null()
      real(rk),pointer             :: data_2d(:,:)   => null()
      real(rk),pointer             :: data_3d(:,:,:) => null()
      type (type_field),pointer    :: next           => null()
   end type type_field

   type,abstract :: type_node
      class (type_node),pointer :: first_child  => null()
      class (type_node),pointer :: next_sibling => null()
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
   end type

   type type_field_manager
      type (type_dimension), pointer :: first_dimension => null()

      type (type_dimension_pointer),allocatable :: prepend_dimensions(:)
      type (type_dimension_pointer),allocatable :: append_dimensions(:)

      type (type_field),pointer :: first_field => null()
      type (type_category_node) :: root
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
      procedure :: select_for_output
      procedure :: select_category_for_output
      procedure :: register_dimension
      procedure :: find_dimension
      generic :: send_data => send_data_0d,send_data_1d,send_data_2d,send_data_3d,send_data_by_name_0d,send_data_by_name_1d
   end type type_field_manager

contains

   subroutine register_dimension(self,name,length,global_length,offset,id)
      class (type_field_manager), intent(inout) :: self
      character(len=*),           intent(in)    :: name
      integer, optional,          intent(in)    :: length
      integer, optional,          intent(in)    :: global_length
      integer, optional,          intent(in)    :: offset
      integer, optional,          intent(in)    :: id

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
      if (present(id)) dim%id = id
      dim%global_length = dim%length
      if (present(global_length)) dim%global_length = global_length

      select case (dim%id)
      case (id_dim_lon)
         dim%iterator = 'i'
      case (id_dim_lat)
         dim%iterator = 'j'
      case (id_dim_z,id_dim_z1)
         dim%iterator = 'k'
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

      type (type_field), pointer :: field, next_field

      character(256) :: line
      field => self%first_field
      write(line,'(A8,4x,A12,4x,A40)') 'name','unit',adjustl('long_name')
      write(*,*) trim(line)
      write(line,'(A68)') '----------------------------------------------------------------'
      write(*,*) trim(line)
      do while (associated(field))
         write(line,'(I2,2x,A15,2x,A15,2x,A45)') field%id,adjustl(field%name),adjustl(field%units),adjustl(field%long_name)
         write(*,*) trim(line)
!KB         write(*,*) field%dimensions
         next_field => field%next
         field => next_field
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

      type (type_field), pointer :: field, next_field

      field => self%first_field
      do while (associated(field))
         next_field => field%next
         deallocate(field)
         field => next_field
      end do
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

      category => find_category(self,name,create=.true.)
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

   recursive subroutine get_all_fields(self,list,output_level)
      class (type_category_node), intent(inout) :: self
      type (type_category_node),  intent(inout) :: list
      integer,                    intent(in)    :: output_level
      class (type_node), pointer :: child, newnode

      child => self%first_child
      do while (associated(child))
         select type (child)
         class is (type_category_node)
            call get_all_fields(child,list,output_level)
         class is (type_field_node)
            if (child%field%output_level<=output_level) then
               allocate(type_field_node::newnode)
               select type (newnode)
               class is (type_field_node)
                  newnode%field => child%field
               end select
               call add_to_category(list,newnode)
            end if
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

      logical :: create_eff

      field => self%first_field
      do while (associated(field))
         if (field%name==name) return
         field => field%next
      end do

      create_eff = .false.
      if (present(create)) create_eff = create
      if (create_eff) then
         allocate(field)
         field%name = name
         field%next => self%first_field
         self%first_field => field
      end if
   end function find

   subroutine register(self, name, units, long_name, standard_name, fill_value, minimum, maximum, dimensions, data0d, data1d, data2d, data3d, no_default_dimensions, category, output_level, coordinate_dimension, used)
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

      type (type_field),     pointer :: field
      type (type_dimension), pointer :: dim
      logical :: no_default_dimensions_
      integer :: i,n

      if (name=='') call fatal_error('add_field','name cannot be empty.')
      if (long_name=='') call fatal_error('add_field','long_name cannot be empty.')

      ! Find existing field (possible created by select_for_output) or create new one.
      field => self%find(name,create=.true.)
      if (field%status>0) call fatal_error('add_field','Field with name "'//trim(name)//'" has already been registered.')
      field%status = status_registered_no_data

      ! Copy field configuration
      counter = counter + 1
      field%id   = counter
      field%name = name
      field%units = units
      field%long_name = long_name
      if (present(standard_name)) field%standard_name = standard_name
      if (present(fill_value)) field%fill_value = fill_value
      if (present(minimum)) field%minimum = minimum
      if (present(maximum)) field%maximum = maximum
      if (present(output_level)) field%output_level = output_level
      if (present(coordinate_dimension)) then
         dim => find_dimension(self,coordinate_dimension)
         if (.not.associated(dim)) call fatal_error('register','coordinate dimension of variable '//trim(field%name)//' has not been registered yet.')
         dim%coordinate => field
      end if

      no_default_dimensions_ = .false.
      if (present(no_default_dimensions)) no_default_dimensions_ = no_default_dimensions
      if (no_default_dimensions_) then
         ! Use actual provided dimensions only (no prepend/append)
         if (present(dimensions)) then
            allocate(field%dimensions(size(dimensions)))
            do i=1,size(dimensions)
               field%dimensions(i)%p => find_dimension(self,dimensions(i))
               if (.not.associated(field%dimensions(i)%p)) &
                  call fatal_error('register','Dimension of variable '//trim(field%name)//' has not been registered yet.')
            end do
         else
            allocate(field%dimensions(0))
         end if
      else
         ! Also prepend/append implicit dimensions
         if (present(dimensions)) then
            allocate(field%dimensions(size(self%prepend_dimensions)+size(dimensions)+size(self%append_dimensions)))
            do i=1,size(dimensions)
               field%dimensions(size(self%prepend_dimensions)+i)%p => find_dimension(self,dimensions(i))
               if (.not.associated(field%dimensions(size(self%prepend_dimensions)+i)%p)) &
                  call fatal_error('register','Dimension of variable '//trim(field%name)//' has not been registered yet.')
            end do
         else
            allocate(field%dimensions(size(self%prepend_dimensions)+size(self%append_dimensions)))
         end if
         field%dimensions(:size(self%prepend_dimensions)) = self%prepend_dimensions
         field%dimensions(size(field%dimensions)-size(self%append_dimensions)+1:) = self%append_dimensions
      end if

      ! Determine extents of field (excluding singleton dimensions)
      n = 0
      do i=1,size(field%dimensions)
         if (field%dimensions(i)%p%length>1) n = n + 1
      end do
      allocate(field%extents(n))
      n = 0
      do i=1,size(field%dimensions)
         if (field%dimensions(i)%p%length>1) then
            n = n + 1
            field%extents(n) = field%dimensions(i)%p%length
         end if
      end do

      call add_field_to_tree(self,field,category)

      ! Note: the "in_output" flag can have been set by a call to select_for_output (typically from the output manager),
      ! even before the actual variable is registered with the field manager.
      if (present(used)) used = field%in_output

      if (present(data0d)) call self%send_data_0d(field,data0d)
      if (present(data1d)) call self%send_data_1d(field,data1d)
      if (present(data2d)) call self%send_data_2d(field,data2d)
      if (present(data3d)) call self%send_data_3d(field,data3d)
   end subroutine register

   subroutine add_field_to_tree(self,field,category)
      class (type_field_manager),intent(inout),target :: self
      type (type_field), target :: field
      character(len=*),intent(in),optional :: category

      class (type_category_node),pointer :: parent
      class (type_node),         pointer :: node

      ! Find parent node
      parent => self%root
      if (present(category)) parent => find_category(self,category,create=.true.)

      ! If field has not been selected for output yet, do so if its output_level does not exceed that the parent category.
      if (.not.field%in_output) field%in_output = field%output_level<=parent%output_level

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
      type (type_category_node), intent(inout) :: parent
      class (type_node),         target        :: node

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

   subroutine check_sent_data(field,extents)
      type (type_field), intent(inout) :: field
      integer,           intent(in)    :: extents(:)

      integer                          :: i
      character(len=2)                 :: str1,str2,str3

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
            call fatal_error('check_sent_data', 'Field '//trim(field%name)//', dimension  '//trim(str1)//': &
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

end module field_manager

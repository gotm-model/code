module text_output

   use field_manager
   use output_manager_core

   implicit none

   public type_text_file

   private

   character,        parameter :: separator = char(9)
   character(len=4), parameter :: extension = '.txt'

   type,abstract :: type_single_text_file
      character(len=max_path)                :: path = ''
      character(len=max_path)                :: title = ''
      integer                                :: unit = -1
      class (type_single_text_file), pointer :: next => null()
   contains
      procedure (host_write_header), deferred :: write_header
      procedure (host_write_data),   deferred :: write_data
   end type

   abstract interface
      subroutine host_write_header(self)
         import type_single_text_file
         class (type_single_text_file),intent(in) :: self
      end subroutine
   end interface

   abstract interface
      subroutine host_write_data(self,timestr)
         import type_single_text_file
         class (type_single_text_file),intent(in) :: self
         character(len=*),             intent(in) :: timestr
      end subroutine
   end interface

   type type_scalar
      class (type_output_field), pointer :: field => null()
      real(rk),                  pointer :: value => null()
      type (type_scalar),        pointer :: next  => null()
   end type

   type,extends(type_single_text_file) :: type_single_text_file_with_scalars
      type (type_scalar),pointer :: first_field => null()
   contains
      procedure :: write_header => single_text_file_with_scalars_write_header
      procedure :: write_data   => single_text_file_with_scalars_write_data
   end type

   type,extends(type_single_text_file) :: type_single_text_file_with_1d_variable
      class (type_output_field), pointer :: field     => null()
      real(rk),                  pointer :: values(:) => null()
   contains
      procedure :: write_header => single_text_file_with_1d_variable_write_header
      procedure :: write_data   => single_text_file_with_1d_variable_write_data
   end type

   type,extends(type_file) :: type_text_file
      class (type_single_text_file), pointer :: first_file => null()
   contains
      procedure :: initialize
      procedure :: save
      procedure :: finalize
   end type

contains

   subroutine initialize(self)
      class (type_text_file),intent(inout) :: self

      integer                                                 :: ios
      class (type_output_field),                      pointer :: output_field
      class (type_single_text_file_with_scalars),     pointer :: scalar_file
      class (type_single_text_file),                  pointer :: current_file
      class (type_single_text_file_with_1d_variable), pointer :: file_with_1d_data
      type (type_scalar),                             pointer :: scalar_field

      ! Create file to write all 0d variables to
      allocate(scalar_file)

      output_field => self%first_field
      do while (associated(output_field))
         if (associated(output_field%data_1d)) then
            ! 1D variable - create separate file for it
            allocate(file_with_1d_data)
            file_with_1d_data%field => output_field
            file_with_1d_data%values => output_field%data_1d
            file_with_1d_data%path = trim(self%path)//'_'//trim(file_with_1d_data%field%output_name)//trim(self%postfix)//extension
            file_with_1d_data%title = self%title
            file_with_1d_data%next => self%first_file
            self%first_file => file_with_1d_data
         elseif (associated(output_field%data_0d)) then
            ! 0D variable - add to global file with all scalars
            if (.not.associated(scalar_file%first_field)) then
               ! First field in list - create head of list.
               allocate(scalar_file%first_field)
               scalar_field => scalar_file%first_field
            else
               ! Not first field in list - find tail of list and append to it.
               scalar_field => scalar_file%first_field
               do while (associated(scalar_field%next))
                  scalar_field => scalar_field%next
               end do
               allocate(scalar_field%next)
               scalar_field => scalar_field%next
            end if
            scalar_field%field => output_field
            scalar_field%value => output_field%data_0d
         else
            call host%log_message('WARNING: in output file "'//trim(self%path)//'", skipping variable "'//trim(output_field%output_name) &
               //'" because it has more than one non-singleton dimension. Currently only 0D and 1D variables are supported in text output.')
         end if
         output_field => output_field%next
      end do

      ! If we have one or more 0d fields to write, add the file with scalars to our list.
      if (associated(scalar_file%first_field)) then
         scalar_file%path = trim(self%path)//trim(self%postfix)//extension
         scalar_file%title = self%title
         scalar_file%next => self%first_file
         self%first_file => scalar_file
      else
         deallocate(scalar_file)
      end if

      if (.not.associated(self%first_file)) then
         call host%log_message('NOTE: output "'//trim(self%path)//trim(self%postfix)//'" will not be created because it would contain no data.')
         return
      end if

      current_file => self%first_file
      do while (associated(current_file))
         ! Open file
         current_file%unit = get_free_unit(1000,10000)
         open(unit=current_file%unit,file=current_file%path,action='write',status='replace',iostat=ios)
         if (ios/=0) call host%fatal_error('type_text_file::initialize','Failed to open "'//trim(current_file%path)//'" for writing.')

         ! Write header
         call current_file%write_header()

         current_file => current_file%next
      end do
   end subroutine initialize

   subroutine save(self,julianday,secondsofday)
      class (type_text_file),intent(inout) :: self
      integer,               intent(in)    :: julianday,secondsofday

      character(len=19)                      :: timestr
      class (type_single_text_file), pointer :: current_file

      call write_time_string(julianday,secondsofday,timestr)
      current_file => self%first_file
      do while (associated(current_file))
         call current_file%write_data(timestr)
         current_file => current_file%next
      end do
   end subroutine save

   subroutine finalize(self)
      class (type_text_file),intent(inout) :: self

      class (type_single_text_file), pointer :: current_file

      current_file => self%first_file
      do while (associated(current_file))
         close(current_file%unit)
         current_file => current_file%next
      end do
   end subroutine finalize

   subroutine single_text_file_with_scalars_write_header(self)
      class (type_single_text_file_with_scalars),intent(in) :: self

      type (type_scalar),pointer :: scalar

      ! Header (three lines: simulation title, variable short names, variable long names + units)
      write(self%unit,fmt='(''# '',A)') trim(self%title)
      write(self%unit,fmt='(''# '',A)',advance='NO') 'time'
      scalar => self%first_field
      do while (associated(scalar))
         write(self%unit,fmt='(A,A)',advance='NO') separator,trim(scalar%field%output_name)
         scalar => scalar%next
      end do
      write(self%unit,*)
      write(self%unit,fmt='(''# '',A)',advance='NO') 'time'
      scalar => self%first_field
      do while (associated(scalar))
         write(self%unit,fmt='(A,A,'' ('',A,'')'')',advance='NO') separator,trim(scalar%field%source%long_name),trim(scalar%field%source%units)
         scalar => scalar%next
      end do
      write(self%unit,*)
   end subroutine single_text_file_with_scalars_write_header

   subroutine single_text_file_with_scalars_write_data(self,timestr)
      class (type_single_text_file_with_scalars),intent(in) :: self
      character(len=*),                          intent(in) :: timestr

      type (type_scalar),pointer :: scalar

      write (self%unit,fmt='(A)',advance='NO') timestr
      scalar => self%first_field
      do while (associated(scalar))
         write (self%unit,fmt='("'//separator//'",G0.8)',advance='NO') scalar%value
         scalar => scalar%next
      end do
      write(self%unit,*)
   end subroutine single_text_file_with_scalars_write_data

   subroutine single_text_file_with_1d_variable_write_header(self)
      class (type_single_text_file_with_1d_variable),intent(in) :: self

      integer :: i
      type (type_dimension),pointer :: dim

      do i=1,size(self%field%source%dimensions)
         if (self%field%source%dimensions(i)%p%length>1) dim => self%field%source%dimensions(i)%p
      end do

      ! Header (three lines: simulation title, variable short names, variable long names + units)
      write(self%unit,fmt='(''# '',A)') trim(self%title)
      write(self%unit,fmt='(''# '',A)',advance='NO') 'time'
      do i=1,size(self%values)
         write(self%unit,fmt='(A,A,A,A,A,I0)',advance='NO') separator,trim(self%field%output_name),'@',trim(dim%name),'=',i
      end do
      write(self%unit,*)
      write(self%unit,fmt='(''# '',A)',advance='NO') 'time'
      do i=1,size(self%values)
         write(self%unit,fmt='(A,A,'' ('',A,'')'')',advance='NO') separator,trim(self%field%source%long_name),trim(self%field%source%units)
      end do
      write(self%unit,*)
   end subroutine single_text_file_with_1d_variable_write_header

   subroutine single_text_file_with_1d_variable_write_data(self,timestr)
      class (type_single_text_file_with_1d_variable),intent(in) :: self
      character(len=*),                              intent(in) :: timestr

      write (self%unit,fmt='(A,"'//separator//'")',advance='NO') timestr
      write (self%unit,fmt='(*(G0.8,:,"'//separator//'"))') self%values
   end subroutine single_text_file_with_1d_variable_write_data

   function get_free_unit(start,stop) result(unit)
      integer, intent(in) :: start,stop
      integer :: unit

      logical :: opened

      do unit=start,stop
         inquire(unit=unit,opened=opened)
         if (.not.opened) return
      end do
      unit = -1
   end function get_free_unit

end module text_output

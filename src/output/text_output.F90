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
   end type

   type,extends(type_single_text_file) :: type_single_text_file_with_scalars
      type (type_scalar),allocatable :: fields(:)
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
      integer                                                 :: nscalar

      nscalar = 0
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
            ! 0D variable - just count these variables for now.
            nscalar = nscalar + 1
         else
            call host%log_message('WARNING: in output file "'//trim(self%path)//'", skipping variable "'//trim(output_field%output_name) &
               //'" because it has more than one non-singleton dimension. Currently only 0D and 1D variables are supported in text output.')
         end if
         output_field => output_field%next
      end do

      ! If we have one or more 0d fields to write, add the file with scalars to our list.
      if (nscalar>0) then
         allocate(scalar_file)
         scalar_file%path = trim(self%path)//trim(self%postfix)//extension
         scalar_file%title = self%title
         allocate(scalar_file%fields(nscalar))
         nscalar = 0
         output_field => self%first_field
         do while (associated(output_field))
            if (associated(output_field%data_0d)) then
               nscalar = nscalar + 1
               scalar_file%fields(nscalar)%field => output_field
               scalar_file%fields(nscalar)%value => output_field%data_0d
            end if
            output_field => output_field%next
         end do
         scalar_file%next => self%first_file
         self%first_file => scalar_file
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

      integer :: i

      ! Header (three lines: simulation title, variable short names, variable long names + units)
      write(self%unit,fmt='("# ",A)') trim(self%title)
      write(self%unit,fmt='("# ",A,*(:,"'//trim(separator)//'",A))') 'time',(trim(self%fields(i)%field%output_name),i=1,size(self%fields))
      write(self%unit,fmt='("# ",A)',advance='NO') 'time'
      do i=1,size(self%fields)
         write(self%unit,fmt='("'//trim(separator)//'",A," (",A,")")',advance='NO') trim(self%fields(i)%field%source%long_name),trim(self%fields(i)%field%source%units)
      end do
      write(self%unit,*)
   end subroutine single_text_file_with_scalars_write_header

   subroutine single_text_file_with_scalars_write_data(self,timestr)
      class (type_single_text_file_with_scalars),intent(in) :: self
      character(len=*),                          intent(in) :: timestr

      integer :: i

      write (self%unit,fmt='(A,*(:,"'//separator//'",G0.8))') timestr,(self%fields(i)%value,i=1,size(self%fields))
   end subroutine single_text_file_with_scalars_write_data

   subroutine single_text_file_with_1d_variable_write_header(self)
      class (type_single_text_file_with_1d_variable),intent(in) :: self

      integer :: i
      type (type_dimension),pointer :: dim

      do i=1,size(self%field%source%dimensions)
         if (self%field%source%dimensions(i)%p%length>1) dim => self%field%source%dimensions(i)%p
      end do

      ! Header (three lines: simulation title, variable short names, variable long names + units)
      write(self%unit,fmt='("# ",A)') trim(self%title)
      write(self%unit,fmt='("# ",A)',advance='NO') 'time'
      do i=1,size(self%values)
         write(self%unit,fmt='(A,A,A,A,A,I0)',advance='NO') separator,trim(self%field%output_name),'@',trim(dim%name),'=',i
      end do
      write(self%unit,*)
      write(self%unit,fmt='("# ",A)',advance='NO') 'time'
      do i=1,size(self%values)
         write(self%unit,fmt='(A,A," (",A,")")',advance='NO') separator,trim(self%field%source%long_name),trim(self%field%source%units)
      end do
      write(self%unit,*)
   end subroutine single_text_file_with_1d_variable_write_header

   subroutine single_text_file_with_1d_variable_write_data(self,timestr)
      class (type_single_text_file_with_1d_variable),intent(in) :: self
      character(len=*),                              intent(in) :: timestr

      write (self%unit,fmt='(A,*(:,"'//separator//'",G0.8))') timestr,self%values
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

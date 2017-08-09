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
      type (type_scalar),allocatable :: variables(:)
      type (type_scalar),allocatable :: constants(:)
   contains
      procedure :: write_header => single_text_file_with_scalars_write_header
      procedure :: write_data   => single_text_file_with_scalars_write_data
   end type

   type,extends(type_single_text_file) :: type_single_text_file_with_1d_variable
      class (type_output_field),   pointer :: field      => null()
      type (type_output_dimension),pointer :: dimension  => null()
      class (type_output_field),   pointer :: coordinate => null()
      real(rk),                    pointer :: values(:)  => null()
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
      integer                                                 :: i
      integer                                                 :: nscalar, nconstants
      class (type_output_field),                      pointer :: output_field
      class (type_single_text_file_with_scalars),     pointer :: scalar_file
      class (type_single_text_file),                  pointer :: current_file
      class (type_single_text_file_with_1d_variable), pointer :: file_with_1d_data

      nscalar = 0
      nconstants = 0
      output_field => self%first_field
      do while (associated(output_field))
         if (associated(output_field%data_1d)) then
            ! 1D variable - create separate file for it
            allocate(file_with_1d_data)
            file_with_1d_data%field => output_field
            file_with_1d_data%values => output_field%data_1d
            file_with_1d_data%path = trim(self%path)//'_'//trim(file_with_1d_data%field%output_name)//trim(self%postfix)//extension
            file_with_1d_data%title = self%title
            do i=1,size(output_field%source%dimensions)
               if (output_field%source%dimensions(i)%p%length>1) then
                  file_with_1d_data%dimension => self%get_dimension(output_field%source%dimensions(i)%p)
                  file_with_1d_data%coordinate => output_field%coordinates(i)%p
                  exit
               end if
            end do
            file_with_1d_data%next => self%first_file
            self%first_file => file_with_1d_data
         elseif (associated(output_field%data_0d)) then
            ! 0D variable - just count these variables for now.
            if (output_field%source%has_dimension(id_dim_time)) then
               nscalar = nscalar + 1
            else
               nconstants = nconstants + 1
            end if
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
         allocate(scalar_file%variables(nscalar))
         allocate(scalar_file%constants(nconstants))
         nscalar = 0
         nconstants = 0
         output_field => self%first_field
         do while (associated(output_field))
            if (associated(output_field%data_0d)) then
               if (output_field%source%has_dimension(id_dim_time)) then
                  nscalar = nscalar + 1
                  scalar_file%variables(nscalar)%field => output_field
                  scalar_file%variables(nscalar)%value => output_field%data_0d
               else
                  nconstants = nconstants + 1
                  scalar_file%constants(nconstants)%field => output_field
                  scalar_file%constants(nconstants)%value => output_field%data_0d
               end if
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

   subroutine save(self,julianday,secondsofday,microseconds)
      class (type_text_file),intent(inout) :: self
      integer,               intent(in)    :: julianday,secondsofday,microseconds

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
      write(self%unit,fmt='("# title: ",A)') trim(self%title)
      do i=1,size(self%constants)
         write(self%unit,fmt='("# ",A,": ",G0.8,X,A)') trim(self%constants(i)%field%source%long_name),self%constants(i)%value,trim(self%constants(i)%field%source%units)
      end do
      write(self%unit,fmt='("# ",A,*(:,"'//trim(separator)//'",A))') 'time',(trim(self%variables(i)%field%output_name),i=1,size(self%variables))
      write(self%unit,fmt='("# ",A)',advance='NO') 'time'
      do i=1,size(self%variables)
         write(self%unit,fmt='("'//trim(separator)//'",A," (",A,")")',advance='NO') trim(self%variables(i)%field%source%long_name),trim(self%variables(i)%field%source%units)
      end do
      write(self%unit,*)
   end subroutine single_text_file_with_scalars_write_header

   subroutine single_text_file_with_scalars_write_data(self,timestr)
      class (type_single_text_file_with_scalars),intent(in) :: self
      character(len=*),                          intent(in) :: timestr

      integer :: i

      write (self%unit,fmt='(A,*(:,"'//separator//'",G0.8))') timestr,(self%variables(i)%value,i=1,size(self%variables))
   end subroutine single_text_file_with_scalars_write_data

   subroutine single_text_file_with_1d_variable_write_header(self)
      class (type_single_text_file_with_1d_variable),intent(in) :: self

      integer :: i
      logical :: first, has_singleton, real_x
      class (type_output_field),pointer :: coordinate

      ! Simulation title
      write(self%unit,fmt='("# title: ",A)') trim(self%title)

      ! Variable name and units
      write(self%unit,fmt='("# variable: ",A," (",A,")")') trim(self%field%source%long_name),trim(self%field%source%units)

      ! Dimensions
      write(self%unit,fmt='("# dimensions: ")',advance='NO')
      first = .true.
      has_singleton = .false.
      do i=1,size(self%field%source%dimensions)
         if (self%field%source%dimensions(i)%p%id==id_dim_time) cycle
         if (.not.first) write(self%unit,fmt='(A)',advance='NO') ','
         write(self%unit,fmt='(A,"=")',advance='NO') trim(self%field%source%dimensions(i)%p%name)
         if (self%field%source%dimensions(i)%p%length>1) then
            ! This is the only non-singleton dimension (length>1).
            write(self%unit,fmt='(I0,":",I0)',advance='NO') self%dimension%global_start,self%dimension%global_stop
            if (self%dimension%stride/=1) write(self%unit,fmt='(":",I0)',advance='NO') self%dimension%stride
         else
            ! This is a singleton dimension (length=1).
            has_singleton = .true.
            write(self%unit,fmt='(A)',advance='NO') '1'
         end if
         first = .false.
      end do
      write (self%unit,*)

      ! Coordinates associated with singleton dimensions (length=1).
      if (has_singleton) then
         write(self%unit,fmt='("# fixed coordinates:")')
         do i=1,size(self%field%source%dimensions)
            if (self%field%source%dimensions(i)%p%id/=id_dim_time .and. self%field%source%dimensions(i)%p%length==1) then
               coordinate => self%field%coordinates(i)%p
               if (associated(coordinate)) write(self%unit,fmt='("#   ",A,": ",G0.8,X,A)') trim(coordinate%source%long_name),coordinate%data_0d,trim(coordinate%source%units)
            end if
         end do
      end if

      ! Row dimension
      write(self%unit,fmt='("# rows: time")')

      ! Column dimension
      real_x = .false.
      if (associated(self%coordinate)) then
         if (associated(self%coordinate,self%field)) then
            ! The variable being saved is itself the coordinate variable.
            write(self%unit,fmt='("# columns: ",A)') trim(self%coordinate%source%long_name)
         elseif (associated(self%coordinate%data_1d).and..not.self%coordinate%source%has_dimension(id_dim_time)) then
            ! The coordinate variable is 1D and time-invariant. We will use it as column header.
            real_x = .true.
            write(self%unit,fmt='("# columns: ",A," (",A,")")') trim(self%coordinate%source%long_name),trim(self%coordinate%source%units)
         else
            ! The coordinate variable is multidimensional (possibly because it is time-varying). It'll be included in a separate file.
            write(self%unit,fmt='("# columns: ",A," (for values see ",A,")")') trim(self%coordinate%source%long_name),self%path(1:len_trim(self%path)-len(extension)-len_trim(self%field%output_name))//trim(self%coordinate%output_name)//extension
         end if
      else
         ! No coordinate variable specified. Just use the dimension name.
         write(self%unit,fmt='("# columns: ",A)') self%dimension%source%name
      end if

      ! Column names
      if (real_x) then
         ! Use actual coordinate in column header.
         write(self%unit,fmt='("# time",*(:,"'//trim(separator)//'",G0.8))') self%coordinate%data_1d
      else
         ! Use dimension indices in column header.
         write(self%unit,fmt='("# time",*(:,"'//trim(separator)//'",I0))') (i,i=self%dimension%global_start,self%dimension%global_stop,self%dimension%stride)
      end if
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

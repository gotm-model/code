module text_output

   use field_manager
   use output_manager_core

   implicit none

   public type_text_file

   private

   character, parameter :: separator = char(9)

   type,abstract :: type_single_text_file
      character(len=max_path)                :: path = ''
      character(len=max_path)                :: title = ''
      integer                                :: unit = -1
      class (type_single_text_file), pointer :: next => null()
      class (type_text_file),        pointer :: owner => null()
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
      subroutine host_write_data(self,julianday,secondsofday)
         import type_single_text_file
         class (type_single_text_file),intent(in) :: self
         integer,                      intent(in) :: julianday,secondsofday
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

      integer                            :: ifield
      class (type_output_field), pointer :: output_field
      integer                            :: nonsingleton
      integer                            :: idim
      integer                            :: length
      integer                            :: ios
      type (type_output_dimension), pointer :: output_dimension
      class (type_single_text_file_with_scalars), pointer :: scalar_file
      class (type_single_text_file), pointer :: current_file

      ! Check for variable with dimensions other than time.
      ! Currently we support those only if such dimensions have length 1.
      ifield = 0
      output_field => self%first_field
      do while (associated(output_field))
         nonsingleton = 0
         do idim=1,size(output_field%source%dimensions)            
            if (output_field%source%dimensions(idim)%p%id/=id_dim_time) then
               output_dimension => self%get_dimension(output_field%source%dimensions(idim)%p)
               length = (output_dimension%stop-output_dimension%start)/output_dimension%stride+1
               if (length>1) then
                  call host%log_message('WARNING: in output file "'//trim(self%path)//'", skipping variable "'//trim(output_field%output_name) &
                     //'" because its dimension '//trim(output_field%source%dimensions(idim)%p%name) &
                     //' has length>1. Currently only scalar variables are supported in text output.')
                  nonsingleton = nonsingleton + 1
               end if
            end if
         end do
         if (nonsingleton==0) ifield = ifield + 1
         output_field => output_field%next
      end do

      if (ifield==0) then
         call host%log_message('NOTE: output "'//trim(self%path)//trim(self%postfix)//'" will not be created because it would contain no data.')
         return
      end if

      allocate(scalar_file)
      scalar_file%path = trim(self%path)//trim(self%postfix)//'.dat'
      scalar_file%title = self%title
      self%first_file => scalar_file

      ! Create a list with pointers to active output fields
      allocate(scalar_file%fields(ifield))
      ifield = 0
      output_field => self%first_field
      do while (associated(output_field))
         nonsingleton = 0
         do idim=1,size(output_field%source%dimensions)            
            if (output_field%source%dimensions(idim)%p%id/=id_dim_time) then
               output_dimension => self%get_dimension(output_field%source%dimensions(idim)%p)
               length = (output_dimension%stop-output_dimension%start)/output_dimension%stride+1
               if (length>1) nonsingleton = nonsingleton + 1
            end if
         end do
         if (nonsingleton==0) then
            ifield = ifield + 1
            scalar_file%fields(ifield)%field => output_field

            ! Store pointer to scalar data
            if (associated(output_field%data_3d)) then
               scalar_file%fields(ifield)%value => output_field%data_3d(1,1,1)
            elseif (associated(output_field%data_2d)) then
               scalar_file%fields(ifield)%value => output_field%data_2d(1,1)
            elseif (associated(output_field%data_1d)) then
               scalar_file%fields(ifield)%value => output_field%data_1d(1)
            elseif (associated(output_field%data_0d)) then
               scalar_file%fields(ifield)%value => output_field%data_0d
            end if
         end if
         output_field => output_field%next
      end do

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

      class (type_single_text_file), pointer :: current_file

      current_file => self%first_file
      do while (associated(current_file))
         call current_file%write_data(julianday,secondsofday)
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

      integer :: ifield

      ! Header (three lines: simulation title, variable short names, variable long names + units)
      write(self%unit,fmt='(''# '',A)') trim(self%title)
      write(self%unit,fmt='(''# '',A)',advance='NO') 'time'
      do ifield=1,size(self%fields)
         write(self%unit,fmt='(A,A)',advance='NO') separator,trim(self%fields(ifield)%field%output_name)
      end do
      write(self%unit,*)
      write(self%unit,fmt='(''# '',A)',advance='NO') 'time'
      do ifield=1,size(self%fields)
         write(self%unit,fmt='(A,A,'' ('',A,'')'')',advance='NO') separator,trim(self%fields(ifield)%field%source%long_name),trim(self%fields(ifield)%field%source%units)
      end do
      write(self%unit,*)
   end subroutine single_text_file_with_scalars_write_header

   subroutine single_text_file_with_scalars_write_data(self,julianday,secondsofday)
      class (type_single_text_file_with_scalars),intent(in) :: self
      integer,                                   intent(in) :: julianday,secondsofday

      character(len=19) :: timestr
      integer           :: ifield

      call write_time_string(julianday,secondsofday,timestr)
      write (self%unit,fmt='(A)',advance='NO') timestr
      do ifield=1,size(self%fields)
         write (self%unit,fmt='(A,E16.8E3)',advance='NO') separator,self%fields(ifield)%value
      end do
      write(self%unit,*)
   end subroutine single_text_file_with_scalars_write_data

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

module netcdf_output

   use field_manager
   use output_manager_core
   use netcdf
   use fabm_config_types, only: type_dictionary, type_error

   implicit none

   public type_netcdf_file

   private

   REAL_4B,parameter :: dummy = 0.
   integer,parameter :: ncrk = kind(dummy)

   type,extends(type_file) :: type_netcdf_file
      integer :: itime         = 0  ! Next time index in NetCDF file
      integer :: ncid          = -1 ! NetCDF identifier for file
      integer :: time_id       = -1 ! Identifier of time dimension
      integer :: reference_julian  = -1
      integer :: reference_seconds = -1
      integer :: sync_interval = 1  ! Number of output time step between calls to nf90_sync (-1 to disable syncing)
   contains
      procedure :: configure
      procedure :: initialize
      procedure :: save
      procedure :: finalize
      procedure :: create_field
   end type

   type,extends(type_output_field) :: type_netcdf_field
      integer :: varid = -1
      integer,allocatable :: start(:)
      integer,allocatable :: edges(:)
      integer :: itimedim = -1
   end type

contains

   subroutine configure(self,mapping)
      class (type_netcdf_file),intent(inout) :: self
      class (type_dictionary), intent(in)    :: mapping

      type (type_error),  pointer :: config_error
      character(len=string_length) :: string

      ! Determine time of first output (default to start of simulation)
      string = mapping%get_string('time_reference',default='',error=config_error)
      if (associated(config_error)) call host%fatal_error('process_file',config_error%message)
      if (string/='') call read_time_string(trim(string),self%reference_julian,self%reference_seconds)
   end subroutine

   subroutine initialize(self)
      class (type_netcdf_file),intent(inout) :: self

      class (type_output_field), pointer :: output_field
      integer                            :: iret
      integer                            :: i
      integer,allocatable                :: current_dim_ids(:)
      integer                            :: yyyy,mm,dd
      integer                            :: length
      character(len=19)                  :: time_string
      character(len=256)                 :: coordinates
      type (type_dimension), pointer     :: dim
      type (type_output_dimension), pointer :: output_dimension

      type type_dimension_ids
         type (type_output_dimension),pointer :: output_dimension => null()
         integer :: netcdf_dimid
         type (type_dimension_ids), pointer :: next => null()
      end type
      type (type_dimension_ids), pointer :: first_dim_id, dim_id

      if (.not.associated(self%first_field)) then
         call host%log_message('NOTE: "'//trim(self%path)//trim(self%postfix)//'.nc" will not be created because it would contain no data.')
         return
      end if

      ! If no reference time is configured (to be used in time units), use time of first output.
      if (self%reference_julian==-1) then
         self%reference_julian  = self%first_julian
         self%reference_seconds = self%first_seconds
      end if

      ! Create NetCDF file
      iret = nf90_create(trim(self%path)//trim(self%postfix)//'.nc',NF90_CLOBBER,self%ncid); call check_err(iret)

      ! Create dimensions
      dim => self%field_manager%first_dimension
      first_dim_id => null()
      do while (associated(dim))
         if (self%is_dimension_used(dim)) then
            allocate(dim_id)
            dim_id%output_dimension => self%get_dimension(dim)
            dim_id%next => first_dim_id
            first_dim_id => dim_id
            if (dim%id==id_dim_time) then
               length = NF90_UNLIMITED
            else
               length = (dim_id%output_dimension%stop-dim_id%output_dimension%start)/dim_id%output_dimension%stride+1
            end if
            iret = nf90_def_dim(self%ncid, trim(dim%name), length, dim_id%netcdf_dimid); call check_err(iret)
         end if
         dim => dim%next
      end do

      ! Create time coordinate
      dim => self%field_manager%find_dimension(id_dim_time)
      if (self%is_dimension_used(dim)) then
         iret = nf90_def_var(self%ncid,'time',NF90_REAL,(/get_dim_id(dim)/),self%time_id); call check_err(iret)
         call write_time_string(self%reference_julian,self%reference_seconds,time_string)
         iret = nf90_put_att(self%ncid,self%time_id,'units','seconds since '//trim(time_string)); call check_err(iret)
      end if

      ! Create variables
      output_field => self%first_field
      do while (associated(output_field))
         select type (output_field)
         class is (type_netcdf_field)
            ! Map internal dimension indices to indices in NetCDF file.
            allocate(current_dim_ids(size(output_field%source%dimensions)))
            do i=1,size(output_field%source%dimensions)
               current_dim_ids(i) = get_dim_id(output_field%source%dimensions(i)%p)
            end do
            iret = nf90_def_var(self%ncid,output_field%output_name, NF90_REAL, current_dim_ids, output_field%varid); call check_err(iret)
            deallocate(current_dim_ids)

            iret = nf90_put_att(self%ncid,output_field%varid,'units',trim(output_field%source%units)); call check_err(iret)
            iret = nf90_put_att(self%ncid,output_field%varid,'long_name',trim(output_field%source%long_name)); call check_err(iret)
            if (output_field%source%standard_name/='') iret = nf90_put_att(self%ncid,output_field%varid,'standard_name',trim(output_field%source%standard_name)); call check_err(iret)
            if (output_field%source%minimum/=default_minimum) iret = nf90_put_att(self%ncid,output_field%varid,'valid_min',real(output_field%source%minimum,ncrk)); call check_err(iret)
            if (output_field%source%maximum/=default_maximum) iret = nf90_put_att(self%ncid,output_field%varid,'valid_max',real(output_field%source%maximum,ncrk)); call check_err(iret)
            if (output_field%source%fill_value/=default_fill_value) iret = nf90_put_att(self%ncid,output_field%varid,'_FillValue',real(output_field%source%fill_value,ncrk)); call check_err(iret)

            coordinates = ''
            do i=1,size(output_field%coordinates)
               if (associated(output_field%coordinates(i)%p)) coordinates = trim(coordinates)//' '//trim(output_field%coordinates(i)%p%output_name)
            end do
            if (coordinates/='') then
               iret = nf90_put_att(self%ncid,output_field%varid,'coordinates',trim(coordinates(2:))); call check_err(iret)
            end if

            select case (output_field%time_method)
               case (time_method_instantaneous)
                  iret = nf90_put_att(self%ncid,output_field%varid,'cell_methods','time: point'); call check_err(iret)
               case (time_method_mean)
                  iret = nf90_put_att(self%ncid,output_field%varid,'cell_methods','time: mean'); call check_err(iret)
               case (time_method_integrated)
                  iret = nf90_put_att(self%ncid,output_field%varid,'cell_methods','time: sum'); call check_err(iret)
            end select
   
            ! Fill arrays with start index and count per dimension
            allocate(output_field%start(size(output_field%source%dimensions)))
            allocate(output_field%edges(size(output_field%source%dimensions)))
            do i=1,size(output_field%source%dimensions)
               if (output_field%source%dimensions(i)%p%id==id_dim_time) then
                  output_field%start(i) = self%itime
                  output_field%edges(i) = 1
                  output_field%itimedim = i
               else
                  output_dimension => self%get_dimension(output_field%source%dimensions(i)%p)
                  output_field%start(i) = 1
                  output_field%edges(i) = (output_dimension%stop-output_dimension%start)/output_dimension%stride+1
               end if
            end do
         end select   
         output_field => output_field%next
      end do

      ! Exit define mode
      iret = nf90_enddef(self%ncid); call check_err(iret)
   contains
      integer function get_dim_id(dim)
         type (type_dimension), target      :: dim
         type (type_dimension_ids), pointer :: dim_id
         get_dim_id = -1
         dim_id => first_dim_id
         do while (associated(dim_id))
            if (associated(dim_id%output_dimension%source,dim)) get_dim_id = dim_id%netcdf_dimid
            dim_id => dim_id%next
         end do
      end function
   end subroutine initialize

   function create_field(self) result(field)
      class (type_netcdf_file),intent(inout) :: self
      class (type_output_field), pointer :: field
      allocate(type_netcdf_field::field)
   end function create_field

   subroutine save(self,julianday,secondsofday)
      class (type_netcdf_file),intent(inout) :: self
      integer,                 intent(in)    :: julianday,secondsofday

      class (type_output_field), pointer :: output_field
      integer                            :: iret
      real(ncrk)                         :: temp_time

      if (self%ncid==-1) return

      ! Increment time index
      self%itime = self%itime + 1

      ! Store time coordinate
      if (self%time_id/=-1) then
         temp_time = (julianday-self%reference_julian)*real(86400,ncrk) + secondsofday-self%reference_seconds
         iret = nf90_put_var(self%ncid,self%time_id,temp_time,(/self%itime/)); call check_err(iret)
      end if

      output_field => self%first_field
      do while (associated(output_field))
         select type (output_field)
         class is (type_netcdf_field)
            if (output_field%itimedim/=-1) output_field%start(output_field%itimedim) = self%itime
            if (associated(output_field%data_3d)) then
               iret = nf90_put_var(self%ncid,output_field%varid,output_field%data_3d,output_field%start,output_field%edges)
            elseif (associated(output_field%data_2d)) then
               iret = nf90_put_var(self%ncid,output_field%varid,output_field%data_2d,output_field%start,output_field%edges)
            elseif (associated(output_field%data_1d)) then
               iret = nf90_put_var(self%ncid,output_field%varid,output_field%data_1d,output_field%start,output_field%edges)
            elseif (associated(output_field%data_0d)) then
               iret = nf90_put_var(self%ncid,output_field%varid,output_field%data_0d,output_field%start)
            end if
            call check_err(iret)
         end select
         output_field => output_field%next
      end do

      if (self%sync_interval>0 .and. mod(self%itime,self%sync_interval)==0) then
         iret = nf90_sync(self%ncid)
         call check_err(iret)
      end if
   end subroutine save

   subroutine finalize(file)
      class (type_netcdf_file),intent(inout) :: file
      integer :: iret
      if (file%ncid/=-1) then
         iret = nf90_close(file%ncid); call check_err(iret)
      end if
   end subroutine finalize

   subroutine check_err(iret)
      integer,intent(in) :: iret
      if (iret/=NF90_NOERR) call host%fatal_error('check_err',nf90_strerror(iret))
   end subroutine

end module netcdf_output
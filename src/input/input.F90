#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: input
!
! !INTERFACE:
   module input
!
! !DESCRIPTION:
!
! !USES:
   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_input, do_input, close_input, register_input_0d, register_input_1d
   public read_obs
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!

!  PRIVATE TYPES
   integer,parameter,public :: maxpathlen=256

!  Information on an observed variable
   type type_1d_variable
      integer                            :: index = -1     ! Column index of variable in input file
      REALTYPE, pointer,dimension(:)     :: data => null() ! Pointer to profile data (depth-dependent variable)
      type (type_1d_variable),pointer    :: next => null() ! Next variable in current input file
      REALTYPE                           :: scale_factor = _ONE_
   end type

   type type_0d_variable
      integer                            :: index = -1     ! Column index of variable in input file
      REALTYPE,pointer                   :: data => null() ! Pointer to scalar data (depth-independent variable)
      type (type_0d_variable),pointer    :: next => null() ! Next variable in current input file
      REALTYPE                           :: scale_factor = _ONE_
      REALTYPE                           :: add_offset = _ZERO_
   end type

!  Information on file with observed profiles
   type type_profile_file
      character(len=maxpathlen)             :: path = ''
      REALTYPE, dimension(:,:), allocatable :: prof1,prof2,alpha
      integer                               :: jul1  = 0
      integer                               :: secs1 = 0
      integer                               :: jul2  = 0
      integer                               :: secs2 = 0
      integer                               :: unit  = -1
      integer                               :: lines = 0
      integer                               :: nprofiles = 0
      logical                               :: one_profile = .false.
      type (type_1d_variable), pointer      :: first_variable => null()
      type (type_profile_file),pointer      :: next => null()
   end type

!  Information on file with observed scalars (time series)
   type type_timeseries_file
      character(len=maxpathlen)           :: path = ''
      REALTYPE, dimension(:), allocatable :: obs1,obs2,alpha
      integer                             :: jul1  = 0
      integer                             :: secs1 = 0
      integer                             :: jul2  = 0
      integer                             :: secs2 = 0
      integer                             :: unit = -1
      integer                             :: lines = 0
      type (type_0d_variable),    pointer :: first_variable => null()
      type (type_timeseries_file),pointer :: next => null()
   end type

!  PRIVATE DATA MEMBERS
!  Pointers to first files with observed profiles and observed scalars.
   type (type_profile_file),    pointer :: first_profile_file
   type (type_timeseries_file), pointer :: first_timeseries_file

!  Unit to use for next data file.
   integer :: next_unit_no
!
!  PRIVATE PARAMETERS
   integer,parameter :: first_unit_no = 555

   integer :: nlev

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize input
!
! !INTERFACE:
   subroutine init_input(n)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   integer,intent(in),optional :: n
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_input'

   if (present(n)) then
      nlev = n
   else
      nlev = -1
   end if
   next_unit_no = first_unit_no
   nullify(first_profile_file)
   nullify(first_timeseries_file)

   LEVEL1 'done'

   end subroutine init_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Register a 1d input variable.
!
! !INTERFACE:
   subroutine register_input_1d(path,icolumn,data,name,scale_factor)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   character(len=*), intent(in) :: path,name
   integer,          intent(in) :: icolumn
   REALTYPE,target              :: data(:)
   REALTYPE,optional,intent(in) :: scale_factor
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_profile_file),pointer :: file
   type (type_1d_variable), pointer :: variable
!
!-----------------------------------------------------------------------
!BOC
   if (nlev==-1) call fatal_error('input::register_input_1d', 'input module has been initialized without depth information; &
         &depth-explicit inputs can therefore not be registered.')

   if (path=='') call fatal_error('input::register_input_1d', 'Empty file path specified to read variable '//trim(name)//' from.')

!  Find a file object for the specified file path; create one if it does exist yet.
   if (.not.associated(first_profile_file)) then
      allocate(first_profile_file)
      file => first_profile_file
   else
      file => first_profile_file
      do while (associated(file))
         if (file%path==path.and.file%unit==-1) exit
         file => file%next
      end do
      if (.not.associated(file)) then
         file => first_profile_file
         do while (associated(file%next))
            file => file%next
         end do
         allocate(file%next)
         file => file%next
      end if
   end if
   file%path = path

!  Create a variable object for the specified column index
   if (associated(file%first_variable)) then
!     Append a new variable object to the linked list.
      variable => file%first_variable
      do while (associated(variable%next))
         variable => variable%next
      end do
      allocate(variable%next)
      variable => variable%next
   else
!     This file does not have any associated variables; create the first.
      allocate(file%first_variable)
      variable => file%first_variable
   end if

   variable%index = icolumn
   variable%data => data
   variable%data = _ZERO_
   if (present(scale_factor)) variable%scale_factor = scale_factor

   end subroutine register_input_1d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Register a 0d input variable.
!
! !INTERFACE:
   subroutine register_input_0d(path,icolumn,data,name,scale_factor,add_offset)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   character(len=*), intent(in) :: path,name
   integer,          intent(in) :: icolumn
   REALTYPE,target              :: data
   REALTYPE,optional,intent(in) :: scale_factor,add_offset
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_timeseries_file),pointer :: file
   type (type_0d_variable),    pointer :: variable
!
!-----------------------------------------------------------------------
!BOC
   if (path=='') call fatal_error('input::register_input_0d', 'Empty file path specified to read variable '//trim(name)//' from.')

!  Find a file object for the specified file path; create one if it does exist yet.
   if (.not.associated(first_timeseries_file)) then
      allocate(first_timeseries_file)
      file => first_timeseries_file
   else
      file => first_timeseries_file
      do while (associated(file))
         if (file%path==path.and.file%unit==-1) exit
         file => file%next
      end do
      if (.not.associated(file)) then
         file => first_timeseries_file
         do while (associated(file%next))
            file => file%next
         end do
         allocate(file%next)
         file => file%next
      end if
   end if
   file%path = path

!  Create a variable object for the specified column index
   if (associated(file%first_variable)) then
!     Append a new variable object to the linked list.
      variable => file%first_variable
      do while (associated(variable%next))
         variable => variable%next
      end do
      allocate(variable%next)
      variable => variable%next
   else
!     This file does not have any associated variables; create the first.
      allocate(file%first_variable)
      variable => file%first_variable
   end if

   variable%index = icolumn
   variable%data => data
   variable%data = _ZERO_
   if (present(scale_factor)) variable%scale_factor = scale_factor
   if (present(add_offset)) variable%add_offset = add_offset

   end subroutine register_input_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read input observations
!
! !INTERFACE:
   subroutine do_input(jul,secs,nlev,z)
!
! !DESCRIPTION:
!  Read observations for all FABM variables for the current time.
!
! !USES:
!
! !INPUT PARAMETERS:
   integer,  intent(in)          :: jul,secs
   integer,  intent(in),optional :: nlev
   REALTYPE, intent(in),optional :: z(:)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_profile_file),   pointer :: profile_file
   type (type_timeseries_file),pointer :: timeseries_file
!-----------------------------------------------------------------------
!BOC
   if (associated(first_profile_file) .and. .not. (present(nlev).and.present(z))) &
      call fatal_error('input::do_input', 'do_input must receive nlev and z since one or more depth-varying inputs have been registered.')

!  Loop over files with observed profiles.
   profile_file => first_profile_file
   do while (associated(profile_file))
      call get_observed_profiles(profile_file,jul,secs,nlev,z)
      profile_file => profile_file%next
   end do

!  Loop over files with observed scalars.
   timeseries_file => first_timeseries_file
   do while (associated(timeseries_file))
      call get_observed_scalars(timeseries_file,jul,secs)
      timeseries_file => timeseries_file%next
   end do

   end subroutine do_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize a single input file with depth-explicit (1D) variables
!
! !INTERFACE:
   subroutine initialize_profile_file(info,nlev)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !USES:
!
! !INPUT PARAMETERS:
   type (type_profile_file),intent(inout) :: info
   integer,                 intent(in)    :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_1d_variable),pointer :: curvar
   integer :: nvar
   integer :: rc
   integer :: ios
!
!-----------------------------------------------------------------------
!BOC
!  Open the input file.
   open(next_unit_no,file=info%path,status='old',action='read',iostat=ios)
   if (ios /= 0) call fatal_error('input::initialize_profile_file', 'Unable to open "'//trim(info%path)//'" for reading')

!  Opening was successful - store the file unit, and increment the next unit with 1.
   info%unit = next_unit_no
   next_unit_no = next_unit_no + 1

!  Determine the maximum number of columns that we need to read.
   nvar = 0
   curvar => info%first_variable
   do while (associated(curvar))
      nvar = max(nvar,curvar%index)
      curvar => curvar%next
   end do

   allocate(info%prof1(0:nlev,nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_profile_file: Error allocating memory (prof1)'
   info%prof1 = _ZERO_

   allocate(info%prof2(0:nlev,nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_profile_file: Error allocating memory (prof2)'
   info%prof2 = _ZERO_

   allocate(info%alpha(0:nlev,nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_profile_file: Error allocating memory (alpha)'
   info%alpha = _ZERO_

   end subroutine initialize_profile_file
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read 1D data from a single input file
!
! !INTERFACE:
   subroutine get_observed_profiles(info,jul,secs,nlev,z)
!
! !DESCRIPTION:
!  Get observations for the current time from a single input file.
!  This reads in new observations if necessary (and available),
!  and performs linear interpolation in time and vertical space.
!
! !USES:
   use time, only: time_diff,julian_day
!
! !INPUT PARAMETERS:
   integer,                 intent(in)   :: jul,secs
   integer,                 intent(in)   :: nlev
   REALTYPE,                intent(in)   :: z(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
   type(type_profile_file), intent(inout):: info
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                      :: rc
   integer                      :: yy,mm,dd,hh,min,ss
   REALTYPE                     :: t,dt
   type (type_1d_variable),pointer :: curvar
   character(len=8)             :: strline
!
!-----------------------------------------------------------------------
!BOC
   if (info%unit==-1) call initialize_profile_file(info,nlev)

!  This part reads in new values if necessary.
   if(.not. info%one_profile .and. time_diff(info%jul2,info%secs2,jul,secs)<0) then
      do
         info%jul1 = info%jul2
         info%secs1 = info%secs2
         info%prof1 = info%prof2
         call read_profiles(info%unit,nlev,ubound(info%prof2,2),yy,mm,dd,hh,min,ss,z,info%prof2,info%lines,rc)
         if(rc/=0) then
            if(info%nprofiles==1) then
               LEVEL3 'Only one set of profiles is present in '//trim(info%path)//'.'
               info%one_profile = .true.
               curvar => info%first_variable
               do while (associated(curvar))
                  curvar%data = curvar%scale_factor*info%prof1(:,curvar%index)
                  curvar => curvar%next
               end do
            elseif (rc<0) then
               call fatal_error('input:get_observed_profiles', 'End of file reached while attempting to read new data from '//trim(info%path)//'. Does this file span the entire simulated period?')
            else
               write (strline,'(i0)') info%lines
               call fatal_error('input:get_observed_profiles', 'Error reading profiles from '//trim(info%path)//' at line '//trim(strline))
            end if
            exit
         else
            info%nprofiles = info%nprofiles + 1
            call julian_day(yy,mm,dd,info%jul2)
            info%secs2 = hh*3600 + min*60 + ss
            if(time_diff(info%jul2,info%secs2,jul,secs)>0) exit
         end if
      end do
      if( .not. info%one_profile) then
         dt = time_diff(info%jul2,info%secs2,info%jul1,info%secs1)
         info%alpha = (info%prof2-info%prof1)/dt
      end if
   end if

!  Do the time interpolation - only if more than one profile
   if( .not. info%one_profile) then
      t = time_diff(jul,secs,info%jul1,info%secs1)
      curvar => info%first_variable
      do while (associated(curvar))
         curvar%data = curvar%scale_factor*(info%prof1(:,curvar%index) + t*info%alpha(:,curvar%index))
         curvar => curvar%next
      end do
   end if

   end subroutine get_observed_profiles
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize a single input file with horizontal (0D) variables.
!
! !INTERFACE:
   subroutine initialize_timeseries_file(info)
!
! !DESCRIPTION:
!  Initialize a single file with observed profiles.
!
! !INPUT PARAMETERS:
   type (type_timeseries_file),intent(inout) :: info
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_0d_variable),pointer :: curvar
   integer :: nvar
   integer :: rc
   integer :: ios
!
!-----------------------------------------------------------------------
!BOC
!  Open the input file.
   open(next_unit_no,file=info%path,status='old',action='read',iostat=ios)
   if (ios /= 0) call fatal_error('input::initialize_timeseries_file', 'Unable to open "'//trim(info%path)//'" for reading')

!  Opening was successful - store the file unit, and increment the next unit with 1.
   info%unit = next_unit_no
   next_unit_no = next_unit_no + 1

!  Determine the maximum number of columns that we need to read.
   nvar = 0
   curvar => info%first_variable
   do while (associated(curvar))
      nvar = max(nvar,curvar%index)
      curvar => curvar%next
   end do

   allocate(info%obs1(nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_timeseries_file: Error allocating memory (obs1)'
   info%obs1 = _ZERO_

   allocate(info%obs2(nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_timeseries_file: Error allocating memory (obs2)'
   info%obs2 = _ZERO_

   allocate(info%alpha(nvar),stat=rc)
   if (rc /= 0) stop 'input::initialize_timeseries_file: Error allocating memory (alpha)'
   info%alpha = _ZERO_

   end subroutine initialize_timeseries_file
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read 0D data from a single input file
!
! !INTERFACE:
   subroutine get_observed_scalars(info,jul,secs)
!
! !DESCRIPTION:
!  Get observations for the current time from a single input file.
!  This reads in new observations if necessary (and available),
!  and performs linear interpolation in time.
!
! !USES:
   use time, only: time_diff,julian_day
!
! !INPUT PARAMETERS:
   integer,                    intent(in)    :: jul,secs
!
! !INPUT/OUTPUT PARAMETERS:
   type(type_timeseries_file), intent(inout) :: info
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer                      :: rc
   integer                      :: yy,mm,dd,hh,mins,ss
   REALTYPE                     :: t,dt
   type (type_0d_variable),pointer :: curvar
   character(len=8)             :: strline
!
!-----------------------------------------------------------------------
!BOC
   if (info%unit==-1) call initialize_timeseries_file(info)

!  This part reads in new values if necessary.
   if(time_diff(info%jul2,info%secs2,jul,secs)<0) then
      do
         info%jul1 = info%jul2
         info%secs1 = info%secs2
         info%obs1 = info%obs2
         call read_obs(info%unit,yy,mm,dd,hh,mins,ss,size(info%obs2),info%obs2,rc,line=info%lines)
         if (rc>0) then
            write (strline,'(i0)') info%lines
            call fatal_error('input:get_observed_scalars', 'Error reading time series from '//trim(info%path)//' at line '//strline)
         elseif (rc<0) then
            call fatal_error('input:get_observed_scalars', 'End of file reached while attempting to read new data from '//trim(info%path)//'. Does this file span the entire simulated period?')
         end if
         call julian_day(yy,mm,dd,info%jul2)
         info%secs2 = hh*3600 + mins*60 + ss
         if(time_diff(info%jul2,info%secs2,jul,secs)>0) exit
      end do
      dt = time_diff(info%jul2,info%secs2,info%jul1,info%secs1)
      info%alpha = (info%obs2-info%obs1)/dt
   end if

!  Do the time interpolation
   t  = time_diff(jul,secs,info%jul1,info%secs1)
   curvar => info%first_variable
   do while (associated(curvar))
      curvar%data = curvar%scale_factor*min(max(info%obs1(curvar%index),info%obs2(curvar%index)),max(min(info%obs1(curvar%index),info%obs2(curvar%index)),info%obs1(curvar%index) + t*info%alpha(curvar%index))) + curvar%add_offset
      curvar => curvar%next
   end do

   end subroutine get_observed_scalars
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Close inputs
!
! !INTERFACE:
   subroutine close_input()
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   type (type_profile_file),     pointer :: profile_file,next_profile_file
   type (type_timeseries_file),  pointer :: timeseries_file,next_scalar_file
   type (type_1d_variable),pointer :: curvar_1d,nextvar_1d
   type (type_0d_variable),pointer :: curvar_0d,nextvar_0d
!
!-----------------------------------------------------------------------
!BOC

   profile_file => first_profile_file
   do while (associated(profile_file))
      curvar_1d => profile_file%first_variable
      do while (associated(curvar_1d))
         nextvar_1d => curvar_1d%next
         deallocate(curvar_1d)
         curvar_1d => nextvar_1d
      end do

      next_profile_file => profile_file%next
      if (profile_file%unit/=-1) close(profile_file%unit)
      if (allocated(profile_file%prof1)) deallocate(profile_file%prof1)
      if (allocated(profile_file%prof2)) deallocate(profile_file%prof2)
      if (allocated(profile_file%alpha)) deallocate(profile_file%alpha)
      deallocate(profile_file)

      profile_file => next_profile_file
   end do
   nullify(first_profile_file)

   timeseries_file => first_timeseries_file
   do while (associated(timeseries_file))
      curvar_0d => timeseries_file%first_variable
      do while (associated(curvar_0d))
         nextvar_0d => curvar_0d%next
         deallocate(curvar_0d)
         curvar_0d => nextvar_0d
      end do

      next_scalar_file => timeseries_file%next
      if (timeseries_file%unit/=-1) close(timeseries_file%unit)
      if (allocated(timeseries_file%obs1))  deallocate(timeseries_file%obs1)
      if (allocated(timeseries_file%obs2))  deallocate(timeseries_file%obs2)
      if (allocated(timeseries_file%alpha)) deallocate(timeseries_file%alpha)
      deallocate(timeseries_file)

      timeseries_file => next_scalar_file
   end do
   nullify(first_timeseries_file)

   end subroutine close_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_obs
!
! !INTERFACE:
   subroutine read_obs(unit,yy,mm,dd,hh,min,ss,N,obs,ios,line)
!
! !DESCRIPTION:
!  This routine will read all non-profile observations.
!  The routine allows for reading more than one scalar variable at a time.
!  The number of data to be read is specified by {\tt N}.
!  Data read-in are returned
!  in the 'obs' array. It is up to the calling routine to assign
!  meaning full variables to the individual elements in {\tt obs}.
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: unit
   integer, intent(in)                 :: N
!
! !OUTPUT PARAMETERS:
   integer, intent(out)                :: yy,mm,dd,hh,min,ss
   REALTYPE,intent(out)                :: obs(:)
   integer, intent(out)                :: ios
   integer, intent(inout), optional    :: line
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   character                 :: c1,c2,c3,c4
   character(len=128)        :: cbuf
!-----------------------------------------------------------------------
!BOC
   do
      if (present(line)) line = line + 1
      read(unit,'(A128)',iostat=ios) cbuf
      if (ios/=0) return
      if (cbuf(1:1)/='#' .and. cbuf(1:1)/='!' .and. len_trim(cbuf)/=0) then
         read(cbuf,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)',iostat=ios) yy,c1,mm,c2,dd,hh,c3,min,c4,ss
         if (ios==0) read(cbuf(20:),*,iostat=ios) (obs(i),i=1,N)
         if (ios<0) ios = 1   ! End-of-file (ios<0) means premature end of line, which is a read error (ios>0) to us
         return
      end if
   end do
   end subroutine read_obs
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_profiles
!
! !INTERFACE:
   subroutine read_profiles(unit,nlev,cols,yy,mm,dd,hh,min,ss,z, &
                            profiles,lines,ios)
!
! !DESCRIPTION:
!  Similar to {\tt read\_obs()} but used for reading profiles instead of
!  scalar data.
!  The data will be interpolated on the grid specified by nlev and z.
!  The data can be read 'from the top' or 'from the bottom' depending on
!  a flag in the actual file.
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: unit
   integer, intent(in)                 :: nlev,cols
   REALTYPE, intent(in)                :: z(:)
!
! !INPUT/OUTPUT PARAMETERS:
   integer, intent(inout)              :: lines
!
! !OUTPUT PARAMETERS:
   integer, intent(out)                :: yy,mm,dd,hh,min,ss
   REALTYPE, intent(out)               :: profiles(:,:)
   integer, intent(out)                :: ios
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   character                 :: c1,c2,c3,c4
   integer                   :: i,j,rc
   integer                   :: N,up_down
   REALTYPE,allocatable,dimension(:)   :: tmp_depth
   REALTYPE,allocatable,dimension(:,:) :: tmp_profs
   character(len=128)        :: cbuf
   integer                   :: idx1,idx2,stride
!-----------------------------------------------------------------------
!BOC
   do
      read(unit,'(A128)', iostat=ios) cbuf
      lines = lines + 1
      if (ios /= 0) return

      if (len_trim(cbuf) > 0 .and. .not.(cbuf(1:1) == '#' .or. cbuf(1:1) == '!')) then
         read(cbuf,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)',iostat=ios) yy,c1,mm,c2,dd,hh,c3,min,c4,ss
         if (ios < 0) ios = 1   ! End-of-file (ios<0) means premature end of line, which is a read error (ios>0) to us
         if (ios /= 0) return
         read(cbuf(20:),*,iostat=ios) N,up_down
         if (ios < 0) ios = 1   ! End-of-file (ios<0) means premature end of line, which is a read error (ios>0) to us
         if (ios /= 0) return
         exit
      end if
   end do

   allocate(tmp_depth(0:N),stat=rc)
   if (rc /= 0) stop 'read_profiles: Error allocating memory (tmp_depth)'
   allocate(tmp_profs(0:N,cols),stat=rc)
   if (rc /= 0) stop 'read_profiles: Error allocating memory (tmp_profs)'

   if(up_down .eq. 1) then
      idx1=1; idx2 = N; stride=1
   else
      idx1=N; idx2 = 1; stride=-1
   end if

   do i=idx1,idx2,stride
      do
         read(unit,'(A128)',iostat=ios) cbuf
         lines = lines + 1
         if (ios /= 0) return

         if (len_trim(cbuf) > 0 .and. .not. (cbuf(1:1) == '#' .or. cbuf(1:1) == '!')) then
            read(cbuf,*,iostat=ios) tmp_depth(i),(tmp_profs(i,j),j=1,cols)
            if (ios < 0) ios = 1   ! End-of-file (ios<0) means premature end of line, which is a read error (ios>0) to us
            if (ios /= 0) return
            exit
         end if
      end do
   end do

   call gridinterpol(N,cols,tmp_depth,tmp_profs,nlev,z,profiles)

   deallocate(tmp_depth)
   deallocate(tmp_profs)

   end subroutine read_profiles
!EOC

   subroutine fatal_error(location,error)
      character(len=*),  intent(in) :: location,error

      FATAL trim(location)//': '//trim(error)
      stop 1
   end subroutine fatal_error

!-----------------------------------------------------------------------

   end module input

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


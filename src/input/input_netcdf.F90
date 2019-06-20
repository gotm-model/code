#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: input_netcdf
!
! !INTERFACE:
   module input_netcdf
!
! !DESCRIPTION:
!
! !USES:
   use time, only: julianday, secondsofday
   use time, only: read_time_string, write_time_string
#ifdef NETCDF_FMT
   use netcdf
   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public check_restart_time, read_restart_data
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding and Jorn Bruggeman
!
!EOP
!-----------------------------------------------------------------------
!BOC

!  PRIVATE TYPES
   integer,parameter,public :: maxpathlen=256

!  Information on an observed variable

!  PRIVATE DATA MEMBERS
!  Pointers to first files with observed profiles and observed scalars.
!
!  PRIVATE PARAMETERS
   integer                     :: ncid=-1
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize input
!
! !INTERFACE:
   subroutine init_input_netcdf(n)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   integer,intent(in),optional :: n
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding and Jorn Bruggeman
!
!EOP
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_input'

   LEVEL1 'done'

   end subroutine init_input_netcdf
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:
!
! !INTERFACE:
   subroutine check_restart_time(var_name)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   character(len=*), intent(in)        :: var_name
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding and Jorn Bruggeman
!
! !LOCAL VARIABLES:
   integer                   :: ierr, id
   integer                   :: jd,secs
   character(len=256)        :: units,timestr_out
!EOP
!-----------------------------------------------------------------------
!BOC
   if (ncid .eq. -1) then
!      ierr = nf90_open(trim(path),NF90_NOWRITE,ncid)
      ierr = nf90_open('restart.nc',NF90_NOWRITE,ncid)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
   end if

   ierr = nf90_inq_varid(ncid, trim(var_name), id)
   if (ierr /= NF90_NOERR) then
      FATAL 'Could not find time-variable in restart file'
      stop 'restart_file()'
   else
      ierr = nf90_get_att(ncid, id, 'units', units)
      call read_time_string(trim(units(15:)),jd,secs)
   end if

   if (jd .ne. julianday .or. secs .ne. secondsofday) then
      FATAL 'start time given in namelist does not match time'
      FATAL 'read from restart file'
      FATAL 'from namelist: ',julianday,secondsofday
      call write_time_string(julianday,secondsofday,timestr_out)
      LEVEL3 trim(timestr_out)
      FATAL 'from hotstart: ',jd,secs
      call write_time_string(jd,secs,timestr_out)
      LEVEL3 trim(timestr_out)
      stop 'restart_file()'
   else
      call write_time_string(jd,secs,timestr_out)
      LEVEL2 'restart time ',trim(timestr_out)
   end if
   return

   end subroutine check_restart_time
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:
!
! !INTERFACE:
   subroutine read_restart_data(var_name,allow_missing_variable,data_0d,data_1d)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   character(len=*), intent(in)        :: var_name
   logical                             :: allow_missing_variable
   REALTYPE, optional                  :: data_0d
   REALTYPE, optional                  :: data_1d(:)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding and Jorn Bruggeman
!
! !LOCAL VARIABLES:
   integer                   :: ierr, id
   integer                   :: start(4), edges(4)
!EOP
!-----------------------------------------------------------------------
!BOC
   if (ncid .eq. -1) then
!      ierr = nf90_open(trim(path),NF90_NOWRITE,ncid)
      ierr = nf90_open('restart.nc',NF90_NOWRITE,ncid)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
   end if

   ierr = nf90_inq_varid(ncid, trim(var_name), id)
   if (ierr /= NF90_NOERR) then
      if (.not. allow_missing_variable) then
         call handle_err(ierr,var_name)
      else
         LEVEL2 'variable ',trim(var_name),' not found - continuing'
         return
      end if
   end if

   if (present(data_0d)) then
      ierr = nf90_get_var(ncid, id, data_0d)
      if (ierr /= NF90_NOERR) then
         call handle_err(ierr)
      end if
   end if

   if (present(data_1d)) then
      start = 1 ; edges = 1; edges(3) = size(data_1d)
      ierr = nf90_get_var(ncid,id,data_1d,start,edges)
      if (ierr /= NF90_NOERR) then
         call handle_err(ierr)
      end if
   end if

   end subroutine read_restart_data
!EOC

   subroutine handle_err(ierr,msg)
   integer, intent(in) :: ierr
   character(len=*), optional :: msg
   LEVEL2 'read_restart_data(): error'
   if (present(msg)) then
      LEVEL3 trim(nf90_strerror(ierr)),' - ',trim(msg)
   else
      LEVEL3 trim(nf90_strerror(ierr))
   end if
   stop
   end subroutine handle_err

!-----------------------------------------------------------------------
#endif
   end module input_netcdf

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


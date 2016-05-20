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
   use netcdf
   use field_manager
   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public read_restart_data
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
   subroutine read_restart_data(var_name,data_0d,data_1d)
!
! !DESCRIPTION:
!
! !INPUT PARAMETERS:
   character(len=*), intent(in)        :: var_name
   REALTYPE, optional                  :: data_0d
   REALTYPE, dimension(:), optional    :: data_1d
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding and Jorn Bruggeman
!
! !LOCAL VARIABLES:
   integer, save             :: ncid=-1
   integer                   :: ierr, id
   integer                   :: start(4), edges(4)
#if 0
   integer                   :: ndims, nvars
   integer, allocatable      :: dim_len(:)
   character(len=64), allocatable :: dim_names, var_names
#endif
!EOP
!-----------------------------------------------------------------------
!BOC
   if (ncid .eq. -1) then
!      ierr = nf90_open(trim(path),NF90_NOWRITE,ncid)
      ierr = nf90_open('restart.nc',NF90_NOWRITE,ncid)
!      if (ierr /= nf90_noerr) call handle_err(ierr)
   end if
STDERR ncid,trim(var_name)
   ierr = nf90_inq_varid(ncid, trim(var_name), id)
STDERR '1 ',id,ierr
   if (present(data_0d)) then
      ierr = nf90_get_var(ncid, id, data_1d)
STDERR '2 ',ierr
   end if

   if (present(data_1d)) then
!      ierr = nf90_get_var(ncid, id, data_1d, start, count, stride, map)
      start = 1 ; edges = 1; edges(3) = size(data_1d)
      ierr = nf90_get_var(ncid,id,data_1d,start,edges)
STDERR '3 ',id,ierr
   end if

#if 0
   ierr = nf90_inquire(ncid, ndims, nvars)
   if (ierr /= nf90_noerr) call handle_err(ierr)
   STDERR ndims,nvars

   nf90_inquire
   allocate(dim_len(ndims))
   allocate(dim_names(ndims))
   do n = 1,ndims
      ierr = nf90_inquire_dimension(ncid, n, dim_names(n), dim_len(n))
   end do

   allocate(var_names(nvars))
   do n = 1,nvars
      ierr = nf90_inquire_variable(ncid, n, var_names(n), xtype, 
   end do
   do n = 1,nvars
      ierr = nf90_get_var
   end do
   nf90_close
#endif

   end subroutine read_restart_data
!EOC

!-----------------------------------------------------------------------

   end module input_netcdf

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------


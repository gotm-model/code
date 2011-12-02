#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: read_hypsography
!
! !INTERFACE:
   module hypsography
!
! !DESCRIPTION:
!  This module is responsible for reading in values for the hypography from a
!  file specified in the meanflow namelist and updating it according to the
!  GOTM (time dependant) grid layers.
!
! !USES
   use meanflow, only: lake,Ac,Af,dAdz,hypsography_file,idealised
   IMPLICIT NONE
   public                                :: init_hypsography,clean_hypsography
   public                                :: read_hypsography,update_hypsography
!  !PUBLIC DATA MEMBERS:

!  !input variables
   integer                               :: N_input
   REALTYPE, dimension(:), allocatable   :: A_input,depth_input

!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: rc
! !DEFINED PARAMETERS:
   integer, parameter        :: hypsography_unit=70
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: initialises everything related to the lake model
!
! !INTERFACE:
   subroutine init_hypsography(nlev)
!
!  !DESCRIPTION:
!  Initialises everything related to the lake model, e.g. allocating memory
!  for arrays.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in) :: nlev
!
!EOP
!-----------------------------------------------------------------------
!BOC
      !always allocate memory for Ac, Af so that diff_center() works
      allocate(Ac(0:nlev),stat=rc)
      if (rc /= 0) stop 'init_hypsography: Error allocating (Ac)'
         Ac = _ONE_
      allocate(Af(0:nlev),stat=rc)
      if (rc /= 0) stop 'init_hypsography: Error allocating (Af)'
         Af = _ONE_

      if (hypsography_file .ne. '') then
         lake = .true.
         allocate(dAdz(0:nlev),stat=rc)
         if (rc /= 0) stop 'init_hypsography: Error allocating (dAdz)'
            dAdz = _ZERO_
            open(hypsography_unit,file=hypsography_file,status='unknown',err=112)
         call read_hypsography(hypsography_unit,rc)
      else
         lake = .false.
      end if

      return
112 FATAL 'Unable to open "',trim(hypsography_file),'" for reading'
      stop 'init_hypsography'

   end subroutine init_hypsography
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Initial read in of the hypsography from specified file
!
! !INTERFACE:
   subroutine read_hypsography(unit,ierr)
!
!  !DESCRIPTION:
!  Reads in the hypsography from file at "unit" and saves everything
!  to the *_input variables.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)       :: unit
! !OUTPUT PARAMETERS:
   integer, intent(out)      :: ierr
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   integer                   :: rc
   integer                   :: up_down
   integer, save             :: lines
   REALTYPE                  :: x
!-----------------------------------------------------------------------
!BOC
   ierr = 0
   read(unit,*,ERR=100,END=110) N_input,up_down
   lines = 1

   if (allocated(depth_input)) deallocate(depth_input)
   allocate(depth_input(0:N_input),stat=rc)
   if (rc /= 0) stop 'read_hypsography: Error allocating memory (depth_input)'
   depth_input = _ZERO_
   if (allocated(A_input)) deallocate(A_input)
   allocate(A_input(0:N_input),stat=rc)
   if (rc /= 0) then
      stop 'read_hypsography: Error allocating memory (A_input)'
   end if
   A_input = _ZERO_

   select case (up_down)
      case(1)  ! surface ref, read from bottom
         do i=1,N_input
            lines = lines+1
            read(unit,*,ERR=100,END=110) depth_input(i), A_input(i)
         end do
      case(2)  ! surface ref, read from surface
         do i=N_input,1,-1
            lines = lines+1
            read(unit,*,ERR=100,END=110) depth_input(i), A_input(i)
         end do
      case(3)  ! bottom ref, read from bottom
         do i=1,N_input
            lines = lines+1
            read(unit,*,ERR=100,END=110) depth_input(i), A_input(i)
         end do
         do i=1,N_input/2
            x = depth_input(i)
            depth_input(i) = -depth_input(N_input-(i-1))
            depth_input(N_input-(i-1)) = -x
         end do
      case(4)  ! bottom ref, read from surface
         do i=N_input,1,-1
            lines = lines+1
            read(unit,*,ERR=100,END=110) depth_input(i), A_input(i)
         end do
         do i=1,N_input/2
            x = depth_input(i)
            depth_input(i) = -depth_input(N_input-(i-1))
            depth_input(N_input-(i-1)) = -x
         end do
      case default
   end select

   return
!  READ_ERROR = -2
100 ierr = -2
   FATAL 'Error reading hypsography (READ_ERROR)'
   return
!  END_OF_FILE = -1
110 ierr = -1
   FATAL 'Error reading hypsography (END_OF_FILE)'
   return
   end subroutine read_hypsography
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: update_hypsography
!
! !INTERFACE:
   subroutine update_hypsography(nlev,z,h)
!
! !DESCRIPTION:
!  This routine is responsible for updating the hypsography every timestep.
!  This includes interpolating the hypsography to the current grid and
!  calculating the derivative dA(z)/dz.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in):: nlev
   REALTYPE, intent(in):: z(0:nlev)
   REALTYPE, intent(in):: h(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   integer                   :: rc
   REALTYPE,allocatable,dimension(:) :: prof
   REALTYPE                  :: zPrime(0:nlev+1)
!
!-----------------------------------------------------------------------
!BOC
!  z' defined on the interfaces, not as z on the grid centers
!  let it start at "1", because the gridinterpol-routine-loop starts at "1"
!  hypsography is defined at grid interfaces
   zPrime(nlev+1) = _ZERO_
   zPrime(nlev) = -h(nlev)
   do i = nlev-1, 1, -1
      zPrime(i) = zPrime(i+1) - h(i)
   end do

   if (allocated(prof)) deallocate(prof)
      allocate(prof(0:nlev+1),stat=rc)
   if (rc /= 0) stop 'read_hypsography: Error allocating memory (prof)'
      prof = _ZERO_
!  interpolate hypsography Af to grid interfaces used by GOTM
   call gridinterpol(N_input,1,depth_input,A_input,nlev+1,zPrime,prof)

   do i = 0, nlev
      Af(i) = prof(i+1)
   end do

   if (allocated(prof)) deallocate(prof)

!  interpolate hypsography Ac to grid centres used by GOTM
   call gridinterpol(N_input,1,depth_input,A_input,nlev,z,Ac)

!  calculate the derivative of the hypsography wrt z
!  dAdz is defined at the grid centres
   do i = 1, nlev
      dAdz(i) = (Af(i) - Af(i-1)) / (h(i))
   end do

   end subroutine update_hypsography
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleaning up the hypsography variables
!
! !INTERFACE:
   subroutine clean_hypsography()
!
! !DESCRIPTION:
!  De-allocates all memory allocated via init\_hypsography()
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!-----------------------------------------------------------------------
!BOC
      if (allocated(Ac)) deallocate(Ac)
      if (allocated(Af)) deallocate(Af)
      if (allocated(dAdz)) deallocate(dAdz)

      return
   end subroutine clean_hypsography

   end module hypsography
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: hypsograph
!
! !INTERFACE:
   module hypsograph
!
! !DESCRIPTION:
!  This module is responsible for reading in values for the hypography from a
!  file specified in the meanflow namelist and updating it according to the
!  GOTM (time dependant) grid layers.
!  The hypsograph is only used if lake is true.
!
! !USES
   use meanflow, only: lake,Ac,Af,dAdz,hypsograph_file
   IMPLICIT NONE
   public                                :: init_hypsograph,clean_hypsograph
   public                                :: read_hypsograph,update_hypsograph
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
   integer, parameter        :: hypsograph_unit=70
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: initialises everything related to the lake model
!
! !INTERFACE:
   subroutine init_hypsograph(nlev)
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
      if (rc /= 0) stop 'init_hypsograph: Error allocating (Ac)'
         Ac = _ONE_
      allocate(Af(0:nlev),stat=rc)
      if (rc /= 0) stop 'init_hypsograph: Error allocating (Af)'
         Af = _ONE_
      if (hypsograph_file .ne. '') then
         LEVEL1 'init_hypsograph'
         LEVEL2 'reading hypsograph from:'
         LEVEL3 trim(hypsograph_file)
         lake = .true.
         allocate(dAdz(0:nlev),stat=rc)
         if (rc /= 0) stop 'init_hypsograph: Error allocating (dAdz)'
            dAdz = _ZERO_
            open(hypsograph_unit,file=hypsograph_file,status='unknown',err=112)
         call read_hypsograph(hypsograph_unit,rc)
      else
         lake = .false.
      end if

      return
112 FATAL 'Unable to open "',trim(hypsograph_file),'" for reading'
      stop 'init_hypsograph'

   end subroutine init_hypsograph
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Initial read in of the hypsograph from specified file
!
! !INTERFACE:
   subroutine read_hypsograph(unit,ierr)
!
!  !DESCRIPTION:
!  Reads in the hypsograph from file at "unit" and saves everything
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
   if (rc /= 0) stop 'read_hypsograph: Error allocating memory (depth_input)'
   depth_input = _ZERO_
   if (allocated(A_input)) deallocate(A_input)
   allocate(A_input(0:N_input),stat=rc)
   if (rc /= 0) then
      stop 'read_hypsograph: Error allocating memory (A_input)'
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
   FATAL 'Error reading hypsograph (READ_ERROR)'
   return
!  END_OF_FILE = -1
110 ierr = -1
   FATAL 'Error reading hypsograph (END_OF_FILE)'
   return
   end subroutine read_hypsograph
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: update_hypsograph
!
! !INTERFACE:
   subroutine update_hypsograph(nlev,z,h)
!
! !DESCRIPTION:
!  This routine is responsible for updating the hypsograph every timestep.
!  This includes interpolating the hypsograph to the current grid and
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
!  hypsograph is defined at grid interfaces
!  TODO: maybe we should use zi from meanflow ???
   zPrime(nlev+1) = _ZERO_
   zPrime(nlev) = -h(nlev)
   do i = nlev-1, 1, -1
      zPrime(i) = zPrime(i+1) - h(i)
   end do

   if (allocated(prof)) deallocate(prof)
      allocate(prof(0:nlev+1),stat=rc)
   if (rc /= 0) stop 'read_hypsograph: Error allocating memory (prof)'
      prof = _ZERO_
!  interpolate hypsograph Af to grid interfaces used by GOTM
   call gridinterpol(N_input,1,depth_input,A_input,nlev+1,zPrime,prof)

   do i = 0, nlev
      Af(i) = prof(i+1)
   end do

   if (allocated(prof)) deallocate(prof)

!  interpolate hypsograph Ac to grid centres used by GOTM
   call gridinterpol(N_input,1,depth_input,A_input,nlev,z,Ac)

!  calculate the derivative of the hypsograph wrt z
!  dAdz is defined at the grid centres
   do i = 1, nlev
      dAdz(i) = (Af(i) - Af(i-1)) / (h(i))
   end do

   end subroutine update_hypsograph
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleaning up the hypsograph variables
!
! !INTERFACE:
   subroutine clean_hypsograph()
!
! !DESCRIPTION:
!  De-allocates all memory allocated via init\_hypsograph()
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
   end subroutine clean_hypsograph

   end module hypsograph
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

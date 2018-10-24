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
   use meanflow, only: lake,depth0,depth,zi,Vc,Af,Afo,hypsograph_file
   IMPLICIT NONE
   public                                :: init_hypsograph,clean_hypsograph
   public                                :: read_hypsograph,update_hypsograph
!  !PUBLIC DATA MEMBERS:

!  !input variables
   integer                               :: nlev_input
   REALTYPE, dimension(:), allocatable   :: zi_input
   REALTYPE, dimension(:), allocatable   :: Af_input,sqrt_Af_input
   REALTYPE, dimension(:), allocatable   :: V_input

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
   allocate(Af(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_hypsograph: Error allocating (Af)'
   Af = _ONE_
   allocate(Afo(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_hypsograph: Error allocating (Afo)'
   Afo = _ONE_
   if (hypsograph_file .ne. '') then
      LEVEL1 'init_hypsograph'
      LEVEL2 'reading hypsograph from:'
      LEVEL3 trim(hypsograph_file)
      lake = .true.
      open(hypsograph_unit,file=hypsograph_file,status='unknown',err=112)
      call read_hypsograph(hypsograph_unit,rc)
      if (depth .ne. -zi_input(0)) then
         LEVEL2 'adjusting depth from namelist to confirm with hypsograph'
         LEVEL3 'depth:',depth,'--->',-zi_input(0)
         depth = -zi_input(0)
         depth0 = depth
      end if
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
   integer                   :: lines
   REALTYPE                  :: x,y
   REALTYPE,parameter        :: OneThird=_ONE_/3
!-----------------------------------------------------------------------
!BOC
   ierr = 0
   read(unit,*,ERR=100,END=110) lines,up_down
   LEVEL3 '# of lines',lines,'read order',up_down

   nlev_input = lines-1

   if (allocated(zi_input)) deallocate(zi_input)
   allocate(zi_input(-1:nlev_input),stat=rc)
   if (rc /= 0) stop 'read_hypsograph: Error allocating memory (zi_input)'
   zi_input = _ZERO_
   if (allocated(Af_input)) deallocate(Af_input)
   allocate(Af_input(-1:nlev_input),stat=rc)
   if (rc /= 0) then
      stop 'read_hypsograph: Error allocating memory (Af_input)'
   end if
   Af_input = _ZERO_
   allocate(sqrt_Af_input(0:nlev_input),stat=rc)
   if (rc /= 0) then
      stop 'read_hypsograph: Error allocating memory (sqrt_Af_input)'
   end if
   sqrt_Af_input = _ZERO_
   allocate(V_input(0:nlev_input),stat=rc)
   if (rc /= 0) then
      stop 'read_hypsograph: Error allocating memory (V_input)'
   end if
   V_input = _ZERO_

!KB - will replace this logic using strip_string() for the simple reading
!     an do the re-ordering after data has been read.
   select case (up_down)
      case(1)  ! surface ref, read from bottom
         do i=0,nlev_input
            read(unit,*,ERR=100,END=110) zi_input(i), Af_input(i)
         end do
      case(2)  ! surface ref, read from surface
         do i=nlev_input,0,-1
            read(unit,*,ERR=100,END=110) zi_input(i), Af_input(i)
         end do
      case(3)  ! bottom ref, read from bottom
         do i=0,nlev_input
            read(unit,*,ERR=100,END=110) zi_input(i), Af_input(i)
         end do
!        completely fill given basin => zi_input(nlev_input)=0
         !do i=0,nlev_input
         !   zi_input(i) = zi_input(i)-zi_input(nlev_input)
         !end do
!        only fill basin to given depth => zi_input(0)=-depth0
         do i=nlev_input,0,-1
            zi_input(i) = zi_input(i)-zi_input(0)-depth0
         end do
      case(4)  ! bottom ref, read from surface
         do i=nlev_input,0,-1
            read(unit,*,ERR=100,END=110) zi_input(i), Af_input(i)
         end do
!        completely fill given basin => zi_input(nlev_input)=0
         !do i=0,nlev_input
         !   zi_input(i) = zi_input(i)-zi_input(nlev_input)
         !end do
!        only fill basin to given depth => zi_input(0)=-depth0
         do i=nlev_input,0,-1
            zi_input(i) = zi_input(i)-zi_input(0)-depth0
         end do
      case default
   end select

   sqrt_Af_input(0) = sqrt( Af_input(0) )
   do i=1,nlev_input
      sqrt_Af_input(i) = sqrt( Af_input(i) )
      V_input(i) = OneThird * ( zi_input(i) - zi_input(i-1) ) &
         * ( Af_input(i-1) + sqrt_Af_input(i-1)*sqrt_Af_input(i) + Af_input(i) )
   end do

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
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC

   call zi2Vc(nlev,zi,Af,Vc(1:nlev))

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
   LEVEL1 'clean_hypsograph'

   LEVEL2 'de-allocation hypsograph memory ...'

   if (allocated(Af)) deallocate(Af)
   if (allocated(Afo)) deallocate(Afo)
   if (allocated(sqrt_Af_input)) deallocate(sqrt_Af_input)
   if (allocated(V_input)) deallocate(V_input)

   LEVEL2 'done'

   return
   end subroutine clean_hypsograph
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate volumes of layers
!
! !INTERFACE:
   subroutine zi2Vc(nlev,zi,Af,Vc)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer , intent(in)      :: nlev

!  z positions of interfaces
   REALTYPE, intent(in)      :: zi(0:nlev)
!
! !OUTPUT PARAMETERS:

!  interface areas
   REALTYPE, intent(out)     :: Af(0:nlev)

!  cell volumes
   REALTYPE, intent(out)     :: Vc(1:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
! !LOCAL VARIABLES:
   integer                   :: i,ii
   REALTYPE                  :: dVfilled,Vfrust,hfrust
   REALTYPE                  :: theta,sqrtAb,sqrtAt,sqrtAf
   REALTYPE,parameter        :: OneThird=_ONE_/3
!EOP
!-----------------------------------------------------------------------
!BOC

   Af(0) = Af_input(0)
   Vc = _ZERO_

   dVfilled = _ZERO_
   ii = 1

   do i=1,nlev
      do while ( ii.lt.nlev_input .and. zi_input(ii).lt.zi(i) )
         Vc(i) = Vc(i) + V_input(ii) - dVfilled
         dVfilled = _ZERO_
         ii = ii + 1
      end do
      hfrust = zi(i) - zi_input(ii-1)
      theta = hfrust / ( zi_input(ii) - zi_input(ii-1) )
      sqrtAb = sqrt_Af_input(ii-1)
      sqrtAt = sqrt_Af_input(ii  )
      sqrtAf = theta*sqrtAt + (1-theta)*sqrtAb
      Af(i) = sqrtAf * sqrtAf
      Vfrust = OneThird * hfrust * ( Af_input(ii-1) + sqrtAb*sqrtAf + Af(i) )
      Vc(i) = Vc(i) + Vfrust - dVfilled
      dVfilled = Vfrust
   end do

   return
   end subroutine zi2Vc
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Calculate layer heights
!
! !INTERFACE:
   subroutine Vc2zi(nlev,Vc,zi)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer , intent(in)      :: nlev

!  cell volumes
   REALTYPE, intent(in)      :: Vc(1:nlev)
!
! !OUTPUT PARAMETERS:

!  z positions of interfaces
   REALTYPE, intent(out)     :: zi(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
! !LOCAL VARIABLES:
   integer                   :: i,ii
   REALTYPE                  :: dVfilled,Vfrust,hfrust
   REALTYPE                  :: theta,sqrtAb,sqrtAt,sqrtAf
   REALTYPE,parameter        :: OneThird=_ONE_/3
!EOP
!-----------------------------------------------------------------------
!BOC

   zi(0) = -depth0

   Vfrust = _ZERO_
   ii = 1

   do i=1,nlev
      dVfilled = _ZERO_
      do while ( ii.lt.nlev_input .and. V_input(ii)-Vfrust.lt.Vc(i)-dVfilled )
         dVfilled = dVfilled + V_input(ii) - Vfrust
         Vfrust = _ZERO_
         ii = ii + 1
      end do
      Vfrust = Vfrust + Vc(i) - dVfilled
      theta = Vfrust / V_input(ii)
      sqrtAb = sqrt_Af_input(ii-1)
      sqrtAt = sqrt_Af_input(ii  )
      sqrtAf = ( theta*sqrtAt**3 + (1-theta)*sqrtAb**3 ) ** OneThird
      hfrust = ( zi_input(ii)-zi_input(ii-1) ) * ( sqrtAf - sqrtAb ) / ( sqrtAt - sqrtAb )
      zi(i) = zi_input(ii-1) + hfrust
   end do

   return
   end subroutine Vc2zi
!EOC

!-----------------------------------------------------------------------

   end module hypsograph

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

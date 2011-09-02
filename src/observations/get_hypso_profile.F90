!$Id: get_hypso_profile.F90,v 0.1 2011-04-19 16:24:48 schueler Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_hypsography
!
! !INTERFACE:
   subroutine read_hypsography(unit,nlev,z,h,ierr)
!
! !DESCRIPTION:
!  This routine is responsible for providing values for hypography.
!
! !USES:
   use observations, only: init_saved_vars,read_profiles,hypsoprof,dhypsodzprof
!   use observations, only: read_profiles,hypsoprof
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in):: unit
   integer, intent(in):: nlev
   REALTYPE, intent(in):: z(0:nlev)
   REALTYPE, intent(in):: h(0:nlev)
! !OUTPUT PARAMETERS:
   integer, intent(out)                :: ierr
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   integer                   :: rc
   integer                   :: N_file, up_down
   integer, save             :: lines
   REALTYPE,allocatable,dimension(:) :: tmp_depth
   REALTYPE,allocatable,dimension(:) :: tmp_prof
!   REALTYPE, save, dimension(:), allocatable :: prof
   REALTYPE,allocatable,dimension(:) :: prof
   REALTYPE                  :: zPrime(0:nlev+1)
!   REALTYPE                  :: hypsoproftemp(0:nlev+1)
   character(len=128)         :: cbuf
!
!-----------------------------------------------------------------------
!BOC
!  TODO remove everything related to time, this is not relevant to hypsography
!  z' defined on the interfaces, not as z on the grid centers
!  let it start at "1", because the gridinterpol-routine-loop starts at "1"
!  hypsography is defined at grid interfaces
   zPrime(nlev+1) = _ZERO_
   zPrime(nlev) = -h(nlev)
   do i = nlev-1, 1, -1
      zPrime(i) = zPrime(i+1) - h(i)
   end do

   if (allocated(prof)) deallocate(prof)
      allocate(prof(0:nlev),stat=rc)
   if (rc /= 0) stop 'read_hypsograph: Error allocating memory (prof)'
      prof = _ZERO_

!  This part initialises and reads in values.
   !TODO replace read_profiles by gridinterpol, cause this is the only reason
   !it's getting used
   ierr = 0
!   read(unit,'(A72)',ERR=100,END=110) cbuf
!   read(cbuf(20:),*,ERR=100,END=110) N_file,up_down
   read(unit,*,ERR=100,END=110) N_file,up_down
   lines = 1

   write(*,*) "N_file = ", N_file
   write(*,*) "up_down = ", up_down

   allocate(tmp_depth(0:N_file+1),stat=rc)
   if (rc /= 0) stop 'read_profiles: Error allocating memory (tmp_depth)'
   allocate(tmp_prof(0:N_file+1),stat=rc)
   if (rc /= 0) stop 'read_profiles: Error allocating memory (tmp_prof)'

   if(up_down .eq. 1) then
      do i=1,N_file
         lines = lines+1
         read(unit,*,ERR=100,END=110) tmp_depth(i), tmp_prof(i)
      end do
   else
      do i=N_file,1,-1
         lines = lines+1
         read(unit,*,ERR=100,END=110) tmp_depth(i), tmp_prof(i)
         write(*,*) "i = ", i, " | depth(i) = ", tmp_depth(i), &
            " | prof(i) = ", tmp_prof(i)
      end do
   end if
!   if(rc .eq. 0) then
!      deallocate(tmp_depth)
!      deallocate(tmp_prof)
!      deallocate(prof)
!      FATAL 'Error reading hypsography around line #',lines
!      stop 'read_hypsography'
!   end if
!   call read_profiles(unit,nlev+1,cols,yy,mm,dd,hh,min,ss,zPrime,prof,lines,rc)
!   subroutine read_profiles(unit,nlev,cols,yy,mm,dd,hh,min,ss,z, &
!                            profiles,lines,ierr)

   call gridinterpol(N_file,1,tmp_depth,tmp_prof,nlev,z,prof)

   do i = 0, nlev
      hypsoprof(i) = prof(i+1)
   end do

!  calculate the derivative of the hypsography wrt z
!  both hypsoprof & dhypsodzprof are defined at the grid interfaces
!  forward diff at bottom
   dhypsodzprof(0) = (hypsoprof(1) - hypsoprof(0)) / h(1)
!  backward diff at surface
   dhypsodzprof(nlev) = (hypsoprof(nlev) - hypsoprof(nlev-1)) / &
                        h(nlev)
!  the rest central diff
   do i = 1, nlev-1
      dhypsodzprof(i) = (hypsoprof(i+1) - hypsoprof(i-1)) / &
                        (h(i+1) + h(i))
   end do

   deallocate(tmp_depth)
   deallocate(tmp_prof)
   deallocate(prof)

   return
!  maybe these parameters should be defined in meanflow.F90
!  like in observations.F90
!  READ_ERROR = -2
100 ierr = -2
   return
!  END_OF_FILE = -1
110 ierr = -1
   return
   end subroutine read_hypsography
!EOC
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

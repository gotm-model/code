!$Id: get_hypso_profile.F90,v 0.1 2011-04-19 16:24:48 schueler Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_hypso_profile
!
! !INTERFACE:
   subroutine get_hypso_profile(unit,jul,secs,nlev,z,h)
!
! !DESCRIPTION:
!  This routine is responsible for providing sane values to `observed'
!  hypography.
!
! !USES:
   use time
   use observations, only: init_saved_vars,read_profiles,hypsoprof,dhypsodzprof
!   use observations, only: read_profiles,hypsoprof
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in):: unit
   integer, intent(in):: jul,secs
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
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: dt
   integer, save             :: jul1,secs1
   integer, save             :: jul2,secs2
   integer, parameter        :: cols=1
   integer, save             :: lines
   integer, save             :: nprofiles
   logical, save             :: one_profile
   REALTYPE, save, dimension(:,:), allocatable :: prof
   REALTYPE                  :: zPrime(0:nlev+1)
   REALTYPE                  :: hypsoproftemp(0:nlev+1)
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
   if (init_saved_vars) then
      jul2=0
      secs2=0
      lines=0
      nprofiles=0
      one_profile=.false.

      if (allocated(prof)) deallocate(prof)
      allocate(prof(0:nlev+1,cols),stat=rc)
      if (rc /= 0) stop 'get_t_profile: Error allocating memory (prof2)'
      prof = _ZERO_

   end if

!  This part initialises and reads in new values if necessary.
   if(.not. one_profile .and. time_diff(jul2,secs2,jul,secs) .lt. 0) then
      do
         jul1 = jul2
         secs1 = secs2
         call read_profiles(unit,nlev+1,cols,yy,mm,dd,hh,min,ss,zPrime,prof,lines,rc)
         if(rc .ne. 0) then
            if(nprofiles .eq. 1) then
               LEVEL3 'One hypsography profile present.'
               one_profile = .true.
               hypsoproftemp = prof(:,1)
               do i = 0, nlev
                  hypsoprof(i) = hypsoproftemp(i+1)
               enddo
!              calculate the derivative of the hypsography wrt z
!              both hypsoprof & dhypsodzprof are defined at the grid interfaces
!              forward diff at bottom
               dhypsodzprof(0) = (hypsoprof(1) - hypsoprof(0)) / h(1)
!              backward diff at surface
               dhypsodzprof(nlev) = (hypsoprof(nlev) - hypsoprof(nlev-1)) / &
                                    h(nlev)
!              the rest central diff
               do i = 1, nlev-1
                  dhypsodzprof(i) = (hypsoprof(i+1) - hypsoprof(i-1)) / &
                                    (h(i+1) + h(i))
               end do
            else
               FATAL 'Error reading hypsography around line #',lines
               stop 'get_hypso_profile'
            end if
            EXIT
         else
            nprofiles = nprofiles + 1
            call julian_day(yy,mm,dd,jul2)
            secs2 = hh*3600 + min*60 + ss
            if(time_diff(jul2,secs2,jul,secs) .gt. 0) EXIT
         end if
      end do
   end if

   return
   end subroutine get_hypso_profile
!EOC
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

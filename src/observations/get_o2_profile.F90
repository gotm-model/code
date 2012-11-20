#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_o2_profile
!
! !INTERFACE:
   subroutine get_o2_profile(unit,jul,secs,nlev,z)
!
! !DESCRIPTION:
!  This routine is responsible for providing sane values to an `observed'
!  oxygen profile.
!  The subroutine is called in the {\tt get\_all\_obs()} subroutine
!  as part of the main integration loop.
!  In case of observations from file the temporal interpolation is
!  done in this routine.
!
! !USES:
   use time
   use observations, only: init_saved_vars,read_profiles,o2_prof,o2_units
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: unit
   integer, intent(in)                 :: jul,secs
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: z(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: rc
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: t,dt
   REALTYPE,parameter        :: mol_per_liter=44.661
   REALTYPE,parameter        :: g_per_liter=0.7
   integer, save             :: jul1,secs1
   integer, save             :: jul2,secs2
   integer, parameter        :: cols=1
   integer, save             :: lines
   integer, save             :: nprofiles
   logical, save             :: one_profile
   REALTYPE, save, dimension(:,:), allocatable :: prof1,prof2,alpha
!
!-----------------------------------------------------------------------
!BOC
   if (init_saved_vars) then
      jul2=0
      secs2=0
      lines=0
      nprofiles=0
      one_profile=.false.

      if (allocated(prof1)) deallocate(prof1)
      allocate(prof1(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_o2_profile: Error allocating memory (prof1)'
      prof1 = _ZERO_

      if (allocated(prof2)) deallocate(prof2)
      allocate(prof2(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_o2_profile: Error allocating memory (prof2)'
      prof2 = _ZERO_

      if (allocated(alpha)) deallocate(alpha)
      allocate(alpha(0:nlev,cols),stat=rc)
      if (rc /= 0) stop 'get_o2_profile: Error allocating memory (alpha)'
   end if

!  This part initialises and reads in new values if necessary.
   if(.not. one_profile .and. time_diff(jul2,secs2,jul,secs) .lt. 0) then
      do
         jul1 = jul2
         secs1 = secs2
         prof1 = prof2
         call read_profiles(unit,nlev,cols,yy,mm,dd,hh,min,ss,z,prof2,lines,rc)
         if(rc .ne. 0) then
            if(nprofiles .eq. 1) then
               LEVEL3 'Only one oxygen profile present.'
               one_profile = .true.
               o2_prof = prof1(:,1)
            else
               FATAL 'Error reading oxygen profile around line # ',lines
               stop 'get_o2_profile'
            end if
            EXIT
         else
            nprofiles = nprofiles + 1
            call julian_day(yy,mm,dd,jul2)
            secs2 = hh*3600 + min*60 + ss
            if(time_diff(jul2,secs2,jul,secs) .gt. 0) EXIT
         end if
      end do
      if( .not. one_profile) then
         dt = time_diff(jul2,secs2,jul1,secs1)
         alpha = (prof2-prof1)/dt
      end if
   end if

!  Do the time interpolation - only if more than one profile
   if( .not. one_profile) then
      t  = time_diff(jul,secs,jul1,secs1)
      o2_prof = prof1(:,1) + t*alpha(:,1)
   end if

!  conversion to mmol/m^3
   select case (o2_units)
         case (1) ! mg/l
            o2_prof=o2_prof*mol_per_liter*g_per_liter
         case (2) ! ml/l
            o2_prof=o2_prof*mol_per_liter
         case (3) ! mmol/m^3
         case default
            STDERR "Invalid choice for oxygen unit conversion given in"
            STDERR "namelist o2_profile of obs.nml. program aborted in"
            STDERR "get_o2_profile.F90."
            stop
   end select

   return
   end subroutine get_o2_profile
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

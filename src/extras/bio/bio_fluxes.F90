#ifdef BIO

#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_fluxes --- handling bio fluxes \label{sec:biofluxes}
!
! !INTERFACE:
   module bio_fluxes
!
! !DESCRIPTION:
!  Remember this Hans
!
! !USES:
!  default: all is private.
   use bio_var
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio_fluxes, do_bio_fluxes, clean_bio_fluxes
!
! !PRIVATE DATA MEMBERS:
   integer                              :: sfl_unit=61
   integer                              :: jul1,secs1,jul2,secs2
   REALTYPE, allocatable                :: obs1(:),obs2(:),alpha(:)
!
! !REVISION HISTORY:!
!  Original author(s): Karsten Bolding and Hans Burchard
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise handling bio fluxes
!
! !INTERFACE:
   subroutine init_bio_fluxes()
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   integer                   :: rc
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'init_bio_fluxes'


   select case (surface_flux_method)
      case (2) ! from file
         LEVEL3 'reading from file'

         allocate(sfl_read(n_surface_fluxes),stat=rc)
         if (rc /= 0) stop 'init_bio_fluxes: Error allocating sfl_read)'
         sfl_read = _ZERO_

         allocate(obs1(n_surface_fluxes),stat=rc)
         if (rc /= 0) stop 'init_bio_fluxes: Error allocating obs1)'
         obs1 = _ZERO_

         allocate(obs2(n_surface_fluxes),stat=rc)
         if (rc /= 0) stop 'init_bio_fluxes: Error allocating obs2)'
         obs2 = _ZERO_

         allocate(alpha(n_surface_fluxes),stat=rc)
         if (rc /= 0) stop 'init_bio_fluxes: Error allocating alpha)'
         alpha = _ZERO_

         open(sfl_unit,file='bio_fluxes.dat',status='unknown')

         jul2=0
         secs2=0
!KBK
      case default
   end select

   return
   end subroutine init_bio_fluxes
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read in surface fluxes
!
! !INTERFACE:
   subroutine do_bio_fluxes(jul,secs)
!
! !DESCRIPTION:
!
! !USES:
   use time,         only: time_diff,julian_day
   use observations, only: read_obs
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
  integer, intent(in)                  :: jul,secs
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding and Hans Burchard
!
! !LOCAL VARIABLES:
   integer                   :: yy,mm,dd,hh,min,ss
   REALTYPE                  :: tfrac,dt
   integer                   :: rc
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (surface_flux_method)

!  NOTE: Positive fluxes into the sea surface must have negative sign !

   case (-1)! no fluxes
   case (0) ! constant - to behandled by the specific bio model
   case (2) ! from file
!     Reading nutrient surface fluxes from file

      if(time_diff(jul2,secs2,jul,secs) .lt. 0) then
         do
            jul1 = jul2
            secs1 = secs2
            obs1 = obs2
            call read_obs(sfl_unit,yy,mm,dd,hh,min,ss,n_surface_fluxes,obs2,rc)
            call julian_day(yy,mm,dd,jul2)
            secs2 = hh*3600 + min*60 + ss
            if(time_diff(jul2,secs2,jul,secs) .gt. 0) EXIT
         end do
         dt = time_diff(jul2,secs2,jul1,secs1)
         alpha = (obs2-obs1)/dt
      end if
!     Do the time interpolation
      tfrac  = time_diff(jul,secs,jul1,secs1)
      sfl_read =  obs1 + tfrac*alpha
   case default
      stop "do_bio_fluxes: no valid surface_flux_method specified!"
   end select

   return
   end subroutine do_bio_fluxes
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Clean up bio flux data
!
! !INTERFACE:
   subroutine clean_bio_fluxes
!
! !DESCRIPTION:
!  Deallocate memory
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC

   if (surface_flux_method.eq.2) then
      if (allocated(sfl_read)) deallocate(sfl_read)
      if (allocated(obs1))     deallocate(obs1)
      if (allocated(obs2))     deallocate(obs2)
      if (allocated(alpha))    deallocate(alpha)

      close(sfl_unit)
   end if

   return
   end subroutine clean_bio_fluxes
!EOC

!-----------------------------------------------------------------------

   end module bio_fluxes

#endif

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

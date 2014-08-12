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
   use bio_var, only: surface_flux_method,n_surface_fluxes,sfl_read
   use input, only: register_input_0d
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio_fluxes, clean_bio_fluxes
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
   integer                   :: rc,n
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'init_bio_fluxes'

   select case (surface_flux_method)
      case (-1)! no fluxes
      case (0) ! constant - to behandled by the specific bio model
      case (2) ! from file
         LEVEL3 'reading from file'
         allocate(sfl_read(n_surface_fluxes),stat=rc)
         if (rc /= 0) stop 'init_bio_fluxes: Error allocating sfl_read)'
         sfl_read = _ZERO_
         do n=1,n_surface_fluxes
            call register_input_0d('bio_fluxes.dat',n,sfl_read(n),'surface flux of biogeochemical variable')
         end do
      case default
         stop "do_bio_fluxes: no valid surface_flux_method specified!"
   end select

   end subroutine init_bio_fluxes
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
   end if

   end subroutine clean_bio_fluxes
!EOC

!-----------------------------------------------------------------------

   end module bio_fluxes

#endif

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

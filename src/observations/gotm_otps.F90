#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: gotm_otps --- Interface to the OSU Tidal Model (OTPS)
!
! !INTERFACE:
   module gotm_otps
!
! !DESCRIPTION:
!  This module provides the link between the OSU Tidal Model
!
! !USES:
!   use observations, only: pi, zeta, dpdx, dpdy
   use time, only: MinN, MaxN, timestep
!
   implicit none
!
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_otps
   public do_gotm_otps
   public clean_gotm_otps
!   public otps_calc_tides

! !PUBLIC DATA MEMBERS:

! !PRIVATE DATA MEMBERS:
   REALTYPE :: MJD0

!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the FABM driver
!
! !INTERFACE:
   subroutine init_gotm_otps(jul,secs,z,u,v)
!
! !DESCRIPTION:
! Initializes the GOTM-FABM driver module by reading settings from otps.nml.
!
! !INPUT PARAMETERS:
   integer,                intent(in)    :: jul,secs
! !INPUT/OUPUT PARAMETERS:
   REALTYPE, dimension(:), allocatable,  intent(inout) :: z, u, v
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
   integer         :: rc, ntimes
#if 1
   REALTYPE                ::  SHPN(4),SHPNP(5)
#endif
!EOP
!-----------------------------------------------------------------------
!BOC
   ntimes=MaxN-MinN+1
   LEVEL1 'init_gotm_otps'
   ! https://en.wikipedia.org/wiki/Julian_day#Variants - used by OTPS
   MJD0 = (jul-2400000.)+secs/86400.
#if 00
   STDERR jul,secs,ntimes,MJD0,timestep
stop
#endif
   allocate(z(ntimes),stat=rc)
   if (rc /= 0) stop 'init_gotm_otps: Error allocating (z)'
   z = _ZERO_

   allocate(u(ntimes),stat=rc)
   if (rc /= 0) stop 'init_gotm_otps: Error allocating (u)'
   u = _ZERO_

   allocate(v(ntimes),stat=rc)
   if (rc /= 0) stop 'init_gotm_otps: Error allocating (v)'
   v = _ZERO_

   call gotm_otps_tides(ntimes,z,u,v)

#if 0
   call ASTROL(901.5, SHPN)
   call ASTRO5(901.5, SHPNP)
   STDERR SHPN
   STDERR SHPNP
   stop
#endif
   return
   end subroutine init_gotm_otps
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the FABM model
!
! !INTERFACE:
   subroutine do_gotm_otps(n)
!
! !DESCRIPTION:
! TODO
!
! !USES:
!
! !INPUT PARAMETERS:
   integer, intent(in)          :: n
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
#if 0
   zeta = z(n)
   dpdx = u(n)
   dpdy = v(n)
#endif
   return
   end subroutine do_gotm_otps
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish biogeochemical model
!
! !INTERFACE:
   subroutine clean_gotm_otps
!
! !DESCRIPTION:
!  Report timing results and deallocate memory.
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
   integer(8) :: clock,ticks_per_sec
   REALTYPE :: tick_rate
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'clean_gotm_otps'

#if 0
   ! Deallocate internal arrays
   if (allocated(z)) deallocate(z)
   if (allocated(u)) deallocate(u)
   if (allocated(v)) deallocate(v)
#endif

   LEVEL1 'done.'
   return
   end subroutine clean_gotm_otps
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the FABM driver
!
! !INTERFACE:
   subroutine gotm_otps_tides(ntimes,z,u,v)
!
! !DESCRIPTION:
! Initializes the GOTM-FABM driver module by reading settings from otps.nml.
!
! !INPUT PARAMETERS:
   integer,                intent(in)    :: ntimes
! !INPUT/OUPUT PARAMETERS:
   REALTYPE, dimension(:), intent(inout) :: z, u, v
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
   integer         :: n
   REALTYPE        :: Az=1., Au=0.1, Av=0.5
   REALTYPE, parameter:: pi=3.141592654d0
   REALTYPE        :: MJD,omega=2.*pi*86400./44714. ! In days
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'gotm_otps_tides'

!  For testing purposes only - assuming 1 hour time step
   do n=1,ntimes
      MJD = MJD0+(n-1)*timestep/86400.
      z(n) = Az*sin(omega*MJD)
      u(n) = Au*sin(omega*MJD)
      v(n) = Av*sin(omega*MJD)
   end do
   LEVEL1 'done.'
   return
   end subroutine gotm_otps_tides
!EOC

!-----------------------------------------------------------------------

   end module gotm_otps

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------

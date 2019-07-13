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
   public post_init_gotm_otps
   public do_gotm_otps
   public clean_gotm_otps
!   public otps_calc_tides
   interface init_gotm_otps
      module procedure init_gotm_otps_nml
      module procedure init_gotm_otps_yaml
   end interface


! !PUBLIC DATA MEMBERS:

! !PRIVATE DATA MEMBERS:
   integer                   :: ncon
   REALTYPE, allocatable     :: t(:)

   type type_constituent
      character(len=8)      :: name = ''
      logical               :: active = .true.
      REALTYPE              :: zr=_ZERO_, zi=_ZERO_
      REALTYPE              :: ur=_ZERO_, ui=_ZERO_
      REALTYPE              :: vr=_ZERO_, vi=_ZERO_
      type (type_constituent), pointer :: next => null()
   end type

   type (type_constituent), pointer :: first_constituent

!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the FABM driver
!
! !INTERFACE:
   subroutine init_gotm_otps_nml()
!
! !DESCRIPTION:
! Initializes the GOTM-FABM driver module by reading settings from otps.nml.
!
! !INPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
   integer                 :: rc
   character(len=64)       :: name
   logical                 :: active
#if 0
   complex                 :: z
   namelist /constituent/ name,active,z
#else
   REALTYPE                :: zr,zi,ur,ui,vr,vi
   namelist /constituent/ name,active,zr,zi,ur,ui,vr,vi
#endif
   type (type_constituent), pointer :: c
!EOP
!-----------------------------------------------------------------------
!BOC
   nullify(first_constituent)
   ncon = 0

!   open(unit,file='constituents.nml',action='read',status='old',err=98)
   open(10,file='constituents.nml',action='read',status='old',err=98)
   LEVEL1 'init_constituents_nml'
   do
      name   = ''
      active = .false.
#if 0
      z = (0.,0.)
#else
      zr = _ZERO_; zi = _ZERO_
      ur = _ZERO_; ui = _ZERO_
      vr = _ZERO_; vi = _ZERO_
#endif

!      read(unit,nml=constituent,err=99,end=97)
      read(10,nml=constituent,err=99,end=97)
      if (name=='') cycle

      if (.not.associated(first_constituent)) then
         allocate(first_constituent)
         c => first_constituent
      else
         c => first_constituent
         do while (associated(c%next))
            c => c%next
         end do
         allocate(c%next)
         c => c%next
      end if

      c%name = name
      c%active = active
#if 0
      c%z   = z
#else
      c%zr   = zr
      c%zi   = zi
      c%ur   = ur
      c%ui   = ui
      c%vr   = vr
      c%vi   = vi
#endif
      ncon = ncon + 1
   end do
97 close(10)
   return

99 FATAL 'Error reading namelist "constituent" from constituents.nml.'
   stop 'init_gotm_otps_nml'

98 LEVEL2 'I could not open constituents.nml.'

   return
   end subroutine init_gotm_otps_nml
!EOC

!-----------------------------------------------------------------------
!BOP
!
   subroutine init_gotm_otps_yaml(a)
      integer, intent(in) :: a
      stop 'not ready yet'
   end subroutine init_gotm_otps_yaml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the FABM model
!
! !INTERFACE:
   subroutine post_init_gotm_otps(jul,secs,lat,z, u, v)
!
! !DESCRIPTION:
! TODO
!
! !USES:
!
! !INPUT PARAMETERS:
   integer,                intent(in)    :: jul,secs
   REALTYPE,               intent(in)    :: lat
!
! !INPUT/OUPUT PARAMETERS:
   REALTYPE, dimension(:), allocatable,  intent(inout) :: z, u, v
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
   integer                        :: n,rc,ntimes
   REALTYPE                       :: MJD
   type (type_constituent), pointer    :: c
!EOP
!-----------------------------------------------------------------------
!BOC
   ! https://en.wikipedia.org/wiki/Julian_day#Variants - used by OTPS
   MJD = (jul-2400000.-1.)+secs/86400.
   LEVEL1 'init_gotm_otps: MJD= ',MJD
   ntimes=MaxN-MinN+1

   allocate(t(ntimes),stat=rc)
   if (rc /= 0) stop 'init_gotm_otps: Error allocating (t)'
   do n=1,ntimes
      t(n) = MJD+(n-1)*timestep/86400.
   end do

   allocate(z(ntimes),stat=rc)
   if (rc /= 0) stop 'init_gotm_otps: Error allocating (z)'
   z = _ZERO_

   allocate(u(ntimes),stat=rc)
   if (rc /= 0) stop 'init_gotm_otps: Error allocating (u)'
   u = _ZERO_

   allocate(v(ntimes),stat=rc)
   if (rc /= 0) stop 'init_gotm_otps: Error allocating (v)'
   v = _ZERO_

   call gotm_otps_tides(1,ntimes,lat,t,z)
   call gotm_otps_tides(2,ntimes,lat,t,u)
   call gotm_otps_tides(3,ntimes,lat,t,v)
   LEVEL1 'done.'
   return
   end subroutine post_init_gotm_otps
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
   subroutine gotm_otps_tides(var,ntimes,lat,t,y)
!
! !DESCRIPTION:
! Initializes the GOTM-FABM driver module by reading settings from otps.nml.
!
! !INPUT PARAMETERS:
   integer,                intent(in)    :: var,ntimes
   REALTYPE, intent(in)                  :: lat,t(:)
! !INPUT/OUPUT PARAMETERS:
   REALTYPE, dimension(:), intent(inout) :: y
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES:
   integer                        :: n,rc
   character(4), allocatable      :: c_id(:)
   integer, allocatable           :: ind(:)
   complex, allocatable           :: z1(:)
   real, allocatable              :: ytmp(:)
   logical                        :: interp = .true.
   type (type_constituent), pointer :: c
   include 'constit.h'
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'gotm_otps_tides'
   interp = .true.

   allocate(c_id(ncon),stat=rc)
   if (rc /= 0) stop 'post_init_gotm_otps: Error allocating (c_id)'

   allocate(z1(ncon),stat=rc)
   if (rc /= 0) stop 'post_init_gotm_otps: Error allocating (z1)'

   allocate(ind(ncon),stat=rc)
   if (rc /= 0) stop 'post_init_gotm_otps: Error allocating (ind)'
   ind = .false.

   allocate(ytmp(ntimes),stat=rc)
   if (rc /= 0) stop 'post_init_gotm_otps: Error allocating (y)'

   select case (var)
      case (1)
         LEVEL0 "z constituents:"
      case (2)
         LEVEL0 "u constituents:"
      case (3)
         LEVEL0 "v constituents:"
      case default
   end select

   n=1
   c => first_constituent
   do while (associated(c))
      c_id(n) = c%name
      select case (var)
         case (1)
            z1(n) = cmplx(c%zr,c%zi)
         case (2)
            z1(n) = cmplx(c%ur,c%ui)
         case (3)
            z1(n) = cmplx(c%vr,c%vi)
         case default
      end select
      n=n+1
      c => c%next
   end do

   call def_con_ind(c_id,ncon,constid,ncmx,ind) 
   call ptide(z1,c_id,ncon,ind,real(lat),t,ntimes,interp,ytmp)
   y = ytmp

#if 0
   do n=1,25
      STDERR t(n),ytmp(n)
   end do
   stop
#endif

   return
   end subroutine gotm_otps_tides
!EOC

!-----------------------------------------------------------------------

   end module gotm_otps

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------

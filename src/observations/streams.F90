#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: streams
!
! !INTERFACE:
   module streams
!
! !DESCRIPTION:
! This module is responsible for all calculations related to streams. This means
! reading in prescribed values from a given file, calculating the depth where
! the inflowing water masses interleave, and calculating vertical fluxes. These
! vertical fluxes are used by other routines to calculate vertical advection
! velocities.
!
! !USES
   use input

!  !PUBLIC DATA MEMBERS:
   IMPLICIT NONE
   public                                :: init_streams,clean_streams
   public                                :: update_streams
   public                                :: type_stream,nstreams,first_stream
   REALTYPE, public                      :: int_inflow=_ZERO_
   REALTYPE, public                      :: int_outflow=_ZERO_
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   type type_stream
      character(len=64)      :: name = ''
      REALTYPE               :: zl,zu,QI,SI,TI
      logical                :: has_S = .false.
      logical                :: has_T = .false.
!      logical                :: interleaving = .false.
!      logical                :: surface      = .false.
!      logical                :: depth_range  = .false.
      REALTYPE, allocatable  :: Q(:)
      type (type_stream), pointer :: next => null()
   end type

   integer                     :: nstreams
   type (type_stream), pointer :: first_stream

! !DEFINED PARAMETERS:
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: initialises everything related to the streams
!
! !INTERFACE:
   subroutine init_streams(lake,nlev)
!
!  !DESCRIPTION:
!  Initialises everything related to the lake model, e.g. allocating memory
!  for arrays.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   logical, intent(in) :: lake
   integer, intent(in) :: nlev

   integer,parameter :: unit = 666

   integer             :: rc
   character(len=64)       :: name
   REALTYPE                :: zl,zu
   character(len=PATH_MAX) :: Q_file,T_file,S_file
   integer                 :: Q_col,T_col,S_col

   namelist /stream/ name,zl,zu,Q_file,T_file,S_file,Q_col,T_col,S_col

   type (type_stream), pointer :: current_stream
!
!EOP
!-----------------------------------------------------------------------
!BOC
   nstreams = 0
   nullify(first_stream)

   if (.not.lake) return

   open(unit,file='streams.nml',action='read',status='old',err=98)
   LEVEL1 'init_streams'
   do
      name   = ''
      zl = 10.0
      zu =  0.0
      Q_file = ''
      S_file = ''
      T_file = ''
      Q_col = 1
      T_col = 1
      S_col = 1

      read(unit,nml=stream,err=99,end=97)
      if (name=='') cycle

      if (.not.associated(first_stream)) then
         allocate(first_stream)
         current_stream => first_stream
      else
         current_stream => first_stream
         do while (associated(current_stream%next))
            current_stream => current_stream%next
         end do
         allocate(current_stream%next)
         current_stream => current_stream%next
      end if

      LEVEL2 '.... ',trim(name),real(zl),real(zu)
      current_stream%name = name
      current_stream%zl   = zl
      current_stream%zu   = zu

!KB will this increase readabillity ? And the use them below
!KK I am not sure. IMO having all possible conditions of zl and zu written
!   out directly in the if clauses avoids scrolling and simplifies understanding...
!      if ( current_stream%zl .gt. current_stream%zu ) then
!         current_stream%interleaving = .true. ! default stream
!         current_stream%surface      = .true. ! default outflow
!      end if

      if (Q_file=='') then
         FATAL 'Error: "Q_file" must be provided in namelist "stream".'
         stop 'init_streams'
      end if
      call register_input_0d(Q_file,Q_col,current_stream%QI,'observed stream: discharge')

      if (T_file/='') then
         call register_input_0d(T_file,T_col,current_stream%TI,'observed stream: temperature')
         current_stream%has_T = .true.
      end if

      if (S_file/='') then
         call register_input_0d(S_file,S_col,current_stream%SI,'observed stream: salinity')
         current_stream%has_S = .true.
      end if

      allocate(current_stream%Q(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_observations: Error allocating (Q)'
      current_stream%Q = _ZERO_
      nstreams = nstreams + 1

   end do
97 close(unit)

   return

99 FATAL 'Error reading namelist "stream" from streams.nml.'
   stop 'init_streams'

98 LEVEL2 'I could not open streams.nml. Inflows will not be used.'
   LEVEL2 'If that''s not what you want you have to supply streams.nml.'

   end subroutine init_streams
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: calculate streams
!
! !INTERFACE:
   subroutine update_streams(nlev,dt,S,T,z,zi,h,Ac,Qs,Qt,Ls,Lt,Q)
!
! !DESCRIPTION:
!  Calculates the depth where the stream occurs and
!  what effects it has on the water column. The resulting flux variables
!  can then be used by routines like salinity or temperature to calculate the
!  vertical advection induced by the streams.
!
! !USES:
   use eqstate, only: unesco

   IMPLICIT NONE

! !INPUT PARAMETERS:
   integer, intent(in)                    :: nlev
   REALTYPE, intent(in)                   :: dt
   REALTYPE, intent(in)                   :: S(0:nlev), T(0:nlev)
   REALTYPE,dimension(0:nlev),intent(in)  :: z,zi,h,Ac
   REALTYPE,dimension(0:nlev),intent(inout) :: Qs,Qt,Ls,Lt,Q
!EOP
!
! !LOCAL VARIABLES:
   integer              :: n,nmin,nmax
   REALTYPE             :: rhoI,rho
   REALTYPE             :: depth
   REALTYPE             :: VI_basin
   REALTYPE             :: hI,TI,SI
   type (type_stream), pointer :: current_stream
!
!-----------------------------------------------------------------------
!BOC
   Qs = _ZERO_
   Qt = _ZERO_
   Ls = _ZERO_
   Lt = _ZERO_
   Q  = _ZERO_
!KBSTDERR zi
!KBSTDERR z

   current_stream => first_stream
   do while (associated(current_stream))
      current_stream%Q = _ZERO_

      if (current_stream%has_T) then
         TI = current_stream%TI
      else
         TI = T(nlev)
      end if
      if (current_stream%has_S) then
         SI = current_stream%SI
      else
         SI = S(nlev)
      end if

      nmin = -1
      nmax = -1

      ! stream triggered or still in progress
!      if (current_stream%QI .eq. _ZERO_) to avoid calculations when Q=0 something must be done
      if (current_stream%QI .gt. _ZERO_) then

!STDERR 'TI ',TI

         ! interleaving
         if ( current_stream%zl .gt. current_stream%zu ) then
!KB         if ( current_stream%interleaving ) then

!STDERR 'interleaving ',current_stream%name

            ! find minimal depth where the inflow will take place
            nmin = 0
            rhoI = unesco(SI,TI,_ZERO_,.false.)
            do n=1,nlev
               depth = zi(nlev) - z(n)
!STDERR 'R ',SI,TI
!STDERR 'A ',S(n),T(n)
!KB - UNPress is false               rhoI = unesco(SI,TI,depth/10.0d0,.false.)
               rho = unesco(S(n),T(n),depth/10.0d0,.false.)
!STDERR n,rhoI,rho
               ! if the density of the inflowing water is greater than the
               ! ambient water then the lowest interleaving depth is found
               if (rhoI > rho) then
                  nmin = n
                  nmax = n
                  exit
               end if
            end do

            ! density of the inflowing water is too small -> surface inflow
            if (nmin .eq. 0) then
               nmin = nlev
               nmax = nlev
            endif

            ! find the z-levels in which the water will interleave
!KB What is the logic behind this - and is needed/correct?
!KK seems to be some kind of CFL checking. Same as above - needs to be discussed with Hans/Lars.
!KB - removed
#if 0
            VI_basin = _ZERO_
            nmax = nmin
            do while (VI_basin < current_stream%QI*dt)
               VI_basin = VI_basin + Ac(nmax) * h(nmax)
               nmax = nmax+1
               if (nmax .gt. nlev) then
                  !if inflow at surface -> no inflow
                  !debug output only
!                  write(*,*) "Warning: Too much water flowing into the basin."
!                  return
               end if
            end do
            ! VI_basin is now too big so go back one step
            nmax = nmax-1
#endif

            ! calculate the source terms
            ! "+1" because loop includes both n and nmin - KB ?
            do n=nmin,nmax
               current_stream%Q(n) = current_stream%QI / (nmax-nmin+1)
            end do

         ! given depth range
         else if ( zi(0) .le. current_stream%zu ) then
!KB - should the water not fill up the basin? I.e. n=1!
!KK - depends on zl

            nmin = nlev
            do n=1,nlev
               if ( current_stream%zl .lt. zi(n) ) then
                  nmin = n
                  exit
               end if
            end do
            nmax = nmin
            do n=nlev,nmin,-1
               if ( zi(n-1) .lt. current_stream%zu ) then
                  nmax = n
                  exit
               end if
            end do

            if (current_stream%zl .eq. current_stream%zu) then
               current_stream%Q(nmax) = current_stream%QI
            else
!              consider full discharge (even if below bathy)
               hI = current_stream%zu - max(current_stream%zl,zi(0)) + SMALL

               do n=nmin,nmax-1
                  current_stream%Q(n) = current_stream%QI * ( min(zi(n),current_stream%zu)-max(current_stream%zl,zi(n-1)) ) / hI
               end do
!              discharge above fse counts for surface layer
               current_stream%Q(n) = current_stream%QI * ( current_stream%zu-max(current_stream%zl,zi(n-1)) ) / hI
            end if

         else
!KB - is the cycle needed?
!           ignore inflows which are fully below the bottom (zl<=zu<zi(0))
            cycle

         end if

         int_inflow = int_inflow + dt*current_stream%QI

         do n=nmin,nmax
            Qs(n) = Qs(n) + SI * current_stream%Q(n) / (Ac(n) * h(n))
            Qt(n) = Qt(n) + TI * current_stream%Q(n) / (Ac(n) * h(n))
         end do

!KB - should this go?
!KK - yes, because the correct FQ is calculated later in water_balance
         ! calculate the sink term at sea surface
         !Qs(nlev) = Qs(nlev) -S(nlev) * FQ(nlev-1) / (Ac(nlev) * h(nlev))
         !Qt(nlev) = Qt(nlev) -T(nlev) * FQ(nlev-1) / (Ac(nlev) * h(nlev))

!STDERR nmin,nmax,nlev

      else ! outflow

!KBSTDERR current_stream%zl,current_stream%zu

         ! surface outflow
         if ( current_stream%zl .gt. current_stream%zu ) then
!KB         if ( current_stream%surface ) then

            nmin = nlev
            nmax = nlev
            current_stream%Q(nlev) = current_stream%QI

         else if ( current_stream%zl .le. zi(nlev) ) then
!KB - should zl=zu=0 not mean surface flow?
!KK - only if zi(nlev-1)<=0<=zi(nlev)
!KK - but you are right: I now added the special case zl=zu=zi(nlev) here

            nmin = nlev
            do n=1,nlev
               if ( current_stream%zl .lt. zi(n) ) then
                  nmin = n
                  exit
               end if
            end do
            nmax = nmin
            do n=nlev,nmin,-1
               if ( zi(n-1) .lt. current_stream%zu ) then
                  nmax = n
                  exit
               end if
            end do

            if (current_stream%zl .eq. current_stream%zu) then

               current_stream%Q(nmax) = current_stream%QI

            else

!              consider full discharge
               hI = min(zi(nlev),current_stream%zu) - max(current_stream%zl,zi(0)) + SMALL

               do n=nmin,nmax
                  current_stream%Q(n) = current_stream%QI * ( min(zi(n),current_stream%zu)-max(current_stream%zl,zi(n-1)) ) / hI
               end do

            end if

!STDERR nmin,nmax,nlev

         else

            cycle

         end if

         int_outflow = int_outflow + dt*current_stream%QI

         if (current_stream%has_T) then
            do n=nmin,nmax
               Qt(n) = Qt(n) + TI * current_stream%Q(n) / (Ac(n) * h(n))
            end do
         else
            do n=nmin,nmax
               Lt(n) = Lt(n) + current_stream%Q(n) / (Ac(n) * h(n))
            end do
         end if
         if (current_stream%has_S) then
            do n=nmin,nmax
               Qs(n) = Qs(n) + SI * current_stream%Q(n) / (Ac(n) * h(n))
            end do
         else
            do n=nmin,nmax
               Ls(n) = Ls(n) + current_stream%Q(n) / (Ac(n) * h(n))
            end do
         end if

      end if

      do n=nmin,nmax
         Q(n) = Q(n) + current_stream%Q(n)
      end do

      current_stream => current_stream%next
   end do

   end subroutine update_streams
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: cleaning up
!
! !INTERFACE:
   subroutine clean_streams()
!
! !DESCRIPTION:
!  De-allocates all memory allocated via init\_streams()
!
! !USES:
   IMPLICIT NONE
!EOP
!
! !LOCAL VARIABLES:
   type (type_stream), pointer :: current_stream, next_stream
!
!-----------------------------------------------------------------------
!BOC
   current_stream => first_stream
   do while (associated(current_stream))
      next_stream => current_stream%next
      deallocate(current_stream%Q)
      deallocate(current_stream)
      current_stream => next_stream
   end do

   end subroutine clean_streams

   end module streams
!EOC
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

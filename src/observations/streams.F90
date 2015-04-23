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
      integer                :: method
      REALTYPE               :: zl,zu
      REALTYPE               :: QI,SI,TI
      logical                :: has_S = .false.
      logical                :: has_T = .false.
      REALTYPE, allocatable  :: weights(:), Q(:)
      type (type_stream), pointer :: next => null()
   end type

   integer, parameter        :: surface_flow=1
   integer, parameter        :: bottom_flow=2
   integer, parameter        :: depth_range=3
   integer, parameter        :: interleaving=4
   integer                   :: nstreams
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

   integer                 :: rc
   integer                 :: method
   character(len=64)       :: name
   REALTYPE                :: zl,zu
   character(len=PATH_MAX) :: Q_file,T_file,S_file
   integer                 :: Q_col,T_col,S_col

   namelist /stream/ name,method,zl,zu,Q_file,T_file,S_file,Q_col,T_col,S_col

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
      method = surface_flow
      zl     = -1.0
      zu     = -0.0
      Q_file = ''
      S_file = ''
      T_file = ''
      Q_col  = 1
      T_col  = 1
      S_col  = 1

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

      LEVEL2 '.... ',trim(name),method,real(zl),real(zu)
      current_stream%name = name
      current_stream%method = method
      current_stream%zu   = zu
      current_stream%zl   = zl

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

      allocate(current_stream%weights(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_observations: Error allocating (weights)'
      current_stream%weights = _ZERO_

      allocate(current_stream%Q(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_observations: Error allocating (Q)'
      current_stream%Q = _ZERO_

      if (current_stream%method .eq. interleaving .and. .not. current_stream%has_T) then
         LEVEL1 trim(current_stream%name),' ....'
         FATAL "Can't specify - interleaving - without providing either S or T"
         stop 'init_streams()'
      end if

      select case (current_stream%method)
         case (surface_flow)
            current_stream%weights(nlev) = _ONE_
         case (bottom_flow)
            current_stream%weights(1) = _ONE_
!        other methods are dynamic are are update in update_streams()
      end select

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
!
! !LOCAL VARIABLES:
   integer              :: n,nmin,nmax
   REALTYPE             :: rhoI,rho
   REALTYPE             :: depth
   REALTYPE             :: VI_basin
   REALTYPE             :: hI,TI,SI
   REALTYPE             :: invV
   type (type_stream), pointer :: current_stream
!
!EOP
!-----------------------------------------------------------------------
!BOC
   Qs = _ZERO_
   Qt = _ZERO_
   Ls = _ZERO_
   Lt = _ZERO_
   Q  = _ZERO_

   current_stream => first_stream
   do while (associated(current_stream))


      if (current_stream%QI .eq. _ZERO_) cycle

      current_stream%Q = _ZERO_

      select case (current_stream%method)
         case (surface_flow)
         case (bottom_flow)
         case (depth_range)
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

            call get_weights(nlev,nmin,nmax,h,zi,current_stream)

         case (interleaving)

            TI = current_stream%TI
            SI = current_stream%SI

            ! find minimal depth where the inflow will take place
            nmin = 0
            rhoI = unesco(SI,TI,_ZERO_,.false.)
            do n=1,nlev
               depth = zi(nlev) - z(n)
               rho = unesco(S(n),T(n),depth/10.0d0,.false.)
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

            call get_weights(nlev,nmin,nmax,h,zi,current_stream)

      end select

!     weights have been found - now apply them for the flow
      current_stream%Q = current_stream%weights*current_stream%QI

!     now get the - active - temperature and salinity
!     think it only make really sense for inflows to specify T and S
      if (current_stream%has_T) then
         TI = current_stream%TI
      else
!KB      is there a mean !!!!
         TI = sum(T(nmin:nmax))/(nmax-nmin+1)
      end if
      if (current_stream%has_S) then
         SI = current_stream%SI
      else
!KB      is there a mean !!!!
         SI = sum(T(nmin:nmax))/(nmax-nmin+1)
      end if

      ! inflow stream
      if (current_stream%QI .gt. _ZERO_) then
         int_inflow = int_inflow + dt*current_stream%QI

         do n=1,nlev
            invV = _ONE_/(Ac(n) * h(n))
            Qt(n) = Qt(n) + TI * current_stream%Q(n) * invV
            Qs(n) = Qs(n) + SI * current_stream%Q(n) * invV
         end do

      else ! outflow

         int_outflow = int_outflow + dt*current_stream%QI

         if (current_stream%has_T) then
            do n=1,nlev
               invV = _ONE_/(Ac(n) * h(n))
               Qt(n) = Qt(n) + TI * current_stream%Q(n) * invV
            end do
         else
            do n=1,nlev
               invV = _ONE_/(Ac(n) * h(n))
               Lt(n) = Lt(n) + current_stream%Q(n) * invV
            end do
         end if
         if (current_stream%has_S) then
            do n=1,nlev
               invV = _ONE_/(Ac(n) * h(n))
               Qs(n) = Qs(n) + SI * current_stream%Q(n) * invV
            end do
         else
            do n=1,nlev
               invV = _ONE_/(Ac(n) * h(n))
               Ls(n) = Ls(n) + current_stream%Q(n) * invV
            end do
         end if

      end if

      Q = Q + current_stream%Q

      current_stream => current_stream%next
   end do

   end subroutine update_streams
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: get the distribution weights
!
! !INTERFACE:
   subroutine get_weights(nlev,nmin,nmax,h,zi,stream)
!
! !DESCRIPTION:
!  Calculate the weights the flow is distributed by over the water
!  column
!
! !USES:
   IMPLICIT NONE
!
   integer, intent(in)                        :: nlev,nmin,nmax
   REALTYPE,dimension(0:nlev),intent(in)      :: h,zi
   type (type_stream), pointer, intent(inout) :: stream
!
! !LOCAL VARIABLES:
   REALTYPE             :: d,yi,yl,yh
!
!EOP
!-----------------------------------------------------------------------
!BOC
   stream%weights = _ZERO_

   if (nmin .eq. nmax) then
      stream%weights(nmin) = _ONE_
      return
   end if

!  check: d == yh+yi+yl
   d  = -(stream%zl - stream%zu)   ! active inflow layer height

   if (nmax-nmin .eq. 1) then
      yh =   (stream%zu - zi(nmax-1)) ! height above - inner - layer
      yl = -(stream%zl - zi(nmin))    ! height below - inner - layer
      stream%weights(nmax) = yh/d
      stream%weights(nmin) = yl/d
      return
   end if

   yh =   (stream%zu - zi(nmax-1)) ! height above - inner - layer
   yi =  sum(h(nmin+1:nmax-1))     ! height of - inner - layers 
   yl = -(stream%zl - zi(nmin))    ! height below - inner - layer
   stream%weights(nmax) = yh/d
   stream%weights(nmin+1:nmax-1) = h(nmin+1:nmax-1)/d
   stream%weights(nmin) = yl/d

   if (abs(sum(stream%weights) - _ONE_) .gt. 0.00001) then
      FATAL 'Check weight calculations in streams::get_weights()'
   end if

#if 0
   STDERR d,yh+yi+yl
   STDERR sum(stream%weights)
   STDERR stream%weights
   stop 'get_weights()'
#endif

   end subroutine get_weights
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

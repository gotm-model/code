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
   use yaml_settings
   use settings

!  !PUBLIC DATA MEMBERS:
   IMPLICIT NONE
   public                                :: configure_streams,post_init_streams,clean_streams
   public                                :: update_streams
   public                                :: type_stream,nstreams,first_stream
   REALTYPE, public                      :: int_inflow=_ZERO_
   REALTYPE, public                      :: int_outflow=_ZERO_
!
   interface configure_streams
!KB      module procedure configure_streams_nml
      module procedure configure_streams_yaml
   end interface

   type, extends(type_dictionary_populator) :: type_stream_populator
   contains
      procedure :: create => register_stream
   end type
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
! !LOCAL VARIABLES:
   type type_stream
      character(len=64)      :: name = ''
      integer                :: method
      REALTYPE               :: zl,zu
      type (type_scalar_input) :: flow
      type (type_scalar_input) :: temp
      type (type_scalar_input) :: salt
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

   class (type_gotm_settings), pointer :: cfg => null()
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: initialises everything related to the streams
!
! !INTERFACE:
   subroutine configure_streams_nml(lake,nlev)
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

   type (type_stream), pointer :: c
!
!EOP
!-----------------------------------------------------------------------
!BOC
   nstreams = 0
   nullify(first_stream)

   if (.not.lake) return

   open(unit,file='streams.nml',action='read',status='old',err=98)
   LEVEL1 'configure_streams_nml'
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
         c => first_stream
      else
         c => first_stream
         do while (associated(c%next))
            c => c%next
         end do
         allocate(c%next)
         c => c%next
      end if

      LEVEL2 '.... ',trim(name),method,real(zl),real(zu)
      c%name = name
      c%method = method
      c%zu   = zu
      c%zl   = zl

      nstreams = nstreams + 1

   end do
97 close(unit)

   return

99 FATAL 'Error reading namelist "stream" from streams.nml.'
   stop 'config_streams_nml'

98 LEVEL2 'I could not open streams.nml. Inflows will not be used.'
   LEVEL2 'If that''s not what you want you have to supply streams.nml.'

   end subroutine configure_streams_nml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: configures everything related to the streams
!
! !INTERFACE:
   subroutine configure_streams_yaml()
!
! !DESCRIPTION:
!  Initialises everything related to the lake model, e.g. allocating memory
!  for arrays.
!
! !LOCAL VARIABLES:
   type (type_stream_populator), pointer :: stream_populator
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'configure_streams_yaml'
   cfg => settings_store%get_typed_child('streams')
   allocate(stream_populator)
   call cfg%populate(stream_populator)
   LEVEL2 'done'
   end subroutine configure_streams_yaml
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: configures everything related to the streams
!
! !INTERFACE:
   subroutine register_stream(self, pair)
!
! !DESCRIPTION:
!
! !USES:
   use yaml_settings
!
! !INPUT PARAMETERS:
   class (type_stream_populator), intent(inout) :: self
   type (type_key_value_pair), intent(inout) :: pair
!
! !LOCAL VARIABLES:
   class (type_settings), pointer :: child
   type (type_stream), pointer :: stream
   integer, parameter :: rk = kind(_ONE_)
!EOP
!-----------------------------------------------------------------------
!BOC
   allocate(stream)
   stream%next => first_stream
   first_stream => stream

   child => type_settings_create(pair,'stream configuration')
   stream%name = pair%name
   select type (child)
      class is (type_gotm_settings)
         call child%get(stream%method, 'method', 'inflow method', default=1)
         call child%get(stream%zu, 'zu', 'upper limit','m',default=0._rk)
         call child%get(stream%zl, 'zl', 'lower limit','m',default=0._rk)
         call child%get(stream%flow,'flow','water flow','m^3/s',default=0._rk)
         call child%get(stream%temp,'temp','flow temperature','Celcius',default=-1._rk)
         stream%has_T = .true.
         if (stream%temp%method .eq. 0 .and. stream%temp%constant_value .lt. 0._rk) stream%has_T = .false.
         call child%get(stream%salt,'salt','flow salinity','PSU', default=-1._rk)
         stream%has_S = .true.
         if (stream%salt%method .eq. 0 .and. stream%salt%constant_value .lt. 0._rk) stream%has_S = .false.
   end select
   end subroutine register_stream
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: configures everything related to the streams
!
! !INTERFACE:
   subroutine post_init_streams(nlev)
!
! !DESCRIPTION:
!
! !USES:
!
! !INPUT PARAMETERS:
   integer, intent(in)            :: nlev
!
! !LOCAL VARIABLES:
   integer                        :: rc
   type (type_stream), pointer    :: c
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'post_init_streams'
   c => first_stream

   do while (associated(c))

      call register_input(c%flow)
      call register_input(c%temp)
      call register_input(c%salt)

      allocate(c%weights(1:nlev),stat=rc)
      if (rc /= 0) STOP 'init_observations: Error allocating (weights)'
      c%weights = _ZERO_

      allocate(c%Q(1:nlev),stat=rc)
      if (rc /= 0) STOP 'init_observations: Error allocating (Q)'
      c%Q = _ZERO_

      if (c%method .eq. interleaving .and. .not. c%has_T) then
         LEVEL1 trim(c%name),' ....'
         FATAL "Can't specify - interleaving - without providing either S or T"
         stop 'post_init_streams()'
      end if

      select case (c%method)
         case (surface_flow)
            c%weights(nlev) = _ONE_
         case (bottom_flow)
            c%weights(1) = _ONE_
!        other methods are dynamic are are update in update_streams()
      end select

      nstreams = nstreams + 1
      c => c%next
   end do
   return
   end subroutine post_init_streams
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: calculate streams
!
! !INTERFACE:
   subroutine update_streams(nlev,dt,S,T,z,zi,h,Qs,Qt,Ls,Lt,Q)
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
   REALTYPE,dimension(0:nlev),intent(in)  :: z,zi,h
   REALTYPE,dimension(0:nlev),intent(inout) :: Qs,Qt,Ls,Lt,Q
!
! !LOCAL VARIABLES:
   integer              :: n,nmin,nmax
   REALTYPE             :: rhoI,rho
   REALTYPE             :: depth
   REALTYPE             :: TI,SI
   type (type_stream), pointer :: c
!EOP
!-----------------------------------------------------------------------
!BOC
   Qs = _ZERO_
   Qt = _ZERO_
   Ls = _ZERO_
   Lt = _ZERO_
   Q  = _ZERO_

   c => first_stream
   do while (associated(c))

      if (c%flow%value .eq. _ZERO_) then
         c => c%next
         cycle
      end if

      c%Q = _ZERO_

      select case (c%method)
         case (surface_flow)
            nmin = nlev ; nmax = nlev
         case (bottom_flow)
            nmin = 1 ; nmax = 1
         case (depth_range)
            nmin = nlev
            do n=1,nlev
               if ( c%zl .lt. zi(n) ) then
                  nmin = n
                  exit
               end if
            end do
            nmax = nmin
            do n=nlev,nmin,-1
               if ( zi(n-1) .lt. c%zu ) then
                  nmax = n
                  exit
               end if
            end do

            call get_weights(nlev,nmin,nmax,h,zi,c)

         case (interleaving)

            TI = c%temp%value
            SI = c%salt%value

            ! find minimal depth where the inflow will take place
            nmin = nlev
            rhoI = unesco(SI,TI,_ZERO_,.false.)
            do n=1,nlev
               depth = zi(nlev) - z(n)
               rho = unesco(S(n),T(n),depth/10.0d0,.false.)
!KBSTDERR depth,TI,T(n)
!KBSTDERR depth,rhoI,rho
               ! if the density of the inflowing water is greater than the
               ! ambient water then the lowest interleaving depth is found
               if (rhoI > rho) then
                  nmin = n
                  exit
               end if
            end do
!KBSTDERR 'nmin ',nmin,rhoI,rho

            nmax = nmin
            do n=nlev,nmin,-1
               depth = zi(nlev) - z(n)
               rho = unesco(S(n),T(n),depth/10.0d0,.false.)
!KBSTDERR depth,TI,T(n)
!KBSTDERR depth,rhoI,rho
               if ( rhoI < rho ) then
                  nmax = n
                  exit
               end if
            end do
!KBSTDERR 'nmax ',nmax,rhoI,rho

            call get_weights(nlev,nmin,nmax,h,zi,c)

      end select
!KBSTDERR c%has_T,c%has_S
!KBSTDERR nmin,nmax,sum(c%weights)


!     weights have been found - now apply them for the flow
      c%Q = c%weights*c%flow%value

      ! inflow stream
      if (c%flow%value .gt. _ZERO_) then
         int_inflow = int_inflow + dt*c%flow%value

         if (c%has_T) then
            TI = c%temp%value
         else
!KB         is there a mean !!!!
!           KK-TODO: maybe the mean should be volume-weighted
            TI = sum(T(nmin:nmax))/(nmax-nmin+1)
         end if
         if (c%has_S) then
            SI = c%salt%value
         else
            SI = _ZERO_
         end if

         do n=1,nlev
            Qt(n) = Qt(n) + TI * c%Q(n)
            Qs(n) = Qs(n) + SI * c%Q(n)
         end do

      else ! outflow

         int_outflow = int_outflow + dt*c%flow%value

         if (c%has_T) then
            TI = c%temp%value
            do n=1,nlev
               Qt(n) = Qt(n) + TI * c%Q(n)
            end do
         else
            do n=1,nlev
               Lt(n) = Lt(n) + c%Q(n)
            end do
         end if
         if (c%has_S) then
            SI = c%salt%value
            do n=1,nlev
               Qs(n) = Qs(n) + SI * c%Q(n)
            end do
         else
            do n=1,nlev
               Ls(n) = Ls(n) + c%Q(n)
            end do
         end if

      end if

      Q(1:nlev) = Q(1:nlev) + c%Q

      c => c%next
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

   select case (stream%method) 
      case (surface_flow, bottom_flow)
      case (depth_range)
!        check: d == yh+yi+yl
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
      case (interleaving)
         d  = zi(nmax) - zi(nmin-1)
         stream%weights(nmin:nmax) = h(nmin:nmax)/d
   end select

   if (abs(sum(stream%weights) - _ONE_) .gt. 0.00001) then
      FATAL 'Check weight calculations in streams::get_weights()'
      STDERR stream%weights
      stop
   end if

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
   type (type_stream), pointer :: c, n
!
!-----------------------------------------------------------------------
!BOC
   c => first_stream
   do while (associated(c))
      n => c%next
      deallocate(c%Q)
      deallocate(c)
      c => n
   end do

   end subroutine clean_streams

   end module streams
!EOC
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

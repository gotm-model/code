#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: inflows
!
! !INTERFACE:
   module inflows
!
! !DESCRIPTION:
! This module is responsible for all calculations related to inflows. This means
! reading in prescribed values from a given file, calculating the depth where
! the inflowing water masses interleave, and calculating vertical fluxes. These
! vertical fluxes are used by other routines to calculate vertical advection
! velocities.
!
! !USES
   use input

!  !PUBLIC DATA MEMBERS:
   IMPLICIT NONE
   public                                :: init_inflows,clean_inflows
   public                                :: update_inflows
   public                                :: type_inflow,ninflows,first_inflow
   REALTYPE, public                      :: int_inflow=_ZERO_
   REALTYPE, public                      :: int_outflow=_ZERO_
!
! !REVISION HISTORY:
!  Original author(s): Lennart Schueler
!
!EOP
!
! !LOCAL VARIABLES:
   type type_inflow
      character(len=64)      :: name = ''
      REALTYPE               :: QI,SI,TI
      logical                :: has_S = .false.
      logical                :: has_T = .false.
      REALTYPE, allocatable  :: Q(:)
      type (type_inflow), pointer :: next => null()
   end type

   integer                     :: ninflows
   type (type_inflow), pointer :: first_inflow

! !DEFINED PARAMETERS:
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: initialises everything related to the inflows
!
! !INTERFACE:
   subroutine init_inflows(lake,nlev)
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
   character(len=PATH_MAX) :: Q_file,T_file,S_file
   integer                 :: Q_col,T_col,S_col

   namelist /inflow/ name,Q_file,T_file,S_file,Q_col,T_col,S_col

   type (type_inflow), pointer :: current_inflow
!
!EOP
!-----------------------------------------------------------------------
!BOC
   ninflows = 0
   nullify(first_inflow)

   if (.not.lake) return

   open(unit,file='inflows.nml',action='read',status='old',err=98)
   LEVEL1 'init_inflows'
   do
      name   = ''
      Q_file = ''
      S_file = ''
      T_file = ''
      Q_col = 1
      T_col = 1
      S_col = 1

      read(unit,nml=inflow,err=99,end=97)
      if (.not.associated(first_inflow)) then
         allocate(first_inflow)
         current_inflow => first_inflow
      else
         current_inflow => first_inflow
         do while (associated(current_inflow%next))
            current_inflow => current_inflow%next
         end do
         allocate(current_inflow%next)
         current_inflow => current_inflow%next
      end if

      if (name=='') then
         FATAL 'Error: "name" must be provided in namelist "inflow".'
         stop 'init_inflows'
      end if
      if (Q_file=='') then
         FATAL 'Error: "Q_file" must be provided in namelist "inflow".'
         stop 'init_inflows'
      end if

      LEVEL2 '.... ',trim(name)
      current_inflow%name = name
      call register_input_0d(Q_file,Q_col,current_inflow%QI,'observed inflow: discharge')
      if (T_file/='') then
         call register_input_0d(T_file,T_col,current_inflow%TI,'observed inflow: temperature')
         current_inflow%has_T = .true.
      end if
      if (S_file/='') then
         call register_input_0d(S_file,S_col,current_inflow%SI,'observed inflow: salinity')
         current_inflow%has_S = .true.
      end if
      allocate(current_inflow%Q(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_observations: Error allocating (Q)'
      current_inflow%Q = _ZERO_
      ninflows = ninflows + 1

   end do
97 close(unit)

   return

99 FATAL 'Error reading namelist "inflow" from inflows.nml.'
   stop 'init_inflows'

98 LEVEL2 'I could not open inflows.nml. Inflows will not be used.'
   LEVEL2 'If that''s not what you want you have to supply inflows.nml.'

   end subroutine init_inflows
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: calculate inflows
!
! !INTERFACE:
   subroutine update_inflows(nlev,dt,S,T,zeta,z,h,Ac,Qs,Qt,FQ)
!
! !DESCRIPTION:
!  Calculates the depth where the inflow occurs and
!  what effects it has on the water column. The resulting flux variables
!  can then be used by routines like salinity or temperature to calculate the
!  vertical advection induced by the inflows.
!
! !USES:
   use eqstate, only: unesco

   IMPLICIT NONE

! !INPUT PARAMETERS:
   integer, intent(in)                    :: nlev
   REALTYPE, intent(in)                   :: dt,zeta
   REALTYPE, intent(in)                   :: S(0:nlev), T(0:nlev)
   REALTYPE, intent(in)                   :: z(0:nlev), h(0:nlev), Ac(0:nlev)
   REALTYPE, intent(inout)                :: Qs(0:nlev), Qt(0:nlev)
   REALTYPE, intent(inout)                :: FQ(0:nlev)
!EOP
!
! !LOCAL VARIABLES:
   integer              :: i,n
   REALTYPE             :: rhoI,rho
   REALTYPE             :: depth
   REALTYPE             :: VI_basin
   REALTYPE             :: TI,SI
   REALTYPE,dimension(0:nlev) :: Q
   integer              :: index_min
   type (type_inflow), pointer :: current_inflow
!
!-----------------------------------------------------------------------
!BOC
   Q  = _ZERO_
   Qs = _ZERO_
   Qt = _ZERO_
   FQ = _ZERO_

   current_inflow => first_inflow
   do while (associated(current_inflow))
      current_inflow%Q = _ZERO_

      ! inflow triggered or still in progress
      if (current_inflow%QI .ge. _ZERO_) then

         int_inflow = int_inflow + dt*current_inflow%QI

         if (current_inflow%has_T) then
            TI = current_inflow%TI
         else
            TI = T(nlev)
         end if
         if (current_inflow%has_S) then
            SI = current_inflow%SI
         else
            SI = S(nlev)
         end if

         ! find minimal depth where the inflow will take place
         index_min = 0
         do i=1,nlev
            depth = zeta - z(i)
            rhoI = unesco(SI,TI,depth/10.0d0,.false.)
            rho = unesco(S(i),T(i),depth/10.0d0,.false.)
            ! if the density of the inflowing water is greater than the
            ! ambient water then the lowest interleaving depth is found
            if (rhoI > rho) then
               index_min = i
               exit
            end if
         end do

         !density of the inflowing water is too small -> no inflow
         if (index_min .eq. 0) then
            return
         endif

         ! find the z-levels in which the water will interleave
         VI_basin = _ZERO_
         n = index_min
         do while (VI_basin < current_inflow%QI*dt)
            VI_basin = VI_basin + Ac(n) * h(n)
            n = n+1
            if (n .gt. nlev) then
               !if inflow at surface -> no inflow
               !debug output only
               write(*,*) "Warning: Too much water flowing into the basin."
               return
            end if
         end do
         ! VI_basin is now too big so go back one step
         n = n-1

         ! calculate the source terms
         ! "+1" because loop includes both n and index_min
         do i=index_min,n
            current_inflow%Q(i) = current_inflow%QI / (n-index_min+1)
            Q (i) = Q (i) +      current_inflow%Q(i)
            Qs(i) = Qs(i) + SI * current_inflow%Q(i) / (Ac(i) * h(i))
            Qt(i) = Qt(i) + TI * current_inflow%Q(i) / (Ac(i) * h(i))
         end do

         ! calculate the sink term at sea surface
         !Qs(nlev) = Qs(nlev) -S(nlev) * FQ(nlev-1) / (Ac(nlev) * h(nlev))
         !Qt(nlev) = Qt(nlev) -T(nlev) * FQ(nlev-1) / (Ac(nlev) * h(nlev))
      else
         int_outflow = int_outflow + dt*current_inflow%QI
STDERR trim(current_inflow%name),' ',int_outflow
      end if
      current_inflow => current_inflow%next
   end do

   ! calculate the vertical flux terms
   do i=1,nlev-1
      FQ(i) = FQ(i-1) + Q(i)
   end do


   end subroutine update_inflows
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: cleaning up
!
! !INTERFACE:
   subroutine clean_inflows()
!
! !DESCRIPTION:
!  De-allocates all memory allocated via init\_inflows()
!
! !USES:
   IMPLICIT NONE
!EOP
!
! !LOCAL VARIABLES:
   type (type_inflow), pointer :: current_inflow, next_inflow
!
!-----------------------------------------------------------------------
!BOC
   current_inflow => first_inflow
   do while (associated(current_inflow))
      next_inflow => current_inflow%next
      deallocate(current_inflow%Q)
      deallocate(current_inflow)
      current_inflow => next_inflow
   end do

   end subroutine clean_inflows

   end module inflows
!EOC
!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

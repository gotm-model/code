#ifdef SEAGRASS

#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: seagrass --- sea grass dynamics \label{sec:seagrass}
!
! !INTERFACE:
   module seagrass
!
! !DESCRIPTION:
! In this module, seagrass canopies are treated as Lagrangian tracers,
! which either advect passively with the horizontal current speed or
! rest at their excursion limits and thus exert friction on the mean flow,
! see \cite{VerduinBackhaus2000}.
! Turbulence generation due to seagrass friction is possible, see
! namelist file {\tt seagrass.nml}. The extra production term
! in the balance of TKE, \eq{tkeA}, is included as described in
! \sect{sec:production}.

!
! !USES:
   IMPLICIT NONE
!
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_seagrass, do_seagrass, end_seagrass
   logical, public                     :: seagrass_calc
!
! !REVISION HISTORY:!
!  Original author(s): Hans Burchard & Karsten Bolding
!
   REALTYPE, dimension(:), allocatable :: xx,yy
   REALTYPE, dimension(:), allocatable :: exc,vfric,grassz,xxP
   logical                   :: init_output
!  from a namelist
   character(len=PATH_MAX)   :: grassfile='seagrass.dat'
   REALTYPE                  :: XP_rat
   integer                   :: grassind
   integer                   :: grassn
   integer                   :: out_unit
   integer                   :: maxn

   REALTYPE, parameter       :: miss_val = -999.0  
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the sea grass module
!
! !INTERFACE:
   subroutine init_seagrass(namlst,fname,unit,nlev,h,fm)
!
! !DESCRIPTION:
! Here, the seagrass namelist {\tt seagrass.nml} is read
! and memory is allocated
! for some relevant vectors. Afterwards, excursion limits and friction
! coefficients are read from a file. The uppermost grid related index
! for the seagrass canopy is then calculated.
!
! !USES:
   use field_manager
!
! !INPUT PARAMETERS:
   integer,          intent(in)   :: namlst
   character(len=*), intent(in)   :: fname
   integer,          intent(in)   :: unit
   integer,          intent(in)   :: nlev
   REALTYPE,         intent(in)   :: h(0:nlev)
   type (type_field_manager), intent(inout) :: fm
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   integer                   :: i,rc
   REALTYPE                  :: z
   namelist /canopy/  seagrass_calc,grassfile,XP_rat
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_seagrass'

   init_output = .true.
   seagrass_calc = .false.

   maxn=nlev

!  Open and read the namelist
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=canopy,err=99)
   close(namlst)

   if (seagrass_calc) then
      out_unit=unit

      open(unit,status='unknown',file=grassfile)

      read(unit,*) grassn

      allocate(xx(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (xx)'
      xx = _ZERO_

      allocate(yy(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (yy)'
      yy = _ZERO_

      allocate(xxP(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (xxP)'
      xxP= _ZERO_

      allocate(exc(0:grassn),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (exc)'
      exc = _ZERO_

      allocate(vfric(0:grassn),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (vfric)'
      vfric = _ZERO_

      allocate(grassz(0:grassn),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (grassz)'
      grassz = _ZERO_

      do i=1,grassn
         read(unit,*) grassz(i),exc(i),vfric(i)
      end do

      z=0.5*h(1)
      do i=2,nlev
         z=z+0.5*(h(i-1)+h(i))
         if (grassz(grassn).gt.z) grassind=i
      end do

      close(unit)

      xx(grassind+1:nlev) = miss_val
      yy(grassind+1:nlev) = miss_val

      call fm%register('x_excur', 'm', 'seagrass excursion(x)', dimensions=(/id_dim_z/), data1d=xx(1:nlev), fill_value=miss_val, category='seagrass')
      call fm%register('y_excur', 'm', 'seagrass excursion(y)', dimensions=(/id_dim_z/), data1d=yy(1:nlev), fill_value=miss_val, category='seagrass')

      LEVEL2 'seagrass initialised ...'

   end if
   return

98 LEVEL2 'I could not open seagrass.nml'
   LEVEL2 'Ill continue but set seagrass_calc to false.'
   LEVEL2 'If thats not what you want you have to supply seagrass.nml'
   LEVEL2 'See the Seagrass example on www.gotm.net for a working seagrass.nml'
   seagrass_calc = .false.
   return
99 FATAL 'I could not read seagrass.nml'
   stop 'init_seagrass'
   end subroutine init_seagrass
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the sea grass model
!
! !INTERFACE:
   subroutine do_seagrass(nlev,dt)
!
! !DESCRIPTION:
!
!  Here the time depending seagrass equation suggested by
!  \cite{VerduinBackhaus2000} is calculated. In order to
!  explain the basic principle, an idealised example is examined here
!  with a simplified momentum equation,
!
!  \begin{equation}
!  \partial_t u - \partial_z(\nu_t \partial_z u) = -g\partial_x\zeta-C_fu|u|
!  \comma
!  \end{equation}
!  and the Lagrangian tracer equation for seagrass,
!  \begin{equation}
!  \partial_t X =
!  \left\{
!  \begin{array}{ll}
!  u & \mbox{ for } |X|< X_{\max} \mbox{ or } X \cdot u <0,\\
!  0 & \mbox{ else}
!  \comma
!  \end{array}
!  \right.
!  \end{equation}
!  where $X$ is the Langrangian horizontal excursion of the seagrass.
!  The seagrass friction coefficient, $C_f$, is only non--zero at heights
!  where seagrass tracers are at their excursion limits:
!
!  \begin{equation}
!  C_f =
!  \left\{
!  \begin{array}{ll}
!  C_f^{\max} & \mbox{ for } |X|=X_{\max} \comma \\
!  0 & \mbox{ else} \point
!  \end{array}
!  \right.
!  \end{equation}
!
!  The maximum excursion limits $X_{\max}$ and the friction coefficients
!  $C_f^{\max}$ are read from a file.
!
!  The production of turbulence is calculated here as the sum of shear
!  production and friction loss at the seagrass leaves,
!  \begin{equation}
!   \label{sgProduction}
!    X_P = \alpha_{sg} C_f |u|^3
!   \comma
!  \end{equation}
!  which is added to the usual shear--production term as illustrated in
!  \eq{computeP}. The efficiency coefficient of turbulence production
!  by sea--grass friction, $\alpha_{sg}$, is denoted as {\tt xP\_rat}
!  in the code. It has to be read--in from the {\tt canopy} namelist.
!  For details and example calculations, see \cite{BurchardBolding2000}.
!
! !USES:
   use meanflow, only:     u,v,h,drag,xP
!
! !INPUT PARAMETERS:
   integer,  intent(in)     :: nlev
   REALTYPE, intent(in)     :: dt
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: dist
   REALTYPE                  :: grassfric(0:nlev)
   REALTYPE                  :: excur(0:nlev)
   REALTYPE                  :: z(0:nlev)
!EOP
!-----------------------------------------------------------------------
   if (seagrass_calc) then

      z(1)=0.5*h(1)
      do i=2,nlev
         z(i)=z(i-1)+0.5*(h(i-1)+h(i))
      end do

!     Interpolate excursion limits and friction to actual grid.

      call gridinterpol(grassn,1,grassz,exc,nlev,z,excur)
      call gridinterpol(grassn,1,grassz,vfric,nlev,z,grassfric)

      do i=1,grassind
         xx(i)=xx(i)+dt*u(i)                ! Motion of seagrass elements with
         yy(i)=yy(i)+dt*v(i)                ! mean flow.
         dist=sqrt(xx(i)*xx(i)+yy(i)*yy(i))
         if (dist .gt. excur(i)) then       ! Excursion limit reached
            xx(i)= excur(i)/dist * xx(i)
            yy(i)= excur(i)/dist * yy(i)

!           Increased drag by seagrass friction
            drag(i)=drag(i)+grassfric(i)

!           Extra turbulence production by seagrass friction
            xxP(i)=xP_rat*grassfric(i)*(sqrt(u(i)**2+v(i)**2))**3
         else
            xxP(i)=_ZERO_
         end if
      end do

!     Interpolate onto turbulence grid points
      do i=1,nlev-1
         xP(i)=0.5*(xxP(i)+xxP(i+1))
      end do

   end if
   return
   end subroutine do_seagrass
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the sea grass calculations
!
! !INTERFACE:
   subroutine end_seagrass
!
! !DESCRIPTION:
!  Nothing done yet --- supplied for completeness.
!
! !USES:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-----------------------------------------------------------------------
!BOC

   if (seagrass_calc) then
      if (allocated(xx))     deallocate(xx)
      if (allocated(yy))     deallocate(yy)
      if (allocated(xxP))    deallocate(xxP)
      if (allocated(exc))    deallocate(exc)
      if (allocated(vfric))  deallocate(vfric)
      if (allocated(grassz)) deallocate(grassz)
   end if

   return
   end subroutine end_seagrass
!EOC
!-----------------------------------------------------------------------

   end module seagrass

#endif

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

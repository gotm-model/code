#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The vertical grid \label{sec:updategrid}
!
! !INTERFACE:
   subroutine updategrid(nlev,dt,zeta)
!
! !DESCRIPTION:
!  This subroutine calculates for each time step new layer thicknesses
!  in order to fit them to the changing water depth.
!  Three different grids can be specified:
!  \begin{enumerate}
!  \item Equidistant grid with possible zooming towards surface and bottom.
!  The number of layers, {\tt nlev}, and the zooming factors,
!  {\tt ddu}=$d_u$ and  {\tt ddl}=$d_l$,
!  are specified in {\tt gotmmean.nml}.
!  Zooming is applied according to the formula
!  \begin{equation}\label{formula_Antoine}
!    \gamma_i = \frac{\mbox{tanh}\left( (d_l+d_u)\frac{i}{M}-d_l\right)
!    +\mbox{tanh}(d_l)}{\mbox{tanh}(d_l)+\mbox{tanh}(d_u)}-1
!   \point
!  \end{equation}
!  with $\gamma_i$ being the non-dimensional vertical position of the
!  $i$-th interface in \eq{grid}.
!
!  From this formula, the following grids are constructed:
!  \begin{itemize}
!    \item $d_l=d_u=0$ results in equidistant discretisations.
!    \item $d_l>0, d_u=0$ results in zooming near the bottom.
!    \item $d_l=0, d_u>0$ results in zooming near the surface.
!    \item $d_l>0, d_u>0$ results in double zooming nea both,
!          the surface and the bottom.
!  \end{itemize}
!
!  \item Sigma-layers. The fraction that every layer occupies is
!  read-in from file, see {\tt gotmmean.nml}.
!  \item Cartesian layers. The height of every layer is read in from file,
!  see {\tt gotmmean.nml}.
!  This method is not recommended when a varying sea surface is considered.
!  \end{enumerate}
!
!  Furthermore, vertical velocity profiles are calculated here, if
!  {\tt w\_adv\_method} is 1 or 2, which has to be chosen in the
!  {\tt w\_advspec} namelist in {\tt obs.nml}. The profiles of vertical
!  velocity are determined by two values,
!  the height of maximum absolute value of vertical velocity, {\tt w\_height},
!  and the vertical velocity at this height, {\tt w\_adv}. From {\tt w\_height},
!  the vertical velocity is linearly decreasing towards the surface and
!  the bottom, where its value is zero.

!
! !USES:
   use meanflow,     only: grid_ready
   use meanflow,     only: depth0,depth
   use meanflow,     only: ga,z,h,ho,ddu,ddl,grid_method
   use meanflow,     only: grid_file,w
   use observations, only: zeta_method,w_adv_method
   use observations, only: w_adv,w_height
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                :: dt,zeta
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i,j,nlayers
   REALTYPE                  :: zi(0:nlev),z_crit
   integer, parameter        :: grid_unit = 101
!-----------------------------------------------------------------------
!BOC
   if (.not. grid_ready) then ! Build up dimensionless grid (0<=ga<=1)
      select case (grid_method)
      case(0) !Equidistant grid with possible zooming to surface and bottom
         LEVEL2 "sigma coordinates (zooming possible)"
         if (ddu .le. 0 .and. ddl .le. 0) then
            do i=1,nlev
               ga(i)=ga(i-1)+_ONE_/nlev
            end do
         else
            do i=1,nlev ! This zooming routine is from Antoine Garapon, ICCH, DK
               ga(i)=tanh((ddl+ddu)*i/nlev-ddl)+tanh(ddl)
               ga(i)=ga(i)/(tanh(ddl)+tanh(ddu))
            end do
         end if
         depth = depth0 + Zeta
         do i=1,nlev
            h(i)=(ga(i)-ga(i-1))*depth
         end do
      case(1) !Sigma, the fraction each layer occupies is specified.
         LEVEL2 "external specified sigma coordinates"
         open (grid_unit,FILE =grid_file,status='unknown',ERR=100)
         read (grid_unit,*) nlayers
         if (nlayers /= nlev) then
            FATAL "number of layers spefified in file <> # of model layers"
            stop 'updategrid'
         end if
         depth = _ZERO_
         j = 0
         do i=nlev,1,-1 !The first layer to be read is at the surface
            read(grid_unit,*,ERR=101,END=101) ga(i)
            depth = depth + ga(i)
            j=j+1
         end do
         if (j /= nlayers) then
            FATAL "number of layers read from file <> # of model layers"
            stop 'updategrid'
         end if
         close (grid_unit)
         if (abs(depth-1.).gt.1.e-8) then
            FATAL "sum of all layers in grid_file should be 1."
            stop 'updategrid'
         end if
     case(2) !Cartesian, the layer thickness is read from file
         LEVEL2 "external specified cartesian coordinates"
         open (grid_unit,FILE =grid_file,ERR=100)
! Observations is called after meanflow is initialised, and we don#t have
! zeta_method
!        if (zeta_method /= 0) then
!          stop "You are using Cartesian coordinates with varying surface elevation"
!        end if
         read (grid_unit,*) nlayers
         if(nlayers /= nlev) then
            FATAL "nlev must be equal to the number of layers in: ", &
                   trim(grid_file)
            stop 'updategrid'
         end if
         depth = _ZERO_
         j=0
         do i=nlev,1,-1 !The first layer read is the surface
            read(grid_unit,*,ERR=101) h(i)
            depth = depth + h(i)
            j=j+1
         end do
         if (j /= nlayers) then
            FATAL "number of layers read from file <> # of model layers"
            stop 'updategrid'
         end if
         close (grid_unit)
         if (abs(depth-depth0).gt.1.e-5) then
            FATAL "sum of all layers should be equal to the total depth",depth0,depth
            stop 'updategrid'
         end if
     case(3) ! Adaptive grid
          ga(0)=-1.
          if (ddu.le.0.and.ddl.le.0) then
             do i=1,nlev
                ga(i)=ga(i-1)+1/float(nlev)
             end do
          else
             do i=1,nlev ! This zooming is from Antoine Garapon, ICCH, DK
                ga(i)=tanh((ddl+ddu)*i/nlev-ddl)+tanh(ddl)
                ga(i)=ga(i)/(tanh(ddl)+tanh(ddu))-1.
             end do
          end if
          depth = depth0 + Zeta
          do i=1,nlev
             h(i)  = (ga(i)-ga(i-1)) * depth
          end do
     case default
         stop "updategrid: No valid grid_method specified"
     end select

     grid_ready=.true.  !  Grid is now initialised !
   end if

   depth = depth0 + zeta

   select case(grid_method)
   case (0)
      do i=1,nlev
         ho(i) = h(i)
         h(i)  = (ga(i)-ga(i-1)) * depth
      end do
   case (1)
      ho = h
      h = ga *depth
   case (2)
      ho=h
    case default
         stop "updategrid: No valid grid_method specified"
   end select

   z(1)=-depth0+0.5*h(1)
   do i=2,nlev
      z(i)=z(i-1)+0.5*(h(i-1)+h(i))
   end do

!  Vertical velocity calculation:

   select case(w_adv_method)
      case(0)
         ! no vertical advection
      case(1,2)
         ! linearly varying advection velocity with peak at "w_height"
         zi(0)=-depth0
         do i=1,nlev
            zi(i)=zi(i-1)+h(i)
         end do
         z_crit=zi(nlev)-0.01*(zi(nlev)-zi(0))
         if (w_height.gt.z_crit) w_height=z_crit
         z_crit=zi(0)+0.01*(zi(nlev)-zi(0))
         if (w_height.lt.z_crit) w_height=z_crit
         do i=1,nlev-1
            if (zi(i).gt.w_height) then
               w(i)=(zi(nlev)-zi(i))/(zi(nlev)-w_height)*w_adv
            else
               w(i)=(zi(0)-zi(i))/(zi(0)-w_height)*w_adv
            end if
         end do
         w(0)    =_ZERO_
         w(nlev) =_ZERO_
      case default
    end select

   return

100 FATAL 'Unable to open ',trim(grid_file),' for reading'
   stop 'updategrid'
101 FATAL 'Error reading grid file ',trim(grid_file)
   stop 'updategrid'

   end subroutine updategrid
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

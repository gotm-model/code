!$Id: bio_npzd_0d.F90,v 1.1 2008-10-31 11:10:32 jorn Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_npzd_0d --- 0D NPZD biogeochemical model
!
! !INTERFACE:
   module bio_npzd_0d
!
! !DESCRIPTION:
! The NPZD (nutrient-phytoplankton-zooplankton-detritus) model described here
! consists of $I=4$ state variables.
! Nutrient uptake (phytoplankton growth) is limited by light and nutrient
! availability, the latter of which is modelled by means
! of Michaelis-Menten kinetics, see eq.\ (\ref{dnp}).
! The half-saturation nutrient concentration $\alpha$ used in this
! formulation has typically a value between 0.2 and 1.5 mmol N\, m$^{-3}$.
! Zooplankton grazing which is limited by the phytoplankton standing stock
! is modelled by means of an Ivlev formulation, see eq.\ (\ref{dpz}).
! All other processes are based on linear first-order kinematics,
! see eqs.\ (\ref{dpn}) - (\ref{dzd}).
! For all details of the NPZD model implemented here, 
! see \cite{Burchardetal2005b}.
!
! !USES:
   use bio_0d_base

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio_npzd_0d, get_var_info_npzd_0d, do_bio_npzd_0d
!
! !PRIVATE DATA MEMBERS:
!
! !REVISION HISTORY:!
!  Original author(s): Jorn Bruggeman
!
!
! !LOCAL VARIABLES:
!  from a namelist
   REALTYPE                  :: n_initial=4.5
   REALTYPE                  :: p_initial=0.
   REALTYPE                  :: z_initial=0.
   REALTYPE                  :: d_initial=4.5
   REALTYPE, public          :: p0=0.0225
   REALTYPE                  :: z0=0.0225
   REALTYPE                  :: w_p=-1.157407e-05
   REALTYPE                  :: w_d=-5.787037e-05
   REALTYPE, public          :: kc=0.03
   REALTYPE                  :: i_min=25.
   REALTYPE                  :: rmax=1.157407e-05
   REALTYPE                  :: gmax=5.787037e-06
   REALTYPE                  :: iv=1.1
   REALTYPE                  :: alpha=0.3
   REALTYPE                  :: rpn=1.157407e-07
   REALTYPE                  :: rzn=1.157407e-07
   REALTYPE                  :: rdn=3.472222e-08
   REALTYPE                  :: rpdu=2.314814e-07
   REALTYPE                  :: rpdl=1.157407e-06
   REALTYPE                  :: rpd
   REALTYPE                  :: rzd=2.314814e-07
   REALTYPE                  :: aa=0.62
   REALTYPE                  :: g2=20.0
   integer, parameter        :: n=1,p=2,z=3,d=4
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the bio module
!
! !INTERFACE:
   subroutine init_bio_npzd_0d(namlst,fname,unit,modelinfo)
!
! !DESCRIPTION:
!  Here, the bio namelist {\tt bio\_npzd.nml} is read and 
!  various variables (rates and settling velocities) 
!  are transformed into SI units.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,          intent(in)   :: namlst
   character(len=*), intent(in)   :: fname
   integer,          intent(in)   :: unit
   
   type (type_model_info), intent(inout)  :: modelinfo
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   REALTYPE, parameter :: secs_pr_day = 86400.
   integer :: numc
   namelist /bio_npzd_nml/ numc, &
                      n_initial,p_initial,z_initial,d_initial,   &
                      p0,z0,w_p,w_d,kc,i_min,rmax,gmax,iv,alpha,rpn,  &
                      rzn,rdn,rpdu,rpdl,rzd,aa,g2
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'init_bio_npzd'

   numc=4

   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=bio_npzd_nml,err=99)
   close(namlst)

   LEVEL3 'namelist "', fname, '" read'

!  Conversion from day to second
   rpn  = rpn  /secs_pr_day
   rzn  = rzn  /secs_pr_day
   rdn  = rdn  /secs_pr_day
   rpdu = rpdu /secs_pr_day
   rpdl = rpdl /secs_pr_day
   rzd  = rzd  /secs_pr_day
   gmax = gmax /secs_pr_day
   rmax = rmax /secs_pr_day
   w_p  = w_p  /secs_pr_day
   w_d  = w_d  /secs_pr_day
   
   ! Transfer some model properties to the framework we are embedded in.
   modelinfo%numc = numc
   modelinfo%par_fraction = _ONE_-aa
   modelinfo%par_background_extinction = _ONE_/g2
   modelinfo%par_bio_background_extinction = kc*p0

   LEVEL3 'module initialized'

   return

98 LEVEL2 'I could not open bio_npzd.nml'
   LEVEL2 'If thats not what you want you have to supply bio_npzd.nml'
   LEVEL2 'See the bio example on www.gotm.net for a working bio_npzd.nml'
   return
99 FATAL 'I could not read bio_npzd.nml'
   stop 'init_bio_npzd_0d'
   end subroutine init_bio_npzd_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get information on the concentration variables
!
! !INTERFACE:
   subroutine get_var_info_npzd_0d(numc,varinfo)
!
! !DESCRIPTION:
!  The subroutine provides all properties of the state variables,
!  including the name, unit, initial value, sinking rate, etc.
!
! !USES:
   IMPLICIT NONE

! !INPUT PARAMETERS:
   integer,                   intent(in)    :: numc
   type (type_variable_info), intent(inout) :: varinfo(numc)

! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman

! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   varinfo(1)%name = 'nut'
   varinfo(1)%unit = 'mmol/m**3'
   varinfo(1)%longname = 'nutrients'
   varinfo(1)%initial_value = n_initial
   varinfo(1)%positive_definite = .true.

   varinfo(2)%name = 'phy'
   varinfo(2)%unit = 'mmol/m**3'
   varinfo(2)%longname = 'phytoplankton'
   varinfo(2)%initial_value = p_initial
   varinfo(2)%sinking_rate = -w_p
   varinfo(2)%positive_definite = .true.
   varinfo(2)%light_extinction = kc

   varinfo(3)%name = 'zoo'
   varinfo(3)%unit = 'mmol/m**3'
   varinfo(3)%longname = 'zooplankton'
   varinfo(3)%initial_value = z_initial
   varinfo(3)%positive_definite = .true.

   varinfo(4)%name = 'det'
   varinfo(4)%unit = 'mmol/m**3'
   varinfo(4)%longname = 'detritus'
   varinfo(4)%initial_value = d_initial
   varinfo(4)%sinking_rate = -w_d
   varinfo(4)%positive_definite = .true.
   varinfo(4)%light_extinction = kc

#if 0
   varinfo(2)%mussels_inhale = .true.
   varinfo(3)%mussels_inhale = .true.
   varinfo(4)%mussels_inhale = .true.
#endif

   end subroutine get_var_info_npzd_0d
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Michaelis-Menten formulation for nutrient uptake
!
! !INTERFACE:
   REALTYPE function fnp(n,p,par,iopt)
!
! !DESCRIPTION:
! Here, the classical Michaelis-Menten formulation for nutrient uptake
! is formulated.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: n,p,par,iopt
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   fnp=rmax*par/iopt*exp(1.-par/iopt)*n/(alpha+n)*(p+p0)
   return
   end function fnp
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Ivlev formulation for zooplankton grazing on phytoplankton
!
! !INTERFACE:
   REALTYPE function fpz(p,z)
!
! !DESCRIPTION:
! Here, the classical Ivlev formulation for zooplankton grazing on 
! phytoplankton is formulated.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: p,z
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   fpz=gmax*(1.-exp(-iv**2*p**2))*(z+z0)
   return
   end function fpz
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Right hand sides of NPZD model
!
! !INTERFACE:
   subroutine do_bio_npzd_0d(first,numc,cc,env,pp,dd)
!
! !DESCRIPTION:
! Seven processes expressed as sink terms are included in this
! conservative model, see eqs.\ (\ref{dnp}) - (\ref{dzd}). \\
!
! Nutrient uptake by phytoplankton:
! \begin{equation}\label{dnp}
! d_{np} = r_{\max}\frac{I_{PAR}}{I_{opt}}
! \exp\left(1-\frac{I_{PAR}}{I_{opt}}\right)
! \frac{c_n}{\alpha+c_n}c_p
! \end{equation}
! 
! with
! 
! \begin{equation}
! I_{opt}=\max\left(\frac14I_{PAR},I_{\min}\right).
! \end{equation}
! 
! Grazing of zooplankton on phytoplankton:
! \begin{equation}\label{dpz}
! d_{pz}=g_{\max}\left(1-\exp\left(-I_v^2c_p^2\right)\right)c_z
! \end{equation}
! 
! Phytoplankton excretion:
! \begin{equation}\label{dpn}
! d_{pn} = r_{pn} c_p
! \end{equation}
! 
! Zooplankton excretion:
! \begin{equation}\label{dzn}
! d_{zn} = r_{zn} c_z
! \end{equation}
! 
! Remineralisation of detritus into nutrients:
! \begin{equation}\label{ddn}
! d_{dn} = r_{dn} c_d
! \end{equation}
! 
! Phytoplankton mortality:
! \begin{equation}\label{dpd}
! d_{pd} = r_{pd} c_p
! \end{equation}
! 
! Zooplankton mortality:
! \begin{equation}\label{dzd}
! d_{zd} = r_{zd} c_z
! \end{equation}
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   logical, intent(in)                  :: first
   integer, intent(in)                  :: numc
   REALTYPE, intent(in)                 :: cc(1:numc)
   type (type_environment), intent(in)  :: env
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(inout)              :: pp(1:numc,1:numc)
   REALTYPE, intent(inout)              :: dd(1:numc,1:numc)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
! !LOCAL VARIABLES:
   REALTYPE, save             :: iopt
   integer                    :: i,j
!EOP
!-----------------------------------------------------------------------
!BOC

   if (first) then
      iopt=max(0.25*env%I_0,I_min)
   end if

   if (env%par .ge. i_min) then
      rpd=rpdu
   else
      rpd=rpdl
   end if

   dd(n,p)=fnp(cc(n),cc(p),env%par,iopt)  ! snp
   dd(p,z)=fpz(cc(p),cc(z))               ! spz
   dd(p,n)=rpn*cc(p)                      ! spn
   dd(z,n)=rzn*cc(z)                      ! szn
   dd(d,n)=rdn*cc(d)                      ! sdn
   dd(p,d)=rpd*cc(p)                      ! spd
   dd(z,d)=rzd*cc(z)                      ! szd

   do i=1,numc
      do j=1,numc
         pp(i,j)=dd(j,i)
      end do
   end do

   end subroutine do_bio_npzd_0d
!EOC


!-----------------------------------------------------------------------

   end module bio_npzd_0d

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

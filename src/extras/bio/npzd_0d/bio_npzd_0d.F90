!$Id: bio_npzd_0d.F90,v 1.4 2008-11-11 13:40:33 jorn Exp $
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
   use bio_types

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public type_npzd,init_bio_npzd_0d, get_var_info_npzd_0d, do_bio_npzd_0d, &
          get_bio_extinction_npzd_0d, get_conserved_quantities_npzd_0d
!
! !PRIVATE DATA MEMBERS:
!
! !REVISION HISTORY:!
!  Original author(s): Jorn Bruggeman
!
!
! !LOCAL VARIABLES:
!  from a namelist
   type type_npzd
      REALTYPE :: p0,z0,kc,i_min,rmax,gmax,iv,alpha,rpn,rzn,rdn,rpdu,rpdl,rzd
   end type
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
   function init_bio_npzd_0d(self,namlst,fname) result(modelinfo)
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
   type (type_npzd), intent(out)   :: self
   integer,          intent(in )   :: namlst
   character(len=*), intent(in )   :: fname
   
   type (type_model_info) :: modelinfo
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   REALTYPE                  :: n_initial=4.5
   REALTYPE                  :: p_initial=0.
   REALTYPE                  :: z_initial=0.
   REALTYPE                  :: d_initial=4.5
   REALTYPE                  :: p0=0.0225
   REALTYPE                  :: z0=0.0225
   REALTYPE                  :: w_p=-1.157407e-05
   REALTYPE                  :: w_d=-5.787037e-05
   REALTYPE                  :: kc=0.03
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
   REALTYPE                  :: rzd=2.314814e-07
   REALTYPE                  :: aa=0.62
   REALTYPE                  :: g2=20.0

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

   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=bio_npzd_nml,err=99)
   close(namlst)

   LEVEL3 'namelist "', fname, '" read'

   ! Store parameter values
   self%p0    = p0
   self%z0    = z0
   self%kc    = kc
   self%i_min = i_min
   self%rmax  = rmax/secs_pr_day
   self%gmax  = gmax/secs_pr_day
   self%iv    = iv
   self%alpha = alpha
   self%rpn  = rpn /secs_pr_day
   self%rzn  = rzn /secs_pr_day
   self%rdn  = rdn /secs_pr_day
   self%rpdu = rpdu/secs_pr_day
   self%rpdl = rpdl/secs_pr_day
   self%rzd  = rzd /secs_pr_day
   
   modelinfo = create_model_info(4,1)
   
   ! Define model properties.
   modelinfo%par_fraction = _ONE_-aa
   modelinfo%par_background_extinction = _ONE_/g2

   modelinfo%variables(1)%name = 'nut'
   modelinfo%variables(1)%unit = 'mmol/m**3'
   modelinfo%variables(1)%longname = 'nutrients'
   modelinfo%variables(1)%initial_value = n_initial
   modelinfo%variables(1)%positive_definite = .true.

   modelinfo%variables(2)%name = 'phy'
   modelinfo%variables(2)%unit = 'mmol/m**3'
   modelinfo%variables(2)%longname = 'phytoplankton'
   modelinfo%variables(2)%initial_value = p_initial
   modelinfo%variables(2)%sinking_rate = -w_p/secs_pr_day
   modelinfo%variables(2)%positive_definite = .true.

   modelinfo%variables(3)%name = 'zoo'
   modelinfo%variables(3)%unit = 'mmol/m**3'
   modelinfo%variables(3)%longname = 'zooplankton'
   modelinfo%variables(3)%initial_value = z_initial
   modelinfo%variables(3)%positive_definite = .true.

   modelinfo%variables(4)%name = 'det'
   modelinfo%variables(4)%unit = 'mmol/m**3'
   modelinfo%variables(4)%longname = 'detritus'
   modelinfo%variables(4)%initial_value = d_initial
   modelinfo%variables(4)%sinking_rate = -w_d/secs_pr_day
   modelinfo%variables(4)%positive_definite = .true.

#if 0
   modelinfo%variables(2)%mussels_inhale = .true.
   modelinfo%variables(3)%mussels_inhale = .true.
   modelinfo%variables(4)%mussels_inhale = .true.
#endif

   modelinfo%conserved_quantities(1)%name = 'N'
   modelinfo%conserved_quantities(1)%unit = 'mmol/m**3'
   modelinfo%conserved_quantities(1)%longname = 'nitrogen'

   LEVEL3 'module initialized'

   return

98 LEVEL2 'I could not open '//trim(fname)
   LEVEL2 'If thats not what you want you have to supply '//trim(fname)
   LEVEL2 'See the bio example on www.gotm.net for a working '//trim(fname)
   return
99 FATAL 'I could not read bio_npzd.nml'
   stop 'init_bio_npzd_0d'
   end function init_bio_npzd_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the light extinction coefficient due to biogeochemical
! variables
!
! !INTERFACE:
   function get_bio_extinction_npzd_0d(self,numc,cc) result(extinction)
!
! !INPUT PARAMETERS:
   type (type_npzd), intent(in) :: self
   integer,  intent(in)  :: numc
   REALTYPE, intent(in)  :: cc(1:numc)
   REALTYPE              :: extinction
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   extinction = self%kc*(self%p0+cc(2)+cc(4))

   end function get_bio_extinction_npzd_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get the total of conserved quantities (currently only nitrogen)
!
! !INTERFACE:
   function get_conserved_quantities_npzd_0d(self,numc,cc,count) result(sums)
!
! !INPUT PARAMETERS:
   type (type_npzd), intent(in) :: self
   integer,  intent(in)  :: numc,count
   REALTYPE, intent(in)  :: cc(1:numc)
   REALTYPE              :: sums(1:count)
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!EOP
!-----------------------------------------------------------------------
!BOC
   sums(1) = sum(cc)

   end function get_conserved_quantities_npzd_0d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Michaelis-Menten formulation for nutrient uptake
!
! !INTERFACE:
   REALTYPE function fnp(self,n,p,par,iopt)
!
! !DESCRIPTION:
! Here, the classical Michaelis-Menten formulation for nutrient uptake
! is formulated.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_npzd), intent(in) :: self
   REALTYPE, intent(in)         :: n,p,par,iopt
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   fnp=self%rmax*par/iopt*exp(1.-par/iopt)*n/(self%alpha+n)*(p+self%p0)
   return
   end function fnp
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Ivlev formulation for zooplankton grazing on phytoplankton
!
! !INTERFACE:
   REALTYPE function fpz(self,p,z)
!
! !DESCRIPTION:
! Here, the classical Ivlev formulation for zooplankton grazing on 
! phytoplankton is formulated.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_npzd), intent(in) :: self
   REALTYPE, intent(in)         :: p,z
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC
   fpz=self%gmax*(1.-exp(-self%iv**2*p**2))*(z+self%z0)
   return
   end function fpz
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Right hand sides of NPZD model
!
! !INTERFACE:
   subroutine do_bio_npzd_0d(self,first,numc,cc,env,pp,dd)
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
   type (type_npzd), intent(in)         :: self
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
   REALTYPE                   :: rpd
   integer                    :: i,j
!EOP
!-----------------------------------------------------------------------
!BOC

   if (first) then
      iopt=max(0.25*env%I_0,self%I_min)
   end if

   if (env%par .ge. self%I_min) then
      rpd=self%rpdu
   else
      rpd=self%rpdl
   end if

   dd(n,p)=fnp(self,cc(n),cc(p),env%par,iopt)  ! snp
   dd(p,z)=fpz(self,cc(p),cc(z))               ! spz
   dd(p,n)=self%rpn*cc(p)                      ! spn
   dd(z,n)=self%rzn*cc(z)                      ! szn
   dd(d,n)=self%rdn*cc(d)                      ! sdn
   dd(p,d)=     rpd*cc(p)                      ! spd
   dd(z,d)=self%rzd*cc(z)                      ! szd

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

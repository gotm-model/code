!$Id: bio.F90,v 1.2 2003-04-04 14:25:52 hb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio --- biological model \label{sec:bio}
!
! !INTERFACE:
   module bio
!
! !DESCRIPTION:
!  Remember this Hans
!
! !USES:
   use turbulence, only:    nuh 
   use meanflow,   only: h
   use output,   only:     out_fmt,write_results,ts
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio, calc_bio, end_bio
!
! !REVISION HISTORY:!
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: bio.F90,v $
!  Revision 1.2  2003-04-04 14:25:52  hb
!  First iteration of four-compartment geobiochemical model implemented
!
!  Revision 1.1  2003/04/01 17:01:00  hb
!  Added infrastructure for geobiochemical model
!
!
!EOP
!-----------------------------------------------------------------------
!
!  private data members
   REALTYPE, dimension(:), allocatable         :: n,p,z,d
   REALTYPE, dimension(:), allocatable, public :: bioshade

!  from a namelist
   logical                   :: bio_calc=.true.
   REALTYPE                  :: N_initial=4.5
   REALTYPE                  :: P_initial=0.
   REALTYPE                  :: Z_initial=0.
   REALTYPE                  :: D_initial=4.5
   REALTYPE                  :: P0=0.0225
   REALTYPE                  :: Z0=0.0225
   REALTYPE                  :: w_P=-1.157407e-05
   REALTYPE                  :: w_D=-5.787037e-05
   REALTYPE                  :: kc=0.03
   REALTYPE                  :: I_min=25.
   REALTYPE                  :: rmax=1.157407e-05
   REALTYPE                  :: gmax=5.787037e-06
   REALTYPE                  :: Iv=1.1
   REALTYPE                  :: alpha=0.3
   REALTYPE                  :: rpn=1.157407e-07
   REALTYPE                  :: rzn=1.157407e-07
   REALTYPE                  :: rdn=3.472222e-08
   REALTYPE                  :: rpdu=2.314814e-07
   REALTYPE                  :: rpdl=1.157407e-06
   REALTYPE                  :: rzd=2.314814e-07
   REALTYPE                  :: cnpar=1.0
   integer                   :: w_adv_discr=6 
   integer                   :: sink_discr=1 
   integer                   :: out_unit

!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the bio module
!
! !INTERFACE:
   subroutine init_bio(namlst,fname,unit,nlev,h) 
!
! !DESCRIPTION:
! Here, the bio namelist {\tt bio.inp} is read and memory is 
! allocated - and various variables are initialised.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,          intent(in)   :: namlst
   character(len=*), intent(in)   :: fname
   integer,          intent(in)   :: unit
   integer,          intent(in)   :: nlev
   REALTYPE,         intent(in)   :: h(0:nlev) 
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: rc
   namelist /bio_nml/ bio_calc,N_initial,P_initial,Z_initial,D_initial,  &
                      P0,Z0,w_P,w_D,kc,I_min,rmax,gmax,Iv,alpha,rpn,    &
		      rzn,rdn,rpdu,rpdl,rzd,cnpar,w_adv_discr,sink_discr
!-----------------------------------------------------------------------
!BOC

   LEVEL1 'init_bio'

!  Open and read the namelist
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=bio_nml,err=99)
   close(namlst)

!  Conversion from day to second
   rpn  = rpn  / (24.*3600.) 
   rzn  = rzn  / (24.*3600.)
   rdn  = rdn  / (24.*3600.)
   rpdu = rpdu / (24.*3600.)
   rpdl = rpdl / (24.*3600.)
   rzd  = rzd  / (24.*3600.)
   gmax = gmax / (24.*3600.)
   rmax = rmax / (24.*3600.)
   w_p  = w_p  / (24.*3600.)
   w_d  = w_d  / (24.*3600.)

   allocate(bioshade(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_bio: Error allocating (bioshade)'
   bioshade=0.

   if (bio_calc) then 
      out_unit=unit

      allocate(n(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_bio: Error allocating (n)'

      allocate(p(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_bio: Error allocating (p)'

      allocate(z(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_bio: Error allocating (z)'

      allocate(d(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_bio: Error allocating (d)'
       
      n=n_initial 
      p=p_initial 
      z=z_initial 
      d=d_initial 


!KBK
#if 0
      close(unit) 
#endif

      LEVEL1 'bio module is initialised ...'

   end if
   return

98 LEVEL2 'I could not open bio.inp'
   LEVEL2 'If thats not what you want you have to supply bio.inp'
   LEVEL2 'See the bio example on www.gotm.net for a working bio.inp'
   bio_calc = .false.
   return
99 FATAL 'I could not read bio.inp'
   stop 'init_bio'
   end subroutine init_bio
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the bio model
!
! !INTERFACE:
   subroutine calc_bio(nlev,I_0,dt)
!
! !DESCRIPTION:
! Let $N$ denote nutrient, $P$ denote phytoplankton, $Z$ denote zooplankton and
! $D$ denote detritus concentration, given as
! nutrient equivalents in mmol\,m$^{-3}$. According to
! \cite{FENNELea96}, a four-compartment geobiochemical model
! could then be constructed as:
! 
! \begin{equation}\label{4compmodel}
! \begin{array}{lcllllllll}
! {\cal D}(N) & = & -s_{NP} & +s_{PN}&   &     &+s_{ZN}        & & +s_{DN}   \\
! {\cal D}(P) + w_P \partial_zP& = & +s_{NP}&-s_{PN}&-s_{PD}& -s_{PZ} & & &  \\
! {\cal D}(Z) & = &         & &       & +s_{PZ}  & -s_{ZN}& -s_{ZD} &        \\
! {\cal D}(D) + w_D \partial_zD& = & & &+s_{PD}  &&& +s_{ZD}& -s_{DN}        \\
! \end{array}
! \end{equation}
! 
! with the diffusion operator
! 
! \begin{equation}
! {\cal D} (X) = \partial_t X - \partial_z (\nu'_t \partial_z X),
! \end{equation}
! 
! where $\nu'_t$ denotes the eddy diffusivity varying in time and space.
! The settling velocity of detritus is given by the negative constant value
! $w_D$. All boundary conditions are zero flux conditions.
! 
! In (\ref{4compmodel}) $s_{XY}$ denotes a flux from the compartment $X$
! to the compartment $Y$. It can be easily seen from (\ref{4compmodel})
! that the model is conservative, since all sink terms are compensated by
! equivalent source terms in other equations and the other terms due to
! diffusion and settling are conservative as well.
! It should be noted that here, that the concentrations used here are
! in fact differences between actual concentrations and minimum concentrations.
! Thus, the actual phytoplankton concentration is $P+P_0$ and the actual
! zooplankton concentration is $Z+Z_0$. This way of
! notation is used in order to have zero concentrations as minimum
! concentartions for all geobiochemical state variables.
! Usually, minimum phytoplankton and zooplankton concentrations are used
! for guaranteeing sufficiently high winter concentrations (see
! \cite{FENNELea96}).
! 
! The reactive terms on the right hand sides of (\ref{4compmodel})
! are defined as follows:
! 
! \begin{itemize}
! \item $s_{NP}$: This is the growth rate of phytoplankton defined as
! \begin{equation}\label{sNP}
! s_{NP} = \frac{I_{PAR}}{I_{opt}} \exp \left(1-\frac{I_{PAR}}{I_{opt}}\right)
! \frac{N^2}{\alpha^2+N^2}(P+P_0)
! \end{equation}
! with the depth depending photosynthetically active radiation (PAR)
! \begin{equation}\label{light}
! I_{PAR}(z)=\frac{I_0}{2}
! \frac{ae^{-z/\eta_1}+(1-a)e^{-z/\eta_2}}
! {\exp\left(-k_c\int_z^0(P(\xi)+P_0+D(\xi)+D_0)\,d\xi\right)},
! \end{equation}
! where the nominator is due to the radiation formula
! by \cite{PAULSONea77} and the denominator is due to turbidity in the water
! column caused by phytoplankton and detritus with the attenuation constant for
! self shading, $k_c$.
! The illuminance optimum for photosynthesis is defined as
! \begin{equation}\label{Iopt}
! I_{opt} = \max\left(\frac{I_0}{4},I_{\min} \right)
! \end{equation}
! with the minimum PAR $I_{\min}$.
! \item $s_{PZ}$: This is the zooplankton grazing term parameterised as
! \begin{equation}\label{sPZ}
! s_{PZ}=G_{\max}\left(1-\exp(-I_v^2P^2 \right)(Z+Z_0)
! \end{equation}
! with the Ivlev constant $I_v$ and the maximum grazing rate $G_{\max}$.
! \item $s_{PN}$, $s_{ZN}$, $s_{DN}$, $s_{PD}$, $s_{PD}$:
! These are all linear transfer functions with
! \begin{equation}\label{rXY}
! s_{XY}=r_{XY}X
! \end{equation}
! with the positive constant rate $r_{XY}$.
! \end{itemize}
! It can be clearly seen from (\ref{sNP}), (\ref{sPZ}), and (\ref{rXY})
! that for all sink and source terms
! \begin{equation}
! s_{XY} \longrightarrow 0 \quad \mbox{ for } \quad x \longrightarrow 0.
! \end{equation}
! 
! !USES:
   use meanflow, only: h,ho,grid_method,w_grid,rad
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)     :: nlev 
   REALTYPE, intent(in)     :: I_0 
   REALTYPE, intent(in)     :: dt
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!BOC
! !LOCAL VARIABLES:
   REALTYPE                  :: Qsour(0:nlev),w(0:nlev)
   REALTYPE                  :: Sup=0,Sdw=0
   REALTYPE                  :: RelaxTau(0:nlev)
   REALTYPE                  :: par(0:nlev),zz,add,iopt
   REALTYPE                  :: rpd
   REALTYPE                  :: snp1,spz1,spn1,szn1,sdn1,spd1,szd1
   REALTYPE                  :: snp2,spz2,spn2,szn2,sdn2,spd2,szd2
   REALTYPE                  :: snp3,spz3,spn3,szn3,sdn3,spd3,szd3
   REALTYPE                  :: snp4,spz4,spn4,szn4,sdn4,spd4,szd4
   REALTYPE                  :: n1,p1,z1,d1
   REALTYPE                  :: totn
   integer                   :: i,Bcup=1,Bcdw=1,flag=2,char=1
   logical                   :: surf_flux=.false.,bott_flux=.false.

!EOP
!-----------------------------------------------------------------------

   if (bio_calc) then 

!      STDERR 'bio_calc'

      Qsour=0.
      RelaxTau=1.e15

!  Diffusion of N
   w=0.
   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Sup,Sdw,RelaxTau,h,h,nuh,w,     &
              QSour,n,char,w_adv_discr,N,surf_flux,bott_flux,         &
              grid_method,w_grid,flag)

!  Diffusion and sinking of P
   w=w_P
   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Sup,Sdw,RelaxTau,h,h,nuh,w,     &
              QSour,p,char,w_adv_discr,P,surf_flux,bott_flux,         &
              grid_method,w_grid,flag)

!  Diffusion of Z
   w=0.
   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Sup,Sdw,RelaxTau,h,h,nuh,w,     &
              QSour,z,char,w_adv_discr,Z,surf_flux,bott_flux,         &
              grid_method,w_grid,flag)

!  Diffusion and sinking of D
   w=w_D
   call Yevol(nlev,Bcup,Bcdw,dt,cnpar,Sup,Sdw,RelaxTau,h,h,nuh,w,     &
              QSour,d,char,w_adv_discr,D,surf_flux,bott_flux,         &
              grid_method,w_grid,flag)


!  Source and sink terms (right hand sides)

   zz=0.
   add=0.
   do i=nlev,1,-1
      add=add+0.5*h(i)*(d(i)+p(i)+p0)
      zz=zz+0.5*h(i)
      par(i)=0.25*(rad(i)+rad(i-1))*exp(-kc*add) 
      add=add+0.5*h(i)*(d(i)+p(i)+p0)
      bioshade(i)=exp(-kc*add)
      zz=zz+0.5*h(i)
   end do 
   iopt=max(0.25*I_0,I_min)

   totn=0.
   do i=nlev,1,-1
      if (par(i).ge.I_min) then 
         rpd=rpdu
      else
         rpd=rpdl
      end if 
!
      snp1=fnp(n(i),p(i),par(i),iopt)
      spz1=fpz(p(i),z(i)) 
      spn1=rpn*p(i)
      szn1=rzn*z(i)
      sdn1=rdn*d(i)
      spd1=rpd*p(i)
      szd1=rzd*z(i)
    
      n1=n(i)+dt*(-snp1+spn1+szn1+sdn1)
      p1=p(i)+dt*(+snp1-spn1-spd1-spz1)
      z1=z(i)+dt*(+spz1-szn1-szd1)
      d1=d(i)+dt*(+spd1+szd1-sdn1)

      if (sink_discr.eq.1) then
         n(i)=n1
         p(i)=p1
         z(i)=z1
         d(i)=d1
      end if 

      if (sink_discr.eq.2) then   ! Second order Runge-Kutta
         snp2=fnp(n1,p1,par(i),iopt)
         spz2=fpz(p1,z1)
         spn2=rpn*P1
         szn2=rzn*Z1
         sdn2=rdn*D1
         spd2=rpd*P1
         szd2=rzd*Z1

         n1=n(i)+dt*0.5*(-snp1+spn1+szn1+sdn1-snp2+spn2+szn2+sdn2)
         p1=p(i)+dt*0.5*(+snp1-spn1-spd1-spz1+snp2-spn2-spd2-spz2)
         z1=z(i)+dt*0.5*(+spz1-szn1-szd1     +spz2-szn2-szd2)
         d1=d(i)+dt*0.5*(+spd1+szd1-sdn1     +spd2+szd2-sdn2)

         if (sink_discr.eq.2) then
            n(i)=n1
            p(i)=p1
            z(i)=z1
            d(i)=d1
         end if 
      end if 

      if (sink_discr.eq.3) then   ! Fourth order Runge-Kutta
         n1=n(i)+0.5*dt*(-snp1+spn1+szn1+sdn1)
         p1=p(i)+0.5*dt*(+snp1-spn1-spd1-spz1)
         z1=z(i)+0.5*dt*(+spz1-szn1-szd1)
         d1=d(i)+0.5*dt*(+spd1+szd1-sdn1)

         snp2=fnp(n1,p1,par(i),iopt)
         spz2=fpz(p1,z1)
         spn2=rpn*p1
         szn2=rzn*z1
         sdn2=rdn*d1
         spd2=rpd*p1
         szd2=rzd*z1

         n1=n(i)+0.5*dt*(-snp2+spn2+szn2+sdn2)
         p1=p(i)+0.5*dt*(+snp2-spn2-spd2-spz2)
         z1=z(i)+0.5*dt*(+spz2-szn2-szd2)
         d1=d(i)+0.5*dt*(+spd2+szd2-sdn2)

         snp3=fnp(n1,p1,par(i),iopt)
         spz3=fpz(p1,z1)
         spn3=rpn*p1
         szn3=rzn*z1
         sdn3=rdn*d1
         spd3=rpd*p1
         szd3=rzd*z1

         n1=n(i)+dt*(-snp3+spn3+szn3+sdn3)
         p1=p(i)+dt*(+snp3-spn3-spd3-spz3)
         z1=z(i)+dt*(+spz3-szn3-szd3)
         d1=d(i)+dt*(+spd3+szd3-sdn3)

         snp4=fnp(n1,p1,par(i),iopt)
         spz4=fpz(p1,z1)
         spn4=rpn*p1
         szn4=rzn*z1
         sdn4=rdn*d1
         spd4=rpd*p1
         szd4=rzd*z1

         n(i)=n(i)+dt/6.*(-snp1+spn1+szn1+sdn1-snp4+spn4+szn4+sdn4)  &
	          +dt/3.*(-snp2+spn2+szn2+sdn2-snp3+spn3+szn3+sdn3)
         p(i)=p(i)+dt/6.*(+snp1-spn1-spd1-spz1+snp4-spn4-spd4-spz4)  &
	          +dt/3.*(+snp2-spn2-spd2-spz2+snp3-spn3-spd3-spz3)
         z(i)=z(i)+dt/6.*(+spz1-szn1-szd1     +spz4-szn4-szd4)       &
	          +dt/3.*(+spz2-szn2-szd2     +spz3-szn3-szd3) 
         d(i)=d(i)+dt/6.*(+spd1+szd1-sdn1     +spd4+szd4-sdn4)       &
	          +dt/3.*(+spd2+szd2-sdn2     +spd3+szd3-sdn3)

      end if 

      if (n(i).lt.0) n(i)=0.
      if (p(i).lt.0) p(i)=0.
      if (z(i).lt.0) z(i)=0.
      if (d(i).lt.0) d(i)=0.

      totn=totn+(n(i)+p(i)+z(i)+d(i))*h(i)
   end do
   write(90,*) totn


   if (write_results) then
      call save_bio()
   end if

   end if

   return
   end subroutine calc_bio 
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Michaelis-Menten formulation for nutrient uptake
!
! !INTERFACE
   REALTYPE function fnp(n,p,par,iopt)
!
! !DESCRIPTION
!
! !USES
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)        :: n,p,par,iopt
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
!-----------------------------------------------------------------------
!BOC
      fnp=rmax*par/iopt*exp(1.-par/iopt)*n**2/(alpha**2+n**2)*(p+p0)
   return
   end function fnp 
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Ivlev formulation for zooplankton grazing on phytoplankton
!
! !INTERFACE
   REALTYPE function fpz(p,z)
!
! !DESCRIPTION
!
! !USES
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)        :: p,z
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
!-----------------------------------------------------------------------
!BOC
      fpz=gmax*(1.-exp(-Iv**2*p**2))*(z+z0)
   return
   end function fpz 
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the bio calculations 
!
! !INTERFACE:
   subroutine end_bio 
!
! !DESCRIPTION:
!  Nothing done yet --- supplied for completeness.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC

   return
   end subroutine end_bio 
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Storing the results
!
! !INTERFACE:
   subroutine save_bio 
!
! !DESCRIPTION:
!  Here, storing of the sediment profiles to an ascii or a
!  netCDF file is managed.

!
! !USES:
   use output, only: out_fmt
#ifdef NETCDF_FMT
   use ncdfout, only: ncid
   use ncdfout, only: lon_dim,lat_dim,z_dim,time_dim,dims
   use ncdfout, only: define_mode,new_nc_variable,set_attributes,store_data
#endif

   IMPLICIT NONE

#ifdef NETCDF_FMT
#include "netcdf.inc"
#endif

!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   logical, save             :: first=.true.
   integer, save             :: nut_id,phy_id,zoo_id,det_id,nn
   integer                   :: i,iret
   REALTYPE                  :: zz


!-----------------------------------------------------------------------
!BOC

   select case (out_fmt)
      case (ASCII)
         if(first) then
            open(out_unit,file='bio.out',status='unknown')
	    nn = ubound(N,1)
            first = .false.
         end if
         write(out_unit,*)
         write(out_unit,*) trim(ts)
         zz = _ZERO_
         do i=nn,1,-1
            zz=zz+0.5*h(i)
            write(out_unit,115) zz,N(i),P(i),Z(i),D(i)
            zz=zz+0.5*h(i)
         end do
      case (NETCDF)
#ifdef NETCDF_FMT
         if(first) then
            dims(1) = lon_dim
            dims(2) = lat_dim
            dims(3) = z_dim
            dims(4) = time_dim
            iret = define_mode(ncid,.true.)
            iret = new_nc_variable(ncid,'NUT',NF_REAL,4,dims,nut_id)
            iret = set_attributes(ncid,nut_id,units='mmol/m**2',    &
                   long_name='Nutrients')
            iret = new_nc_variable(ncid,'PHY',NF_REAL,4,dims,phy_id)
            iret = set_attributes(ncid,phy_id,units='mmol/m**2',    &
                   long_name='Phytoplankton')
            iret = new_nc_variable(ncid,'ZOO',NF_REAL,4,dims,zoo_id)
            iret = set_attributes(ncid,zoo_id,units='mmol/m**2',    &
                   long_name='Zooplankton')
            iret = new_nc_variable(ncid,'DET',NF_REAL,4,dims,det_id)
            iret = set_attributes(ncid,det_id,units='mmol/m**2',    &
                   long_name='Detritus')
            iret = define_mode(ncid,.false.)
            nn = ubound(N,1)
            first = .false.
         end if
         iret = store_data(ncid,nut_id,XYZT_SHAPE,nn,array=N)
         iret = store_data(ncid,phy_id,XYZT_SHAPE,nn,array=P+P0)
         iret = store_data(ncid,zoo_id,XYZT_SHAPE,nn,array=Z+Z0)
         iret = store_data(ncid,det_id,XYZT_SHAPE,nn,array=D)
#endif
      case default
         FATAL 'A non valid output format has been chosen'
         stop 'save_bio'
   end select   

115 format(F10.4,4(1x,E10.4E2))
   return
   end subroutine save_bio 
!EOC

!-----------------------------------------------------------------------

   end module bio

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

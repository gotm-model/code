!$Id: turbulence.F90,v 1.4 2002-02-08 08:59:59 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: turbulence: its all in here.. 
!
! !INTERFACE:
   module turbulence 
!
! !DESCRIPTION: 
! In this module all turbulence calculations are integrated. Further 
! descriptions are given in the single subroutines. 
!
! !USES:
   IMPLICIT NONE
!
!  Default all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_turbulence 
   public stabilityfunctions,do_tke,lengthscale
   public kolpran,do_turbulence,internal_wave 
!
! !PUBLIC DATA MEMBERS:
   REALTYPE, public, dimension(:), allocatable	:: tke,eps,L
   REALTYPE, public, dimension(:), allocatable	:: num,nuh
   REALTYPE, public, dimension(:), allocatable	:: cmue1,cmue2
   REALTYPE, public, dimension(:), allocatable	:: uu,vv,ww,tt,chi
   REALTYPE, public, dimension(:), allocatable	:: tkeo
   REALTYPE, public, dimension(:), allocatable	:: as,an
   REALTYPE, public, dimension(:), allocatable	:: xRf 
   REALTYPE, public	:: cde,sig_e0,sig_e1
   REALTYPE, public	:: craig_m,beta_gen 
   REALTYPE, public	:: sig_phi,c_phi1,c_phi2,nnx,mmx,nx 
!  These variables are read from the 'turbulence' namelist.
   integer, public	:: turb_method=2 
   integer, public	:: tke_method=2
   integer, public	:: len_scale_method=8
   integer, public	:: stab_method=3
   logical, public	:: length_lim=.true.
   logical, public	:: craig_banner=.false.
   REALTYPE, public	:: nnn=0.
   REALTYPE, public	:: mmm=1.
   REALTYPE, public	:: const_num= 5.e-4 
   REALTYPE, public	:: const_nuh= 5.e-4 
   REALTYPE, public	:: k_min=3e-6 
   REALTYPE, public	:: eps_min=5e-10
   REALTYPE, public	:: L_min=0.01 
!  Turbulence parameters - 'umlauf' namelist.
   logical, public      :: umlauf_burchard=.false. 
   REALTYPE, public     :: mx=1.0
   REALTYPE, public     :: d_gen=-1.2
   REALTYPE, public     :: alpha_gen=-2.0
   REALTYPE, public     :: l_gen=0.2 
!  Turbulence parameters - 'turb_parameters' namelist.
   REALTYPE, public	:: kappa=0.4
   REALTYPE, public 	:: Prandtl0=0.714 
   REALTYPE, public	:: cm0=0.527
   REALTYPE, public	:: cm_craig=0.73
   REALTYPE, public	:: cw=100.
   REALTYPE, public	:: galp=0.53
!  The k-eps model - 'keps' namelist.
   REALTYPE, public	:: ce1=1.44
   REALTYPE, public	:: ce2=1.92
   REALTYPE, public	:: ce3plus=1.
   REALTYPE, public	:: ce3minus=-0.629
   REALTYPE, public	:: sig_k=1.
   logical, public	:: flux_bdy
!
!  The MY model - 'my' namelist.
   REALTYPE		:: sl=0.2
   REALTYPE, public	:: e1=1.8
   REALTYPE, public	:: e2=1.33
   REALTYPE, public	:: e3=1.8
   integer, public	:: MY_length=1
!  Stability functions - 'stabfunc' namelist.
   REALTYPE, public	:: a1=0.92
   REALTYPE, public	:: a2=0.74
   REALTYPE, public	:: b1=16.6
   REALTYPE, public	:: b2=10.1
   REALTYPE, public	:: c1=0.08
   REALTYPE, public	:: c2=0.7
   REALTYPE, public	:: c3=0.2
   logical, public	:: qesmooth=.true.
   REALTYPE, public	:: qeghmax=0.0233
   REALTYPE, public	:: qeghmin=-0.28
   REALTYPE, public	:: qeghcrit=0.02

! !PRIVATE DATA MEMBERS:
!  All namelist variables are described in the prototype namelist.
!
!  Internal wave model - the 'iw' namelist.
   integer		:: iw_model=0
   REALTYPE, public	:: alpha=0.0
   REALTYPE		:: klimiw=1e-6
   REALTYPE		:: rich_cr  = 0.7
   REALTYPE		:: numiw    = 1e-4
   REALTYPE		:: nuhiw    = 5e-5
   REALTYPE		:: numshear = 5e-3

!  Finished with all 'turbulence' related namelists.

!  Turbulent Kinetic Energy.
   integer, parameter	:: tke_local_eq=1
   integer, parameter	:: tke_keps=2 
   integer, parameter	:: tke_MY=3

!  Stability functions.
   integer, parameter	:: KanClay=1
   integer, parameter	:: BurBaum=2
   integer, parameter	:: CanutoA=3
   integer, parameter	:: CanutoB=4
   integer, parameter	:: KanClayQe=5 
   integer, parameter	:: BurBaumQe=6
   integer, parameter	:: CanutoAQe=7
   integer, parameter	:: CanutoBQe=8
   integer, parameter	:: Constant=9
   integer, parameter	:: MunkAnderson=10
   integer, parameter	:: SchumGerz=11
   integer, parameter	:: FluxRich=12

!  Length scale calculation.
   integer, parameter	:: diss_eq=8
   integer, parameter	:: length_eq=9
   integer, parameter   :: generic_eq=10
   integer, parameter	:: BougeaultAndre=6
   integer, parameter	:: ispra_length=6

! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard & Manuel Ruiz Villarreal
!                      Hans Burchard and Lars Umlauf for the generic model
!
!  $Log: turbulence.F90,v $
!  Revision 1.4  2002-02-08 08:59:59  gotm
!  Added Manuel as author and copyright holder
!
!  Revision 1.3  2001/11/27 19:42:58  gotm
!  Cleaned
!
!  Revision 1.2  2001/11/18 16:15:30  gotm
!  New generic two-equation model
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
! !BUGS
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize the Turbulence Module.
!
! !INTERFACE:
   subroutine init_turbulence(namlst,fn,nlev)
!
! !DESCRIPTION:
!  Initialize all turbulence related stuff - reads a number of namelists
!  and allocates memory for turbulence related vectors.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: namlst
   character(len=*), intent(in)	:: fn
   integer, intent(in)	:: nlev
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard 
!                      & Manuel Ruiz Villarreal
!
!  See turbulence module
!
! !LOCAL VARIABLES:
   integer		:: rc
   REALTYPE             :: rad
   namelist /turbulence/ turb_method,tke_method,len_scale_method,nnn,mmm,&
                         stab_method,&
                         craig_banner,const_num,const_nuh,&
                         length_lim,const_num,const_nuh,k_min,&
                         L_min,eps_min
   namelist /umlauf/ umlauf_burchard,mx,d_gen,alpha_gen,l_gen  
   namelist /turb_parameters/ kappa,Prandtl0,cm0,cm_craig,cw,galp
   namelist /keps/ ce1,ce2,ce3minus,ce3plus,sig_k,flux_bdy
   namelist /my/ sl,e1,e2,e3,MY_length
   namelist /stabfunc/ a1,a2,b1,b2,c2,c3,qesmooth,qeghmax,qeghmin,qeghcrit
   namelist /iw/ iw_model,alpha,klimiw,rich_cr,numiw,nuhiw,numshear
!
!EOP
!-----------------------------------------------------------------------
!BOC
!  Allocate space for global and local arrays.

   LEVEL1 'init_turbulence'

   open(namlst,file=fn,status='old',action='read',err=80)
   LEVEL2 'reading turbulence namelists..'
   read(namlst,nml=turbulence,err=81)
   read(namlst,nml=umlauf,err=87)
   read(namlst,nml=turb_parameters,err=82)
   read(namlst,nml=keps,err=83)
   read(namlst,nml=my,err=84)
   read(namlst,nml=stabfunc,err=85)
   c1=(1.-B1**(-1./3.)/A1-6*A1/B1)/3. !See Kantha & Clayson 1994, eq. (23)
   read(namlst,nml=iw,err=86)
   close (namlst)
   LEVEL2 'done.'

   cde=cm0*cm0*cm0
   craig_m=sqrt(1.5*cm_craig**2*sig_k/kappa**2)
   sig_e0=(4./3.*craig_m+1.)*(craig_m+1.)*kappa**2/(ce2*cm_craig**2)
   sig_e1= kappa*kappa*cm0/(ce2-ce1)/cde

   if (umlauf_burchard) then
      if ((stab_method.lt.3).or.(stab_method.eq.4)) then
         STDERR 'Please chose stab_func=3 or stab_func>4 when '
         STDERR 'working with the Umlauf & Burchard model.'
         STDERR 'Program aborted in turbulence.F90.'
         stop
      end if    
      beta_gen=1.5*alpha_gen-1.   ! slope of epsilon in shear-free turbulence
      c_phi1=mx        ! with (3.9)
      sig_k=1.5*(alpha_gen*L_gen)**2/cm0**2
!      l-exponent for Umlauf & Burchard model calculated here:
      rad=8.*alpha_gen**2*(2.+d_gen)**2*l_gen**2*             &
       (kappa**2-l_gen**2)*mx*(1.+2.*mx) +                   &
        (-4.*d_gen*kappa**2*mx                                &
        + alpha_gen*(2.+d_gen)*l_gen**2*(1.+4.*mx))**2
      if (rad.gt.0) then
         nx= -1./( 4.*(2.+d_gen)*(kappa**2-l_gen**2) )*          &
           (4.*d_gen*kappa**2*mx                                 &
           -alpha_gen*(2.+d_gen)*l_gen**2*(1.+4.*mx)  +          &
           sqrt(rad) )
         if (nx.gt.0) then
            STDERR 'Negative nx-exponent in Umlauf & Burchard model.'
            STDERR 'Please choose other value for mx.'
            STDERR 'Program abort now in generic_eq.F90.'
            stop
         end if
      else
         STDERR 'Negative radicant in Umlauf & Burchard model.'
         STDERR 'Please choose other value for mx or empirical parameters.'
         STDERR 'Program abort now in generic_eq.F90.'
         stop
      endif
      sig_phi=2.*kappa**2*d_gen*nx/(cm0**2*(d_gen+2.))  ! with (3.16)
      c_phi2=nx**2*kappa**2/(cm0**2*sig_phi)+c_phi1     ! with (3.2)
      mmx=-nx         ! exponent for epsilon in Burchard 2001 notation
      nnx=1.5*nx+mx   ! exponent for k in Burchard 2001 notation
   else
      mmx=mmm         ! exponent for epsilon in Burchard 2001 notation
      nnx=nnn         ! exponent for k in Burchard 2001 notation
      sig_phi = mmx*kappa*kappa*cm0/(ce2-ce1)/cde
      c_phi1=ce1*mmx+nnx
      c_phi2=ce2*mmx+nnx
   end if


   LEVEL2 'allocation memory..'
   allocate(tke(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (tke)'
   tke = k_min

   allocate(eps(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (eps)'
   eps = eps_min

   allocate(L(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (L)'
   L = L_min

   allocate(num(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (num)'
   num = 0.

   allocate(nuh(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (nuh)'
   nuh = 0.

   allocate(tkeo(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (tkeo)'
   tkeo = 0.

   allocate(uu(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (uu)'
   uu = 0.

   allocate(vv(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (vv)'
   vv = 0.

   allocate(ww(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (ww)'
   ww = 0.

   allocate(tt(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (tt)'
   tt = 0.

   allocate(chi(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (chi)'
   chi = 0.

   allocate(cmue1(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (cmue1)'
   cmue1 = 0.

   allocate(cmue2(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (cmue2)'
   cmue2 = 0.

   allocate(xRF(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (xRF)'
   xRF = 0.

   allocate(an(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (an)'
   an = 0.

   allocate(as(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_turbulence: Error allocating (as)'
   as = 0.

   LEVEL2 'done.'

   return
80 FATAL 'I could not open "gotmturb.inp"'
   stop 'init_turbulence'
81 FATAL 'I could not read "turbulence" namelist'
   stop 'init_turbulence'
82 FATAL 'I could not read "turb_parameters" namelist'
   stop 'init_turbulence'
83 FATAL 'I could not read "keps" namelist'
   stop 'init_turbulence'
84 FATAL 'I could not read "my" namelist'
   stop 'init_turbulence'
85 FATAL 'I could not read "stabfunc" namelist'
   stop 'init_turbulence'
86 FATAL 'I could not read "iw" namelist'
   stop 'init_turbulence'
87 FATAL 'I could not read "umlauf" namelist'
   stop 'init_turbulence'

   end subroutine init_turbulence 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Stability Functions: 
!
! !INTERFACE:
   subroutine stabilityfunctions(nlev,NN,SS)
!
! !DESCRIPTION:
!  Based on user input - this routine calls the appropriate stability
!  method. 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: NN(0:nlev),SS(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard 
!                      & Manuel Ruiz Villarreal
!
!  See turbulence module
!
! !LOCAL VARIABLES:
   integer		:: i 
   REALTYPE		:: LLk
!
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=0,nlev
      LLk=L(i)*L(i)/tke(i)
      as(i)=LLk*SS(i)
      an(i)=LLk*NN(i)
   end do
   
   select case(stab_method)
      case(KanClay)
         call cmue_kc(nlev)
      case(BurBaum)
         call cmue_bb(nlev)
      case(CanutoA)
         call cmue_ca(nlev)
      case(CanutoB)
         call cmue_cb(nlev)
      case(KanClayQe)
         call cmue_kcqe(nlev)
      case(BurBaumQe)
         call cmue_bbqe(nlev)
      case(CanutoAQe)
         call cmue_caqe(nlev)
      case(CanutoBQe)
         call cmue_cbqe(nlev)
      case(Constant)
         cmue1=cm0
         cmue2=cm0/Prandtl0
      case(MunkAnderson)
         call cmue_ma(nlev,NN,SS)
      case(SchumGerz)
         call cmue_sg(nlev,NN,SS)
      case(FluxRich)
         call cmue_rf(nlev,NN,SS)
      case default
   end select

   cmue1(0)=cmue1(1)
   cmue1(nlev)=cmue1(nlev-1)
   cmue2(0)=cmue2(1)
   cmue2(nlev)=cmue2(nlev-1)
 
   return
   end subroutine stabilityfunctions 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Turbulent Kinetic Energy Calculation.
!
! !INTERFACE:
   subroutine do_tke(nlev,dt,u_taus,u_taub,z0s,h,NN,SS,P,B)
!
! !DESCRIPTION:
!  Based on user input - this routines calls the appropriate routine for
!  calculating the turbulent kinetic energy.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: dt
   REALTYPE, intent(in)	:: u_taus,u_taub,z0s
   REALTYPE, intent(in)	:: h(0:nlev)
   REALTYPE, intent(in)	:: NN(0:nlev),SS(0:nlev)
   REALTYPE, intent(in)	:: P(0:nlev),B(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!                      & Manuel Ruiz Villarreal
!
!  See turbulence module
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE		:: numtke(0:nlev)
!
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (tke_method)
      case(tke_local_eq)
         call tkealgebraic(nlev,u_taus,u_taub,NN,SS)
      case(tke_keps)
         do i=1,nlev-1
            numtke(i)=num(i)/sig_k
         end do
         call tkeeq(nlev,dt,u_taus,u_taub,z0s,h,P,B,numtke)
      case(tke_MY)
         do i=1,nlev-1
            numtke(i)=Sl*sqrt(2.*tke(i))*L(i)
         end do
         call tkeeq(nlev,dt,u_taus,u_taub,z0s,h,P,B,numtke)
      case default
   end select
      
   return
   end subroutine do_tke 
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Length Scales: 
!
! !INTERFACE:
   subroutine lengthscale(nlev,dt,z0b,z0s,u_taus,u_taub,depth,h,NN,P,B)
!
! !DESCRIPTION:
!  Calls different subroutines that calculate the lengthscale $L$  
!  and the dissipation rate $\epsilon$ with different methods.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: dt
   REALTYPE, intent(in)	:: z0b,z0s
   REALTYPE, intent(in)	:: u_taus,u_taub
   REALTYPE, intent(in)	:: depth
   REALTYPE, intent(in)	:: h(0:nlev),NN(0:nlev)
   REALTYPE, intent(in)	:: P(0:nlev),B(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard 
!                      & Manuel Ruiz Villarreal
!
!  See turbulence module
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE		:: numtke(0:nlev)
!EOP
!-----------------------------------------------------------------------
!BOC
   select case(len_scale_method)
      case(diss_eq)
         call dissipationeq(nlev,dt,z0b,z0s,u_taus,u_taub,h,NN,P,B)
      case(generic_eq)
         call genericeq(nlev,dt,z0b,z0s,u_taus,u_taub,h,NN,P,B)
      case(length_eq)
         do i=1,nlev-1
            numtke(i)=Sl*sqrt(2.*tke(i))*L(i)
         end do
         call lengthscaleeq(nlev,dt,z0b,z0s,depth,h,NN,P,B,numtke)
      case(BougeaultAndre)		! Bougeault and Andre (1986)
         call potentialml(nlev,z0b,z0s,h,depth,NN)
      case default
         call algebraiclength(len_scale_method,nlev,z0b,z0s,depth,h,NN)
   end select
 
   return
   end subroutine lengthscale
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Internal Waves. 
!
! !INTERFACE:
   subroutine internal_wave(nlev,NN,SS) 
!
! !DESCRIPTION:
!  Imposes eddy viscosity and diffisivity characteristic 
!  of internal wave activity and shear instability when there is extinction 
!  of turbulence as suggested by Kantha and Clayson [1994]. 
!  In this case, these new num and nuh 
!  are used instead of those computed with the model.
!
!  When k is small (extinction of turbulence, diagnosed by $k<klimiw$), 
!  $\nu_t$ and $\nu'_t$ are set to empirical values typical 
!  in the presence of internal wave activity (IW) and shear 
!  instability (SI). 
!  {\large
!  \begin{equation}
!  \nu_t=(\nu_t)^{IW}+(\nu_t)^{SI}, \quad
!  \nu_t'=(\nu_t')^{IW}+(\nu'_t)^{SI}
!  \end{equation}
!  \begin{equation}
!  (\nu_t)^{IW}=10^{-4}, \quad         
!  (\nu'_t)^{IW}=5 10^{-5}
!  \end{equation} 
!  \begin{eqnarray}
!  (\nu_t)^{SI}=(\nu_t')^{SI}=0, & R_i>0.7 \\
!  (\nu_t)^{SI}=(\nu_t')^{SI}=5 10^{-3} \left[1-\left(\frac {R_i} 
!  {0.7}\right)^2\right]^3, & 0<R_i<0.7 \\
!  (\nu_t)^{SI}= (\nu_t')^{SI}=5 10^{-3}, & R_i < 0
!  \end{eqnarray}
! 
!  The unit of all diffusivities is $m^2 s^{-1}$
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: NN(0:nlev),SS(0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Manuel Ruiz Villarreal & Karsten Bolding 
!                      & Hans Burchard
!
!  See turbulence module
!
! !LOCAL VARIABLES:
   REALTYPE 		:: rich(0:nlev)	 
   REALTYPE 		:: rich2,pot,x      
   integer 		:: i
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (iw_model)
      case (0)
      case (1)
      case (2)
         rich2 = rich_cr*rich_cr
         do i=1,nlev-1 
            if (tke(i).le.klimiw) then
               rich(i)=NN(i)/(SS(i)+1.e-10)
               if (rich(i).lt.rich_cr) then
                  if (rich(i).gt.0) then
                     pot=1-rich(i)*rich(i)/rich2 
                     x=numshear*pot*pot*pot
                     num(i)=numiw+x 
                     nuh(i)=nuhiw+x  
                  else
                     num(i)=numiw+numshear
                     nuh(i)=nuhiw+numshear
                  end if          
               else
                  num(i)=numiw
                  nuh(i)=nuhiw
               end if
            end if   
         end do
      case default
   end select

   return
   end subroutine internal_wave 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: The Kolmogorov-Prandtl relation. 
!
! !INTERFACE:
   subroutine kolpran(nlev,u_taub,u_taus,z0b,z0s)
!
! !DESCRIPTION:
!  Eddy viscosity/diffusivity are calculated by means of the relation of 
!  Kolmogorov and Prandtl from the computed values of k, L and 
!  stability functions. 
!  \begin{equation}
!  \nu_t = c_{\mu} \sqrt{k}L;\quad \nu'_t = c'_{\mu} \sqrt{k}L,
!  \end{equation}
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: u_taub,u_taus
   REALTYPE, intent(in)	:: z0b,z0s
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard 
!                      & Manuel Ruiz Villarreal
!
!  See turbulence module
!
! !LOCAL VARIABLES:
   REALTYPE		:: x
   integer		:: i
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev
      x=sqrt(tke(i))*L(i)
      num(i)=cmue1(i)*x
      nuh(i)=cmue2(i)*x
   end do

   num(0)=kappa*u_taub*z0b
   num(nlev)=kappa*u_taus*z0s 
   nuh(0)=kappa*u_taub*z0b
   nuh(nlev)=kappa*u_taus*z0s 
 
   return
   end subroutine kolpran 
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: do_turbulence() - do the vertical 1D turbulence
!
! !INTERFACE:
   subroutine do_turbulence(nlev,dt,depth,u_taus,u_taub,z0s,z0b,  &
                            h,NN,SS,P,B)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: dt,depth,u_taus,u_taub,z0s,z0b
   REALTYPE, intent(in)	:: h(0:nlev)
   REALTYPE, intent(in)	:: NN(0:nlev),SS(0:nlev),P(0:nlev),B(0:nlev)
!
! !OUTPUT PARAMETERS:
!
! !BUGS:
!
! !SEE ALSO: 
!
! !SYSTEM ROUTINES:
!
! !FILES USED:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard 
!                      & Manuel Ruiz Villarreal
!
!  See turbulence module
!
! !LOCAL VARIABLES:
!EOP
!-------------------------------------------------------------------------
!BOC

   call stabilityfunctions(nlev,NN,SS)
   call do_tke(nlev,dt,u_taus,u_taub,z0s,h,NN,SS,P,B)
   call lengthscale(nlev,dt,z0b,z0s,u_taus,u_taub,depth,h,NN,P,B)
   call kolpran(nlev,u_taub,u_taus,z0b,z0s)

   return
   end subroutine do_turbulence
!EOC


!-----------------------------------------------------------------------

   end module turbulence 

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard, Karsten Bolding 
!                     & Manuel Ruiz Villarreal.
!-----------------------------------------------------------------------

!$Id: sediment.F90,v 1.5 2004-01-13 10:00:52 lars Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: sediment --- suspended sediment dynamics\label{sec:sediment}
!
! !INTERFACE:
   module sediment 
!
! !DESCRIPTION:
!  This subroutine computes the transport of sediment, given
!  by its concentration, $C$. The transport equation for this
!  quantity can be written as
!  \begin{equation}
!   \label{CEq}
!    \partder{C}{t} +  \partder{w_s C}{z}
!    = {\cal D}_C
!    \comma
!  \end{equation}
!  where $w_s$ is the constant sinking speed of the sediment, and 
!  ${\cal D}_C$ denotes the sum of the turbulent and viscous transport
!   terms modelled according to
!  \begin{equation}
!   \label{DC}
!    {\cal D}_C 
!    = \frstder{z} 
!     \left( 
!        \nu_t \partder{C}{z}
!      \right) 
!    \point
!  \end{equation}
!  For simplicity, we set the turbulent diffusivity of the sediment
!  equal to $\nu_t$, the turbulent diffusivity of momentum, see 
!   \sect{sec:turbulenceIntro}. Surface fluxes and inner sources or 
!  sinks are not considered. 
!
!  The sinking speed, $w_s$, is negative by definition, and may depend on the grain 
!  diameter, molecular viscosity of water
!  and sediment density as discussed in \sect{sec:initSediment}.
!  Diffusion is discretized implicitly as discussed in \sect{sec:diffusionMean},
!  advection (settling) is treated explicitely, 
!  see \sect{sec:advectionMean}. 
! 
!  There is an interesting stationary solution of \eq{CEq} for the case that
!  advection by vertical settling and mixing by diffusion balance exactly. If
!  one consideres only the region of a turbulent flow near to a rigid wall, 
!  where the law-of-the-wall
!  relation $\nu_t = \kappa u_* (z+z_0)$ holds, it is easy to show that 
!  \begin{equation}
!   \label{rouseProfile}
!     \dfrac{C}{C_0} = \left( \dfrac{z+z_0}{z_0} \right)^R
!  \end{equation}
!  is a solution of \eq{CEq}. Here, $C_0$ is the reference sediment concentration
!  at $z=0$ and $R=w_s/(\kappa u_*)$ is the so-called Rouse number. \eq{rouseProfile}
!  can be used to derive boundary conditions for \eq{CEq}.
!
! !USES:
   use meanflow,   only:    depth,u_taub,gravity,rho_0,z0b,za
   use meanflow,   only:    h,avh,NN,buoy 
   use turbulence, only:    kappa,num 
   use output,     only:    out_fmt,write_results,ts
!
   IMPLICIT NONE
!
!  default: all is private
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_sediment, calc_sediment, end_sediment
!
! !PUBLIC DATA MEMBERS:

!
!  !DEFINED PARAMETERS:

!  specification of possible boundary conditions

!  for diffusion   
   integer, parameter      :: Dirichlet       = 0
   integer, parameter      :: Neumann         = 1

!  for advection
   integer,parameter       :: flux           = 1
   integer,parameter       :: value          = 2
   integer,parameter       :: oneSided       = 3
   integer,parameter       :: zeroDivergence = 4

!  how to compute bottom sediment flux or concentration
   integer, parameter      :: NoFlux          = 1
   integer, parameter      :: SmithMcLean     = 2

! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: sediment.F90,v $
!  Revision 1.5  2004-01-13 10:00:52  lars
!  partical re-write using new adv.-diff. routines
!
!  Revision 1.4  2003/03/28 09:20:34  kbk
!  added new copyright to files
!
!  Revision 1.3  2003/03/28 08:24:01  kbk
!  removed tabs
!
!  Revision 1.2  2003/03/10 09:13:24  gotm
!  Improved documentation
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS

!EOP
!
!  private data members from here on
!
!  the 'sedi' namelist
   logical          :: sedi_calc=.false.
   logical          :: sedi_dens=.false.
   REALTYPE         :: rho_sed=2650.
   REALTYPE         :: size=62.5e-6
   REALTYPE         :: init_conc=0.0001
   integer          :: adv_method=1
   REALTYPE         :: cnpar=0.5
   integer          :: sedi_method
   REALTYPE         :: CbObsDt=600.
   integer          :: z0bMethod
!
!  sediment concentration
   REALTYPE, dimension(:), allocatable :: C
!
!  sinking speed, observed cocentration, source term
   REALTYPE, dimension(:), allocatable :: wc,Cobs,Qsour
!
!  boundary condition types for diffusion and advection
   integer                   :: DiffBcup
   integer                   :: DiffBcdw
   integer                   :: AdvBcup
   integer                   :: AdvBcdw

!  boundary values for diffusion and advection
   REALTYPE                  :: DiffCup
   REALTYPE                  :: DiffCdw
   REALTYPE                  :: AdvCup
   REALTYPE                  :: AdvCdw

!  some parameter of the sediment model
   REALTYPE                  :: ustarc,gs

!  output unit
   integer                   :: out_unit

!
!----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the sediment module\label{sec:initSediment}
!
! !INTERFACE:
   subroutine init_sediment(namlst,fname,unit,nlev,g,rho_0) 
!
! !DESCRIPTION:
!  This routine reads the sediment namelist from {\tt sediment.inp} 
!  and allocates memory for the sediment-related vectors. 
!  Further, depending on the sediment model, the settling velocity, $w_s$,
! and the critical friction velocity, $u_*^c$, are calculated here.   
!  The settling velocity is based on a formula proposed
!  by \cite{Zanke77} which is valid for spherical particles.
!  The critical friction velocity is a function of the settling
!  velocity and the particle size (see \cite{SmithMcLean77}). 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,intent(in)                  :: namlst
   character(len=*), intent(in)        :: fname
   integer,intent(in)                  :: unit
   integer,intent(in)                  :: nlev
   REALTYPE,intent(in)                 :: g,rho_0
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: rc 
   REALTYPE                  :: x,Dsize,avmolu=1.3e-6
   namelist /sedi/  sedi_calc,sedi_dens,rho_sed,size,init_conc,      &
                    adv_method,cnpar,sedi_method,CbObsDt,z0bMethod 
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_sediment'

!  Open and read the namelist
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=sedi,err=99)
   close(namlst)

   if (sedi_calc) then
      out_unit = unit 

      allocate(C(0:nlev),stat=rc)
      if (rc /= 0) STOP 'InitSediment: Error allocating (C)'
      C = _ZERO_

      allocate(wc(0:nlev),stat=rc)
      if (rc /= 0) STOP 'InitSediment: Error allocating (wc)'
      wc = _ZERO_

      allocate(Cobs(0:nlev),stat=rc)
      if (rc /= 0) STOP 'InitSediment: Error allocating (Cobs)'
      Cobs = _ZERO_

      allocate(Qsour(0:nlev),stat=rc)
      if (rc /= 0) STOP 'InitSediment: Error allocating (Qsour)'

      Qsour = _ZERO_
      C     = init_conc 

      ! reduced gravity  
      gs=g*(rho_sed-rho_0)/rho_0

      ! Zanke formula for fall velocity of sediment  
      x         = -10.0*avmolu/size*(sqrt(1+(0.01*gs*size**3)/avmolu/avmolu)-1.0) 
      wc        = x
      
      select case(sedi_method)
      case(NoFlux)
         LEVEL2 'Assuming no net flux across the lowest interface'
      case(SmithMcLean)
         LEVEL2 'Computing sediment concentration in lowest box according to Smith and McLean (1977)'
         Dsize=size*(gs/avmolu/avmolu)**0.3333333  
         if (Dsize.gt.10.0) then
            ustarc=-0.4*wc(1)  
         else 
            ustarc=-4.0/Dsize*wc(1)  
         endif
      case default
         FATAL "unkown method to compute sediment flux"
         stop "init sediment"
      end select
      
   end if

   return

98 LEVEL2 'I could not open sediment.inp'
   LEVEL2 'Ill continue but set sedi_calc to false.'
   LEVEL2 'If thats not what you want you have to supply sediment.inp'
   LEVEL2 'See the Rouse example on www.gotm.net for a working sediment.inp'
   sedi_calc = .false.
   return
99 FATAL 'I could not read sediment.inp'
   stop 'init_sediment'
   end subroutine init_sediment
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the sediment transport equation\label{sec:calcSediment}
!
! !INTERFACE:
   subroutine calc_sediment(nlev,dt)
!
! !DESCRIPTION:
!  In this subroutine, the dynamical equation for sediment \eq{CEq}
!  including turbulent diffusion and settling of suspended matter  is updated.
!
!  The models to compute the boundary  conditions at the lowest grid box are 
!  set by the parameter {\tt sedi\_method} in {\tt sediment.inp}.
!  Currently, there are the following models available in GOTM:
!  \begin{itemize}
!    \item zero-flux at the lowest interface ({\tt sedi\_method = 1}). 
!    With this option, no sediment can
!    leave the domain. The net zero-flux boundary condition is implemented as zero-flux
!    for both, advection and diffusion. Note, that this boundary condition 
!    results in the Rouse-profile \eq{rouseProfile}, however, with unkown
!    reference concentration $C_0$.
!    \item Dirichlet boundary condition suggested by \cite{SmithMcLean77} 
!    ({\tt sedi\_method = 2}). These authors set the concentration 
!    at the lowest interface to zero, unless
!    the friction velocity is larger than a threshold, $u_*^c$. Then, they
!    assume that the concentration at the lowest interface, $C_0$, is a 
!    quadratic function of the ratio $u_*/u_*^c$. In GOTM, the Rouse profile
!    \eq{rouseProfile} is assumed to interpolate the value of $C_0$ (located
!   at the lowest interface) to the center of the lowest grid cell.
!  \end{itemize}
!
!  The sediment induced bottom roughness $z_a$ (also see \sect{sec:friction}) can
!  be either ignored ({\tt z0bMethod = 1}) or updated from empirical formulae 
!  as suggested for example by \cite{SmithMcLean77} ({\tt z0bMethod = 2}).
!  If {\tt sedi\_dens = .true.}, the effect of sediment on the density
!  stratifiction (and hence on turbulence) is considered in the code.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: nlev
   REALTYPE, intent(in)                :: dt

! !DEFINED PARAMETERS:
   REALTYPE, parameter                 :: g1            =  1.56E-3
   REALTYPE, parameter                 :: a0            = 26.3
 !
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: CBott,Cbalg
   REALTYPE                  :: y,z,ya
   REALTYPE                  :: dcdz,rho_mean
   REALTYPE                  :: rho(0:nlev)
   REALTYPE,save             :: Cb
   REALTYPE                  :: RelaxT(0:nlev)
   integer                   :: i,flag
   

   REALTYPE                  :: Cint
   REALTYPE                  :: rouse

!
!-----------------------------------------------------------------------
!BOC
   if (sedi_calc) then 

      select case(sedi_method)
      case(NoFlux)

         za            = _ZERO_      ! no model to update za

         DiffBcup      = Neumann
         DiffBcdw      = Neumann
         DiffCup       = _ZERO_
         DiffCdw       = _ZERO_

         AdvBcup       = flux
         AdvBcdw       = flux
         AdvCup        = _ZERO_
         AdvCdw        = _ZERO

      case(SmithMcLean)
         
         if (u_taub .ge. ustarc) then  

            ! compute reference level
            if (z0bMethod.ne.1) then
               za  = a0/gs*(u_taub**2-ustarc**2)
            else
               za  = _ZERO_
            end if

            ! Compute reference concentration at the interface
            Cbott = g1*((u_taub/ustarc)**2-1)     
    
            ! compute Rouse number
            rouse = wc(1)/kappa/u_taub

         else                                 
            Cbott = _ZERO_                        
            za    = _ZERO_
            rouse = _ZERO_
         end if
         

       
         DiffBcup      = Neumann
         DiffBcdw      = Dirichlet
         DiffCup       = 0.
         DiffCdw       = Cbott*((0.5*h(1)+z0b)/z0b)**rouse

         AdvBcup       = flux
         AdvBcdw       = oneSided     ! allow for sinking sediment to leave domain
         AdvCup        = 0.
         AdvCdw        = 0.           ! not used
         
      case default
         FATAL 'Invalid method for sedi_calc'
         stop  'sediment.F90'
      end select
      
      
!     Does not work for prescribed vertical current velocity and 
!     adaptive grids!!

      flag=2          ! conservative form of vertical advection
      RelaxT = 1.e15  ! no relaxation to observed value

      call advectionMean(nlev,dt,h,wc,AdvBcup,AdvBcdw,AdvCup,AdvCdw,adv_method,C)

      call diffusionMean(nlev,dt,cnpar,h,DiffBcup,DiffBcdw,DiffCup,DiffCdw,        &
                         num,Qsour,RelaxT,Cobs,C)



      ! Update buoyancy and NN if stratification plays a role. 
      if (sedi_dens) then
        do i=1,nlev
           rho(i)  = rho_0*(1.0-buoy(i)/gravity)
           buoy(i) = -gravity*((1.0-C(i))*rho(i) + C(i)*rho_sed - rho_0)/rho_0 
        end do 
        do i=1,nlev-1
           rho_mean = 0.5*(rho(i+1)+rho(i))
           dcdz     = (C(i+1)-C(i))/(0.5*(h(i+1)+h(i)))
           NN(i)=(1-C(i))*NN(i)-gravity/rho_0*(rho_sed-rho_mean)*dcdz 
        end do 
      end if
      if (write_results) then
         call save_sediment()
      end if
   end if
   return
   end subroutine calc_sediment 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish  sediment calculations 
!
! !INTERFACE:
   subroutine end_sediment 
!
! !DESCRIPTION:
!  Nothing done yet --- supplied for completeness.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-----------------------------------------------------------------------
!BOC
   return
   end subroutine end_sediment 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Storing the results 
!
! !INTERFACE:
   subroutine save_sediment 
!
! !DESCRIPTION:
!  Here, the storing of the sediment profiles to an ascii or a
!  netCDF file is managed. 
!
! !USES:
   use output,  only:    out_fmt
#ifdef NETCDF_FMT
   use ncdfout, only:    ncid
   use ncdfout, only:    lon_dim,lat_dim,z_dim,time_dim,dims
   use ncdfout, only:    define_mode,new_nc_variable,set_attributes,store_data
#endif
   IMPLICIT NONE
#ifdef NETCDF_FMT
   include "netcdf.inc"
#endif
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:
   logical, save             :: first=.true.
   integer, save             :: sedi_id,n
   integer                   :: i,iret
   REALTYPE                  :: z
!
!-----------------------------------------------------------------------
!BOC
   select case (out_fmt)
      case (ASCII)
         if(first) then
            open(out_unit,file='sediment.out',status='unknown')
            n = ubound(C,1)
            first = .false.
         end if
         write(out_unit,*) trim(ts)
         z = _ZERO_
         do i=n,1,-1
            z=z-0.5*h(i)
            write(out_unit,*) z,C(i)
            z=z-0.5*h(i)
         end do
      case (NETCDF)
#ifdef NETCDF_FMT
         if(first) then
            dims(1) = lon_dim
            dims(2) = lat_dim
            dims(3) = z_dim
            dims(4) = time_dim
            iret = define_mode(ncid,.true.)
            iret = new_nc_variable(ncid,'sediment',NF_REAL,4,dims,sedi_id)
            iret = set_attributes(ncid,sedi_id,units='-',  &
                                  long_name='sediment concentration')
            iret = define_mode(ncid,.false.)
            n = ubound(C,1)
            first = .false.
         end if
         iret = store_data(ncid,sedi_id,XYZT_SHAPE,n,array=C)
#endif
      case default
         FATAL 'A non valid output format has been chosen'
         stop 'save_sediment'
   end select   
   return
   end subroutine save_sediment 
!EOC

!-----------------------------------------------------------------------

   end module sediment

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

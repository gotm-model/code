!$Id: sediment.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: Suspended sediment calculation. 
!
! !INTERFACE:
   module sediment 
!
! !DESCRIPTION:
!  This subroutine calculates the diffusion equation for sediment $C$:
!
!  \begin{equation}\label{CEq}
!  \partial_tC
!  -\partial_z(w_sC+\nu'_t(C) \partial_z C)  = 0.  
!  \end{equation}
!
!  The fall velocity $w_s$ is defined positive here and depends accorcing
!  to Zanke [1977] on the grain diameter, molecular viscosity of water
!  and sediment density.
!
!  At the bed, a Dirichlet condition is given following 
!  Smith and McLean [1977]. This is extrapolated to the center of the
!  lowest grid box by assuming a Rouse profile in that grid box. 
!  Relaxation of the bottom sediment concentration to algebraicly
!  calculated value is possible. This is recommended if density
!  if sediment is considered for water column stability. 
!
!  Surface fluxes and inner sources or sinks are not considered. 
!
!  Advection (sinking) is treated explicitely. 
!
!  Diffusion is numerically treated implicitly.
!  The tri-diagonal matrix is solved then by a simplified Gauss elimination.
!
! !USES:
   use meanflow, only: depth,u_taub,gravity,rho_0,z0b
   use meanflow, only: h,avh,NN,buoy 
   use turbulence, only: kappa,nuh 
   use output, only: out_fmt,write_results,ts
!
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_sediment, calc_sediment, end_sediment
!
! !PUBLIC DATA MEMBERS:
!
! !PRIVATE DATA MEMBERS:
!  From a namelist
   logical		:: sedi_calc=.true.
   logical		:: sedi_dens=.true.
   REALTYPE		:: rho_sed=2650.
   REALTYPE		:: size=62.5e-6
   REALTYPE		:: init_conc=0.
   integer		:: adv_method=1
   REALTYPE		:: cnpar=0.5
   integer		:: Bcup=1
   REALTYPE		:: Cup=0.
   REALTYPE		:: CbObsDt=600.
   integer		:: z0bMethod
   REALTYPE		:: Cdw
   REALTYPE		:: ustarc,gs
   integer		:: Bcdw=2
   integer		:: out_unit
   REALTYPE, dimension(:), allocatable	:: C
   REALTYPE, dimension(:), allocatable	:: wc,Cobs,Qsour
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: sediment.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
!EOP
!-----------------------------------------------------------------------

   CONTAINS

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize sediment calculations. 
!
! !INTERFACE:
   subroutine init_sediment(namlst,fname,unit,nlev,g,rho_0) 
!
! !DESCRIPTION:
!  After reading the sediment namelist and allocating memory for
!  some related vectors, the settling velocity $w_s$ and the critical
!  shear stress $u_*^c$ are calculated here.   
!  The settling velocity is based on a formula proposed
!  by Zanke [1977] which is valid for sphere shaped particles.
!  Critical shear stress here a function of settling velocity and
!  particle size. 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: namlst
   character(len=*), intent(in)	:: fname
   integer, intent(in)	:: unit
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: g,rho_0
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See sediment module
!
! !LOCAL VARIABLES:
   integer 		:: rc 
   REALTYPE		:: x,Dsize,avmolu=1.3e-6
   namelist /sedi/  sedi_calc,sedi_dens,rho_sed,size,init_conc,		&
                    adv_method,cnpar,Bcup,Bcdw,Cup,CbObsDt,z0bMethod 
!
!EOP
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
      Qsour= _ZERO_

      C=init_conc 

      gs=g*(rho_sed-rho_0)/rho_0 			! reduced gravity  
      ! Zanke formula for fall velocity of sediment  
      x=-10.0*avmolu/size*(sqrt(1+(0.01*gs*size**3)/avmolu/avmolu)-1.0) 
      wc=x
      wc(0) = _ZERO_
      wc(nlev) = _ZERO_
      Dsize=size*(gs/avmolu/avmolu)**0.3333333  
      if (Dsize.gt.10.0) then 				! critical fall velocity
         ustarc=-0.4*wc(1)  
      else 
         ustarc=-4.0/Dsize*wc(1)  
      end if
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
! !IROUTINE: The time depending sediment equation.
!
! !INTERFACE:
   subroutine calc_sediment(nlev,dt)
!
! !DESCRIPTION:
!  In this subroutine, a dynamical equation for sediment including 
!  turbulent diffusion and settling of suspended matter $C$ is calculated.
!  The bed concentration is imposed by means of a Dirichlet boundary
!  condition proposed by Smith and McLean [1977] which is a function of
!  actual bed shear velocity $u_*^b$ and the critical  bed shear velocity 
!  $u_*^c$. The latter and the settling velocity $w_s$ are calculated in
!  subroutine {\tt init\_sediment}.
!  Since the tracer points, on which the discrete sediment values are 
!  positioned, are half a grid box above the bed, an analytical solution 
!  by Rouse has been used for calculating the lowest discrete sediment 
!  concentration. For this, a roughness length
!  $z\_a$ is calculated according to Smith and McLean [1977]. The bottom
!  concentration might be numerically noisy. Therefore, a relaxation 
!  procedure to the instantaneous value of the Rouse profile is provided.    
!  For the vertical advection of sediment due to settling, three numerical
!  schemes are available: 
!
!  \begin{enumerate}
!  \item First order upstream
!  \item Third-order QUICKEST
!  \item A combination of the previous two schemes, knows as 
!        Flux Corrected Transport (FCT, see Zalesak [1979].
!  \end{enumerate}
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: dt
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See sediment module
!
! !LOCAL VARIABLES:
   REALTYPE, parameter	:: g1=1.56E-3
   REALTYPE, parameter	:: a0=26.3
   REALTYPE, parameter	:: sigma=0.7963
   REALTYPE		:: CBott,Cbalg
   REALTYPE		:: y,z,ya,za
   REALTYPE		:: dcdz,rho_mean
   REALTYPE		:: rho(0:nlev)
   REALTYPE		:: Cb
   REALTYPE		:: RelaxT(0:nlev)
   integer		:: i
   LOGICAL              :: surf_flux=.false.,bott_flux=.false.  
!EOP
!-----------------------------------------------------------------------
!BOC
   if (sedi_calc) then 
      if (u_taub .ge. ustarc) then  
         Cbott = g1*((u_taub/ustarc)**2-1)  
         if (CBott.gt.1.) CBott=1. 
         if (z0bMethod.eq.1) then
            za    = a0/gs*(u_taub**2-ustarc**2)
         else 
            za    = z0b
         end if 
      else                                 
         Cbott = 0.0                        
         za    = 0.0  
      end if 

!     Calculate analytically average sediment concentration in bottom layer
!     from Rouse profile at the centre of the lowest layer.
      if (za .gt. 1.e-12) then 
         ya=log(za/depth)  
         z=0.5*h(1)+za  
         y=log(z/depth)  
         CBalg=CBott*exp(sigma*wc(1)/kappa/u_taub*			&
               (y -log(1-exp(y ))-(ya-log(1-exp(ya)))))  
      else 
         CBalg = 0.0 
      end if   

!     In order to avoid oscillations of Cb, it should be relaxed to
!     the analytical value.

      Cb=(Cb+Cbalg*dt/CbObsDt)/(1.0+dt/CbObsDt) 
      if (Cb.lt.C(2)) Cb=C(2) 

      Cdw=Cb              !Impose concentration

      avh=nuh

      RelaxT=1.e15

      call yevol(nlev,Bcup,Bcdw,dt,cnpar,Cup,Cdw,RelaxT,h,avh,wc,Qsour,	&
                 Cobs,adv_method,C,surf_flux,bott_flux)
      
      if (sedi_dens) then   ! Update buoyancy and NN. 
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
! !IROUTINE: end_sediment: the calculation is over and we stop. 
!
! !INTERFACE:
   subroutine end_sediment 
!
! !DESCRIPTION:
!  Nothing done yet - supplied for completeness.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See sediment module
!
! !LOCAL VARIABLES:
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
! !IROUTINE: save_sediment: store the results 
!
! !INTERFACE:
   subroutine save_sediment 
!
! !DESCRIPTION:
!  Nothing done yet - supplied for completeness.
!
! !USES:
   use output, only: out_fmt
#ifdef NETCDF_FMT
   use ncdfout, only: ncid
   use ncdfout, only: lon_dim,lat_dim,z_dim,time_dim,dims
   use ncdfout, only: define_mode,new_nc_variable,set_attributes,store_data
   IMPLICIT NONE
#include "netcdf.inc"
#endif
!
! !INPUT PARAMETERS:
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See sediment module
!
! !LOCAL VARIABLES:
   logical, save	:: first=.true.
   integer, save	:: sedi_id,n
   integer		:: i,iret
   REALTYPE		:: z
!
!EOP
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
            iret = set_attributes(ncid,sedi_id,units=' ',	&
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
!Copyright (C) 2000 - Hans Burchard & Karsten Bolding.
!-----------------------------------------------------------------------

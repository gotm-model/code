!$Id: seagrass.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: seagrass dynamics calculation. 
!
! !INTERFACE:
   module seagrass
!
! !DESCRIPTION:
! In this module, seagrass canopies are treated as Lagrangian tracers
! which either advect passively with the horizontal current speed or
! rest at excursion limits and then exerts friction to the mean flow.
! Turbulence generation due to seagrass friction is possible, see
! namelist seagrass.inp.  

! !USES:
   use meanflow, only:  u,v,h,drag,xP
   use output, only: out_fmt,write_results,ts
!
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_seagrass, calc_seagrass, end_seagrass
!
! !PRIVATE DATA MEMBERS:
!  From a namelist
   logical		:: grass_calc=.false.
   character(len=PATH_MAX)	:: grassfile='seagrass.dat'
   REALTYPE		:: XP_rat
   integer		:: grassind 
   integer		:: grassn 
   integer		:: out_unit
   REALTYPE, dimension(:), allocatable	:: xx,yy
   REALTYPE, dimension(:), allocatable	:: exc,vfric,grassz
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: seagrass.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize seagrass calculations. 
!
! !INTERFACE:
   subroutine init_seagrass(namlst,fname,unit,nlev,h) 
!
! !DESCRIPTION:
! Here, the seagrass namelist seagrass.inp is read and memory is allocated 
! to some relevant vectors. Afterwards, excursion limits and friction
! coefficients are read from a file. The uppermost grid related index
! for the seagrass canopy is then calculated.  
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: namlst
   character(len=*), intent(in)	:: fname
   integer, intent(in)	:: unit
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: h(0:nlev) 
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
   integer		:: i,rc
   REALTYPE		:: z
   namelist /canopy/  grass_calc,grassfile,XP_rat
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_seagrass'

!  Open and read the namelist
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=canopy,err=99)
   close(namlst)

   if (grass_calc) then 
      out_unit=unit

      allocate(xx(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (xx)'

      allocate(yy(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (yy)'

      allocate(exc(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (exc)'

      allocate(vfric(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (vfric)'

      allocate(grassz(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_seagrass: Error allocating (grassz)'

      xx = -999.0
      yy = -999.0

      open(unit,status='unknown',file=grassfile)

      read(unit,*) grassn

      do i=1,grassn
         read(unit,*) grassz(i),exc(i),vfric(i) 
      end do 

      z=0.5*h(1)
      do i=2,nlev
         z=z+0.5*(h(i-1)+h(i))
         if (grassz(grassn).gt.z) grassind=i
      end do 

      xx = _ZERO_
      yy = _ZERO_

      close(unit) 

      LEVEL1 'Seagrass initialised ...'

   end if
   return

98 LEVEL2 'I could not open seagrass.inp'
   LEVEL2 'Ill continue but set grass_calc to false.'
   LEVEL2 'If thats not what you want you have to supply seagrass.inp'
   LEVEL2 'See the Seagrass example on www.gotm.net for a working seagrass.inp'
   grass_calc = .false.
   return
99 FATAL 'I could not read seagrass.inp'
   stop 'init_seagrass'
   end subroutine init_seagrass
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: The time depending seagrass equation according to Verduin and
!            Backhaus, 1998. 
!
! !INTERFACE:
   subroutine calc_seagrass(nlev,dt)
!
! !DESCRIPTION:
!
!  The basic equations used here are the momentum equation,
! 
!  \begin{equation}
!  \partial_t u - \partial_z(\nu_t \partial_z u) = -g\partial_x\zeta-C_fu|u|,
!  \end{equation}
!  and the tracer equation for seagrass:
! 
!  \begin{equation}
!  \partial_t x =
!  \left\{
!  \begin{array}{ll}
!  u & \mbox{ for } |x|<x_{\max} \mbox{ or } x\cdot u <0,\\
!  0 & \mbox{ else.}
!  \end{array}
!  \right.
!  \end{equation}
! 
!  The seagrass friction coefficient $C_f$ is only non-zero at heights
!  where seagrass tracers are at their excursion limits:
! 
!  \begin{equation}
!  C_f =
!  \left\{
!  \begin{array}{ll}
!  C_f^{\max} & \mbox{ for } |x|=x_{\max}, \\
!  0 & \mbox{ else.}
!  \end{array}
!  \right.
!  \end{equation}
! 
!  The maximum excursion limits $x_{\max}$ and the friction coefficients
!  $C_f^{\max}$ are given in
!  figures \ref{FigExcursion} and \ref{FigFriction}.
! 
!  The production of turbulence is calculated here as the sum of shear
!  production and friction loss at the seagrass leaves:
!  \begin{equation}
!  P=\nu_t (\partial_zu)^2 + \alpha C_f |u|^3.
!  \end{equation}
! 
!  Here, $0\leq\alpha\leq1$ gives the efficiency of
!  turbulence production caused by
!  friction between current and seagrass leaves.
! 
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)	:: nlev 
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
   integer		:: i
   REALTYPE		:: dist
   REALTYPE		:: grassfric(0:nlev) 
   REALTYPE		:: excur(0:nlev) 
   REALTYPE		:: z(0:nlev) 
   REALTYPE		:: xxP(0:nlev) 
!EOP
!-----------------------------------------------------------------------
!BOC
   if (grass_calc) then 

      z(1)=0.5*h(1) 
      do i=2,nlev
         z(i)=z(i-1)+0.5*(h(i-1)+h(i))
      end do 

! Interpolate excursion limits and frcition to actual grid.   

      call gridinterpol(grassn,1,grassz,exc,nlev,z,excur)
      call gridinterpol(grassn,1,grassz,vfric,nlev,z,grassfric)

      do i=1,nlev
         xx(i)=xx(i)+dt*u(i)             ! Motion of seagrass elements with 
         yy(i)=yy(i)+dt*v(i)             ! mean flow.  
         dist=sqrt(xx(i)*xx(i)+yy(i)*yy(i))
         if (dist .gt. excur(i)) then      ! Excursion limit reached 
            xx(i)= excur(i)/dist * xx(i)
            yy(i)= excur(i)/dist * yy(i)
            drag(i)=drag(i)+grassfric(i) ! Increased drag by seagrass friction 
            xxP(i)=xP_rat*grassfric(i)*(sqrt(u(i)**2+v(i)**2))**3
         else
            drag(i)=drag(i) 
            xxP(i)=0.
         end if
      end do
      do i=1,nlev-1
         xP(i)=0.5*(xxP(i)+xxP(i+1))  ! Extra turbulence production 
                                      ! due to seagrass friction 
      end do

      if (write_results) then
         call save_seagrass()
      end if

   end if

   return
   end subroutine calc_seagrass 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: end_seagrass: the calculation is over and we stop. 
!
! !INTERFACE:
   subroutine end_seagrass 
!
! !DESCRIPTION:
!  Please fill out here what happens in this routine - Latex accepted.
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
   end subroutine end_seagrass 
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: save_seagrass: store the results 
!
! !INTERFACE:
   subroutine save_seagrass 
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
!  See seagrass module
!
! !LOCAL VARIABLES:
   logical, save	:: first=.true.
   integer, save	:: x_excur_id,y_excur_id,n
   integer		:: i,iret
   REALTYPE		:: zz
   REALTYPE		:: miss_val
!
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (out_fmt)
      case (ASCII)
         if(first) then
	    open(out_unit,file='seagrass.out',status='unknown')
	    first = .false.
	 end if
         write(out_unit,*)
         write(out_unit,*) trim(ts)
         zz = _ZERO_
         do i=1,grassind
            zz=zz+0.5*h(i)
            write(out_unit,*) zz,xx(i),yy(i)
            zz=zz+0.5*h(i)
         end do
      case (NETCDF)
#ifdef NETCDF_FMT
         if(first) then
            dims(1) = lon_dim
            dims(2) = lat_dim
            dims(3) = z_dim
            dims(4) = time_dim
	    miss_val = -999.0
            iret = define_mode(ncid,.true.)
            iret = new_nc_variable(ncid,'x-excur',NF_REAL,4,dims,x_excur_id)
	    iret = set_attributes(ncid,x_excur_id,units='m',	&
	            long_name='seagrass excursion(x)',missing_value=miss_val)
            iret = new_nc_variable(ncid,'y-excur',NF_REAL,4,dims,y_excur_id)
	    iret = set_attributes(ncid,y_excur_id,units='m',	&
	            long_name='seagrass excursion(y)',missing_value=miss_val)
            iret = define_mode(ncid,.false.)
	    n = ubound(xx,1)
            first = .false.
         end if
         iret = store_data(ncid,x_excur_id,XYZT_SHAPE,n,array=xx)
         iret = store_data(ncid,y_excur_id,XYZT_SHAPE,n,array=yy)
#endif
      case default
         FATAL 'A non valid output format has been chosen'
	 stop 'save_seagrass'
   end select   
   return
   end subroutine save_seagrass 
!EOC

!-----------------------------------------------------------------------

   end module seagrass

!-----------------------------------------------------------------------
!Copyright (C) 2001 - Hans Burchard & Karsten Bolding.

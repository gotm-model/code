!$Id: lagrange.F90,v 1.1 2002-04-30 14:22:55 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: lagrange.
!
! !INTERFACE:
   module lagrange
!
! !DESCRIPTION:
!
! !USES:
   use meanflow, only: depth,h
   use turbulence, only: nuh
   use output, only: out_fmt,write_results,ts
!
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_lagrange, calc_lagrange, end_lagrange
!
! !PUBLIC DATA MEMBERS:
!
! !PRIVATE DATA MEMBERS:
   integer	:: out_unit
   REALTYPE, dimension(:), allocatable	:: zpos
   REALTYPE, dimension(:), allocatable	:: concentration
!  From namelist
   logical	:: lagrange_calc=.false.
   integer	:: np=10000
   REALTYPE	:: ws=0.001
   REALTYPE	:: load=0.1
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: lagrange.F90,v $
!  Revision 1.1  2002-04-30 14:22:55  gotm
!  Added lagrangian module
!
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize lagrange calculations.
!
! !INTERFACE:
   subroutine init_lagrange(namlst,fname,unit,nlev)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: namlst
   character(len=*), intent(in)	:: fname
   integer, intent(in)	:: unit
   integer, intent(in)	:: nlev
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See lagrange module
!
! !LOCAL VARIABLES:
   integer 		:: n,rc
   namelist /lagrange/  lagrange_calc,np,ws,load
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_lagrange'

!  Open and read the namelist
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=lagrange,err=99)
   close(namlst)

   if (lagrange_calc) then
      out_unit = unit

      allocate(zpos(1:np),stat=rc)
      if (rc /= 0) STOP 'InitSediment: Error allocating (zpos)'

      allocate(concentration(1:nlev),stat=rc)
      if (rc /= 0) STOP 'InitSediment: Error allocating (concentration)'

      do n=1,np
          zpos(n)=-depth+n/float(np+1)*depth ! Equidist. particle distribution
      end do

   end if

   return

98 LEVEL2 'I could not open lagrange.inp'
   LEVEL2 'Ill continue but set lagrange_calc to false.'
   LEVEL2 'If thats not what you want you have to supply lagrange.inp'
   LEVEL2 'See the Rouse example on www.gotm.net for a working lagrange.inp'
   lagrange_calc = .false.
   return
99 FATAL 'I could not read lagrange.inp'
   stop 'init_lagrange'
   end subroutine init_lagrange
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: The time depending lagrange equation.
!
! !INTERFACE:
   subroutine calc_lagrange(nlev,dt)
!
! !DESCRIPTION:
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
!  See lagrange module
!
! !LOCAL VARIABLES:
  integer i,n
  REALTYPE visc,rnd,z,rat
!EOP
!-----------------------------------------------------------------------
!BOC
    do n=1,np
       z=-depth
       do i=1,nlev         ! Determine vertical grid box index
          z=z+h(i)
          if (z .gt. zpos(n)) EXIT
       end do
       rat=(zpos(n)-(z-h(i)))/h(i)
       visc=rat*nuh(i)+(1.-rat)*nuh(i-1)      ! Local viscosity for part.
       call random_number(rnd)
       rnd=rnd*2.-1.

       zpos(n)=zpos(n)+sqrt(6.*dt*visc)*rnd & ! Update particle position
               +dt*(nuh(i)-nuh(i-1))/h(i)-dt*ws

      if (zpos(n).lt.-depth) zpos(n)=-depth+(-depth-zpos(n))
      if (zpos(n).gt.0.) zpos(n)=-zpos(n)

   end do

   if (write_results) then
       call save_lagrange()
   end if

   return
   end subroutine calc_lagrange
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: end_lagrange: the calculation is over and we stop.
!
! !INTERFACE:
   subroutine end_lagrange
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
!  See lagrange module
!
! !LOCAL VARIABLES:
!
!EOP
!-----------------------------------------------------------------------
!BOC


   return
   end subroutine end_lagrange
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: save_lagrange: store the results
!
! !INTERFACE:
   subroutine save_lagrange
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
!  See lagrange module
!
! !LOCAL VARIABLES:
   logical, save	:: first=.true.
   integer, save	:: lagrange_id,n
   integer		:: i,j,iret
   REALTYPE		:: z
!
!EOP
!-----------------------------------------------------------------------
!BOC
   concentration=_ZERO_
   n = ubound(concentration,1)
   do j=1,np        ! Calculate total number of particles per grid box
      z=-depth
      do i=n,1,-1
         z=z+h(i)
         if (z .gt. zpos(j)) EXIT
      end do
      concentration(i)=concentration(i)+1
   end do
   do i=1,n
      concentration(i)=concentration(i)*load/h(i)
   end do
   select case (out_fmt)
!      case (ASCII)
      case (NETCDF)
         if(first) then
            open(out_unit,file='lagrange.out',status='unknown')
            first = .false.
	 end if
         write(out_unit,*) '# ',trim(ts)

         z = _ZERO_
         do i=1,n
            z=z-0.5*h(i)
            write(out_unit,*) z,concentration(i)
            z=z-0.5*h(i)
         end do
      case (ASCII)
#ifdef NETCDF_FMT
         if(first) then
#if 0
            dims(1) = lon_dim
            dims(2) = lat_dim
            dims(3) = z_dim
            dims(4) = time_dim
            iret = define_mode(ncid,.true.)
            iret = new_nc_variable(ncid,'lagrange',NF_REAL,4,dims,lagrange_id)
            iret = set_attributes(ncid,lagrange_id,units=' ',	&
	            long_name='lagrange concentration')
            iret = define_mode(ncid,.false.)
            first = .false.
#endif
         end if
         iret = store_data(ncid,lagrange_id,XYZT_SHAPE,n,array=concentration)
#endif
      case default
         FATAL 'A non valid output format has been chosen'
	 stop 'save_lagrange'
   end select
   return
   end subroutine save_lagrange
!EOC

!-----------------------------------------------------------------------

   end module lagrange

!-----------------------------------------------------------------------
!Copyright (C) 2002 - Hans Burchard, Karsten Bolding and Frank Wolk.
!-----------------------------------------------------------------------

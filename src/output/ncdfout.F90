!$Id: ncdfout.F90,v 1.6 2003-10-14 08:04:32 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: ncdfout --- saving the results in NetCDF
!
! !INTERFACE:
   module ncdfout
!
! !DESCRIPTION:
!  This module provides routines for saving the GOTM results using
!  NetCDF format. A hack has been provided for saving in a way
!  that can be used by the GrADS graphics software.
!  The {\tt sdfopen()} interface to GrADS
!  does not allow for smaller time units than 1 hour, so if GrADS
!  output is selected the units for time are set to {\tt hours} and
!  not {\tt secs}. In addition, all variables are saved on 
!  the same grid, which is actually a bug to be fixed at a later stage. 
!
! !USES:
   IMPLICIT NONE
   include 'netcdf.inc'
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_ncdf, do_ncdf_out, close_ncdf
   public define_mode, new_nc_variable, set_attributes, store_data
!
! !PUBLIC DATA MEMBERS:
   integer, public                     :: ncid
!  dimension ids
   integer                             :: lon_dim,lat_dim,z_dim,z1_dim
   integer                             :: time_dim
   integer, parameter                  :: dim1=1,dim4=4
   integer                             :: dims(dim4)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: ncdfout.F90,v $
!  Revision 1.6  2003-10-14 08:04:32  kbk
!  time is now stored as real
!
!  Revision 1.5  2003/06/13 09:27:16  hb
!  Implemented freshwater fluxes
!
!  Revision 1.4  2003/03/28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.3  2003/03/10 08:53:05  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
!
! !PRIVATE DATA MEMBERS
!  dimension lengths
   integer, parameter        :: lon_len=1
   integer, parameter        :: lat_len=1
   integer                   :: depth_len
   integer                   :: time_len=NF_UNLIMITED
!  variable ids
   integer, private          :: lon_id,lat_id,z_id,z1_id,time_id
   integer, private          :: zeta_id
   integer, private          :: sst_id,sss_id
   integer, private          :: x_taus_id,y_taus_id
   integer, private          :: swr_id,heat_id,total_id,p_e_id
   integer, private          :: int_sw_id,int_hf_id,int_total_id
   integer, private          :: u_taus_id,u_taub_id
   integer, private          :: h_id
   integer, private          :: u_id,u_obs_id
   integer, private          :: v_id,v_obs_id
   integer, private          :: temp_id,temp_obs_id
   integer, private          :: salt_id,salt_obs_id
   integer, private          :: num_id,nuh_id
   integer, private          :: SS_id,SS_obs_id
   integer, private          :: NN_id,NN_obs_id
   integer, private          :: sigma_t_id,sigma_t_obs_id
   integer, private          :: tke_id,tmls_id
   integer, private          :: tked_id,tked_obs_id
   integer, private          :: prod_shear_id,prod_buoy_id
   integer, private          :: uu_id,vv_id,ww_id,tt_id,chi_id
   integer, private          :: ncdf_time_unit
   integer, private          :: set=1
   integer, private          :: start(4),edges(4)
   logical,save,private      :: GrADS=.false.
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Create the NetCDF file
!
! !INTERFACE:
   subroutine init_ncdf(fn,title,lat,lon,nlev,start_time,time_unit)
   IMPLICIT NONE
!
! !DESCRIPTION:
!  Opens and creates the NetCDF file, and initialises all dimensions and
!  variables for the core GOTM model. 
!
! !INPUT PARAMETERS:
   character(len=*), intent(in)        :: fn,title,start_time
   REALTYPE, intent(in)                :: lat,lon
   integer, intent(in)                 :: nlev,time_unit
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See ncdfout module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: iret
   character(len=128)        :: ncdf_time_str,history,name
   REAL_4B                   :: r4
   REALTYPE                  :: miss_val
!
!-------------------------------------------------------------------------
!BOC
   iret = nf_create(fn,NF_CLOBBER,ncid)
   call check_err(iret)

   depth_len=nlev
   ncdf_time_unit = time_unit
   if(time_unit .eq. 2) then
      GrADS = .true.
   end if

!  define dimensions
   iret = nf_def_dim(ncid, 'lon', 1, lon_dim)
   call check_err(iret)
   iret = nf_def_dim(ncid, 'lat', 1, lat_dim)
   call check_err(iret)
   iret = nf_def_dim(ncid, 'z', nlev, z_dim)
   call check_err(iret)
   if( .not. GrADS ) then
      iret = nf_def_dim(ncid, 'z1', nlev, z1_dim)
      call check_err(iret)
   end if
   iret = nf_def_dim(ncid, 'time', NF_UNLIMITED, time_dim)
   call check_err(iret)

!  define coordinates
   dims(1) = lon_dim
   iret = nf_def_var(ncid,'lon',NF_REAL,1,dims,lon_id)
   call check_err(iret)
   dims(1) = lat_dim
   iret = nf_def_var(ncid,'lat',NF_REAL,1,dims,lat_id)
   call check_err(iret)
   dims(1) = z_dim
   iret = nf_def_var(ncid,'z',NF_REAL,1,dims,z_id)
   call check_err(iret)
   if( .not. GrADS ) then
      dims(1) = z1_dim
      iret = nf_def_var(ncid,'z1',NF_REAL,1,dims,z1_id)
      call check_err(iret)
   end if
   dims(1) = time_dim
   iret = nf_def_var(ncid,'time',NF_REAL,1,dims,time_id)
   call check_err(iret)

!  define variables

!  x,y,t
   dims(1) = lon_dim
   dims(2) = lat_dim
   dims(3) = time_dim
   iret = nf_def_var(ncid,'zeta',NF_REAL,3,dims, zeta_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'sst',NF_REAL,3,dims, sst_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'sss',NF_REAL,3,dims, sss_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'x-taus',NF_REAL,3,dims, x_taus_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'y-taus',NF_REAL,3,dims, y_taus_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'swr',NF_REAL,3,dims, swr_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'heat',NF_REAL,3,dims, heat_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'total',NF_REAL,3,dims, total_id)
   call check_err(iret)

   iret = nf_def_var(ncid,'int_sw',NF_REAL,3,dims, int_sw_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'int_hf',NF_REAL,3,dims, int_hf_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'int_total',NF_REAL,3,dims, int_total_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'p_e',NF_REAL,3,dims, p_e_id)
   call check_err(iret)

   iret = nf_def_var(ncid,'u_taus',NF_REAL,3,dims, u_taus_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'u_taub',NF_REAL,3,dims, u_taub_id)
   call check_err(iret)

!  x,y,z,t
   dims(1) = lon_dim
   dims(2) = lat_dim
   dims(3) = z_dim
   dims(4) = time_dim
   iret = nf_def_var(ncid,'h',NF_REAL,4,dims,h_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'u',NF_REAL,4,dims,u_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'u_obs',NF_REAL,4,dims,u_obs_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'v',NF_REAL,4,dims,v_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'v_obs',NF_REAL,4,dims,v_obs_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'salt',NF_REAL,4,dims,salt_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'salt_obs',NF_REAL,4,dims,salt_obs_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'temp',NF_REAL,4,dims,temp_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'temp_obs',NF_REAL,4,dims,temp_obs_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'SS',NF_REAL,4,dims,SS_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'SS_obs',NF_REAL,4,dims,SS_obs_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'NN',NF_REAL,4,dims,NN_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'NN_obs',NF_REAL,4,dims,NN_obs_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'sigma_t',NF_REAL,4,dims,sigma_t_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'sigma_t_obs',NF_REAL,4,dims,sigma_t_obs_id)
   call check_err(iret)
   if( .not. GrADS ) then
      dims(3) = z1_dim
   end if
   iret = nf_def_var(ncid,'num',NF_REAL,4,dims,num_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'nuh',NF_REAL,4,dims,nuh_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'tke',NF_REAL,4,dims,tke_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'tmls',NF_REAL,4,dims,tmls_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'tked',NF_REAL,4,dims,tked_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'tked_obs',NF_REAL,4,dims,tked_obs_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'prod_shear',NF_REAL,4,dims,prod_shear_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'prod_buoy',NF_REAL,4,dims,prod_buoy_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'uu',NF_REAL,4,dims,uu_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'vv',NF_REAL,4,dims,vv_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'ww',NF_REAL,4,dims,ww_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'tt',NF_REAL,4,dims,tt_id)
   call check_err(iret)
   iret = nf_def_var(ncid,'chi',NF_REAL,4,dims,chi_id)
   call check_err(iret)

!  assign attributes

!  coordinates
   iret = set_attributes(ncid,lon_id,units='degrees_east')
   iret = set_attributes(ncid,lat_id,units='degrees_north')
   iret = set_attributes(ncid,z_id,units='meters')
   iret = set_attributes(ncid,z1_id,units='meters')

   select case (ncdf_time_unit)
      case(0)                           ! seconds
         write(ncdf_time_str,100) 'secs',trim(start_time)
      case(1)                           ! minutes
         write(ncdf_time_str,100) 'mins',trim(start_time)
      case(2)                           ! hours
         write(ncdf_time_str,100) 'hours',trim(start_time)
      case default
         write(ncdf_time_str,100) 'secs',trim(start_time)
   end select
100 format(A,' since ',A)
   iret = set_attributes(ncid,time_id,units=trim(ncdf_time_str))

!  x,y,t
   iret = set_attributes(ncid,zeta_id,units='m',long_name='sea surface elevation')
   iret = set_attributes(ncid,sst_id,units='celsius',long_name='sea surface temperature')
   iret = set_attributes(ncid,sss_id,units='psu',long_name='sea surface salinity')
   iret = set_attributes(ncid,x_taus_id,units='N/m2',long_name='x-wind stress')
   iret = set_attributes(ncid,y_taus_id,units='N/m2',long_name='y-wind stress')
   iret = set_attributes(ncid,swr_id,units='W/m2',long_name='short wave radiation')
   iret = set_attributes(ncid,heat_id,units='W/m2',long_name='surface heat flux')
   iret = set_attributes(ncid,total_id,units='W/m2',long_name='total surface heat exchange')
   iret = set_attributes(ncid,int_sw_id,units='J/m2',long_name='integrated short wave radiation')
   iret = set_attributes(ncid,int_hf_id,units='J/m2',long_name='integrated surface heat flux')
   iret = set_attributes(ncid,int_total_id,units='J/m2',long_name='integrated total surface heat exchange')
   iret = set_attributes(ncid,p_e_id,units='m/s',long_name='surface freshwater flux')
   iret = set_attributes(ncid,u_taus_id,units='m/s',long_name='surface friction velocity')
   iret = set_attributes(ncid,u_taub_id,units='m/s',long_name='bottom friction velocity')

!  x,y,z,t
   iret = set_attributes(ncid,h_id,units='meters',long_name='layer thickness')
   iret = set_attributes(ncid,u_id,units='m/s',long_name='x-velocity')
   iret = set_attributes(ncid,u_obs_id,units='m/s',long_name='obs. x-velocity')
   iret = set_attributes(ncid,v_id,units='m/s',long_name='y-velocity')
   iret = set_attributes(ncid,v_obs_id,units='m/s',long_name='obs. y-velocity')
   iret = set_attributes(ncid,salt_id,units='ppt',long_name='salinity')
   iret = set_attributes(ncid,salt_obs_id,units='ppt',long_name='obs. salinity')
   iret = set_attributes(ncid,temp_id,units='celsius',long_name='temperature')
   iret = set_attributes(ncid,temp_obs_id,units='celcius',long_name='obs. temperature')
   iret = set_attributes(ncid,SS_id,units='s-1',long_name='shear frequency')
   iret = set_attributes(ncid,NN_id,units='s-1',long_name='buoyancy frequency')
   iret = set_attributes(ncid,sigma_t_id,units='s-1',long_name='sigma_t')
   iret = set_attributes(ncid,SS_obs_id,units='s-1',long_name='observed shear frequency')
   iret = set_attributes(ncid,NN_obs_id,units='s-1',long_name='observed buoyancy frequency')
   iret = set_attributes(ncid,sigma_t_obs_id,units='s-1',long_name='observed sigma_t')

!  x,y,z1,t
   iret = set_attributes(ncid,num_id,units='m2/s',long_name='viscosity')
   iret = set_attributes(ncid,nuh_id,units='m2/s',long_name='diffusivity')
   iret = set_attributes(ncid,tke_id,units='m2/s2',long_name='turbulent kinetic energy')
   iret = set_attributes(ncid,tmls_id,units='meters',long_name='turbulent macro length scale')
   iret = set_attributes(ncid,tked_id,units='m2/s3',long_name='turbulent kinetic energy dissipation')
   iret = set_attributes(ncid,tked_obs_id,units='m2/s3',long_name='obs. dissipation')
   iret = set_attributes(ncid,prod_shear_id,units='m2/s3',long_name='shear production')
   iret = set_attributes(ncid,prod_buoy_id,units='m2/s3',long_name='buoyancy production')

   iret = set_attributes(ncid,uu_id,units='(m/s)2',long_name='Reynolds stress (u)')
   iret = set_attributes(ncid,vv_id,units='(m/s)2',long_name='Reynolds stress (v)')
   iret = set_attributes(ncid,ww_id,units='(m/s)2',long_name='Reynolds stress (w)')
   iret = set_attributes(ncid,tt_id,units='K2',long_name='temperature variance')
   iret = set_attributes(ncid,chi_id,units='K2/s',long_name='temperature dissipation')

!  global attributes
   iret = nf_put_att_text(ncid,NF_GLOBAL,'Title',LEN_TRIM(title),title)
   history = 'Created by GOTM v. '//RELEASE
   iret = nf_put_att_text(ncid,NF_GLOBAL,'history',LEN_TRIM(history),history)
   iret = nf_put_att_text(ncid,NF_GLOBAL,'Conventions',6,'COARDS')
   call check_err(iret)

!  leave define mode
   iret = nf_enddef(ncid)
   call check_err(iret)

!  save latitude and logitude
   iret = store_data(ncid,lon_id,POINT,1,scalar=lon)
   iret = store_data(ncid,lat_id,POINT,1,scalar=lat)

   iret = nf_sync(ncid)
   call check_err(iret)

   return
   end subroutine init_ncdf
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Save model results to file
!
! !INTERFACE:
   subroutine do_ncdf_out(nlev,secs)
!
! !DESCRIPTION:
!  Write the GOTM core variables to the NetCDF file.
!
! !USES:
   use airsea,       only: tx,ty,I_0,heat,p_e,sst,sss,int_sw,int_hf,int_total
   use meanflow,     only: depth0,u_taub,u_taus,rho_0,gravity
   use meanflow,     only: h,u,v,z,S,T,buoy,SS,NN,P,B
   use turbulence,   only: num,nuh,tke,eps,L,uu,vv,ww,tt,chi
   use observations, only: zeta,uprof,vprof,tprof,sprof,epsprof
   use eqstate,      only: eqstate1

   IMPLICIT NONE

!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   REALTYPE, intent(in)                 :: secs
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See ncdfout module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: iret,i
   REALTYPE                  :: time
   REALTYPE                  :: dum(0:nlev)
   REAL_4B                   :: buoyp,buoym,dz
   REALTYPE                  :: zz
   logical, save             :: first = .true.
!
!-------------------------------------------------------------------------
!BOC
   if ( first ) then
      iret = store_data(ncid,z_id,Z_SHAPE,nlev,array=z)
      if( .not. GrADS ) then
         dum(1) = -depth0 + h(1)
         do i=2,nlev
            dum(i)=dum(i-1)+h(i)
         end do
         iret = store_data(ncid,z1_id,Z_SHAPE,nlev,array=dum)
      end if
      first = .false.
   end if

!  Storing the time - both the coordinate and later a time string.
   select case (ncdf_time_unit)
      case(0)                           ! seconds
         time = secs
      case(1)                           ! minutes
         time = secs/60
      case(2)                           ! hours
         time = secs/3600
      case default
         time = secs
   end select
   iret = store_data(ncid,time_id,T_SHAPE,1,scalar=time)

!  Time varying data : x,y,t 
   iret = store_data(ncid,zeta_id,XYT_SHAPE,1,scalar=zeta)
   iret = store_data(ncid,sst_id,XYT_SHAPE,1,scalar=sst)
   iret = store_data(ncid,sss_id,XYT_SHAPE,1,scalar=sss)
   iret = store_data(ncid,x_taus_id,XYT_SHAPE,1,scalar=tx)
   iret = store_data(ncid,y_taus_id,XYT_SHAPE,1,scalar=ty)
   iret = store_data(ncid,swr_id,XYT_SHAPE,1,scalar=I_0)
   iret = store_data(ncid,heat_id,XYT_SHAPE,1,scalar=heat)
   iret = store_data(ncid,total_id,XYT_SHAPE,1,scalar=heat+I_0)
   iret = store_data(ncid,int_sw_id,XYT_SHAPE,1,scalar=int_sw)
   iret = store_data(ncid,int_hf_id,XYT_SHAPE,1,scalar=int_hf)
   iret = store_data(ncid,int_total_id,XYT_SHAPE,1,scalar=int_total)
   iret = store_data(ncid,p_e_id,XYT_SHAPE,1,scalar=p_e)
   iret = store_data(ncid,u_taub_id,XYT_SHAPE,1,scalar=u_taub)
   iret = store_data(ncid,u_taus_id,XYT_SHAPE,1,scalar=u_taus)

!  Time varying profile data : x,y,z,t
   iret = store_data(ncid,h_id,XYZT_SHAPE,nlev,array=h)
   iret = store_data(ncid,u_id,XYZT_SHAPE,nlev,array=u)
   iret = store_data(ncid,u_obs_id,XYZT_SHAPE,nlev,array=uprof)
   iret = store_data(ncid,v_id,XYZT_SHAPE,nlev,array=v)
   iret = store_data(ncid,v_obs_id,XYZT_SHAPE,nlev,array=vprof)
   iret = store_data(ncid,salt_id,XYZT_SHAPE,nlev,array=S)
   iret = store_data(ncid,salt_obs_id,XYZT_SHAPE,nlev,array=sprof)
   iret = store_data(ncid,temp_id,XYZT_SHAPE,nlev,array=T)
   iret = store_data(ncid,temp_obs_id,XYZT_SHAPE,nlev,array=tprof)
   iret = store_data(ncid,SS_id,XYZT_SHAPE,nlev,array=SS)
   iret = store_data(ncid,NN_id,XYZT_SHAPE,nlev,array=NN)

   dum(1:nlev)=-buoy(1:nlev)*rho_0/gravity+rho_0-1000.
   iret = store_data(ncid,sigma_t_id,XYZT_SHAPE,nlev,array=dum)

   do i=1,nlev-1
     dum(i)=((uprof(i+1)-uprof(i))/(0.5*(h(i+1)+h(i))))**2 +  &
            ((vprof(i+1)-vprof(i))/(0.5*(h(i+1)+h(i))))**2
   end do 
   dum(nlev)=dum(nlev-1)
   iret = store_data(ncid,SS_obs_id,XYZT_SHAPE,nlev,array=dum)

   zz = _ZERO_
   do i=nlev-1,1,-1
      zz=zz+h(i+1)
      dz=0.5*(h(i)+h(i+1))
      buoyp=eqstate1(sprof(i+1),tprof(i+1),zz/10.,gravity,rho_0)
      buoym=eqstate1(sprof(i  ),tprof(i  ),zz/10.,gravity,rho_0)
      dum(i)=(buoyp-buoym)/dz
   end do
   iret = store_data(ncid,NN_obs_id,XYZT_SHAPE,nlev,array=dum)

   dum(1:nlev)=-buoy(1:nlev)*rho_0/gravity+rho_0-1000.
   zz = _ZERO_
   do i=nlev,1,-1
      zz=zz+0.5*h(i)
      dum(i)=eqstate1(sprof(i),tprof(i),zz/10.,gravity,rho_0)
      zz=zz+0.5*h(i)
   end do 
   dum(1:nlev)=-dum(1:nlev)*rho_0/gravity+rho_0-1000.
   iret = store_data(ncid,sigma_t_obs_id,XYZT_SHAPE,nlev,array=dum)

!  Time varying profile data : x,y,z1,t
   iret = store_data(ncid,num_id,XYZT_SHAPE,nlev,array=num)
   iret = store_data(ncid,nuh_id,XYZT_SHAPE,nlev,array=nuh)
   iret = store_data(ncid,tke_id,XYZT_SHAPE,nlev,array=tke)
   iret = store_data(ncid,tked_id,XYZT_SHAPE,nlev,array=eps)
   iret = store_data(ncid,tmls_id,XYZT_SHAPE,nlev,array=L)
   iret = store_data(ncid,tked_obs_id,XYZT_SHAPE,nlev,array=epsprof)
   iret = store_data(ncid,prod_shear_id,XYZT_SHAPE,nlev,array=P)
   iret = store_data(ncid,prod_buoy_id,XYZT_SHAPE,nlev,array=B)
   iret = store_data(ncid,uu_id,XYZT_SHAPE,nlev,array=uu)
   iret = store_data(ncid,vv_id,XYZT_SHAPE,nlev,array=vv)
   iret = store_data(ncid,ww_id,XYZT_SHAPE,nlev,array=ww)
   iret = store_data(ncid,tt_id,XYZT_SHAPE,nlev,array=tt)
   iret = store_data(ncid,chi_id,XYZT_SHAPE,nlev,array=chi)

   set = set + 1

   iret = nf_sync(ncid)
   call check_err(iret)

   return
   end subroutine do_ncdf_out
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Close files used for saving model results
!
! !INTERFACE:
   subroutine close_ncdf()
   IMPLICIT NONE
!
! !DESCRIPTION:
!  Closes the NetCDF file.
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See ncdfout module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: iret
!
!-------------------------------------------------------------------------
!BOC
   LEVEL1 'Output has been written in NetCDF'

   iret = nf_close(ncid)
   call check_err(iret)

   return
   end subroutine close_ncdf
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Begin or end define mode
!
! !INTERFACE:
   integer function define_mode(ncid,action)
!
! !DESCRIPTION:
!  Depending on the value of the argument {\tt action},
!  this routine put NetCDF in the `define' mode or not.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)       :: ncid
   logical, intent(in)       :: action
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See ncdfout module
!
!EOP
!
! !LOCAL VARIABLES:
   integer	:: iret
!
!-----------------------------------------------------------------------
!BOC
   if(action) then
      iret = nf_redef(ncid)
!kbk      call check_err(iret)
   else
      iret = nf_enddef(ncid)
!kbk      call check_err(iret)
   end if
   define_mode = 0
   return
   end function define_mode
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Define a new NetCDF variable
!
! !INTERFACE:
   integer function new_nc_variable(ncid,name,data_type,n,dims,id)
!
! !DESCRIPTION:
!  This routine is used to define a new variable to store in a NetCDF file.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: ncid
   character(len=*), intent(in)        :: name
   integer, intent(in)                 :: data_type,n
   integer, intent(in)                 :: dims(:)
!
! !OUTPUT PARAMETERS:
   integer, intent(out)                :: id
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See ncdfout module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: iret
!
!-----------------------------------------------------------------------
!BOC
   iret = nf_def_var(ncid,name,data_type,n,dims,id)
   call check_err(iret)
   new_nc_variable = iret
   return
   end function new_nc_variable
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set attributes for a NetCDF variable.
!
! !INTERFACE:
   integer function set_attributes(ncid,id,                         &
                                   units,long_name,                 &
                                   valid_min,valid_max,valid_range, &
                                   scale_factor,add_offset,         &
                                   FillValue,missing_value,         &
                                   C_format,FORTRAN_format)
!
! !DESCRIPTION:
!  This routine is used to set a number of attributes for 
!  variables. The routine makes heavy use of the {\tt optional} keyword. 
!  The list of recognized keywords is very easy to extend. We have 
!  included a sub-set of the COARDS conventions. 
!
! !USES:
!  IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: ncid,id
   character(len=*), optional          :: units,long_name
   REALTYPE, optional                  :: valid_min,valid_max
   REALTYPE, optional                  :: valid_range(2)
   REALTYPE, optional                  :: scale_factor,add_offset
   REALTYPE, optional                  :: FillValue,missing_value
   character(len=*), optional          :: C_format,FORTRAN_format
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See ncdfout module
!
! !LOCAL VARIABLES:
   integer                   :: len,iret
   REAL_4B                   :: vals(2)
!
!EOP
!-----------------------------------------------------------------------
!BOC
   if(present(units)) then
      len = len_trim(units)
      iret = nf_put_att_text(ncid,id,'units',len,units)
   end if

   if(present(long_name)) then
      len = len_trim(long_name)
      iret = nf_put_att_text(ncid,id,'long_name',len,long_name)
   end if

   if(present(C_format)) then
      len = len_trim(C_format)
      iret = nf_put_att_text(ncid,id,'C_format',len,C_format)
   end if

   if(present(FORTRAN_format)) then
      len = len_trim(FORTRAN_format)
      iret = nf_put_att_text(ncid,id,'FORTRAN_format',len,FORTRAN_format)
   end if

   if(present(valid_min)) then
      vals(1) = valid_min
      iret = nf_put_att_real(ncid,id,'valid_min',NF_FLOAT,1,vals)
   end if

   if(present(valid_max)) then
      vals(1) = valid_max
      iret = nf_put_att_real(ncid,id,'valid_max',NF_FLOAT,1,vals)
   end if

   if(present(valid_range)) then
      vals(1) = valid_range(1)
      vals(2) = valid_range(2)
      iret = nf_put_att_real(ncid,id,'valid_range',NF_FLOAT,2,vals)
   end if

   if(present(scale_factor)) then
      vals(1) = scale_factor
      iret = nf_put_att_real(ncid,id,'scale_factor',NF_FLOAT,1,vals)
   end if

   if(present(add_offset)) then
      vals(1) = add_offset
      iret = nf_put_att_real(ncid,id,'add_offset',NF_FLOAT,1,vals)
   end if

   if(present(FillValue)) then
      vals(1) = FillValue
      iret = nf_put_att_real(ncid,id,'_FillValue',NF_FLOAT,1,vals)
   end if

   if(present(missing_value)) then
      vals(1) = missing_value
      iret = nf_put_att_real(ncid,id,'missing_value',NF_FLOAT,1,vals)
   end if

   set_attributes = 0
   return
   end function set_attributes
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Store values in a NetCDF file
!
! !INTERFACE:
   integer function store_data(ncid,id,var_shape,nlev, &
                               iscalar,iarray,scalar,array)
!
! !DESCRIPTION:
!  This routine is used to store a  variable in the NetCDF file.
!  The subroutine uses {\tt optional} parameters to find out which data
!  type to save.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: ncid,id,var_shape,nlev
   integer, optional                   :: iscalar
   integer, optional                   :: iarray(0:nlev)
   REALTYPE, optional                  :: scalar
   REALTYPE, optional                  :: array(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See ncdfout module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: iret,n=0
   integer                   :: idum(1:nlev)
   REAL_4B                   :: r4,dum(1:nlev)
!
!-----------------------------------------------------------------------
!BOC
   if (.not. present(iscalar) .and. .not. present(iarray) .and. &
       .not. present(scalar)  .and. .not. present(array) ) then
      FATAL 'At least one optional argument has to be passed to - store_data()'
      stop 'store_data'
   end if
   n = 0
   if(present(iscalar)) n = n+1
   if(present(iarray))  n = n+1
   if(present(scalar))  n = n+1
   if(present(array))   n = n+1
   if(n .ne. 1) then
      FATAL 'Only one optional argument must be passed to - store_data()'
      stop 'store_data'
   end if

   if (present(iscalar)) then
      select case (var_shape)
         case(POINT)
            iret = nf_put_var_int(ncid,id,iscalar)
         case(T_SHAPE)
            start(1) = set; edges(1) = 1
            idum(1)=iscalar
            iret = nf_put_vara_int(ncid,id,start,edges,idum)
         case default
            FATAL 'A non valid - var_shape - has been passed in store_data()'
            stop 'store_data'
      end select
   else if (present(scalar)) then
      select case (var_shape)
         case(POINT)
            r4 = scalar
            iret = nf_put_var_real(ncid,id,r4)
         case(T_SHAPE)
            start(1) = set; edges(1) = 1
            dum(1)=scalar
            iret = nf_put_vara_real(ncid,id,start,edges,dum)
         case(XYT_SHAPE)
            start(1) = 1;   edges(1) = lon_len
            start(2) = 1;   edges(2) = lat_len
            start(3) = set; edges(3) = 1
            dum(1)=scalar
            iret = nf_put_vara_real(ncid,id,start,edges,dum)
         case default
            FATAL 'A non valid - var_shape - has been passed in store_data()'
            stop 'store_data'
      end select
   else if (present(array)) then
      select case (var_shape)
         case(Z_SHAPE)
            start(1) = 1;   edges(1) = depth_len
         case(XYZT_SHAPE)
            start(1) = 1;   edges(1) = lon_len
            start(2) = 1;   edges(2) = lat_len
            start(3) = 1;   edges(3) = depth_len
            start(4) = set; edges(4) = 1
         case default
            FATAL 'A non valid - var_shape - has been passed in store_data()'
            stop 'store_data'
      end select
      dum(1:nlev)=array(1:nlev)
      iret = nf_put_vara_real(ncid,id,start,edges,dum)
   else
   end if
   call check_err(iret)
   store_data = iret
   return
   end function store_data
!EOC

!-----------------------------------------------------------------------

   end module ncdfout

!-----------------------------------------------------------------------

   subroutine check_err(iret)
   integer iret
   include 'netcdf.inc'
   if (iret .ne. NF_NOERR) then
   print *, nf_strerror(iret)
   stop
   endif
   end


!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

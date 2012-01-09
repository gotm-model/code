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
!  not {\tt secs}.
!
!  In both cases, the type and number of variables appearing in the
!  output file depends on the turbulence model and the output flags
!  set by the user. If you use, for example, the KPP turbulence module
!  no information for the TKE, the dissipation rate, the turbulence
!  production terms are saved, because the KPP model does not provide
!  information about these quantities.
!
!  Note that if you {\tt \#define EXTRA\_OUTPUT}
!  in {\tt cppdef.h}, then you will find the a number of dummy fields
!  called {\tt mean1, mean2, ...} and {\tt turb1, turb2, ...} in the
!  netCDF output file after re-compiling and runnign GOTM. These extra
!  variables are public members  of the {\tt meanflow} and
!  {\tt turbulence} modules and are convenient for testing and
!  debuging.
!
! !USES:
   use turbulence, only: turb_method
   use netcdf
   IMPLICIT NONE
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_ncdf, do_ncdf_out, close_ncdf
   public define_mode, new_nc_variable, set_attributes, store_data
!
! !PUBLIC DATA MEMBERS:

!  netCDF file id
   integer, public                     :: ncid

!  dimension ids
   integer                             :: lon_dim,lat_dim,z_dim,z1_dim
   integer                             :: time_dim
   integer                             :: dim1d(1)
   integer                             :: dim3d(3)
   integer                             :: dim4d(4)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: ncdfout.F90,v $
!  Revision 1.21  2010-12-16 09:52:37  kb
!  updated to Fortran90 NetCDF interface
!
!  Revision 1.20  2010-09-17 12:53:52  jorn
!  extensive code clean-up to ensure proper initialization and clean-up of all variables
!
!  Revision 1.19  2008-08-01 07:32:25  lars
!  updated unit description
!
!  Revision 1.18  2007-12-07 10:12:20  kb
!  replaced p_e with precip and included evap
!
!  Revision 1.17  2007-11-02 10:58:34  jorn
!  Made set_no public to allow other modules to save to NetCDF directly
!
!  Revision 1.16  2007-08-19 08:25:54  jorn
!  fixed typo: celcius -> celsius
!
!  Revision 1.15  2006-11-27 15:13:43  kbk
!  re-initialse first and set_no when closing .nc file
!
!  Revision 1.14  2005-12-27 08:37:58  hb
!  Oxygen units indicated as mmol o2/m**3 in netCDF output
!
!  Revision 1.13  2005-12-23 14:10:35  kbk
!  support for reading oxygen profiles
!
!  Revision 1.12  2005/11/18 11:16:27  kbk
!  removed unused variables
!
!  Revision 1.11  2005/09/14 11:53:06  kbk
!  fixed position of counter for time dimension - fixes bio storing
!
!  Revision 1.10  2005/08/11 14:15:33  kbk
!  when storing time changed variable time to temp_time - Portland compiler
!
!  Revision 1.9  2005/07/06 14:22:40  kbk
!  updated documentation - saves KPP related variables
!
!  Revision 1.8  2004/01/09 10:14:01  kbk
!  consistency between stored surface stress and units (now N/m^2)
!
!  Revision 1.7  2003/12/11 09:58:22  kbk
!  now compiles with FORTRAN_COMPILER=IFORT - removed TABS
!
!  Revision 1.6  2003/10/14 08:04:32  kbk
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
   logical, private          :: first
   integer, public          :: set_no
!  dimension lengths
   integer, parameter        :: lon_len=1
   integer, parameter        :: lat_len=1
   integer                   :: depth_len
   integer, parameter        :: time_len=NF90_UNLIMITED
!  variable ids
   integer, private          :: lon_id,lat_id,z_id,z1_id,time_id
   integer, private          :: zeta_id
   integer, private          :: sst_id,sss_id
   integer, private          :: x_taus_id,y_taus_id
   integer, private          :: swr_id,heat_id,total_id,precip_id,evap_id
   integer, private          :: int_precip_id,int_evap_id,int_fwf_id
   integer, private          :: int_swr_id,int_heat_id,int_total_id
   integer, private          :: u_taus_id,u_taub_id
   integer, private          :: zsbl_id,zbbl_id
   integer, private          :: h_id
   integer, private          :: u_id,u_obs_id
   integer, private          :: v_id,v_obs_id
   integer, private          :: temp_id,temp_obs_id
   integer, private          :: salt_id,salt_obs_id
   integer, private          :: num_id,nuh_id,nus_id
   integer, private          :: gamu_id,gamv_id,gamh_id,gams_id
   integer, private          :: SS_id,SS_obs_id
   integer, private          :: NN_id,NN_obs_id
   integer, private          :: sigma_t_id,sigma_t_obs_id
   integer, private          :: tke_id,kb_id,l_id
# ifdef EXTRA_OUTPUT
   integer, private          :: mean1_id,mean2_id,mean3_id,mean4_id,mean5_id
   integer, private          :: turb1_id,turb2_id,turb3_id,turb4_id,turb5_id
# endif
   integer, private          :: eps_id,epsb_id,eps_obs_id
   integer, private          :: P_id,G_id,Pb_id
   integer, private          :: uu_id,vv_id,ww_id
   integer, private          :: o2_obs_id
   integer, private          :: ncdf_time_unit
   integer, private          :: start(4),edges(4)
   logical,private           :: GrADS
   integer, private          :: Ac_id,Af_id,dAdz_id
   integer, private          :: total_salt_id
   integer, private          :: Qs_id, Qt_id
   integer, private          :: wIs_id
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
! !USES:
   use meanflow, only: lake
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
   character(len=128)        :: ncdf_time_str,history
!-------------------------------------------------------------------------
!BOC
   first = .true.

   ncid = -1
   iret = nf90_create(fn,NF90_CLOBBER,ncid)
   call check_err(iret)

   depth_len=nlev
   ncdf_time_unit = time_unit
   if(time_unit .eq. 2) then
      GrADS = .true.
   end if

!  define dimensions
   iret = nf90_def_dim(ncid, 'lon', 1, lon_dim)
   call check_err(iret)
   iret = nf90_def_dim(ncid, 'lat', 1, lat_dim)
   call check_err(iret)
   iret = nf90_def_dim(ncid, 'z', nlev, z_dim)
   call check_err(iret)
   if( .not. GrADS ) then
      iret = nf90_def_dim(ncid, 'z1', nlev, z1_dim)
      call check_err(iret)
   end if
   iret = nf90_def_dim(ncid, 'time', time_len, time_dim)
   call check_err(iret)

!  define coordinates
   dim1d = lon_dim
   iret = nf90_def_var(ncid,'lon',NF90_REAL,dim1d,lon_id)
   call check_err(iret)
   dim1d = lat_dim
   iret = nf90_def_var(ncid,'lat',NF90_REAL,dim1d,lat_id)
   call check_err(iret)
   dim1d = z_dim
   iret = nf90_def_var(ncid,'z',NF90_REAL,dim1d,z_id)
   call check_err(iret)
   if( .not. GrADS ) then
      dim1d = z1_dim
      iret = nf90_def_var(ncid,'z1',NF90_REAL,dim1d,z1_id)
      call check_err(iret)
   end if
   dim1d = time_dim
   iret = nf90_def_var(ncid,'time',NF90_REAL,dim1d,time_id)
   call check_err(iret)

!  define variables

!  x,y,t
   dim3d(1) = lon_dim
   dim3d(2) = lat_dim
   dim3d(3) = time_dim
   iret = nf90_def_var(ncid,'zeta',NF90_REAL,dim3d,zeta_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'sst',NF90_REAL,dim3d,sst_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'sss',NF90_REAL,dim3d,sss_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'x-taus',NF90_REAL,dim3d,x_taus_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'y-taus',NF90_REAL,dim3d,y_taus_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'swr',NF90_REAL,dim3d,swr_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'heat',NF90_REAL,dim3d,heat_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'total',NF90_REAL,dim3d,total_id)
   call check_err(iret)

   iret = nf90_def_var(ncid,'int_precip',NF90_REAL,dim3d,int_precip_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'int_evap',NF90_REAL,dim3d,int_evap_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'int_fwf',NF90_REAL,dim3d,int_fwf_id)
   call check_err(iret)

   iret = nf90_def_var(ncid,'int_swr',NF90_REAL,dim3d,int_swr_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'int_heat',NF90_REAL,dim3d,int_heat_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'int_total',NF90_REAL,dim3d,int_total_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'precip',NF90_REAL,dim3d,precip_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'evap',NF90_REAL,dim3d,evap_id)
   call check_err(iret)

   iret = nf90_def_var(ncid,'u_taus',NF90_REAL,dim3d,u_taus_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'u_taub',NF90_REAL,dim3d,u_taub_id)
   call check_err(iret)

   if (turb_method.eq.99) then
      iret = nf90_def_var(ncid,'zsbl',NF90_REAL,dim3d,zsbl_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'zbbl',NF90_REAL,dim3d,zbbl_id)
      call check_err(iret)
   endif


!  x,y,z,t
   dim4d(1) = lon_dim
   dim4d(2) = lat_dim
   dim4d(3) = z_dim
   dim4d(4) = time_dim
   iret = nf90_def_var(ncid,'h',NF90_REAL,dim4d,h_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'u',NF90_REAL,dim4d,u_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'u_obs',NF90_REAL,dim4d,u_obs_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'v',NF90_REAL,dim4d,v_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'v_obs',NF90_REAL,dim4d,v_obs_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'salt',NF90_REAL,dim4d,salt_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'salt_obs',NF90_REAL,dim4d,salt_obs_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'temp',NF90_REAL,dim4d,temp_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'temp_obs',NF90_REAL,dim4d,temp_obs_id)
   call check_err(iret)
   if (lake) then
      iret = nf90_def_var(ncid,'Ac',NF90_REAL,dim4d,Ac_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'dAdz',NF90_REAL,dim4d, dAdz_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'total salt',NF90_REAL,dim3d,total_salt_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'Qs',NF90_REAL,dim4d,Qs_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'Qt',NF90_REAL,dim4d,Qt_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'wIs',NF90_REAL,dim4d,wIs_id)
      call check_err(iret)
   end if
   iret = nf90_def_var(ncid,'SS',NF90_REAL,dim4d,SS_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'SS_obs',NF90_REAL,dim4d,SS_obs_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'NN',NF90_REAL,dim4d,NN_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'NN_obs',NF90_REAL,dim4d,NN_obs_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'sigma_t',NF90_REAL,dim4d,sigma_t_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'sigma_t_obs',NF90_REAL,dim4d,sigma_t_obs_id)
   call check_err(iret)
   if( .not. GrADS ) then
      dim4d(3) = z1_dim
   end if
   iret = nf90_def_var(ncid,'num',NF90_REAL,dim4d,num_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'nuh',NF90_REAL,dim4d,nuh_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'nus',NF90_REAL,dim4d,nus_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'gamu',NF90_REAL,dim4d,gamu_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'gamv',NF90_REAL,dim4d,gamv_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'gamh',NF90_REAL,dim4d,gamh_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'gams',NF90_REAL,dim4d,gams_id)
   call check_err(iret)
   if (lake) then
      iret = nf90_def_var(ncid,'Af',NF90_REAL,dim4d,Af_id)
      call check_err(iret)
   end if

   if (turb_method.ne.99) then
      iret = nf90_def_var(ncid,'tke',NF90_REAL,dim4d,tke_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'kb',NF90_REAL,dim4d,kb_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'l',NF90_REAL,dim4d,l_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'eps',NF90_REAL,dim4d,eps_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'epsb',NF90_REAL,dim4d,epsb_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'eps_obs',NF90_REAL,dim4d,eps_obs_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'P',NF90_REAL,dim4d,P_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'G',NF90_REAL,dim4d,G_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'Pb',NF90_REAL,dim4d,Pb_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'uu',NF90_REAL,dim4d,uu_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'vv',NF90_REAL,dim4d,vv_id)
      call check_err(iret)
      iret = nf90_def_var(ncid,'ww',NF90_REAL,dim4d,ww_id)
      call check_err(iret)
   endif

   iret = nf90_def_var(ncid,'o2_obs',NF90_REAL,dim4d,o2_obs_id)
   call check_err(iret)

# ifdef EXTRA_OUTPUT
   iret = nf90_def_var(ncid,'mean1',NF90_REAL,dim4d,mean1_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'mean2',NF90_REAL,dim4d,mean2_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'mean3',NF90_REAL,dim4d,mean3_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'mean4',NF90_REAL,dim4d,mean4_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'mean5',NF90_REAL,dim4d,mean5_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'turb1',NF90_REAL,dim4d,turb1_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'turb2',NF90_REAL,dim4d,turb2_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'turb3',NF90_REAL,dim4d,turb3_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'turb4',NF90_REAL,dim4d,turb4_id)
   call check_err(iret)
   iret = nf90_def_var(ncid,'turb5',NF90_REAL,dim4d,turb5_id)
   call check_err(iret)
# endif


!  assign attributes

!  coordinates
   iret = set_attributes(ncid,lon_id,units='degrees_east')
   iret = set_attributes(ncid,lat_id,units='degrees_north')
   iret = set_attributes(ncid,z_id,units='meters')
   iret = set_attributes(ncid,z1_id,units='meters')

   select case (ncdf_time_unit)
      case(0)                           ! seconds
         write(ncdf_time_str,100) 'seconds',trim(start_time)
      case(1)                           ! minutes
         write(ncdf_time_str,100) 'minutes',trim(start_time)
      case(2)                           ! hours
         write(ncdf_time_str,100) 'hours',trim(start_time)
      case default
         write(ncdf_time_str,100) 'seconds',trim(start_time)
   end select
100 format(A,' since ',A)
   iret = set_attributes(ncid,time_id,units=trim(ncdf_time_str))

!  x,y,t
   iret = set_attributes(ncid,zeta_id,units='m',long_name='sea surface elevation')
   iret = set_attributes(ncid,sst_id,units='celsius',long_name='sea surface temperature')
   iret = set_attributes(ncid,sss_id,units='g/kg',long_name='sea surface salinity')
   iret = set_attributes(ncid,x_taus_id,units='Pa',long_name='x-wind stress')
   iret = set_attributes(ncid,y_taus_id,units='Pa',long_name='y-wind stress')
   iret = set_attributes(ncid,swr_id,units='W/m2',long_name='short wave radiation')
   iret = set_attributes(ncid,heat_id,units='W/m2',long_name='surface heat flux')
   iret = set_attributes(ncid,total_id,units='W/m2',long_name='total surface heat exchange')
   iret = set_attributes(ncid,int_precip_id,units='m',long_name='integrated precipitation')
   iret = set_attributes(ncid,int_evap_id,units='m',long_name='integrated evaporation')
   iret = set_attributes(ncid,int_fwf_id,units='m',long_name='integrated fresh water fluxes')
   iret = set_attributes(ncid,int_swr_id,units='J/m2',long_name='integrated short wave radiation')
   iret = set_attributes(ncid,int_heat_id,units='J/m2',long_name='integrated surface heat flux')
   iret = set_attributes(ncid,int_total_id,units='J/m2',long_name='integrated total surface heat exchange')
   iret = set_attributes(ncid,precip_id,units='m/s',long_name='precipitation')
   iret = set_attributes(ncid,evap_id,units='m/s',long_name='evaporation')
   iret = set_attributes(ncid,u_taus_id,units='m/s',long_name='surface friction velocity')
   iret = set_attributes(ncid,u_taub_id,units='m/s',long_name='bottom friction velocity')

   if (turb_method.eq.99) then
      iret = set_attributes(ncid,zsbl_id,units='m',long_name='SBL position (KPP)')
      iret = set_attributes(ncid,zbbl_id,units='m',long_name='BBL position (KPP)')
   endif

!  x,y,z,t
   iret = set_attributes(ncid,h_id,units='m',long_name='layer thickness')
   iret = set_attributes(ncid,u_id,units='m/s',long_name='x-velocity')
   iret = set_attributes(ncid,u_obs_id,units='m/s',long_name='obs. x-velocity')
   iret = set_attributes(ncid,v_id,units='m/s',long_name='y-velocity')
   iret = set_attributes(ncid,v_obs_id,units='m/s',long_name='obs. y-velocity')
   iret = set_attributes(ncid,salt_id,units='g/kg',long_name='salinity')
   iret = set_attributes(ncid,salt_obs_id,units='g/kg',long_name='obs. salinity')
   iret = set_attributes(ncid,temp_id,units='celsius',long_name='temperature')
   iret = set_attributes(ncid,temp_obs_id,units='celsius',long_name='obs. temperature')
   if (lake) then
      iret = set_attributes(ncid,Ac_id,units='m2', &
                            long_name='hypsography at grid centres')
      iret = set_attributes(ncid,Af_id,units='m2', &
                            long_name='hypsography at grid interfaces')
      iret = set_attributes(ncid,dAdz_id,units='m',long_name='slope of hypsography')
      iret = set_attributes(ncid,total_salt_id,units='kg',long_name='total mass of salt')
      iret = set_attributes(ncid,Qs_id,units='g/(kg*s)', &
                            long_name='salt inflow')
      iret = set_attributes(ncid,Qt_id,units='celsius/s', &
                            long_name='temperature inflow')
      iret = set_attributes(ncid,wIs_id,units='m/s', &
                            long_name='vertical salinity advection velocity')
   end if
   iret = set_attributes(ncid,SS_id,units='1/s2',long_name='shear frequency squared')
   iret = set_attributes(ncid,NN_id,units='1/s2',long_name='buoyancy frequency squared')
   iret = set_attributes(ncid,sigma_t_id,units='kg/m3',long_name='sigma_t')
   iret = set_attributes(ncid,SS_obs_id,units='1/s2',long_name='observed shear frequency')
   iret = set_attributes(ncid,NN_obs_id,units='1/s2',long_name='observed buoyancy frequency')
   iret = set_attributes(ncid,sigma_t_obs_id,units='kg/m3',long_name='observed sigma_t')

!  x,y,z1,t
   iret = set_attributes(ncid,num_id,units='m2/s',long_name='viscosity')
   iret = set_attributes(ncid,nuh_id,units='m2/s',long_name='heat diffusivity')
   iret = set_attributes(ncid,nus_id,units='m2/s',long_name='salt diffusivity')
   iret = set_attributes(ncid,gamu_id,units='m2/s2',long_name='non-local x-momentum flux')
   iret = set_attributes(ncid,gamv_id,units='m2/s2',long_name='non-local y-momentum flux')
   iret = set_attributes(ncid,gamh_id,units='K m/s',long_name='non-local heat flux')
   iret = set_attributes(ncid,gams_id,units='g/kg m/s',long_name='non-local salinity flux')

   if (turb_method.ne.99) then
      iret = set_attributes(ncid,tke_id,units='m2/s2',long_name='turbulent kinetic energy')
      iret = set_attributes(ncid,kb_id,units='m2/s4',long_name='(half) buoyancy variance')
      iret = set_attributes(ncid,l_id,units='m',long_name='turbulent macro length scale')
      iret = set_attributes(ncid,eps_id,units='m2/s3',long_name='dissipation rate of tke')
      iret = set_attributes(ncid,epsb_id,units='m2/s5',long_name='destruction of kb')
      iret = set_attributes(ncid,eps_obs_id,units='m2/s3',long_name='obs. dissipation')
      iret = set_attributes(ncid,P_id,units='m2/s3',long_name='shear production')
      iret = set_attributes(ncid,G_id,units='m2/s3',long_name='buoyancy production')
      iret = set_attributes(ncid,Pb_id,units='m2/s5',long_name='production of kb')
      iret = set_attributes(ncid,uu_id,units='m2/s2',long_name='variance of u-fluctuation')
      iret = set_attributes(ncid,vv_id,units='m2/s2',long_name='variance of v-fluctuation')
      iret = set_attributes(ncid,ww_id,units='m2/s2',long_name='variance of w-fluctuation')
   endif

   iret = set_attributes(ncid,o2_obs_id,units='mmol o2/m**3',long_name='obs. oxygen')

# ifdef EXTRA_OUTPUT
   iret = set_attributes(ncid,mean1_id,units='---',long_name='mean 1')
   iret = set_attributes(ncid,mean2_id,units='---',long_name='mean 2')
   iret = set_attributes(ncid,mean3_id,units='---',long_name='mean 3')
   iret = set_attributes(ncid,mean4_id,units='---',long_name='mean 4')
   iret = set_attributes(ncid,mean5_id,units='---',long_name='mean 5')
   iret = set_attributes(ncid,turb1_id,units='---',long_name='turb 1')
   iret = set_attributes(ncid,turb2_id,units='---',long_name='turb 2')
   iret = set_attributes(ncid,turb3_id,units='---',long_name='turb 3')
   iret = set_attributes(ncid,turb4_id,units='---',long_name='turb 4')
   iret = set_attributes(ncid,turb5_id,units='---',long_name='turb 5')
# endif

!  global attributes
   iret = nf90_put_att(ncid,NF90_GLOBAL,'title',trim(title))
   history = 'Created by GOTM v. '//RELEASE
   iret = nf90_put_att(ncid,NF90_GLOBAL,'history',trim(history))
   iret = nf90_put_att(ncid,NF90_GLOBAL,'Conventions','COARDS')
   call check_err(iret)

!  leave define mode
   iret = nf90_enddef(ncid)
   call check_err(iret)

!  save latitude and logitude
   iret = store_data(ncid,lon_id,POINT,1,scalar=lon)
   iret = store_data(ncid,lat_id,POINT,1,scalar=lat)

   iret = nf90_sync(ncid)
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
   use airsea,       only: tx,ty,I_0,heat,precip,evap,sst,sss
   use airsea,       only: int_precip,int_evap,int_fwf
   use airsea,       only: int_swr,int_heat,int_total
   use meanflow,     only: lake
   use meanflow,     only: depth0,u_taub,u_taus,rho_0,gravity
   use meanflow,     only: h,Ac,Af,dAdz
   use meanflow,     only: u,v,z,S,T,buoy,SS,NN
   use turbulence,   only: P,B,Pb
   use turbulence,   only: num,nuh,nus
   use turbulence,   only: gamu,gamv,gamh,gams
   use turbulence,   only: tke,kb,eps,epsb,L,uu,vv,ww
   use kpp,          only: zsbl,zbbl
   use observations, only: zeta,uprof,vprof,tprof,sprof,epsprof,o2_prof
   use observations, only: Qs, Qt, FQs
   use eqstate,      only: eqstate1
# ifdef EXTRA_OUTPUT
   use meanflow,     only: mean1,mean2,mean3,mean4,mean5
   use turbulence,   only: turb1,turb2,turb3,turb4,turb5
# endif
   IMPLICIT NONE

!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: nlev
   REALTYPE, intent(in)                :: secs
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See ncdfout module
!
!EOP
!
! !LOCAL VARIABLES:
   integer                             :: iret,i
   REALTYPE                            :: temp_time
   REALTYPE                            :: dum(0:nlev)
   REAL_4B                             :: buoyp,buoym,dz
   REALTYPE                            :: zz
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
      set_no = 0
      first = .false.
   end if

   set_no = set_no + 1

!  Storing the time - both the coordinate and later a time string.
   select case (ncdf_time_unit)
      case(0)                           ! seconds
         temp_time = secs
      case(1)                           ! minutes
         temp_time = secs/60.
      case(2)                           ! hours
         temp_time = secs/3600.
      case default
         temp_time = secs
   end select
   iret = store_data(ncid,time_id,T_SHAPE,1,scalar=temp_time)

!  Time varying data : x,y,t
   iret = store_data(ncid,zeta_id,XYT_SHAPE,1,scalar=zeta)
   iret = store_data(ncid,sst_id,XYT_SHAPE,1,scalar=sst)
   iret = store_data(ncid,sss_id,XYT_SHAPE,1,scalar=sss)
   iret = store_data(ncid,x_taus_id,XYT_SHAPE,1,scalar=rho_0*tx)
   iret = store_data(ncid,y_taus_id,XYT_SHAPE,1,scalar=rho_0*ty)
   iret = store_data(ncid,swr_id,XYT_SHAPE,1,scalar=I_0)
   iret = store_data(ncid,heat_id,XYT_SHAPE,1,scalar=heat)
   iret = store_data(ncid,total_id,XYT_SHAPE,1,scalar=heat+I_0)
   iret = store_data(ncid,int_precip_id,XYT_SHAPE,1,scalar=int_precip)
   iret = store_data(ncid,int_evap_id,XYT_SHAPE,1,scalar=int_evap)
   iret = store_data(ncid,int_fwf_id,XYT_SHAPE,1,scalar=int_fwf)
   iret = store_data(ncid,int_swr_id,XYT_SHAPE,1,scalar=int_swr)
   iret = store_data(ncid,int_heat_id,XYT_SHAPE,1,scalar=int_heat)
   iret = store_data(ncid,int_total_id,XYT_SHAPE,1,scalar=int_total)
   iret = store_data(ncid,precip_id,XYT_SHAPE,1,scalar=precip)
   iret = store_data(ncid,evap_id,XYT_SHAPE,1,scalar=evap)
   iret = store_data(ncid,u_taub_id,XYT_SHAPE,1,scalar=u_taub)
   iret = store_data(ncid,u_taus_id,XYT_SHAPE,1,scalar=u_taus)
   if (lake) then
      dum(1) = 0
      do i=1,nlev
         dum(1) = dum(1) + Ac(i) * S(i) * h(i)
      end do
      iret = store_data(ncid,total_salt_id,XYT_SHAPE,1,scalar=dum(1))
   end if

   if (turb_method.eq.99) then
      iret = store_data(ncid,zsbl_id,XYT_SHAPE,1,scalar=zsbl)
      iret = store_data(ncid,zbbl_id,XYT_SHAPE,1,scalar=zbbl)
   endif

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
   if (lake) then
      iret = store_data(ncid,Ac_id,XYZT_SHAPE,nlev,array=Ac)
      iret = store_data(ncid,dAdz_id,XYZT_SHAPE,nlev,array=dAdz)
      iret = store_data(ncid,Qs_id,XYZT_SHAPE,nlev,array=Qs)
      iret = store_data(ncid,Qt_id,XYZT_SHAPE,nlev,array=Qt)
      do i=1,nlev
         dum(i) = FQs(i) / Ac(i)
      end do
      iret = store_data(ncid,wIs_id,XYZT_SHAPE,nlev,array=dum)
   end if
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
   iret = store_data(ncid,nus_id,XYZT_SHAPE,nlev,array=nus)
   iret = store_data(ncid,gamu_id,XYZT_SHAPE,nlev,array=gamu)
   iret = store_data(ncid,gamv_id,XYZT_SHAPE,nlev,array=gamv)
   iret = store_data(ncid,gamh_id,XYZT_SHAPE,nlev,array=gamh)
   iret = store_data(ncid,gams_id,XYZT_SHAPE,nlev,array=gams)
   if (lake) then
      iret = store_data(ncid,Af_id,XYZT_SHAPE,nlev,array=Af)
   end if

   if (turb_method.ne.99) then
      iret = store_data(ncid,tke_id,XYZT_SHAPE,nlev,array=tke)
      iret = store_data(ncid,kb_id,XYZT_SHAPE,nlev,array=kb)
      iret = store_data(ncid,eps_id,XYZT_SHAPE,nlev,array=eps)
      iret = store_data(ncid,epsb_id,XYZT_SHAPE,nlev,array=epsb)
      iret = store_data(ncid,l_id,XYZT_SHAPE,nlev,array=L)
      iret = store_data(ncid,eps_obs_id,XYZT_SHAPE,nlev,array=epsprof)
      iret = store_data(ncid,P_id,XYZT_SHAPE,nlev,array=P)
      iret = store_data(ncid,G_id,XYZT_SHAPE,nlev,array=B)
      iret = store_data(ncid,Pb_id,XYZT_SHAPE,nlev,array=Pb)
      iret = store_data(ncid,uu_id,XYZT_SHAPE,nlev,array=uu)
      iret = store_data(ncid,vv_id,XYZT_SHAPE,nlev,array=vv)
      iret = store_data(ncid,ww_id,XYZT_SHAPE,nlev,array=ww)
   endif

   iret = store_data(ncid,o2_obs_id,XYZT_SHAPE,nlev,array=o2_prof)

# ifdef EXTRA_OUTPUT
   iret = store_data(ncid,mean1_id,XYZT_SHAPE,nlev,array=mean1)
   iret = store_data(ncid,mean2_id,XYZT_SHAPE,nlev,array=mean2)
   iret = store_data(ncid,mean3_id,XYZT_SHAPE,nlev,array=mean3)
   iret = store_data(ncid,mean4_id,XYZT_SHAPE,nlev,array=mean4)
   iret = store_data(ncid,mean5_id,XYZT_SHAPE,nlev,array=mean5)
   iret = store_data(ncid,turb1_id,XYZT_SHAPE,nlev,array=turb1)
   iret = store_data(ncid,turb2_id,XYZT_SHAPE,nlev,array=turb2)
   iret = store_data(ncid,turb3_id,XYZT_SHAPE,nlev,array=turb3)
   iret = store_data(ncid,turb4_id,XYZT_SHAPE,nlev,array=turb4)
   iret = store_data(ncid,turb5_id,XYZT_SHAPE,nlev,array=turb5)
# endif

   iret = nf90_sync(ncid)
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

   if (ncid .ne. -1) then
      iret = nf90_close(ncid)
      call check_err(iret)
   end if

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
   integer         :: iret
!
!-----------------------------------------------------------------------
!BOC
   if(action) then
      iret = nf90_redef(ncid)
!kbk      call check_err(iret)
   else
      iret = nf90_enddef(ncid)
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
   integer function new_nc_variable(ncid,name,data_type,dims,id)
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
   integer, intent(in)                 :: data_type
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
   iret = nf90_def_var(ncid,name,data_type,dims,id)
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
      iret = nf90_put_att(ncid,id,'units',trim(units))
   end if

   if(present(long_name)) then
      iret = nf90_put_att(ncid,id,'long_name',trim(long_name))
   end if

   if(present(C_format)) then
      iret = nf90_put_att(ncid,id,'C_format',trim(C_format))
   end if

   if(present(FORTRAN_format)) then
      iret = nf90_put_att(ncid,id,'FORTRAN_format',trim(FORTRAN_format))
   end if

   if(present(valid_min)) then
      vals(1) = valid_min
      iret = nf90_put_att(ncid,id,'valid_min',vals(1:1))
   end if

   if(present(valid_max)) then
      vals(1) = valid_max
      iret = nf90_put_att(ncid,id,'valid_max',vals(1:1))
   end if

   if(present(valid_range)) then
      vals(1) = valid_range(1)
      vals(2) = valid_range(2)
      iret = nf90_put_att(ncid,id,'valid_range',vals(1:2))
   end if

   if(present(scale_factor)) then
      vals(1) = scale_factor
      iret = nf90_put_att(ncid,id,'scale_factor',vals(1:1))
   end if

   if(present(add_offset)) then
      vals(1) = add_offset
      iret = nf90_put_att(ncid,id,'add_offset',vals(1:1))
   end if

   if(present(FillValue)) then
      vals(1) = FillValue
      iret = nf90_put_att(ncid,id,'_FillValue',vals(1:1))
   end if

   if(present(missing_value)) then
      vals(1) = missing_value
      iret = nf90_put_att(ncid,id,'missing_value',vals(1:1))
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
   integer                   :: iret,n
   integer                   :: idum(1)
   REAL_4B                   :: r4
   REAL_4B                   :: dum(1)
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
            iret = nf90_put_var(ncid,id,iscalar)
         case(T_SHAPE)
            start(1) = set_no; edges(1) = 1
            idum(1)=iscalar
            iret = nf90_put_var(ncid,id,idum,start,edges)
         case default
            FATAL 'A non valid - var_shape - has been passed in store_data()'
            stop 'store_data'
      end select
   else if (present(scalar)) then
      select case (var_shape)
         case(POINT)
            r4 = scalar
            iret = nf90_put_var(ncid,id,r4)
         case(T_SHAPE)
            start(1) = set_no; edges(1) = 1
            dum(1)=scalar
            iret = nf90_put_var(ncid,id,dum,start,edges)
         case(XYT_SHAPE)
            start(1) = 1;   edges(1) = lon_len
            start(2) = 1;   edges(2) = lat_len
            start(3) = set_no; edges(3) = 1
            dum(1)=scalar
            iret = nf90_put_var(ncid,id,dum,start,edges)
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
            start(4) = set_no; edges(4) = 1
         case default
            FATAL 'A non valid - var_shape - has been passed in store_data()'
            stop 'store_data'
      end select
!KB      dum(1:nlev)=array(1:nlev)
      iret = nf90_put_var(ncid,id,array(1:nlev),start,edges)
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
   use netcdf
   integer iret
   if (iret .ne. NF90_NOERR) then
   print *, nf90_strerror(iret)
   stop
   endif
   end


!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

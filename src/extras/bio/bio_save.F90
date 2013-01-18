#ifdef BIO

#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
! !ROUTINE: Storing the results
!
! !INTERFACE:
   subroutine bio_save(totn)
!
! !DESCRIPTION:
! Here, the output of biogeochemical parameters either as ascii or as
! NetCDF files is managed.
!
! !USES:
   use bio_var
#ifndef NO_0D_BIO
   use bio_0d,only: save_bio_0d
#endif
   use output, only: out_fmt,ts
#ifdef NETCDF_FMT
   use netcdf
   use ncdfout, only: ncid
   use ncdfout, only: lon_dim,lat_dim,z_dim,time_dim,dim1d,dim4d
   use ncdfout, only: define_mode,new_nc_variable,set_attributes,store_data
#endif
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: totn
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   integer, save             :: nn
   integer, save             :: totn_id
   integer                   :: iret
   integer                   :: out_unit=67
   REALTYPE                  :: zz
   integer                   :: i,j,n
!EOP
!-----------------------------------------------------------------------
!BOC
   select case (bio_model)
#ifndef NO_0D_BIO
      case (1000:)
         call save_bio_0d(init_saved_vars,out_fmt,out_unit,ncid)
#endif
   end select

   select case (out_fmt)
      case (ASCII)
         if (init_saved_vars) then
            open(out_unit,file='bio.out',status='unknown')
            nn = ubound(cc(1,:),1)
         end if
         write(out_unit,*)
         write(out_unit,*) trim(ts)
         zz = _ZERO_
         do i=nn,1,-1
            zz=zz+0.5*h(i)
            write(out_unit,115) zz,(cc(j,i) , j=1,numc)
            zz=zz+0.5*h(i)
         end do
115 format(F10.4,100(1x,E10.4E2))

      case (NETCDF)
#ifdef NETCDF_FMT
         if (init_saved_vars) then
            dim4d(1) = lon_dim
            dim4d(2) = lat_dim
            dim4d(3) = z_dim
            dim4d(4) = time_dim

            iret = define_mode(ncid,.true.)

            do n=1,numc
               iret = new_nc_variable(ncid,var_names(n),NF90_REAL, &
                                      dim4d,var_ids(n))
               iret = set_attributes(ncid,var_ids(n),       &
                                     units=var_units(n),    &
                                     long_name=var_long(n))
            end do

            dim1d(1) = time_dim
            iret = new_nc_variable(ncid,'totn',NF90_REAL,dim1d,totn_id)
            iret = set_attributes(ncid,totn_id,units='mmol/m**2',    &
                   long_name='total N')

            iret = define_mode(ncid,.false.)
         end if

         do n=1,numc
            iret = store_data(ncid,var_ids(n),XYZT_SHAPE,nlev,array=cc(n,:))
         end do
!KBK         iret = store_data(ncid,phy_id,XYZT_SHAPE,nlev,array=cc(2,:)+P0)
!KBK         iret = store_data(ncid,zoo_id,XYZT_SHAPE,nlev,array=cc(3,:)+Z0)

         iret = store_data(ncid,totn_id,T_SHAPE,1,scalar=totn)
#endif
      case default
         FATAL 'A non valid output format has been chosen'
         stop 'bio_save'
   end select

   if (init_saved_vars) init_saved_vars = .false.

   return
   end subroutine bio_save
!EOC

#endif

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

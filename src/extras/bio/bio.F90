!$Id: bio.F90,v 1.1 2003-04-01 17:01:00 hb Exp $
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
   use meanflow, only:     u,v,h,drag,xP
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
!  Revision 1.1  2003-04-01 17:01:00  hb
!  Added infrastructure for geobiochemical model
!
!
!EOP
!-----------------------------------------------------------------------
!
!  private data members

!  from a namelist
   logical                   :: bio_calc=.true.
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
   namelist /bio_nml/ bio_calc
!-----------------------------------------------------------------------
!BOC

   LEVEL1 'init_bio'

!  Open and read the namelist
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=bio_nml,err=99)
   close(namlst)

   if (bio_calc) then 
!KBK
#if 0
      out_unit=unit
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
   subroutine calc_bio(nlev,dt)
!
! !DESCRIPTION:
!  Remember Hans 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)     :: nlev 
   REALTYPE, intent(in)     :: dt
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!BOC
! !LOCAL VARIABLES:

!EOP
!-----------------------------------------------------------------------

   if (bio_calc) then 

      STDERR 'bio_calc'

   end if

   return
   end subroutine calc_bio 
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

!-----------------------------------------------------------------------
!BOC

   select case (out_fmt)
      case (ASCII)
!HB
#if 0
         if(first) then
            open(out_unit,file='bio.out',status='unknown')
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
#endif
      case (NETCDF)
!HB
#if 0
#ifdef NETCDF_FMT
         if(first) then
            dims(1) = lon_dim
            dims(2) = lat_dim
            dims(3) = z_dim
            dims(4) = time_dim
            miss_val = -999.0
            iret = define_mode(ncid,.true.)
            iret = new_nc_variable(ncid,'x-excur',NF_REAL,4,dims,x_excur_id)
            iret = set_attributes(ncid,x_excur_id,units='m',    &
                   long_name='bio excursion(x)',missing_value=miss_val)
            iret = new_nc_variable(ncid,'y-excur',NF_REAL,4,dims,y_excur_id)
            iret = set_attributes(ncid,y_excur_id,units='m',    &
                   long_name='bio excursion(y)',missing_value=miss_val)
            iret = define_mode(ncid,.false.)
            n = ubound(xx,1)
            first = .false.
         end if
         iret = store_data(ncid,x_excur_id,XYZT_SHAPE,n,array=xx)
         iret = store_data(ncid,y_excur_id,XYZT_SHAPE,n,array=yy)
#endif
#endif
      case default
         FATAL 'A non valid output format has been chosen'
         stop 'save_bio'
   end select   
   return
   end subroutine save_bio 
!EOC

!-----------------------------------------------------------------------

   end module bio

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

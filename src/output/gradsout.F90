!$Id: gradsout.F90,v 1.2 2003-03-10 08:53:05 gotm Exp $
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  GrADS - output in GrADS format
! 
! !INTERFACE:
   MODULE gradsout
!
! !DESCRIPTION: 
!  Writes output in native GrADS format - the use of this format is not
!  recommended - and will most properly not be included in future versions.
!  Instead use NetCDF - which GrADS can read.
!
! !USES:
   use time, only: julianday,secondsofday,calendar_date
   IMPLICIT NONE
!
!  Default all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_grads, do_grads_out, close_grads
!   
! !REVISION HISTORY: 
!  Original author(s): Karsten Bolding & Hans Burchard
!  01Jan2000: Ver. 2.0.0 (kbk): A complete rewrite to F90.
! 
!EOP
! 
! !LOCAL VARIABLES:
   integer, parameter        :: grads_unit=50
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Open the file unit for writing.
!
! !INTERFACE:
   subroutine init_grads(fn,title,nlev)
!
! !DESCRIPTION:
!  Opens the two GrADS files - and write the control file.
!
! !USES:
   use meanflow, only: z
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*), intent(in)        :: fn,title
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!  01Jan2000: Ver. 2.0.0 (kbk): A complete rewrite to F90.
!
!EOP
!
! !LOCAL VARIABLES:
   character(len=3)   	     :: mlist(12)
   character(len=200)        :: varlist
   integer                   :: i,reclen
   integer                   :: yy,mm,dd,hh
   integer, parameter        :: nvar_tot=14
   data mlist /'jan','feb','mar','apr','may','jun', 	&
               'jul','aug','sep','oct','nov','dec'/

!
!-------------------------------------------------------------------------
!BOC
   open(grads_unit,status='unknown',file=trim(fn)//'.ctl')

   hh=mod(secondsofday,3600)
   call calendar_date(julianday,yy,mm,dd) 

   varlist=								&
         '|1:ZETA (cm) '//'|2:U_taus (cm/s) '//'|3:U_taub (cm/s) '//	&
         '|4:KE_t '//'|5:SED_t '//'|6:PHY_t '//				&
         '|7:NIT_t '//'|8:SED(0) '//'|9:PHY(0) '

   LEVEL2 'Experience: ',trim(fn)//".grd"
   open(grads_unit,file=trim(fn)//'.ctl',status='old',form='formatted')
   write(grads_unit,"('dset ',A20)") fn//'.grd'
   write(grads_unit,*) trim(title)
   write(grads_unit,"('undef -999.')")
   write(grads_unit,"('xdef 1 linear 1 1')")
   write(grads_unit,"('ydef 1 linear 1 1')")
   write(grads_unit,"('zdef ',I3,' levels')") nlev
   write(grads_unit,"(5(F15.5))") (z(i),i=1,nlev)
   write(grads_unit,20) 'tdef',nlev,'linear',hh,'Z',dd,mlist(mm),yy,360,'mn'
20 format (a4,i6,a8,i3.2,a1,i2.2,a3,i4,1x,i3,a2)
   write(grads_unit,"('vars',I5,' ')") nvar_tot
   write(grads_unit,"('U   ',I5' 0 VELOCITY-X   (m/s)   ')") nlev
   write(grads_unit,"('V   ',I5' 0 VELOCITY-Y   (m/s)   ')") nlev
   write(grads_unit,"('T   ',I5' 0 TEMPERATURE  (deg)   ')") nlev
   write(grads_unit,"('S   ',I5' 0 SALINITY     (psu)   ')") nlev
   write(grads_unit,"('SED ',I5' 0 SEDIMENT     (kg/m3) ')") nlev
   write(grads_unit,"('RO  ',I5' 0 BUOYANCY     (kg/m3) ')") nlev
   write(grads_unit,"('NU  ',I5' 0 DIFFUSIVITY  (m2/s)  ')") nlev
   write(grads_unit,"('TKE ',I5' 0 TKE          (m2/s2) ')") nlev
   write(grads_unit,"('EPS ',I5' 0 DISSIPATION  (m2/s3) ')") nlev
   write(grads_unit,"('LEN ',I5' 0 LENGTH SCALE (m)     ')") nlev
   write(grads_unit,"('TOBS',I5' 0 TMP OBS      (deg)   ')") nlev
   write(grads_unit,"('SOBS',I5' 0 SAL OBS      (psu)   ')") nlev
   write(grads_unit,"('EPSD',I5' 0 EPS OBS      (m2/s3) ')") nlev
   write(grads_unit,"('PHY ',I5' 0 PHYTO        (C/m3)  ')") nlev
   write(grads_unit,"('endvars')")
   close(grads_unit)

   reclen=nlev
   reclen=4*nlev

   open(grads_unit,file=trim(fn)//'.grd',status='unknown',access='direct',recl=reclen)

   return
   end subroutine init_grads
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Save the model results to file.
!
! !INTERFACE:
   subroutine do_grads_out(nlev)
!
! !DESCRIPTION:
!  Writes calculated fields to file.
!
! !USES:
   use meanflow, only: h,u,v,S,T,buoy
   use turbulence, only: num,nuh,tke,eps,L
   use observations, only: tprof,sprof
#ifdef SEDIMENT
   use sediment, only: C 
#endif
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!  01Jan2000: Ver. 2.0.0 (kbk): A complete rewrite to F90.
!
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: i
   integer, save             :: irec 
   REALTYPE                  :: C_tot,H_tot,KE_tot
   real*4                    :: dum4(nlev)
!
!-------------------------------------------------------------------------
!BOC
   C_tot=0.
   H_tot=0.
   KE_tot=0.
   do i=1,nlev
      KE_tot=KE_tot+h(i)*0.5*(u(i)**2+v(i)**2) 
      H_tot=H_tot+h(i)
   enddo

   dum4 = 0.

   irec=irec+1
   write(grads_unit,rec=irec) (real((u(i-1)+u(i))/2.0) ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real((v(i-1)+v(i))/2.0) ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real(T(i))   ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real(S(i))   ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real(buoy(i)),i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real((num(i-1)+num(i))/2.0) ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real((nuh(i-1)+nuh(i))/2.0) ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real(tke(i))   ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real(eps(i)) ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real(L(i))   ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real(tprof(i))  ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (real(sprof(i))  ,i=1,nlev)
   irec=irec+1
   write(grads_unit,rec=irec) (dum4(i) ,i=1,nlev)

   return
   end subroutine do_grads_out
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Close files used for saving model results.
!
! !INTERFACE:
   subroutine close_grads()
   IMPLICIT NONE
!
! !DESCRIPTION:
!  Closes the GrADS file.
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!  01Jan2000: Ver. 2.0.0 (kbk): A complete rewrite to F90.
!
!EOP
!-------------------------------------------------------------------------
!BOC
   LEVEL2 'Output has been written in GrADS'
   close(grads_unit)

   return
   end subroutine close_grads
!EOC
!-----------------------------------------------------------------------

   end module gradsout

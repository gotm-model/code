!$Id: asciiout.F90,v 1.2 2001-11-18 11:51:16 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: Saving the results in ASCII format.
! 
! !INTERFACE:
   MODULE asciiout
!
! !DESCRIPTION: 
!  This module contains 3 subroutines for writing model output in ASCII format.
!  The authors do not encourage using ASCII for output - instead we recommend
!  NetCDF. We only provide ASCII output because some users might not have 
!  access to NetCDF - yet :-).
!
! !USES:
   IMPLICIT NONE
!   
!  Default all is private.
!  private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_ascii, do_ascii_out, close_ascii
!
! !PUBLIC DATA MEMBERS:
!
! !REVISION HISTORY: 
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: asciiout.F90,v $
!  Revision 1.2  2001-11-18 11:51:16  gotm
!  Now format statements
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
! 
! !LOCAL VARIABLES:
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Open the file unit for writing.
!
! !INTERFACE:
   subroutine init_ascii(fn,title,unit)
   IMPLICIT NONE
!
! !DESCRIPTION:
!  Connects a unit with a file name. 
!
! !INPUT PARAMETERS:
!
! !INPUT/OUTPUT PARAMETERS:
   character(len=*), intent(in)	:: fn,title
   integer, intent(in)		:: unit
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See asciiout module
!
! !LOCAL VARIABLES:
!EOP
!-------------------------------------------------------------------------
!BOC
   open(unit,status='unknown',file=fn)
   write(unit,*) '# ',trim(title)
   return
   end subroutine init_ascii
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Save the model results to file.
!
! !INTERFACE:
   subroutine do_ascii_out(nlev,timestr,variances,unit)
!
! !DESCRIPTION:
!  Writes calculated fields to file.
!
! !USES:
   use meanflow, only: depth0,h,u,v,z,S,T,NN,buoy
   use turbulence, only: num,nuh,tke,eps,L,uu,vv,ww,tt,chi
   use observations, only: tprof,sprof,epsprof
!#ifdef SEDIMENT
!   use sediment, only: ascii_sediment
!#endif
!#ifdef SEDIMENT
!   use seagrass, only: ascii_seagrass
!#endif
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)		:: nlev
   CHARACTER(len=*), intent(in)	:: timestr
   logical, intent(in)		:: variances
   integer, intent(in)		:: unit
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See asciiout module
!
! !LOCAL VARIABLES:
   integer			:: i
   integer, save		:: set=0
   REALTYPE			:: d
   REALTYPE			:: zz(0:nlev)
!
!EOP
!-------------------------------------------------------------------------
!BOC
   set = set + 1
   write(unit,*) timestr,'  Set# ',set
   write(unit,112) 'z','u','v','T','S','buoy'
   d=0.
   do i=nlev,1,-1
      if (abs(u(i)).lt.1.e-32) u(i)=0.
      if (abs(v(i)).lt.1.e-32) v(i)=0.
      d=d+0.5*h(i)
      write(unit,114) z(i),u(i),v(i),T(i),S(i),buoy(i)
   end do

   write(unit,112) 'z','num','nuh','k','eps','L','NN'
   zz(1)=-depth0+h(1)
   do i=2,nlev
      zz(i)=zz(i-1)+h(i)
   end do
   do i=nlev-1,1,-1
      write(unit,115) zz(i),num(i),nuh(i),tke(i),eps(i),L(i),NN(i)
   end do

   if (variances) then
      write(unit,112) 'z','uu','vv','ww','tt','chi'
      do i=nlev-1,1,-1
         write(unit,115) zz(i),uu(i),vv(i),ww(i),tt(i),chi(i)
      end do
   end if 

   write(unit,113) 'z','Tobs','Sobs','epsobs'
   do i=nlev,1,-1
     write(unit,116) z(i),tprof(i),sprof(i),epsprof(i)
   end do

112 format(A9,6(1x,A10))
113 format(A9,3(1x,A10))
114 format(F10.4,2(1x,E10.4E2),2(1x,F10.6),2(1x,E10.4E2))
115 format(F10.4,6(1x,E10.4E2))
116 format(F10.4,2(1x,F10.6),1(1x,E10.4E2))
117 format(A9,4(1x,A10))

!#ifdef SEDIMENT
!    call ascii_sediment(nlev,timestr)
!#endif
!#ifdef SEAGRASS
!    call ascii_seagrass(timestr)
!#endif

   return
   end subroutine do_ascii_out
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Close files used for saving model results.
!
! !INTERFACE:
   subroutine close_ascii(unit)
   IMPLICIT NONE
!
! !DESCRIPTION:
!  Close the open file.
!
! !INPUT PARAMETERS:
   integer, intent(in)		:: unit
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See asciiout module
!
! !LOCAL VARIABLES:
!
!EOP
!-------------------------------------------------------------------------
!BOC
   LEVEL2 'Output has been written in ASCII'
   close(unit)

   return
   end subroutine close_ascii
!EOC

!-----------------------------------------------------------------------

   end module asciiout

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard.

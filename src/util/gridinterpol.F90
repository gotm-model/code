!$Id: gridinterpol.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
! !ROUTINE: gridinterpol - interpolates from observation space to model grid.
! 
! !INTERFACE:
   subroutine gridinterpol(N,cols,obs_z,obs_prof,nlev,model_z,model_prof)
!
! !DESCRIPTION:
! 
!  This is a utility subroutine in which observational data which might 
!  be given on an arbitrary, but ordered grid, are interpolated and 
!  extrapolated to the actual (moving) model grid. 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in) 	:: N,cols
   REALTYPE, intent(in)	:: obs_z(0:N),obs_prof(0:N,cols)
   integer, intent(in) 	:: nlev
   REALTYPE, intent(in)	:: model_z(0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out):: model_prof(0:nlev,cols)
!
! !REVISION HISTORY: 
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: gridinterpol.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer 		:: i,j,ii
   REALTYPE 		:: rat 
! 
!EOP
!-----------------------------------------------------------------------
!BOC
!  Set surface values to uppermost input value 
   do i=nlev,1,-1
      if(model_z(i) .ge. obs_z(N)) then
         do j=1,cols
            model_prof(i,j) = obs_prof(N,j)
         end do
      end if
   end do

!  Set bottom values to lowest input value 
   do i=1,nlev
      if(model_z(i) .le. obs_z(1)) then
         do j=1,cols
            model_prof(i,j) = obs_prof(1,j)
         end do
      end if
   end do

!  Interpolate inner values linearly 
   do i=1,nlev 
      if ((model_z(i) .lt. obs_z(N)) .and. (model_z(i) .gt. obs_z(1))) then 
         ii=0 
224      ii=ii+1 
         if (obs_z(ii) .le. model_z(i)) goto 224 
         rat=(model_z(i)-obs_z(ii-1))/(obs_z(ii)-obs_z(ii-1)) 
         do j=1,cols
            model_prof(i,j)=(1-rat)*obs_prof(ii-1,j)+rat*obs_prof(ii,j) 
         end do
      end if 
   end do 

#ifdef OLD_METHOD
   i=Nmx+1 
222 i=i-1 
   if ((z(i).ge.zz(md)).and.(i.gt.0)) then 
      S(i)=ss(md) 
      goto 222 
   end if 

!  Set bottom values to lowest input value 
   i=0 
223 i=i+1 
   if (z(i).le.zz(1)) then 
      S(i)=ss(1) 
      goto 223 
   end if 

!  Interpolate inner values linearly 
   do i=1,Nmx 
      if ((z(i).lt.zz(Nmx)).and.(z(i).gt.zz(1))) then 
         ii=0 
224      ii=ii+1 
         if (zz(ii).le.z(i)) goto 224 
         rat=(z(i)-zz(ii-1))/(zz(ii)-zz(ii-1)) 
         S(i)=(1-rat)*ss(ii-1)+rat*ss(ii) 
      end if 
   end do 
#endif

   return  
   end 
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard & Karsten Bolding.

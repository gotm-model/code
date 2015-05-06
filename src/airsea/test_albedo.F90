#include"cppdefs.h"
   program test_albedo
!
! !DESCRIPTION:
!  program to test the albedo_water routine
!
!  To build:
!  make test_albedo
!  To execute:
!  ./test_albedo
!  To plot:
!  python $GOTM_BASE/scr/plot_albedo.py
!
! !USES:
   implicit none
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding
!
! !LOCAL VARIABLES
   integer  :: i
   REALTYPE :: albedo_water
   REALTYPE :: p1,p2,p3,p4
   REALTYPE :: c1,c2,c3,c4
!EOP
!-----------------------------------------------------------------------
!BOC
   write(*,*) 'Testing for zenith angle ...'
   write(*,*) 'Adjust 3rd parameter in c? to calculate for different year days'
   write(*,*) 
   write(100,'(a5,5a8)') 'angle','Payne','C1','C2','C3','C4'
   do i=0,90,5
      p1 = albedo_water(1,i*_ONE_)
      c1 = albedo_water(2,i*_ONE_,1)
      c2 = albedo_water(2,i*_ONE_,91)
      c3 = albedo_water(2,i*_ONE_,182)
      c4 = albedo_water(2,i*_ONE_,274)
      write(100,'(I5,5F8.4)') i,p1,c1,c2,c3,c4
   end do

   write(*,*) 'Testing for annual variation ...'
   write(*,*) 'Adjust 2nd parameter in p? and c? to calculate for zenith solar angels'
   write(*,*) 
   write(101,'(a4,8a8)') 'day','P1','C1','P2','C2','P3','C3','P4','C4'
   do i=1,365,5
      p1 = albedo_water(1,0.*_ONE_)
      c1 = albedo_water(2,0.*_ONE_,i)
      p2 = albedo_water(1,30.*_ONE_)
      c2 = albedo_water(2,30.*_ONE_,i)
      p3 = albedo_water(1,60.*_ONE_)
      c3 = albedo_water(2,60.*_ONE_,i)
      p4 = albedo_water(1,90.*_ONE_)
      c4 = albedo_water(2,90.*_ONE_,i)
      write(101,'(I4,8F8.4)') i,p1,c1,p2,c2,p3,c3,p4,c4
   end do

   end program test_albedo
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2015 - Karsten Bolding                                 !
!-----------------------------------------------------------------------


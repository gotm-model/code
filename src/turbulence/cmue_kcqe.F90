!$Id: cmue_kcqe.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Kantha and Clayson [1995] stability functions.
! 
! !INTERFACE:
   subroutine cmue_kcqe(nlev)
!
! !DESCRIPTION:
!  This subroutine computes Kantha and Clayson [19995] stability functions
!  including smoothing for convective conditions as discussed by Burchard
!  and Petersen [1999].
!
! !USES:
   use turbulence, only: a1,a2,b1,b2,c2,c3,an
   use turbulence, only: qesmooth,qeghmax,qeghmin,qeghcrit
   use turbulence, only: cmue1,cmue2
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: nlev
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: cmue_kcqe.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE 		:: gh,sm,sh
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      gh=-0.5*an(i)           ! Transformation to MY notation 
      qeghmax=1/(a2*(b1+12*a1+3*b2*(1-c3)))
      if (qesmooth) then
         if (gh.gt.qeghcrit) gh=gh-(gh-qeghcrit)**2/(gh+qeghmax-2*qeghcrit)
      else 
         if (gh.gt.qeghmax) gh=qeghmax  
      end if     
      if (gh.lt.qeghmin)  gh = qeghmin  
      sh=a2*(1-6*a1/b1)/(1-3*a2*gh*(6*a1+b2*(1-c3)))
      sm=(b1**(-1./3.)+9*a1*(2*a1+a2*(1-c2))*sh*gh)/(1-9*a1*a2*gh)

      cmue1(i)=sqrt(2.)*sm    ! Retransformation to GOTM notation 
      cmue2(i)=sqrt(2.)*sh    ! Retransformation to GOTM notation
   end do

   return
   end subroutine cmue_kcqe
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard & Karsten Bolding.

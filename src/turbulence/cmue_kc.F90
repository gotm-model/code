!$Id: cmue_kc.F90,v 1.2 2002-02-08 08:59:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Kantha and Clayson [1994] non-equilibrium stability functions.
! 
! !INTERFACE:
   subroutine cmue_kc(nlev)
!
! !DESCRIPTION:
!  This subroutine computes Kantha and Clayson [1994] stability functions.
!
! !USES:
   use turbulence, only: a1,a2,b2,c1,c2,c3
   use turbulence, only: as,an
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
!  Original author(s): Hans Burchard, Karsten Bolding 
!                      & Manuel Ruiz Villarreal
!
!  $Log: cmue_kc.F90,v $
!  Revision 1.2  2002-02-08 08:59:58  gotm
!  Added Manuel as author and copyright holder
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!
! !LOCAL VARIABLES:
   integer		:: i
   REALTYPE		:: gm,gh,sm,sh
   REALTYPE		:: c11,c12,c13,c21,c22,c23
! 
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev-1
      gm=0.5*as(i)             !Transformation to MY notation
      gh=-0.5*an(i)            !Transformation to MY notation
      if (gh.gt.0.029) gh=0.029
#ifdef NewConstraint
      bb1=6*a1**2
      bb2=-3.*a2*(3*a1+b2*(1-c3)+4*a1)
      bb3=18*a1**2*a2*(3*a2*(1-c2)-b2*(1-c3))
      bb4=27*a1*a2**2*(b2*(1-c3)+4*a1)
      gmlim=(1.+bb2*gh+bb4*gh**2)/(bb1+bb3*gh) 
      if (gm.gt.gmlim) gm=gmlim
#else
      if (gm.gt.0.825-25.0*gh) gm=0.825-25.0*gh
#endif 
      c11=6*a1*a2*gm
      c12=1-3*a2*b2*(1-c3)*gh-12*a1*a2*gh
      c13=a2
      c21=1+6*a1*a1*gm-9*a1*a2*gh
      c22=-12*a1*a1*gh-9*a1*a2*(1-c2)*gh
      c23=a1*(1-3*c1)
      sm=(c12*c23-c22*c13)/(c12*c21-c22*c11)
      sh=(c21*c13-c11*c23)/(c12*c21-c22*c11)
      cmue1(i)=sqrt(2.)*sm     !Retransformation to GOTM notation
      cmue2(i)=sqrt(2.)*sh     !Retransformation to GOTM notation
   end do

   return
   end subroutine cmue_kc
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard, Karsten Bolding 
!                     & Manuel Ruiz Villarreal.
!-----------------------------------------------------------------------

!$Id: cmue_gpqe.F90,v 1.1 2003-03-10 09:00:35 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE:  \cite{Galperinetal88} quasi-eq.\ stability func. 
! 
! !INTERFACE:
   subroutine cmue_gpqe(nlev)
!
! !DESCRIPTION:
!  This subroutine computes the quasi-equilibrium stability functions suggested by 
!  \cite{Galperinetal88} including smoothing for convective conditions 
!  as discussed by \cite{BurchardPetersen99}. This is the quasi-equilibrium version
!  of the stability functions originally suggested by \cite{MellorYamada82} (see
!  section \ref{cmue_my}), see
!  also \cite{Deleersnijder94} for a performance comparison.
!
! !USES:
   use turbulence, only: an
   use turbulence, only: cmue1,cmue2
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: cmue_gpqe.F90,v $
!  Revision 1.1  2003-03-10 09:00:35  gotm
!  Part of new generic turbulence model
!
!  Revision 1.2  2002/02/08 08:59:58  gotm
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
!
! !LOCAL VARIABLES:
   logical                   :: qesmooth
   integer		     :: i
   REALTYPE 		     :: gh,sm,sh
   REALTYPE                  :: a1,a2,b1,b2,c2,c3
   REALTYPE                  :: qeghmax,qeghmin,qeghcrit
!   
!-----------------------------------------------------------------------
!BOC
! do smoothing
   qesmooth=     .true.

! define the model constants
   a1=           0.92
   a2=           0.74
   b1=           16.6
   b2=           10.1
   c2=           0.0
   c3=           0.0
 
! define the smoothing parameters   
   qeghmax=      0.0233
   qeghmin=      -0.28
   qeghcrit=     0.02

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
   end subroutine cmue_gpqe
!EOC

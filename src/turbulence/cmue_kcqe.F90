!$Id: cmue_kcqe.F90,v 1.4 2003-03-28 09:20:35 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: \cite{KanthaClayson94} quasi-eq.\ stability func.
! 
! !INTERFACE: 
   subroutine cmue_kcqe(nlev)
!
! !DESCRIPTION:
!  This subroutine computes the stability functions suggested by 
!  \cite{KanthaClayson94}, which are based on the quasi-equilibrium
!  assumption, including smoothing for convective conditions as discussed 
!  by \cite{BurchardPetersen99}. This is the quasi-equilibrium version of the
!  stability functions calculated in section \ref{cmue_kc}.
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
!  $Log: cmue_kcqe.F90,v $
!  Revision 1.4  2003-03-28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.3  2003/03/10 09:02:03  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
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
   integer                   :: i
   REALTYPE                  :: gh,sm,sh
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
   c2=           0.7
   c3=           0.2
 
! define the smoothing parameters   
   qeghmax=      0.0233
   qeghmin=      -0.2809
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
   end subroutine cmue_kcqe
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

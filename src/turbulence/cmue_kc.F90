!$Id: cmue_kc.F90,v 1.5 2003-03-28 09:20:35 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: \cite{KanthaClayson94} non-eq.\ stability func. 
! 
! !INTERFACE:
   subroutine cmue_kc(nlev)
!
! !DESCRIPTION:
!  \label{cmue_kc}
!  This subroutine computes a generalised non-equilibrium version
!  of the stability functions suggested by \cite{KanthaClayson94},
!  which are obtained from the original functions of \cite{MellorYamada82} (see
!  section \ref{cmue_my})
!  by setting $C_2$ and $C_3$ to non-zero values, see \cite{BurchardBolding2001}.
!  With this, the stability functions are of the following form:
! \begin{equation}\label{StabKC1}
! \begin{array}{l}
! \displaystyle
! c_{\mu}=\frac{0.1682+0.03269\alpha_N}{A},
! \\
! \\
! \displaystyle
! c'_{\mu}=\frac{0.1783+0.01586\alpha_N+0.003173\alpha_M}{A},
! \end{array}
! \end{equation}
! with
! $A=1+0.4679\alpha_N+0.07372\alpha_M
!             +0.01761\alpha_N\alpha_M+0.03371\alpha_N^2$.
! 
! !USES:
   use turbulence, only: as,an
   use turbulence, only: cmue1,cmue2,new_constr
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)       :: nlev
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: cmue_kc.F90,v $
!  Revision 1.5  2003-03-28 09:20:35  kbk
!  added new copyright to files
!
!  Revision 1.4  2003/03/28 08:37:26  kbk
!  removed tabs
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
   integer                   :: i
   REALTYPE                  :: gm,gh,sm,sh
   REALTYPE                  :: c11,c12,c13,c21,c22,c23
   REALTYPE                  :: a1,a2,b1,b2,c1,c2,c3
   REALTYPE                  :: bb1,bb2,bb3,bb4,gmlim
!
!-----------------------------------------------------------------------
!BOC
   a1=           0.92
   a2=           0.74
   b1=           16.6
   b2=           10.1
   c2=           0.7
   c3=           0.2
 
   c1=(1.-b1**(-1./3.)/a1-6*a1/b1)/3. !See Kantha & Clayson 1994, eq. (23)

   do i=1,nlev-1
      gm=0.5*as(i)             !Transformation to MY notation
      gh=-0.5*an(i)            !Transformation to MY notation
      if (gh.gt.0.029) gh=0.029
      if (new_constr) then
         bb1=6*a1**2
         bb2=-3.*a2*(3*a1+b2*(1-c3)+4*a1)
         bb3=18*a1**2*a2*(3*a2*(1-c2)-b2*(1-c3))
         bb4=27*a1*a2**2*(b2*(1-c3)+4*a1)
         gmlim=(1.+bb2*gh+bb4*gh**2)/(bb1+bb3*gh) 
         if (gm.gt.gmlim) gm=gmlim
      else
         if (gm.gt.0.825-25.0*gh) gm=0.825-25.0*gh
      endif
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
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

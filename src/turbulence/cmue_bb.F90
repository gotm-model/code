!$Id: cmue_bb.F90,v 1.3 2003-03-28 08:37:26 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: \cite{BurchardBaumert95} non-eq.\ stability func.
! 
! !INTERFACE:
   subroutine cmue_bb(nlev)
!
! !DESCRIPTION:
!  \label{cmue_bb}
!  This subroutine computes non-equilibrium version of the stability functions
!  according to \cite{Rodi80}, \cite{Hossain80} and \cite{BurchardBaumert95}. 
!
! In contrast to the models of \cite{KanthaClayson94}
! and \cite{Canutoetal2001a}, the model of
! \cite{Rodi80} and \cite{Hossain80}
! in the version of \cite{BurchardBaumert95}
! results in 
! stability functions $c_{\mu}$ and $c'_{\mu}$  which not only
! depend on  $\alpha_M$ and $\alpha_N$, but additionally depend on the
! non-dimensional term $(P+B)/\eps-1$, i.e.\ on the degree of deviation from
! local turbulence equilibrium.
! Traditionally, these 
! stability functions
! have been solved in numerical models by using the value for
! $(P+B)/\eps-1$ on an old time level.
! In this form, the 
! stability functions have been presented first by
! \cite{Rodi80} and \cite{Hossain80}.
! 
! However, because of $(P+B)/\eps=c_{\mu}\alpha_M-
! c'_{\mu}\alpha_N$, these equations for $c_{\mu}$ and $c'_{\mu}$ can be
! expressed as implicit functions of $\alpha_M$ and $\alpha_N$.
! The evaluation procedure which has been suggested by
! \cite{BurchardBaumert95} is first solving for
! $(P+B)/\eps-1$ by means of a Newton iteration
! and then inserting that value into the formulations for
! $c_{\mu}$ and $c'_{\mu}$. This procedure is discussed in detail in
! \cite{BurchardBolding2001}.
! 
!
! !USES:
   use turbulence, only: an,as 
   use turbulence, only: cmue1,cmue2,cm0
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: cmue_bb.F90,v $
!  Revision 1.3  2003-03-28 08:37:26  kbk
!  removed tabs
!
!  Revision 1.2  2003/03/10 09:02:03  gotm
!  Added new Generic Turbulence Model + improved documentation and cleaned up code
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
! !LOCAL VARIABLES:
   integer                   :: i,j
   REALTYPE                  :: k(1:5),kk(1:4)
   REALTYPE                  :: X,V,D,A,E,H,ww
   REALTYPE, parameter       :: c1=1.8
   REALTYPE, parameter       :: c2=0.6
   REALTYPE, parameter       :: c3=0.6
   REALTYPE, parameter       :: c1t=3.0
   REALTYPE, parameter       :: c2t=0.33
   REALTYPE, parameter       :: c3t=0.33
   REALTYPE, parameter       :: ct=1.6
!   REALTYPE, parameter      :: cm0=0.59
   REALTYPE                  :: a1,a2,a3,a4,a5,a6
   REALTYPE                  :: a7,a8,a9,a10,a11
! 
!-----------------------------------------------------------------------
!BOC
   a1=c1-1.0
   a2=c1-1.0
   a3=c1t-0.5
   a4=c1t-0.5
   a5=c2
   a6=c2
   a7=1-c3
   a8=1-c3
   a9=1-c2t
   a10=1-c3t
   a11=1.5-c3

   as = 1./cm0**6 * as
   an = 1./cm0**6 * an

   do i=1,nlev-1
      k(1)= a1+a2+2*(a3+a4+a10*ct*an(i))

      k(2)=4.*a3*(a4+a10*ct*an(i))                                   &
           +2.*(a1+a2)*(a3+a4+a10*ct*an(i))+a1*a2                    &
           +8./3.*a11*an(i)+2*a7*an(i)-2./3.*(1.-a5)*a6*as(i)

      k(3)=16./3.*a11*(0.5*a1+a3)*an(i)                              &
           +4.*a7*(0.5*a2+a4+a10*ct*an(i))*an(i)                     &
           -4./3.*a6*(1.-a5)*(a3+a4+a10*ct*an(i))*as(i)              &
           +4.*a3*(a1+a2)*(a4+a10*ct*an(i))                          &
           +2.*a1*a2*(a3+a4+a10*ct*an(i))                            &
           -8./3.*(c1-1.)*(0.25*as(i)*(1.-a5)-0.5*an(i))

      k(4)=16./3.*a1*a3*a11*an(i)+4.*a2*a7*(a4+a10*ct*an(i))*an(i)   &
           -8./3.*a3*a6*(1.-a5)*(a4+a10*ct*an(i))*as(i)              &
           +16./3.*a7*a11*an(i)*an(i)                                &
           +8./3.*a6*a7*a9*as(i)*an(i)+4.*a1*a2*a3*(a4+a10*ct*an(i)) &
           -8./3.*(c1-1.)*(0.5*(1.-a5)*(a3+a4+a10*ct*an(i))*as(i)    &
           -(0.5*a1+a3)*an(i))

      k(5)=-8./3.*(c1-1.)*(((1.-a5)*a3*(a4+a10*ct*an(i))             &
           -a7*a9*an(i))*as(i)-a7*an(i)*an(i)-a1*a3*an(i))

      kk(1) = 4*k(1)
      kk(2) = 3*k(2)
      kk(3) = 2*k(3)
      kk(4) =   k(4)

      X=0.25*as(i)
      if (an(i).lt.0.) X=X-2.0*an(i)

111   V=1.0
      do j=1,5
         V=V*X+k(j)
      end do

      D=5.0
      do j=1,4
         D=D*X+kk(j)
      end do

      X=X-V/D

      if (abs(V/D).gt.1e-4) goto 111

      X=X-1.

      A=1./(1.+1.072*an(i)/(3.0+0.5*X))
      D=1.+0.4*an(i)/(1.8+X)/(3.0+0.5*X)
      E=1.+1.2*A*an(i)/(1.8+X)/(3.0+0.5*X)
      H=1.-0.67*A*an(i)/(3.0+0.5*X)/(3.0+0.5*X)
      ww=2./3.*0.8/(E*(1.8+X)-0.16/(1.8+X)*H/D*as(i))

      cmue1(i) = 0.4/(1.8+X)*H/D*ww
      cmue2(i) = A/(3.0+0.5*X)*ww

      cmue1(i) = 1./cm0**3 * cmue1(i)
      cmue2(i) = 1./cm0**3 * cmue2(i)
   end do

   return
   end subroutine cmue_bb
!EOC

!$Id: eqstate.F90,v 1.5 2003-03-28 09:20:36 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: eqstate --- the equation of state \label{sec:eqstate}
!
! !INTERFACE:
   MODULE eqstate
!
! !DESCRIPTION:
!  Calculates the equation of state for sea water based on temperature,
!  salinity and pressure. At present, four different methods are implemented.
!  \begin{enumerate}
!     \item the full UNESCO equation --- including pressure effects
!     \item the full UNESCO equation --- without pressure effects
!     \item the linearised UNESCO equation
!     \item a general linear form of the equation of state
!  \end{enumerate}
!
! !USES:
   IMPLICIT NONE

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_eqstate,eqstate1,unesco
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard \& Karsten Bolding
!
!  $Log: eqstate.F90,v $
!  Revision 1.5  2003-03-28 09:20:36  kbk
!  added new copyright to files
!
!  Revision 1.4  2003/03/28 08:06:33  kbk
!  removed tabs
!
!  Revision 1.3  2003/03/10 08:54:16  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.2  2001/11/27 19:44:32  gotm
!  Fixed an initialisation bug
!
!  Revision 1.1.1.1  2001/02/12 15:55:58  gotm
!  initial import into CVS
!
!EOP
!
!  private data memebers
   integer                   :: eq_state_method
   REALTYPE                  :: T0,S0,p0,dtr0,dsr0
   logical                   :: use_density=.false.
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read the namelist {\tt eqstate}
!
! !INTERFACE:
   subroutine init_eqstate(namlst)
!
! !DESCRIPTION:
!  Here, the namelist {\tt eqstate} in the namelist file {\tt gotmrun.inp}
!  is read. 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, optional, intent(in)       :: namlst
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard \& Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   namelist /eqstate/ eq_state_method,T0,S0,p0,dtr0,dsr0
!
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_eqstate'
   if(present(namlst)) then
      read(namlst,nml=eqstate,err=80)
   end if
   return
   80 FATAL 'I could not read "eqstate" namelist'
   stop 'init_eqstate'
   end subroutine init_eqstate
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Select an equation of state
!
! !INTERFACE:
   REALTYPE function eqstate1(S,T,p,g,rho_0) 
!
! !DESCRIPTION:
!  Calculates the buoyancy according to the selected method. For values of
!  {\tt eq\_state\_method} ranging from 1 to 4, one of the following methods 
!  will be used.
!
!   \begin{enumerate}
!     \item the full UNESCO equation of state for sea water 
!           including the thermobaric effect, see \cite{FofonoffMillard83}  
!     \item the UNESCO equation of state for sea water 
!           related to surface pressure, see \cite{FofonoffMillard83}
!     \item a linearised UNESCO equation of state, 
!           see \cite{FofonoffMillard83}. The parameters {\tt T0}, 
!           {\tt S0} and {\tt p0} have to be specified in the namelist.
!     \item a linear equation of state with prescribed {\tt rho0}, {\tt T0},
!           {\tt S0}, {\tt dtrho}, {\tt dsrho}
!   \end{enumerate}

!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in)                 :: S,T,p
   REALTYPE,optional,intent(in)        :: g,rho_0
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard \& Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: x
   REALTYPE, save            :: rh0,dtr,dsr
   REALTYPE                  :: dTT,dSS
   logical                   :: press
   logical, save             :: first=.true.
!
!-----------------------------------------------------------------------
!BOC
   select case (eq_state_method)
      case (1)
         press=.true.
         x=unesco(S,T,p,press)
      case (2)
         press=.false.
         x=unesco(S,T,p,press)
      case (3)
         if (first) then
            press=.true.   ! This allows for choosing potentials other than p=0
            dTT=0.001
            dSS=0.001
            rh0= unesco(S0,T0,p0,press)
            dtr=(unesco(S0,T0+0.5*dTT,p0,press)        &
                -unesco(S0,T0-0.5*dTT,p0,press))/dTT
            dsr=(unesco(S0+0.5*dSS,T0,p0,press)        &
                -unesco(S0-0.5*dSS,T0,p0,press))/dSS
            first=.false.
         end if
         x=rh0+dtr*(T-T0)+dsr*(S-S0)
      case (4)
         x=rho_0+dtr0*(T-T0)+dsr0*(S-S0)
      case default
   end select 
   if (use_density) then
      eqstate1=x
   else
      eqstate1=-g*(x-rho_0)/rho_0
   end if
   return
   end function eqstate1
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: The UNESCO equation of state
!
! !INTERFACE:
   REALTYPE function unesco(S,T,p,UNPress) 
!
! !DESCRIPTION:
!  Calculates the density according to the UNESCO equation of state 
!  (see \cite{FofonoffMillard83}) for sea water. The thermobaric effect 
!  can be switched on ({\tt UNPress=.true.}) or off ({\tt UNPress=.false.}). 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE, intent(in)                :: S,T,p
   LOGICAL, intent(in)                 :: UNPress
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard \& Karsten Bolding
!
!EOP
!
! !LOCAL VARIABLES:
   REALTYPE                  :: x,K  
   REALTYPE                  :: T2,T3,T4,T5,S15,S2,S3,p2 
!
!-----------------------------------------------------------------------
!BOC
   T2 = T*T
   T3 = T*T2
   T4 = T2*T2
   T5 = T*T4
   S15= S**1.5
   S2 = S*S
   S3 = S*S2

   x=999.842594+6.793952e-02*T-9.09529e-03*T2+1.001685e-04*T3
   x=x-1.120083e-06*T4+6.536332e-09*T5
   x=x+S*(0.824493-4.0899e-03*T+7.6438e-05*T2-8.2467e-07*T3)
   x=x+S*5.3875e-09*T4
   x=x+sqrt(S3)*(-5.72466e-03+1.0227e-04*T-1.6546e-06*T2)
   x=x+4.8314e-04*S2

     if ((UNPress).and.(p.gt.0)) then
     p2=p*p
     K= 19652.21                                         &
       +148.4206     *T          -2.327105    *T2        &
       +  1.360477E-2*T3         -5.155288E-5 *T4        &
       +  3.239908      *p       +1.43713E-3  *T *p      &
       +  1.16092E-4 *T2*p       -5.77905E-7  *T3*p      &
       +  8.50935E-5    *p2      -6.12293E-6  *T *p2     &
       +  5.2787E-8  *T2*p2                              &
       + 54.6746             *S  -0.603459    *T    *S   &
       +  1.09987E-2 *T2     *S  -6.1670E-5   *T3   *S   &
       +  7.944E-2           *S15+1.6483E-2   *T    *S15 &
       -  5.3009E-4  *T2     *S15+2.2838E-3      *p *S   &
       -  1.0981E-5  *T *p   *S  -1.6078E-6   *T2*p *S   &
       +  1.91075E-4    *p   *S15-9.9348E-7      *p2*S   &
       +  2.0816E-8  *T *p2*S    +9.1697E-10  *T2*p2*S
     x=x/(1.-p/K) 
   end if

   unesco=x
   return
   end function unesco
!EOC

!-----------------------------------------------------------------------

   end module eqstate

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

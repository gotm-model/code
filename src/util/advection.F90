!$Id: advection.F90,v 1.3 2003-03-28 09:20:36 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Vertical advection. 
! 
! !INTERFACE:
   subroutine advection(N,dt,h,Y,w,Qadv,Method,surf_flux,bott_flux)
!
! !DESCRIPTION:
!  This subroutine computes the vertical advection term as e.g. needed for
!  sinking of sediments. Three methods are included:
!
!  \begin{itemize}
!  \item 1. first order upstream, which is monotone but diffusive
!  \item 2. QUICKEST (Leonard et al. [1995]), which is non-monotone, but with
!     smalll numerical diffusion only.
!  \item 3. FCT (Flux Corrected Transport, Zalesak [1979]) combining 1. and 2., 
!     i.e. the scheme is monotone and with only small numerical diffusion. 
!  \end{itemize}
!
!  Note, that 2. and 3. are only consistent for 
!  equidistant vertical discretization. 
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)       :: N
   REALTYPE, intent(in)      :: dt
   REALTYPE, intent(in)      :: h(0:N),Y(0:N)
   REALTYPE, intent(in)      :: w(0:N)
   integer, intent(in)       :: Method
   LOGICAL,  intent(in)      :: surf_flux,bott_flux
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out):: Qadv(0:N)
!
! !REVISION HISTORY: 
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: advection.F90,v $
!  Revision 1.3  2003-03-28 09:20:36  kbk
!  added new copyright to files
!
!  Revision 1.2  2003/03/28 09:10:39  kbk
!  removed tabs
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
! !LOCAL VARIABLES:
   REALTYPE                  :: Cint(0:N),Clow(0:N)
   REALTYPE                  :: Chig(0:N),Ci(0:N)   
   REALTYPE                  :: rp(0:N),rm(0:N) 
   REALTYPE                  :: Cd,Cu,Cuu,ww,CMin,CMax,Cl
   REALTYPE                  :: UppFlux,LowFlux,fac     
   integer                   :: i 
! 
!EOP
!-----------------------------------------------------------------------
!BOC
!  First order upstream  
   if ((Method.eq.1).or.(Method.eq.3)) then
      do i=1,N-1 
         if (w(i).gt.0.0) then
            Cint(i)=w(i)*Y(i) 
         else
            Cint(i)=w(i)*Y(i+1)
         end if
         Clow(i)=Cint(i) 
      end do
      if (bott_flux) then 
         Cint(0)=Cint(1)
         Clow(0)=Clow(1)
      else
         Cint(0)=0.0 
         Clow(0)=0.0 
      end if 
      if (surf_flux) then 
         Cint(N)=Cint(N-1)
         Clow(N)=Clow(N-1)
      else
         Cint(N)=0.0 
         Clow(N)=0.0 
      end if 
   end if
!  Third order QICKEST  
   if ((Method.eq.2).or.(Method.eq.3)) then
      do i=1,N-1 
         if (w(i).gt.0.0) then
            Cint(1)=w(1)*Y(1)
            if (i.gt.1) then
               Cd  = Y(i+1)  
               Cu  = Y(i  )
               Cuu = Y(i-1)
               ww  = abs(w(i))*dt/(0.5*(h(i)+h(i+1)))
               Cint(i) = w(i)*(0.5*(Cd+Cu) - 0.5*ww*(Cd-Cu)          &
                        - (1-ww*ww)/6.*(Cd-2*Cu+Cuu))   
            end if
         else
            Cint(N-1)=w(N-1)*Y(N)
            if (i.lt.N-1) then
               Cd  = Y(i  )  
               Cu  = Y(i+1)
               Cuu = Y(i+2)
               ww  = abs(w(i))*dt/(0.5*(h(i)+h(i+1)))
               Cint(i) = w(i)*(0.5*(Cd+Cu) - 0.5*ww*(Cd-Cu)          &
                        - (1-ww*ww)/6.*(Cd-2*Cu+Cuu))   
            end if
         end if
         Chig(i)=Cint(i) 
      end do
      if (bott_flux) then 
         Cint(0)=Cint(1)
         Chig(0)=Chig(1)
      else
         Cint(0)=0.0 
         Chig(0)=0.0 
      end if 
      if (surf_flux) then 
         Cint(N)=Cint(N-1)
         Chig(N)=Chig(N-1)
      else
         Cint(N)=0.0 
         Chig(N)=0.0 
      end if 
   end if
!
!  Flux corrected transport 
!  Result after low order flux  
!
   if (Method.eq.3) then
      if (surf_flux.or.bott_flux) then
         STDERR 'Flux corrected transport in combination with '
         STDERR 'surface fluxes is not coded yet, change to Method 1 or 2. '
         STDERR 'Program aborted... '
         stop
      end if
      do i=1,N
         Ci(i)=Y(i)-dt*(Clow(i)-Clow(i-1))/h(i)  
      end do
      do i=1,N
!        Determine Min and Max of triple            
         CMax=Y(i) 
         if ((i.gt.1  ).and.(Y(i-1).gt.CMax)) CMax=Y(i-1)  
         if ((i.lt.N).and.(Y(i+1).gt.CMax)) CMax=Y(i+1)  
         if                 (Ci(i  ).gt.CMax)  CMax=Ci(i  )  
         if ((i.gt.1  ).and.(Ci(i-1).gt.CMax)) CMax=Ci(i-1)  
         if ((i.lt.N).and.(Ci(i+1).gt.CMax)) CMax=Ci(i+1)  
         CMin=Y(i) 
         if ((i.gt.1  ).and.(Y(i-1).lt.CMin)) CMin=Y(i-1)  
         if ((i.lt.N).and.(Y(i+1).lt.CMin)) CMin=Y(i+1)  
         if                 (Ci(i  ).lt.CMax)  CMin=Ci(i  )  
         if ((i.gt.1  ).and.(Ci(i-1).lt.CMax)) CMin=Ci(i-1)  
         if ((i.lt.N).and.(Ci(i+1).lt.CMax)) CMin=Ci(i+1)  
!        Determine possible Max (Cu) and Min (Cl)  
         UppFlux = Chig(i)-Clow(i)
         if (UppFlux.gt.0.) UppFlux = 0.0 
         LowFlux = Chig(i-1)-Clow(i-1)
         if (LowFlux.lt.0.) LowFlux = 0.0 
         Cu = Ci(i) - dt * (UppFlux-LowFlux) / h(i)   
         UppFlux = Chig(i)-Clow(i)
         if (UppFlux.lt.0.) UppFlux = 0.0 
         LowFlux = Chig(i-1)-Clow(i-1)
         if (LowFlux.gt.0.) LowFlux = 0.0 
         Cl = Ci(i) - dt * (UppFlux-LowFlux) / h(i)  
!        Determine upper and lower limiters rm nd rp 
         if (Cu.eq.Ci(i)) then
            rp(i)=0.0 
         else
            rp(i)=(Cmax-Ci(i))/(Cu-Ci(i))
            if (rp(i).gt.1.) rp(i) = 1.0
         end if
         if (Cl.eq.Ci(i)) then
            rm(i)=0.0 
         else
            rm(i)=(Ci(i)-CMin)/(Ci(i)-Cl)
            if (rm(i).gt.1.) rm(i) = 1.0
         end if
      end do
!     Determine high order to low order flux used for 
!     calculating the new concentration.  
      do i=1,N-1  
         if (Chig(i)-Clow(i).ge.0.0) then
            fac=rm(i)
            if (fac.gt.rp(i+1)) fac=rp(i+1)
            else
            fac=rm(i+1)
            if (fac.gt.rp(i)) fac=rp(i)
         end if
         Cint(i)=(1.-fac)*Clow(i)+fac*Chig(i)  
      end do
      Cint(0)=Cint(1)
      Cint(N)=Cint(N-1)
   end if

   do i=1,N
      Qadv(i)=-(Cint(i)-Cint(i-1))/h(i) 
   end do

   return
   end subroutine advection
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

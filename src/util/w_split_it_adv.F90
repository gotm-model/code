!$Id: w_split_it_adv.F90,v 1.5.2.2 2005-09-20 06:59:15 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: A collection of vertical advection routines \label{sec:advection}
!
! !INTERFACE:
   subroutine w_split_it_adv_1d(kmax,dt,h,ho,f,ww,method,surf_flux,bott_flux,flag)
!
! !DESCRIPTION:
!
! This routine for vertical advection has been extracted from the GETM 
! model (see \cite{BurchardBolding2002}). 
! It provides five different numerical schemes for vertical advection.
!
! \begin{itemize}
!  \item first-order upstream ({\tt method=1})
!  \item third-order upstream-biased polynomial scheme ({\tt method=3})
!  \item TVD-scheme with Superbee limiter ({\tt method=4})
!  \item TVD-scheme with MUSCL limiter ({\tt method=5})
!  \item TVD-scheme with ULTIMATE QUICKEST limiter ({\tt method=6})
! \end{itemize}
!
! An overview over these schemes may be found in the paper by 
! \cite{Pietrzak98}. Methods 1, 4, 5, 6 should be positive definite.
! The actual method
! has to be choosen in the namelist {\tt w\_advspec} in {\tt obs.inp}.
! This vertical advection will be used if a prescribed vertical
! velocity is given and for the case of vertically moving
! adaptive grids, see \sect{sec:adaptivegrid} and \cite{BurchardBeckers2003}.
! The non-conservative or the conservative form of the advection term
! will be discretised for {\tt flag=1} or {\tt flag=2}, respectively.
!
! If surface or bottom vertical velocities are not zero, then
! {\tt surf\_flux=.true.} or {\tt bott\_flux=.true.}, respectively,
! should be chosen. Then, zero-gradient boundary conditions will be used.
! If during a certain time step the maximum Courant number is larger 
! than zero, an iteration will be carried out which guarantees that the
! split step Courant numbers are just smaller than zero.
! For details, see the GETM report by \cite{BurchardBolding2002}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: kmax,flag
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: h(0:kmax),ho(0:kmax)
   REALTYPE, intent(in)                :: ww(0:kmax)
   integer,  intent(in)                :: method
   LOGICAL,  intent(in)                :: surf_flux,bott_flux
!
! !INPUT/OUTPUT PARAMETERS:
   REALTYPE                            :: f(0:kmax)
!
! !DEFINED PARAMETERS:
   integer,parameter                    :: UPSTREAM=2
   integer,parameter                    :: P2=3
   integer,parameter                    :: Superbee=4
   integer,parameter                    :: MUSCL=5
   integer,parameter                    :: P2_PDM=6
   REALTYPE,parameter                   :: one6th=_ONE_/6.
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard
!
!  $Log: w_split_it_adv.F90,v $
!  Revision 1.5.2.2  2005-09-20 06:59:15  kbk
!  fixed one6th parameter
!
!  Revision 1.5.2.1  2005/07/06 09:25:05  hb
!  renamed w_split_it_adv() to w_split_it_adv_1d() to avoid clash with GETM
!
!  Revision 1.5  2003/12/11 09:58:21  kbk
!  now compiles with FORTRAN_COMPILER=IFORT - removed TABS
!
!  Revision 1.4  2003/03/28 09:20:36  kbk
!  added new copyright to files
!
!  Revision 1.3  2003/03/28 09:10:39  kbk
!  removed tabs
!
!  Revision 1.2  2003/03/10 08:54:16  gotm
!  Improved documentation and cleaned up code
!
!  Revision 1.1  2001/11/18 16:27:33  gotm
!  Higher order advektion schemes implemented
! 
!EOP
!
! !LOCAL VARIABLES:
   integer                   :: ii,k,it
   REALTYPE                  :: c,alpha,beta,x,r,Phi,limit,fu,fc,fd,cmax
   REALTYPE                  :: cu(0:kmax)
   logical                   :: READY
!
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(*,*) 'w_split_it_adv() # ',Ncall
#endif

   cu = _ZERO_

! Calculating w-interface fluxes !
   cmax= _ZERO_
   it=1
   ready=.false.
111  do ii=1,it
      do k=1,kmax-1
         cu(k) = _ZERO_
         if (ww(k) .gt. _ZERO_) then
            c=ww(k)/float(it)*dt/(0.5*(h(k)+h(k+1)))
            if (c.gt.cmax) cmax=c
            if (k .gt. 1) then
               fu=f(k-1)         ! upstream
            else
               fu=f(k)
            end if
            fc=f(k  )            ! central
            fd=f(k+1)            ! downstream
            if (abs(fd-fc) .gt. 1e-10) then
               r=(fc-fu)/(fd-fc)     ! slope ratio
            else
               r=(fc-fu)*1.e10
            end if
         else
            c=-ww(k)/float(it)*dt/(0.5*(h(k)+h(k+1)))
            if (c.gt.cmax) cmax=c
            if (k .lt. kmax-1) then
               fu=f(k+2)         ! upstream
            else
               fu=f(k+1)
            end if
            fc=f(k+1)            ! central
            fd=f(k  )            ! downstream
            if (abs(fc-fd) .gt. 1e-10) then
               r=(fu-fc)/(fc-fd)     ! slope ratio
            else
               r=(fu-fc)*1.e10
            end if
         end if
         x = one6th*(1.-2.0*c)
         Phi=(0.5+x)+(0.5-x)*r
         select case (method)
            case (UPSTREAM)
               limit=0.
            case ((P2),(P2_PDM))
               x = one6th*(1.-2.0*c)
               Phi=(0.5+x)+(0.5-x)*r
               if (method.eq.P2) then
                  limit=Phi
               else
                  limit=max(_ZERO_,min(Phi,2./(1.-c),2.*r/(c+1.e-10)))
               end if
            case (Superbee)
               limit=max(_ZERO_, min(_ONE_, 2.0*r), min(r,2.*_ONE_) )
            case (MUSCL)
               limit=max(_ZERO_,min(2.*_ONE_,2.0*r,0.5*(1.0+r)))
            case default
               FATAL 'This is not so good - w_split_it_adv()'
               stop
          end select
         cu(k)=ww(k)*(fc+0.5*limit*(1-c)*(fd-fc))
      end do
      if (.not.READY) then
         it=min(200,int(cmax)+1)
         if (it.gt.1)  &
            STDERR it,' iterations for advection, Courant number is',cmax
      end if
      if ((it.gt.1).and.(.not.READY)) then
         READY=.true.
         goto 111
      end if
      if (surf_flux) cu(kmax)=cu(kmax-1)
      if (bott_flux) cu(0   )=cu(1     )
      do k=1,kmax   ! Doing a w-advection step
         if (flag.eq.1) then
            f(k)=f(k)-1./float(it)*dt*((cu(k)-cu(k-1))/        &
                 h(k)-f(k)*(ww(k)-ww(k-1))/h(k))
         else
            f(k)=(f(k)*ho(k)-1./float(it)*dt*(cu(k)-cu(k-1)))/h(k)
         end if
      end do
   end do
#ifdef DEBUG
   write(*,*) 'Leaving w_split_it_adv()'
   write(*,*)
#endif

   return
   end subroutine w_split_it_adv_1d
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

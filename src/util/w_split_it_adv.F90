!$Id: w_split_it_adv.F90,v 1.1 2001-11-18 16:27:33 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  w_split_it_adv()
!
! !INTERFACE:
   subroutine w_split_it_adv(kmax,dt,h,f,ww,method,surf_flux,bott_flux)
!
! !DESCRIPTION:
!
! This routine for vertical advection has been extracted from the GETM 
! model. It provides 5 different numerical schemes for vertical advection.
! These schemes are:
!
! \begin{itemize}
! \item First-order upstream ({\tt method=1}).
! \item Third-order upstream-biased polynomial scheme ({\tt method=3}).
! \item TVD-scheme with Superbee limiter ({\tt method=4}).
! \item TVD-scheme with MUSCL limiter ({\tt method=5}).
! \item TVD-scheme with ULTIMATE QUICKEST limiter ({\tt method=6}).
! \end{itemize}
!
! Methods 1, 4, 5, 6 should be positive definite.
!
! If surface or bottom vertical velocities are not zero, then
! {\tt surf_flux=.true.} or {\tt bott_flux=.true.}, respectively
! should be chosen. Then, no-flux boundary conditions will be used.
!
! If during a certain time step the maximum Courant number is larger 
! than zero, an iteration will be carried out which guarantees that the
! split step Courant numbers are just smaller than zero.
!
! For details, see the GETM report (Burchard and Bolding, to appear in 2002).

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)  :: kmax
   REALTYPE, intent(in) :: dt
   REALTYPE, intent(in) :: h(0:kmax)
   REALTYPE, intent(in) :: ww(0:kmax)
   integer, intent(in)  :: method
   LOGICAL,  intent(in) :: surf_flux,bott_flux
!
! !INPUT/OUTPUT PARAMETERS:
!
   REALTYPE :: f(0:kmax)
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!
!  $Log: w_split_it_adv.F90,v $
!  Revision 1.1  2001-11-18 16:27:33  gotm
!  Higher order advektion schemes implemented
! 
!
! !LOCAL VARIABLES:
   integer	:: ii,k,it
   REALTYPE	:: c,alpha,beta,x,r,Phi,limit,fu,fc,fd,cmax
   REALTYPE	:: cu(0:kmax)
   LOGICAL      :: READY
   integer,parameter      :: UPSTREAM=2
   integer,parameter      :: P2=3
   integer,parameter      :: Superbee=4
   integer,parameter      :: MUSCL=5
   integer,parameter      :: P2_PDM=6
   integer,parameter      :: one6th=0.1666666666
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'w_split_it_adv() # ',Ncall
#endif

   cu = _ZERO_

! Calculating w-interface fluxes !

     cmax=0.
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
	       limit=max(_ZERO_, min(1.0, 2.0*r), min(r,2.0) )
	    case (MUSCL) 	
	       limit=max(_ZERO_,min(2.0,2.0*r,0.5*(1.0+r)))
            case default
               FATAL 'This is not so good - w_split_it_adv()'
	       stop
	    end select
	    cu(k)=ww(k)*(fc+0.5*limit*(1-c)*(fd-fc))
         end do
	 if (.not.READY) then
	    it=min(200,int(cmax)+1)
	    if (it.gt.1) STDERR it,' iterations for advection, Courant number is',cmax
	 end if
	 if ((it.gt.1).and.(.not.READY)) then
	    READY=.true.
	    goto 111
	 end if
         if (surf_flux) cu(kmax)=cu(kmax-1)
         if (bott_flux) cu(0   )=cu(1     )
         do k=1,kmax   ! Doing a w-advection step
            f(k)=f(k)-1./float(it)*dt*(cu(k)-cu(k-1))/h(k)
         end do
      end do

#ifdef DEBUG
   write(debug,*) 'Leaving w_split_it_adv()'
   write(debug,*)
#endif
   return
   end subroutine w_split_it_adv

!-----------------------------------------------------------------------
!Copyright (C) 2001 - Karsten Bolding & Hans Burchard.
!-----------------------------------------------------------------------

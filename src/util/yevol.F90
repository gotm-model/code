!$Id: yevol.F90,v 1.2 2001-11-27 19:49:48 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Source/sink + vertical mixing/advection operator.
! 
! !INTERFACE:
   subroutine yevol(N,Bcup,Bcdw,dt,cnpar,Yup,Ydw,Taur,h,avh,w,Qsour,Yobs,Method,Y,surf_flux,bott_flux)

! !DESCRIPTION:
!  This subroutine computes the evolution of any state variable "Y" 
!  after Mixing/Advection/Source
!  \begin{itemize} 
!  \item with Neuman    Boundary Condition: Bc=1
!  \item with Dirichlet Boundary Condition: Bc=2
!  \end{itemize} 
!  Convention: fluxex are taken positive upward
!
! !USES:
   use mtridiagonal
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: N
   integer, intent(in)	:: Bcup,Bcdw
   REALTYPE, intent(in)	:: dt,cnpar
   REALTYPE, intent(in)	:: Yup,Ydw
   REALTYPE, intent(in)	:: Taur(0:N)
   REALTYPE, intent(in)	:: h(0:N),avh(0:N)
   REALTYPE, intent(in)	:: w(0:N),Qsour(0:N)
   REALTYPE, intent(in)	:: Yobs(0:N)
   logical, intent(in)	:: surf_flux,bott_flux
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out):: Y(0:N)
!
! !REVISION HISTORY: 
!  Original author(s): Pierre-Philippe Mathieu
!
!  $Log: yevol.F90,v $
!  Revision 1.2  2001-11-27 19:49:48  gotm
!  Added higher order advection via w_split_it_adv()
!
!  Revision 1.1.1.1  2001/02/12 15:55:57  gotm
!  initial import into CVS
!
! !LOCAL VARIABLES:
   integer 		:: i,Method
   REALTYPE 		:: a,c
!EOP
!-----------------------------------------------------------------------
!BOC
!  Advection step: 
   if (Method .ne. 0) then 
      call w_split_it_adv(N,dt,h,Y,w,Method,surf_flux,bott_flux)
   end if

!  Array of Diffusion/Advection terms + sources 
!  Water column 
!  => [a b c].X=[d] where X=[2, ...,i,i+1,...N-1]
   do i=2,N-1
      c    =2*dt*avh(i)  /(h(i)+h(i+1))/h(i) 
      a    =2*dt*avh(i-1)/(h(i)+h(i-1))/h(i)
      cu(i)=-cnpar*c                                 		!i+1,n+1
      au(i)=-cnpar*a                                 		!i-1,n+1
      bu(i)=1-au(i)-cu(i)                            		!i  ,n+1
      du(i)=Y(i)+dt*Qsour(i)                        &		!i  ,n
            +(1-cnpar)*(a*Y(i-1)-(a+c)*Y(i)+c*Y(i+1))
    end do

!  Surface 
! => [a b /].X=[d] where X=[N-1,N,/]
   if (Bcup.eq.1) then                       !BC Neuman
      a      =2*dt*avh(N-1)/(h(N)+h(N-1))/h(N)
      au(N)=-cnpar*a
      bu(N)=1-au(N)
      du(N)=Y(N)+dt*(Qsour(N)-Yup/h(N))+(1-cnpar)*a*(Y(N-1)-Y(N))
   else if (Bcup.eq.2) then                    !BC Dirichlet
      au(N)=0.
      bu(N)=1.
      du(N)=Yup
   end if

!  Bottom  
! => [/ b c].X=[d] where X=[/,1,2]
   if (Bcdw.eq.1) then					!BC Neuman              
      c    =2*dt*avh(1)/(h(1)+h(2))/h(1)
      cu(1)=-cnpar*c 
      bu(1)=1-cu(1)
      du(1)=Y(1)+dt*(Qsour(1)+Ydw/h(1))+(1-cnpar)*c*(Y(2)-Y(1))
   else if (Bcdw.eq.2) then				!BC Dirichlet
      cu(1)=0.
      bu(1)=1.
      du(1)=Ydw
   end if

!  Implicit internal restoring
   do i=1,N
      if (Taur(i).lt.1.E10) then
         bu(i)=bu(i)+dt/Taur(i)
         du(i)=du(i)+dt/Taur(i)*Yobs(i)
      end if
   end do

!  Implicit vertical mixing
   call tridiagonal(N,1,N,Y)

   return
   end subroutine yevol
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Pierre-Philippe Mathieu

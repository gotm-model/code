!$Id: tridiagonal.F90,v 1.1 2001-02-12 15:55:58 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: mtridiagonal 
!
! !INTERFACE:
   MODULE mtridiagonal
!
! !DESCRIPTION: 
!
!  Solves a linear system of equations with a tridiagonal matrix
!  using Gauss Elimination. 
!
! !USE:
!
! !PUBLIC MEMBER FUNCTIONS:
   PUBLIC init_tridiagonal,tridiagonal
!
! !PUBLIC DATA MEMBERS:
   REALTYPE, dimension(:), allocatable		:: au,bu,cu,du
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  $Log: tridiagonal.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   REALTYPE,private,dimension(:),allocatable	::  ru,qu
!
! !BUGS
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Allocates memory for later use.
!
! !INTERFACE:
   subroutine init_tridiagonal(N)
!
! !DESCRIPTION:
!  This routines allocates memory to be used in \em{tridiagonal}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: N
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding 
!
!  See tridiagonal module
!
! !LOCAL VARIABLES:
   integer 		:: rc
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_tridiagonal'
   allocate(au(0:N),stat=rc)
   if (rc /= 0) stop 'init_tridiagonal: Error allocating au)'
   au = 0.

   allocate(bu(0:N),stat=rc)
   if (rc /= 0) stop 'init_tridiagonal: Error allocating bu)'
   bu = 0.

   allocate(cu(0:N),stat=rc)
   if (rc /= 0) stop 'init_tridiagonal: Error allocating cu)'
   cu = 0.

   allocate(du(0:N),stat=rc)
   if (rc /= 0) stop 'init_tridiagonal: Error allocating du)'
   du = 0.

   allocate(ru(0:N),stat=rc)
   if (rc /= 0) stop 'init_tridiagonal: Error allocating ru)'

   allocate(qu(0:N),stat=rc)
   if (rc /= 0) stop 'init_tridiagonal: Error allocating qu)'

   return
   end subroutine init_tridiagonal
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: tridiagonal 
!
! !INTERFACE:
   subroutine tridiagonal(N,fi,lt,value)
!
! !DESCRIPTION:
!
! A linear equation with tridiagonal matrix is solved here. The main
! diagonal is stored on bu, the upper diagonal on au, and the
! lower diagonal on cu, the right hand side is stored on du. The method
! used here is the simplified Gauss elimination, also called Thomas algorithm.  
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: N,fi,lt
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
   REALTYPE		:: value(0:N)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: tridiagonal.F90,v $
!  Revision 1.1  2001-02-12 15:55:58  gotm
!  Initial revision
!
!
! !LOCAL VARIABLES:
   integer 		:: i
!
!EOP
!-----------------------------------------------------------------------
!BOC
   ru(lt)=au(lt)/bu(lt)
   qu(lt)=du(lt)/bu(lt)

   do i=lt-1,fi+1,-1
      ru(i)=au(i)/(bu(i)-cu(i)*ru(i+1))
      qu(i)=(du(i)-cu(i)*qu(i+1))/(bu(i)-cu(i)*ru(i+1))
   end do

   qu(fi)=(du(fi)-cu(fi)*qu(fi+1))/(bu(fi)-cu(fi)*ru(fi+1))

   value(fi)=qu(fi)
   do i=fi+1,lt
      value(i)=qu(i)-ru(i)*value(i-1)
   end do


   return
   end subroutine tridiagonal
!EOC

!-----------------------------------------------------------------------

   end module mtridiagonal

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

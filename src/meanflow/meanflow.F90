!$Id: meanflow.F90,v 1.1 2001-02-12 15:55:57 gotm Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: A dynamical 1D model (ex. turbulence). 
!
! !INTERFACE:
   module meanflow 
!
! !DESCRIPTION: 
!  This module provides all variables necessary for the meanflow 
!  calculation and also make the proper initializations.
!
! !USES:
   IMPLICIT NONE
!
!  Default all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_meanflow
!
! !PUBLIC DATA MEMBERS:
   REALTYPE, public, dimension(:), allocatable	:: z,h,ho
   REALTYPE, public, dimension(:), allocatable	:: u,v
   REALTYPE, public, dimension(:), allocatable	:: fric,drag 
   REALTYPE, public, dimension(:), allocatable	:: T,S
   REALTYPE, public, dimension(:), allocatable	:: NN,SS
   REALTYPE, public, dimension(:), allocatable	:: P,B
   REALTYPE, public, dimension(:), allocatable	:: buoy,rad,xP 
   REALTYPE, public, dimension(:), allocatable	:: avh
!  depth0 is read from the 'station' namelist.
   REALTYPE, public	:: depth0=0.
   integer, public	:: eq_state_method

!  All namelist variables are described in the prototype namelist.
!
!  These variables are read from the 'meanflow' namelist.
   REALTYPE, public	:: h0b=0.05
   REALTYPE, public	:: z0s_min=0.02
   logical, public	:: charnok=.false.
   REALTYPE, public	:: charnok_val=1400.
   REALTYPE, public	:: ddu=0.
   REALTYPE, public	:: ddl=0.
   REALTYPE, public	:: gravity=9.81
   REALTYPE, public	:: rho_0=1027.
   REALTYPE, public	:: cp=3985.
   REALTYPE, public	:: avmolu=1.3e-6
   REALTYPE, public	:: avmolT=1.4e-7
   REALTYPE, public	:: avmolS=1.1e-9
   integer, public		:: MaxItz0b=10

   REALTYPE, public	:: depth
   REALTYPE, public 	:: z0b,z0s
   REALTYPE, public	:: cori
   REALTYPE, public	:: u_taub,u_taus
   REALTYPE, public	:: obs_heat_content=0.,calc_heat_content=0.
   REALTYPE, public, parameter	:: pi=3.141592654

! !PRIVATE DATA MEMBERS:
   REALTYPE, parameter	:: omega=2*pi/86400.
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard 
!
!  $Log: meanflow.F90,v $
!  Revision 1.1  2001-02-12 15:55:57  gotm
!  Initial revision
!
!
! !BUGS
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize the 1D-dynamical model.
!
! !INTERFACE:
   subroutine init_meanflow(namlst,fn,nlev,latitude)
!
! !DESCRIPTION:
!  Initialize everything releated to the meanflow component of \em{GOTM}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)	:: namlst
   character(len=*), intent(in)	:: fn
   integer, intent(in)	:: nlev
   REALTYPE, intent(in)	:: latitude
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  See meanflow module
!
! !LOCAL VARIABLES:
   integer		:: rc
   namelist /meanflow/  h0b,z0s_min,charnok,charnok_val,ddu,ddl,        & 
                        gravity,rho_0,cp,                               &
                        avmolu,avmolT,avmolS,MaxItz0b
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'init_meanflow'

   open(namlst,file=fn,status='old',action='read',err=80)
   LEVEL2 'reading meanflow namelists..'
   read(namlst,nml=meanflow,err=81)
   close (namlst)
   LEVEL2 'done.'

   LEVEL2 'allocation meanflow memory..'
   allocate(z(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (z)'
   z = 0.

   allocate(h(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (h)'
   h = 0.

   allocate(ho(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (ho)'
   ho = 0.

   allocate(u(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (u)'
   u = 0.

   allocate(v(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (v)'
   v = 0.

   allocate(fric(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (fric)'
   fric = 0.

   allocate(drag(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (drag)'
   drag = 0.

   allocate(T(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (T)'
   T = 0.

   allocate(S(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (S)'
   S = 0.

   allocate(NN(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (NN)'
   NN = 0.

   allocate(SS(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (SS)'
   SS = 0.

   allocate(P(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_meanflow: Error allocating (P)'
   P = 0.

   allocate(B(0:nlev),stat=rc)
   if (rc /= 0) stop 'init_meanflow: Error allocating (B)'
   B = 0.

   allocate(xP(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (xP)'
   xP = 0.

   allocate(buoy(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (buoy)'
   buoy = 0.

   allocate(rad(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (rad)'
   rad = 0.

   allocate(avh(0:nlev),stat=rc)
   if (rc /= 0) STOP 'init_meanflow: Error allocating (avh)'
   avh = 0.

   LEVEL2 'done.'

   depth0=depth
   z0b=0.03*h0b
   cori=2*omega * sin(2*pi*latitude/360.)

   return
80 FATAL 'I could not open: ',trim(fn)
   stop 'init_meanflow'
81 FATAL 'I could not read "meanflow" namelist'
   stop 'init_meanflow'

   end subroutine init_meanflow 
!EOC

!-----------------------------------------------------------------------

   end module meanflow 

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard

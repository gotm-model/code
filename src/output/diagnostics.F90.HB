#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Calculation of a number of diagnostic quantities. 
! 
! !INTERFACE:
   subroutine diagnostics(n,nlev,h,buoy,u,v,T,S,k,NN,SS,tprof,heat,     &
                          I_0,rad,                                            & 
                          u_taus,u_taub,kappa,                                &
                          cp,dt,g,rho_0,T0,S0,p0,dtr0,dsr0,BObsSBF,BuoyMeth,  &
                          EqMeth,MLDSurf,MLDBott,MLDMeth,Diffk,RiCrit,RadCorr)
!
! !DESCRIPTION: 
!
! !USES:
!
! !INPUT PARAMETERS:
   integer, intent(in)			:: N,nlev
   REALTYPE, intent(in)		        :: h(0:nlev)
   REALTYPE, intent(in)		        :: buoy(0:nlev)
   REALTYPE, intent(in)		        :: u(0:nlev)
   REALTYPE, intent(in)		        :: v(0:nlev)
   REALTYPE, intent(in)		        :: T(0:nlev)
   REALTYPE, intent(in)		        :: S(0:nlev)
   REALTYPE, intent(in)		        :: k(0:nlev)
   REALTYPE, intent(in)		        :: NN(0:nlev)
   REALTYPE, intent(in)		        :: SS(0:nlev)
   REALTYPE, intent(in)		        :: rad(0:nlev)
   REALTYPE, intent(in)		        :: tprof(0:nlev)
   REALTYPE, intent(in)		        :: heat,u_taus,u_taub
   REALTYPE, intent(in)		        :: I_0
   REALTYPE, intent(in)		        :: g,rho_0,T0,S0,p0,dtr0,dsr0
   REALTYPE, intent(in)		        :: cp 
   REALTYPE, intent(in)		        :: dt
   REALTYPE, intent(in)		        :: BObsSBF
   INTEGER, intent(in)		        :: BuoyMeth,EqMeth
   INTEGER                              :: MLDMeth 
   REALTYPE                             :: Diffk
   REALTYPE                             :: RiCrit
   LOGICAL                              :: RadCorr
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
   REALTYPE              			        :: MLDSurf,MLDBott
!
! !REVISION HISTORY: 
!  Author(s): Hans Burchard & Karsten Bolding 
!  01Jan2000: Ver. 2.0.0 (kbk): A complete rewrite to F90.
!
! !LOCAL VARIABLES:
  INTEGER                                :: i,first
  REALTYPE                               :: Ri(0:nlev)
  REALTYPE                               :: eqstate,p,z,dTb,dTT,Tstar,wstar
  REALTYPE                               :: ekin,epot,epot0,eturb
  REALTYPE                               :: MOL
  REALTYPE                               :: HeatContSim,HeatContObs
  REALTYPE                               :: HeatContSim0,HeatContObs0,HeatFlux
  SAVE                                   :: HeatContSim0,HeatContObs0,HeatFlux
  SAVE                                   :: first,epot0

! 
!EOP
!-----------------------------------------------------------------------
!BOC

      select case(MLDMeth)         
         case(1)          ! MLD according to TKE criterium 
         MLDSurf    = 0.0 
         i=nlev
100         i=i-1
            MLDSurf=MLDSurf+h(i+1)
         if ((k(i).gt.Diffk).and.(i.gt.0)) goto 100
         MLDBott    = 0.0 
         i=0
101         i=i+1
            MLDBott=MLDBott+h(i)
         if ((k(i).gt.Diffk).and.(i.lt.nlev)) goto 101
         case(2)          ! MLD according to critical Ri number  
         do i=1,nlev-1
            Ri(i)=NN(i)/(SS(i)+1.e-10)
         end do
         MLDSurf    = 0.0 
         i=nlev
200         i=i-1
            MLDSurf=MLDSurf+h(i+1)
         if ((Ri(i).lt.RiCrit).and.(i.gt.0)) goto 200
         MLDBott    = 0.0 
         i=0
201         i=i+1
            MLDBott=MLDBott+h(i)
         if ((Ri(i).lt.RiCrit).and.(i.lt.nlev)) goto 201
      end select 
         
!     Here, the surface buoyancy flux (SBF) and the surface temperature
!     flux (STF) are calculated. 

      if (BuoyMeth.eq.1) then 
         dTT=0.01
         p=0.5*h(nlev)/10.
         dTb=(eqstate(g,rho_0,T0,S0,p0,dtr0,dsr0,S(nlev),T(nlev)+0.5*dTT,p,   &
                      EqMeth)-                                              &
              eqstate(g,rho_0,T0,S0,p0,dtr0,dsr0,S(nlev),T(nlev)-0.5*dTT,p,   &
                      EqMeth))/dTT
         SBF=-heat/cp/rho_0*dTb
!        Correction of surface buoyancy and temperature flux
!        for solar radiation penetration
         if (RadCorr) then
         z=0.
         i=nlev+1
444         i=i-1
            z=z+h(i)
         if (z.lt.MLDSurf) goto 444
         SBF=SBF-dTb*(rad(nlev)-rad(i)) !Using dTb such as here is not fully
                                    !correct, but we assume that the
                                    !thermal expansion coefficient does not
                                    !vary significantly over the mixed layer.
         end if 
         STF=SBF/dTb
      else
         SBF=BObsSBF
         STF=0.  
      end if 
      if (SBF.ge.0) then
         wstar=(SBF*MLDSurf)**(1./3.)
      else
         wstar=-(-SBF*MLDSurf)**(1./3.)
      end if  
      if (wstar.eq.0) then
         TStar=1.e15
      else 
         Tstar=STF/wstar
      end if 
!     Calculation of Monin-Obukhov length MOL:
      if (abs(SBF).lt.1.e-10) then
         if (SBF.ge.0) MOL=-1.e10
         if (SBF.lt.0) MOL= 1.e10
      else
         MOL=-u_taus**3/kappa/SBF
      end if 




      HeatContSim=0
      HeatContObs=0
      do i=1,nlev 
         HeatContSim=HeatContSim+T(i)*h(i)*rho_0*cp
         HeatContObs=HeatContObs+tprof(i)*h(i)*rho_0*cp
      end do
      if (first.eq.0) then
         HeatContSim0=HeatContSim
         HeatContObs0=HeatContObs
         HeatFlux=0.
      end if
      HeatContSim=HeatContSim-HeatContSim0
      HeatContObs=HeatContObs-HeatContObs0

      HeatFlux=HeatFlux+dt*(I_0+heat)

      ekin=0.
      epot=0.
      eturb=0.
      z=0.
      do i=1,nlev
         z=z-0.5*h(i)
         ekin=ekin+0.5*h(i)*(u(i)**2+v(i)**2)
         eturb=eturb+h(i)*(k(i)+k(i-1))
         epot=epot+h(i)*buoy(i)*z
         z=z-0.5*h(i)
      end do
      if (first.eq.0) then
         epot0=epot 
      end if 
      epot=epot-epot0
      ekin=ekin*rho_0
      epot=epot*rho_0
      eturb=eturb*rho_0
      
! The output parameters are:
!       MLDSurf:  Surface mixed layer depth
!       MLDBott:  Bottom mixed layer depth
!       SBF    :  Surface buoyancy flux
!       STF    :  Surface temperature flux
!       MOL    :  Monin-Obukhov length
!       wstar  :  Deardorff convective velocity scale
!       Tstar  :  Deardorff convective temperature scale
!       u_taus :  Surface friction velocity
!       u_taub :  Bottom friction velocity
!       HeatFlux: Accumulated surface heat flux J/m^2
!       HeatContSim: Relative heat content from simulated T-profiles J/m^2
!       HeatContObs: Relative heat content from observed T-profiles J/m^2
!       ekin       : Kinetic energy of the water column J/m^2 
!       epot       : Potential energy of the water column J/m^2 
!       eturb      : Turbulent energy of the water column J/m^2 
       
      write(81,111)  N*dt/24./3600.,MLDSurf,MLDBott
      write(82,111)  N*dt/24./3600.,SBF,STF,MOL 
      write(83,111)  N*dt/24./3600.,wstar,Tstar,u_taus,u_taub  
      write(84,111)  N*dt/24./3600.,HeatFlux,HeatContSim,HeatContObs  
      write(85,111)  N*dt/24./3600.,ekin,epot,eturb   

      first=1
111   format(F10.5,1x,4(E12.5,1x))

      return
      end subroutine diagnostics
!EOC

!Copyright (C) 2000 - Hans Burchard and Karsten Bolding

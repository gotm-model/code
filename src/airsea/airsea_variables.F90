#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: airsea_variables\label{sec:airsea-variables}
!
! !INTERFACE:
   module airsea_variables
!
! !DESCRIPTION:
!
!  Here, number of public variables in the airsea module is declared.
!
! !USES:
   IMPLICIT NONE

!  default: all is private.
   private
!
! !PUBLIC DATA MEMBERS:
   REALTYPE, public, parameter         :: cpa=1008.
   REALTYPE, public, parameter         :: cpw=3985.
   REALTYPE, public, parameter         :: emiss=0.97
   REALTYPE, public, parameter         :: bolz=5.670374419e-8
   REALTYPE, public, parameter         :: kelvin=273.15
   REALTYPE, public, parameter         :: const06=0.62198
   REALTYPE, public, parameter         :: rgas = 287.1    !
   REALTYPE, public, parameter         :: g = 9.81        ! [m/s2]
   REALTYPE, public, parameter         :: rho_0 = 1025.   ! [kg/m3]
   REALTYPE, public, parameter         :: kappa = 0.41    ! von Karman
   REALTYPE, public                    :: es
   REALTYPE, public                    :: ea
   REALTYPE, public                    :: qs
   REALTYPE, public, target            :: qa              ! specific humidity (kg/kg)
   REALTYPE, public                    :: L
   REALTYPE, public                    :: rhoa
   REALTYPE, public, target            :: ta              ! 2m air temperature (degree_Celsius)
   logical, public                     :: rain_impact
   logical, public                     :: calc_evaporation
!
! !PUBLIC DEFINED PARAMETERS:

   ! Albedo
   integer, public,  parameter         :: CONST=0
   integer, public,  parameter         :: PAYNE=1
   integer, public,  parameter         :: COGLEY=2

   ! Longwave radiation
   integer, public,  parameter         :: CLARK = 3               ! Clark et al, 1974
   integer, public,  parameter         :: HASTENRATH_LAMB = 4     ! Hastenrath and Lamb, 1978
   integer, public,  parameter         :: BIGNAMI = 5             ! Bignami et al., 1995 - Medsea
   integer, public,  parameter         :: BERLIAND_BERLIAND = 6   ! Berliand and Berliand, 1952 - ROMS
   integer, public,  parameter         :: JOSEY1 = 7              ! Josey 2003, (J1,9)
   integer, public,  parameter         :: JOSEY2 = 8              ! Josey 2003, (J2,14)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!
!EOP
!-----------------------------------------------------------------------

   end module airsea_variables

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

!$Id: airsea_variables.F90,v 1.1 2007-09-25 10:06:10 kbk Exp $
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
   REALTYPE, public, parameter         :: bolz=5.67e-8
   REALTYPE, public, parameter         :: kelvin=273.16
   REALTYPE, public, parameter         :: const06=0.62198
   REALTYPE, public                    :: es,ea,qs,qa,L,rhoa
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Hans Burchard
!
!  $Log: airsea_variables.F90,v $
!  Revision 1.1  2007-09-25 10:06:10  kbk
!  modularized the airsea module - added Fairall method
!
!
!EOP
!-----------------------------------------------------------------------

   end module airsea_variables

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

!$Id: bio_template.F90,v 1.1 2003-07-23 12:27:31 hb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_template --- TEMPLATE bio model \label{sec:bio_template}
!
! !INTERFACE:
   module bio_template
!
! !DESCRIPTION:
!  Remember this Hans
!
! !USES:
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio_template, init_var_template, var_info_template, &
          light_template, do_bio_template, end_bio_template
!
! !PRIVATE DATA MEMBERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: bio_template.F90,v $
!  Revision 1.1  2003-07-23 12:27:31  hb
!  more generic support for different bio models
!
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the template bio module
!
! !INTERFACE:
   subroutine init_bio_template(namlst,fname,unit,numc)
!
! !DESCRIPTION:
!  Here, the bio namelist {\tt bio_template.inp} is read and memory is
!  allocated - and various variables are initialised.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,          intent(in)   :: namlst
   character(len=*), intent(in)   :: fname
   integer,          intent(in)   :: unit

! !OUTPUT PARAMETERS:
   integer,          intent(out)   :: numc
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   namelist /bio_template_nml/ numc
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'init_bio_template'

   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=bio_template_nml,err=99)
   close(namlst)

   LEVEL3 'TEMPLATE bio module initialised ...'

   return

98 LEVEL2 'I could not open bio_template.inp'
   LEVEL2 'If thats not what you want you have to supply bio_template.inp'
   LEVEL2 'See the bio example on www.gotm.net for a working bio_template.inp'
   return
99 FATAL 'I could not read bio_template.inp'
   stop 'init_bio_template'
   end subroutine init_bio_template
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the concentration variables
!
! !INTERFACE:
   subroutine init_var_template(numc,nlev,cc,ws)
!
! !DESCRIPTION:
!  Here, the cc and ws varibles are filled with initial conditions
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: numc,nlev

! !INPUT/OUTPUT PARAMETERS:
  REALTYPE, intent(inout)              :: cc(1:numc,0:nlev)
  REALTYPE, intent(inout)              :: ws(1:numc,0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding

! !LOCAL:
  integer                    :: i
!EOP
!-----------------------------------------------------------------------
!BOC

   LEVEL3 'TEMPLATE bio model should be initialised here ...'

   return
   end subroutine init_var_template
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Providing info on variables
!
! !INTERFACE:
   subroutine var_info_template(numc,var_names,var_units,var_long)
!
! !DESCRIPTION:
!  This subroutine provides information on the variables. To be used
!  when storing data in NetCDF files.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: numc
!
! !OUTPUT PARAMETERS:
   character(len=64), intent(out)       :: var_names(:)
   character(len=64), intent(out)       :: var_units(:)
   character(len=64), intent(out)       :: var_long(:)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   var_names(1) = 'name_1'
   var_units(1) = 'units_1'
   var_long(1)  = 'long_1'

   var_names(2) = 'name_2'
   var_units(2) = 'units_2'
   var_long(2)  = 'long_2'

   var_names(3) = 'name_3'
   var_units(3) = 'units_3'
   var_long(3)  = 'long_3'

   var_names(4) = 'name_4'
   var_units(4) = 'units_4'
   var_long(4)  = 'long_4'

   var_names(5) = 'name_5'
   var_units(5) = 'units_5'
   var_long(5)  = 'long_5'

   return
   end subroutine var_info_template
!EOC


!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Light properties for the template model
!
! !INTERFACE
   subroutine light_template(numc,nlev,h,rad,cc,par,bioshade)
!
! !DESCRIPTION
!
! !USES
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer                              :: numc,nlev
   REALTYPE, intent(in)                 :: h(0:nlev)
   REALTYPE, intent(in)                 :: rad(0:nlev)
   REALTYPE, intent(in)                 :: cc(1:numc,0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: par(0:nlev)
   REALTYPE, intent(out)               :: bioshade(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC

   LEVEL2 'calculating TEMPLATE light properties'

   end subroutine light_template
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Right hand sides of geobiochemical model
!
! !INTERFACE
   subroutine do_bio_template(first,numc,nlev,cc,pp,dd)
!
! !DESCRIPTION
!
! !USES
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
  integer                              :: numc,nlev
  REALTYPE, intent(in)                 :: cc(1:numc,0:nlev)
!
! !INPUT/OUTPUT PARAMETERS:
  logical                              :: first
!
! !OUTPUT PARAMETERS:
  REALTYPE, intent(out)                :: pp(1:numc,1:numc,0:nlev)
  REALTYPE, intent(out)                :: dd(1:numc,1:numc,0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
! !LOCAL VARIABLES:
  REALTYPE, save             :: iopt
  integer                    :: i,j,ci
!EOP
!-----------------------------------------------------------------------
!BOC

   STDERR 'Here the biological process model is defined.'
   STDERR 'Since this is only a template the program is now terminated.'
   stop 'do_bio_template()'
   return
   end subroutine do_bio_template
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the bio calculations
!
! !INTERFACE:
   subroutine end_bio_template
!
! !DESCRIPTION:
!  Nothing done yet --- supplied for completeness.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
!EOP
!-----------------------------------------------------------------------
!BOC

   return
   end subroutine end_bio_template
!EOC

!-----------------------------------------------------------------------

   end module bio_template

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

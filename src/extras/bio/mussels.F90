!$Id: mussels.F90,v 1.4 2003-12-11 09:58:22 kbk Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: mussels --- mussels model \label{sec:mussels}
!
! !INTERFACE:
   module mussels
!
! !DESCRIPTION:
!  Remember this Karsten, Marie and Hans
!
! !USES:
   use bio_var, only: mussels_inhale,cc,bfl
!
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_mussels, do_mussels, end_mussels
!
! !PUBLIC DATA MEMBERS:
!  from a namelist
   logical, public           :: mussels_calc=.false.
!
! !REVISION HISTORY:!
!  Original author(s): Karsten Bolding, Marie Maar & Hans Burchard
!
!  $Log: mussels.F90,v $
!  Revision 1.4  2003-12-11 09:58:22  kbk
!  now compiles with FORTRAN_COMPILER=IFORT - removed TABS
!
!  Revision 1.3  2003/10/28 10:26:33  hb
!  allow for delayed filtering + removed dt from flux calc.
!
!  Revision 1.2  2003/10/17 14:36:49  kbk
!  skeleton bio mass subroutine added
!
!  Revision 1.1  2003/10/16 15:42:16  kbk
!  simple mussesl model implemented - filter only
!
!
! !PRIVATE DATA MEMBERS:
!  from a namelist
   integer                   :: mussels_model=1
   REALTYPE                  :: filter_rate=1.
   REALTYPE                  :: filter_lag=0.0
   REALTYPE                  :: nmussels=1000. 
   REALTYPE                  :: rate
   REALTYPE                  :: biomass=_ZERO_
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the mussels module
!
! !INTERFACE:
   subroutine init_mussels(namlst,fname,unit,nlev)
!
! !DESCRIPTION:
!  Here, the mussels namelist {\tt mussels.inp} is read and memory is
!  allocated - and various variables are initialised.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: namlst
   character(len=*), intent(in)        :: fname
   integer, intent(in)                 :: unit
   integer, intent(in)                 :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Marie Maar & Hans Burchard
!
! !LOCAL VARIABLES:
   integer                   :: rc,n
   namelist /mussels_nml/ mussels_calc,mussels_model,filter_rate, &
                          filter_lag,nmussels
!EOP
!-----------------------------------------------------------------------
!BOC

   LEVEL2 'init_mussels'

!  Open and read the namelist
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=mussels_nml,err=99)
   close(namlst)

   if (mussels_calc) then

      LEVEL3 "We will do mussels calculations..."
      select case (mussels_model)
         case(1)
            LEVEL4 "using the simple mussels model..."
            rate=0.001*filter_rate/3600.
         case default
      end select

   end if

   return

98 LEVEL2 'I could not open mussels.inp'
   LEVEL2 'If thats not what you want you have to supply mussels.inp'
   LEVEL2 'See the mussels example on www.gotm.net for a working mussels.inp'
   mussels_calc = .false.
   return
99 FATAL 'I could not read mussels.inp'
   stop 'init_mussels'
   end subroutine init_mussels
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the mussels model
!
! !INTERFACE:
   subroutine do_mussels(numc,dt)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: numc
   REALTYPE, intent(in)                :: dt
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   integer                   :: n
   REALTYPE                  :: flux
   REALTYPE, save            :: elapsed=_ZERO_
!EOP
!-----------------------------------------------------------------------
!BOC
   if (mussels_calc) then

      elapsed=elapsed+dt
      select case (mussels_model)
         case (1)
            flux=rate*nmussels
            do n=1,numc
               if(mussels_inhale(n)) then
                  if (elapsed .gt. filter_lag) then
                     bfl(n) = -flux*cc(n,1)
                  else   
                     bfl(n) = _ZERO_
                  end if   
               end if
            end do
            call mussel_biomass()
         case default
      end select

#if 0
      if (write_results) then
         call mussels_save(numc,nlev,h,totn)
      end if
#endif

   end if
   return
   end subroutine do_mussels
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Mussels bio-mass 
!
! !INTERFACE:
   subroutine mussel_biomass
!
! !DESCRIPTION:
!  Marie - remember this!!!!
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Marie Maar & Hans Burchard
!
   logical, save             :: first=.true.
!
!EOP
!-----------------------------------------------------------------------
!BOC

   if (first) then
      first=.false.      
      STDERR "here we should do mussel bio-mass calculation ..."
   end if

!   biomass= et eller andet funktionsudtryk

   return
   end subroutine mussel_biomass
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the mussels calculations
!
! !INTERFACE:
   subroutine end_mussels
!
! !DESCRIPTION:
!  Nothing done yet --- supplied for completeness.
!
! !USES:
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding, Marie Maar & Hans Burchard
!
!EOP
!-----------------------------------------------------------------------
!BOC

   LEVEL1 "Finishing mussels calculations..."

   return
   end subroutine end_mussels
!EOC

!-----------------------------------------------------------------------

   end module mussels

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------

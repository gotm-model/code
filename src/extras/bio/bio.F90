!$Id: bio.F90,v 1.7 2003-10-14 08:00:09 hb Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio --- biological model \label{sec:bio}
!
! !INTERFACE:
   module bio
!
! !DESCRIPTION:
!  Remember this Hans
!
! !USES:
   use bio_var

   use bio_template, only : init_bio_template,init_var_template
   use bio_template, only : var_info_template,light_template

   use bio_npzd, only : init_bio_npzd,init_var_npzd,var_info_npzd
   use bio_npzd, only : light_npzd

   use bio_iow, only : init_bio_iow,init_var_iow,var_info_iow
   use bio_iow, only : light_iow,surface_fluxes_iow

   use output, only: out_fmt,write_results,ts
!
!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio, do_bio, end_bio
!
! !REVISION HISTORY:!
!  Original author(s): Hans Burchard & Karsten Bolding
!
!  $Log: bio.F90,v $
!  Revision 1.7  2003-10-14 08:00:09  hb
!  initialise sfl - no special treatment when cc(,) < 0
!
!  Revision 1.6  2003/09/16 12:11:24  hb
!  added new biological model - bio_iow
!
!  Revision 1.5  2003/07/23 12:27:31  hb
!  more generic support for different bio models
!
!  Revision 1.3  2003/04/05 07:01:41  kbk
!  moved bioshade variable to meanflow - to compile properly
!
!  Revision 1.2  2003/04/04 14:25:52  hb
!  First iteration of four-compartment geobiochemical model implemented
!
!  Revision 1.1  2003/04/01 17:01:00  hb
!  Added infrastructure for geobiochemical model
!
! !PRIVATE DATA MEMBERS:
!  from a namelist
   logical                   :: bio_calc=.false.
   integer                   :: numc
   REALTYPE                  :: cnpar=0.5
   integer                   :: w_adv_discr=6
   integer                   :: ode_method=1
   integer                   :: split_factor=1
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the bio module
!
! !INTERFACE:
   subroutine init_bio(namlst,fname,unit,nlev)
!
! !DESCRIPTION:
! Here, the bio namelist {\tt bio.inp} is read and memory is
! allocated - and various variables are initialised.
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
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   integer                   :: rc,n
   namelist /bio_nml/ bio_calc,bio_model, &
                      cnpar,w_adv_discr,ode_method,split_factor
!EOP
!-----------------------------------------------------------------------
!BOC

   LEVEL1 'init_bio'

!  Open and read the namelist
   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=bio_nml,err=99)
   close(namlst)

   if (bio_calc) then

      allocate(par(0:nlev),stat=rc)
      if (rc /= 0) STOP 'init_bio: Error allocating (par)'

      select case (bio_model)

      case (-1)
         call init_bio_template(namlst,'bio_template.inp',unit,numc)
         allocate(cc(1:numc,0:nlev),stat=rc)
         if (rc /= 0) STOP 'init_bio: Error allocating (cc)'

         allocate(ws(1:numc,0:nlev),stat=rc)
         if (rc /= 0) STOP 'init_bio: Error allocating (ws)'

         call init_var_template(numc,nlev,cc,ws)

         allocate(var_ids(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_ids)'

         allocate(var_names(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_names)'

         allocate(var_units(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_units)'

         allocate(var_long(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_long)'

	 call var_info_template(numc,var_names,var_units,var_long)

      case (1)  ! The NPZD model
         call init_bio_npzd(namlst,'bio_npzd.inp',unit,numc)
         allocate(cc(1:numc,0:nlev),stat=rc)
         if (rc /= 0) STOP 'init_bio: Error allocating (cc)'

         allocate(ws(1:numc,0:nlev),stat=rc)
         if (rc /= 0) STOP 'init_bio: Error allocating (ws)'

         allocate(sfl(1:numc),stat=rc)
         if (rc /= 0) STOP 'init_bio: Error allocating (sfl)'
	 sfl=_ZERO_

         call init_var_npzd(numc,nlev,cc,ws)

         allocate(var_ids(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_ids)'

         allocate(var_names(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_names)'

         allocate(var_units(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_units)'

         allocate(var_long(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_long)'

	 call var_info_npzd(numc,var_names,var_units,var_long)

      case (2)  ! The IOW model
         call init_bio_iow(namlst,'bio_iow.inp',unit,numc)
         allocate(cc(1:numc,0:nlev),stat=rc)
         if (rc /= 0) STOP 'init_bio: Error allocating (cc)'

         allocate(ws(1:numc,0:nlev),stat=rc)
         if (rc /= 0) STOP 'init_bio: Error allocating (ws)'

         allocate(sfl(1:numc),stat=rc)
         if (rc /= 0) STOP 'init_bio: Error allocating (sfl)'
	 sfl=_ZERO_

         call init_var_iow(numc,nlev,cc,ws,sfl)

         allocate(var_ids(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_ids)'

         allocate(var_names(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_names)'

         allocate(var_units(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_units)'

         allocate(var_long(numc),stat=rc)
         if (rc /= 0) stop 'bio_save(): Error allocating var_long)'

	 call var_info_iow(numc,var_names,var_units,var_long)

      case default
         stop "bio: no valid biomodel specified in bio.inp !"
      end select

      do n=1,numc
         LEVEL4 trim(var_names(n)),'  ',trim(var_units(n)), &
                '  ',trim(var_long(n))
      end do

      select case (ode_method)
         case (1)
            LEVEL2 'Using euler_forward()'
         case (2)
            LEVEL2 'Using runge_kutta_2()'
         case (3)
            LEVEL2 'Using runge_kutta_4()'
         case (4)
            LEVEL2 'Using patankar()'
         case (5)
            LEVEL2 'Using patankar_runge_kutta_2()'
         case (6)
            LEVEL2 'Using patankar_runge_kutta_4()'
         case (7)
            LEVEL2 'Using modified_patankar()'
         case (8)
            LEVEL2 'Using modified_patankar_2()'
         case (9)
            LEVEL2 'Using modified_patankar_4()'
         case default
            stop "bio: no valid ode_method specified in bio.inp!"
      end select

   end if

   return

98 LEVEL2 'I could not open bio.inp'
   LEVEL2 'If thats not what you want you have to supply bio.inp'
   LEVEL2 'See the bio example on www.gotm.net for a working bio.inp'
   bio_calc = .false.
   return
99 FATAL 'I could not read bio.inp'
   stop 'init_bio'
   end subroutine init_bio
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Update the bio model
!
! !INTERFACE:
   subroutine do_bio(nlev,I_0,dt,h,t,nuh,rad,bioshade)
!
! !DESCRIPTION:
!
! !USES:
   use bio_var, only: I_0_local => I_0
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,  intent(in)                :: nlev
   REALTYPE, intent(in)                :: I_0
   REALTYPE, intent(in)                :: dt
   REALTYPE, intent(in)                :: h(0:nlev)
   REALTYPE, intent(in)                :: nuh(0:nlev)
   REALTYPE, intent(in)                :: t(0:nlev)
   REALTYPE, intent(in)                :: rad(0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: bioshade(0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   REALTYPE                  :: Qsour(0:nlev),w_grid(0:nlev)
   REALTYPE                  :: Sup=0,Sdw=0
   REALTYPE                  :: RelaxTau(0:nlev)
   REALTYPE                  :: zz,add
   REALTYPE                  :: totn,dt_eff
   integer                   :: i,j,ci
   integer                   :: Bcup=1,Bcdw=1,flag=2,char=1,grid_method=0
   integer                   :: split
   logical                   :: surf_flux=.false.,bott_flux=.false.
!EOP
!-----------------------------------------------------------------------
!BOC
   if (bio_calc) then

      I_0_local = I_0

      Qsour    = _ZERO_
      RelaxTau = 1.e15
      w_grid   = _ZERO_

      select case (bio_model)
         case (-1)
         case (1)
         case (2)
            call surface_fluxes_iow(numc,nlev,cc,t(nlev),sfl)
      end select

      do j=1,numc
         call Yevol(nlev,Bcup,Bcdw,dt,cnpar,sfl(j),Sdw,RelaxTau,h,h,nuh,ws(j,:), &
                 QSour,cc(j,:),char,w_adv_discr,cc(j,:),surf_flux,bott_flux,  &
                 grid_method,w_grid,flag)
      end do

      do split=1,split_factor
         dt_eff=dt/float(split_factor)

         select case (bio_model)
            case (-1)
               call light_template(numc,nlev,h,rad,cc,par,bioshade)
            case (1)
               call light_npzd(numc,nlev,h,rad,cc,par,bioshade)
            case (2)
               call light_iow(numc,nlev,h,rad,cc,par,bioshade)
         end select

         call ode_solver(ode_method,numc,nlev,dt_eff,cc,t)

      end do

      if (write_results) then
         totn= _ZERO_
         do i=nlev,1,-1
            do ci=1,numc
               totn=totn+cc(ci,i)*h(i)
            end do
         end do

         call bio_save(numc,nlev,h,totn)
      end if

   end if
   return
   end subroutine do_bio
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the bio calculations
!
! !INTERFACE:
   subroutine end_bio
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
   end subroutine end_bio
!EOC

!-----------------------------------------------------------------------

   end module bio

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------

!$Id: bio_mangan.F90,v 1.2 2010-05-18 15:50:19 jorn Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: bio_mangan --- SImple manganese model \label{sec:bio-mangan}
!
! !INTERFACE:
   module bio_mangan
!
! !DESCRIPTION:
!
! Water column equations for manganese-2 ($\left[Mn^{2+}\right]$),
! manganese oxyde ($\left[MnO_2\right]$) and suspended particulate matter
! ($C$):
! \begin{equation}
! \begin{array}{lllll}
! \displaystyle
! \partial_t \left[Mn^{2+}\right] & 
! \displaystyle
! - \partial_z \left(K_v\partial_z 
! \left[Mn^{2+}\right]\right) &
! &=&
! \displaystyle
! -r_{ox}\left[Mn^{2+}\right] \frac{C}{C+C_{1/2}}, \\ \\
! \displaystyle
! \partial_t \left[MnO_2\right] &
! \displaystyle
! - \partial_z \left(K_v\partial_z \left[MnO_2\right]\right) &
! \displaystyle
! - \partial_z \left(w_s\left[MnO_2\right]\right) 
! &=&
! \displaystyle
! +r_{ox}\left[Mn^{2+}\right] \frac{C}{C+C_{1/2}}, \\ \\
! \displaystyle
! \partial_t C &
! - \partial_z \left(K_v\partial_z C\right) &
! - \partial_z \left(w_s C\right) 
! &=&
! \displaystyle
! 0, 
! \end{array}
! \end{equation}
! with the bottom boundary conditions
! \begin{equation}
! \begin{array}{lll}
! \displaystyle
! \left(K_v\partial_z\left[Mn^{2+}\right]\right)_{z=-H} &=& 
! \displaystyle
! v_t \left(\frac{10^3[Mn^{2+}]_s}{D_s}-[Mn^{2+}]_{z=-H}\right),
! \\ \\
! \displaystyle
! \left(K_v\partial_z\left[MnO_2\right]+w_s\left[MnO_2\right]\right)_{z=-H} &=& 
! \displaystyle
! - F\left([MnO_2]\right),
! \\ \\
! \displaystyle
! \left(K_v\partial_zC+w_sC\right)_{z=-H} &=& 
! \displaystyle
! - F(C).
! \\ \\
! \end{array}
! \end{equation}
! 
! Sediment equations for manganese-2 ($\left[Mn^{2+}\right]_s$),
! manganese oxyde ($\left[MnO_2\right]_s$) and suspended particulate matter
! ($C_s$):
! \begin{equation}
! \begin{array}{llll}
! \displaystyle
! \partial_t \left[Mn^{2+}\right]_s 
! &=& 
! \displaystyle
! +r_{red} \left[MnO_2\right]_s &
! \displaystyle
! -\frac{v_t}{10^3} \left(\frac{10^3[Mn^{2+}]_s}{D_s}-[Mn^{2+}]_{z=-H}\right),
! \\ \\
! \displaystyle
! \partial_t \left[MnO_2\right]_s &=& 
! \displaystyle
! -r_{red} \left[MnO_2\right]_s &
! \displaystyle
! + \frac{F\left([MnO_2]\right)}{10^3},
! \\ \\
! \displaystyle
! \partial_t C_s 
! &=& 
! \displaystyle
!  &
! \displaystyle
! + F(C),
! \end{array}
! \end{equation}
! with the sediment to water column fluxes
! \begin{equation}
! F\left([MnO_2]\right)=
! \left\{
! \begin{array}{ll}
! \displaystyle
!  w_s \frac{\tau_{bc}-|\tau_b|}{\tau_{bc}}\left[MnO_2\right]_{z=-H},
! & \mbox{ for } \tau_{bc}>|\tau_b|,\\ \\
! \displaystyle
!  r_{ero} \frac{\tau_{bc}-|\tau_b|}{\tau_{bc}}
! \frac{\left[MnO_2\right]_s\left[MnO_2\right]_{s,{1/2}}}{\left[MnO_2\right]_s+\left[MnO_2\right]_{s,{1/2}}},
! & \mbox{ else, }
! \end{array}
! \right.
! \end{equation}
! and
! \begin{equation}
! F\left(C\right)=
! \left\{
! \begin{array}{ll}
! \displaystyle
!  w_s \frac{\tau_{bc}-|\tau_b|}{\tau_{bc}}C_{z=-H}, 
! & \mbox{ for } \tau_{bc}>|\tau_b|,\\ \\
! \displaystyle
!  r_{ero} \frac{\tau_{bc}-|\tau_b|}{\tau_{bc}}
! \frac{C_sC_{s,1/2}}{C_s+C_{s,{1/2}}},
! & \mbox{ else. }
! \end{array}
! \right.
! \end{equation}
! 
! \begin{table}[h]
! \begin{center}
! \begin{tabular}{|l|l|l|}
! \hline
! Model parameter & Symbol & unit \\ \hline \hline 
! Manganese oxide concentration in water & $\left[MnO_2\right]$ & $\mu$M\,m$^{-3}$ \\ \hline
! Maganese-2 concentration in water & $\left[Mn^{2+}\right]$ & $\mu$M\,m$^{-3}$ \\ \hline
! Suspended matter concentration in water & $C$ & kg\,m$^{-3}$ \\ \hline
! Manganese oxide content in sediment & $\left[MnO_2\right]_s$ & mM\,m$^{-2}$ \\ \hline
! Maganese-2 content in sediment & $\left[Mn^{2+}\right]_s$ & mM\,m$^{-2}$ \\ \hline
! Suspended matter content in sediment & $C_s$ & kg\,m$^{-2}$ \\ \hline
! \end{tabular}
! \end{center}
! \caption{Model parameters for the manganese model.}
! \end{table}
! 
! 
! \begin{table}[h]
! \begin{center}
! \begin{tabular}{|l|l|l|l|}
! \hline
! Empirical parameter & Symbol & value & unit \\ \hline \hline 
! Initial $\left[MnO_2\right]$ concentration & $\left[MnO_2\right]^0$ && $\mu$M\,m$^{-3}$ \\ \hline
! Initial $\left[Mn^{2+}\right]$ concentration & $\left[Mn^{2+}\right]^0$ && $\mu$M\,m$^{-3}$ \\ \hline
! Initial $C$ concentration & $C^0$ && kg\,m$^{-3}$ \\ \hline
! Initial $\left[MnO_2\right]_s$ concentration & $\left[MnO_2\right]_s^0$ && mM\,m$^{-2}$ \\ \hline
! Initial $\left[Mn^{2+}\right]_s$ concentration & $\left[Mn^{2+}\right]_s^0$ && mM\,m$^{-2}$ \\ \hline
! Initial $C_s$ concentration & $C_s^0$ && kg\,m$^{-2}$ \\ \hline
! Oxydation rate in water & $r_{ox}$ && d$^{-1}$ \\ \hline
! Reduction rate in sediment & $r_{red}$ && d$^{-1}$ \\ \hline
! Erosion rate & $r_{ero}$ && d$^{-1}$ \\ \hline
! Critical shear stress & $\tau_{bc}$ && N\,m$^{-2}$ \\ \hline
! Transfer velocity & $v_t$ && m\,s$^{-1}$ \\ \hline
! Settling velocity & $w_s$ && m\,s$^{-1}$ \\ \hline
! Sediment depth & $D_s$ && m \\ \hline
! Half saturation $C$ in water & $C_{1/2}$ && kg\,m$^{-3}$ \\ \hline
! Half saturation $C_s$ in sediment & $C_{s,1/2}$ && kg\,m$^{-2}$ \\ \hline
! Half saturation $\left[Mn^{2+}\right]_s$ in sediment & 
! $\left[Mn^{2+}\right]_{s,1/2}$ && mM\,m$^{-2}$ \\ \hline
! \end{tabular}
! \caption{Empirical parameters for the manganese model.}
! \end{center}
! \end{table}
!
! !USES:
!  default: all is private.
   use bio_var
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_bio_mangan, init_var_mangan,                 &
          light_mangan, do_bio_mangan, end_bio_mangan
!
! !PRIVATE DATA MEMBERS:
!
! !REVISION HISTORY:!
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
!  from a namelist
   REALTYPE                  :: mnO_ini=     5.
   REALTYPE                  :: mn2_ini=     5.
   REALTYPE                  :: spm_ini=     0.1
   REALTYPE                  :: mnO_sed_ini= 0.1
   REALTYPE                  :: mn2_sed_ini= 0.1
   REALTYPE                  :: spm_sed_ini= 2.0
   REALTYPE                  :: r_ox=        0.1
   REALTYPE                  :: r_red=       0.1
   REALTYPE                  :: r_ero=       0.1
   REALTYPE                  :: tau_bc=      0.2
   REALTYPE                  :: v_t=         5.
   REALTYPE                  :: w_s=         50.
   REALTYPE                  :: d_s=         0.1
   REALTYPE                  :: spm12=       0.01
   REALTYPE                  :: spm_sed12=   0.2
   REALTYPE                  :: mno_sed12=   0.01
   integer                   :: out_unit
   integer, parameter        :: mno=1,mn2=2,spm=3,mno_sed=4,mn2_sed=5,spm_sed=6
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the bio module
!
! !INTERFACE:
   subroutine init_bio_mangan(namlst,fname,unit)
!
! !DESCRIPTION:
!  Here, the bio namelist {\tt bio\_mangan.nml} is read and 
!  various variables (rates and settling velocities) 
!  are transformed into SI units.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,          intent(in)   :: namlst
   character(len=*), intent(in)   :: fname
   integer,          intent(in)   :: unit
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   namelist /bio_mangan_nml/ numc, mnO_ini, mn2_ini, spm_ini, mnO_sed_ini, &
                             mn2_sed_ini, spm_sed_ini, r_ox, r_red, r_ero, &
                             tau_bc, v_t, w_s, d_s,spm12,spm_sed12,mno_sed12
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'init_bio_mangan'

   numc=4

   open(namlst,file=fname,action='read',status='old',err=98)
   read(namlst,nml=bio_mangan_nml,err=99)
   close(namlst)

   LEVEL3 'namelist "', fname, '" read'

!  Conversion from day to second
   r_ox   = r_ox  /secs_pr_day
   r_red  = r_red /secs_pr_day
   r_ero  = r_ero /secs_pr_day
   v_t    = v_t   /secs_pr_day
   w_s    = w_s   /secs_pr_day


!  initialize variable descriptions

   call bio_alloc_info


   var_names(1) = 'mno'
   var_units(1) = 'muM/m**3'
   var_long(1)  = 'Mangenese Oxyde in water'

   var_names(2) = 'mn2'
   var_units(2) = 'muM/m**3'
   var_long(2)  = 'Maganese-2 in water'

   var_names(3) = 'spm'
   var_units(3) = 'kg/m**3'
   var_long(3)  = 'SPM in water'

   var_names(4) = 'mno_sed'
   var_units(4) = 'mM/m**2'
   var_long(4)  = 'Mangenese Oxyde in sediment'

   var_names(5) = 'mn2_sed'
   var_units(5) = 'mM/m**2'
   var_long(5)  = 'Maganese-2 in sediment'

   var_names(6) = 'spm_sed'
   var_units(6) = 'kg/m**2'
   var_long(6)  = 'SPM in sediment'


   out_unit=unit

   LEVEL3 'module initialized'

   return

98 LEVEL2 'I could not open bio_mangan.nml'
   LEVEL2 'If thats not what you want you have to supply bio_mangan.nml'
   LEVEL2 'See the bio example on www.gotm.net for a working bio_mangan.nml'
   return
99 FATAL 'I could not read bio_mangan.nml'
   stop 'init_bio_mangan'
   end subroutine init_bio_mangan
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise the concentration variables
!
! !INTERFACE:
   subroutine init_var_mangan
!
! !DESCRIPTION:
!  Here, the the initial conditions are set and the settling velocities are
!  transferred to all vertical levels. All concentrations are declared
!  as non-negative variables, and it is defined which variables would be 
!  taken up by benthic filter feeders.
!
! !USES:
   IMPLICIT NONE

! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding

! !LOCAL VARIABLES:
  integer                    :: i
!EOP
!-----------------------------------------------------------------------
!BOC
   do i=1,nlev
      cc(mno,i)     = mno_ini
      cc(mn2,i)     = mn2_ini
      cc(spm,i)     = spm_ini
      cc(mno_sed,i) = 1.e-15
      cc(mn2_sed,i) = 1.e-15
      cc(spm_sed,i) = 1.e-15
   end do
   cc(mno_sed,1) = mno_sed_ini
   cc(mn2_sed,1) = mn2_sed_ini
   cc(spm_sed,1) = spm_sed_ini

   do i=0,nlev
      ws(mno,i)     = w_s
      ws(mn2,i)     = _ZERO_
      ws(spm,i)     = w_s
      ws(mno_sed,i) = _ZERO_
      ws(mn2_sed,i) = _ZERO_
      ws(spm_sed,i) = _ZERO_
   end do

   posconc(mno)     = 1
   posconc(mn2)     = 1
   posconc(spm)     = 1
   posconc(mno_sed) = 1
   posconc(mn2_sed) = 1
   posconc(spm_sed) = 1

   LEVEL3 'Maganese model variables initialised ...'

   return

   end subroutine init_var_mangan
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Light properties for the NPZD model
!
! !INTERFACE:
   subroutine light_mangan(nlev,bioshade_feedback)
!
! !DESCRIPTION:
! Here, the photosynthetically available radiation is calculated
! by simply assuming that the short wave part of the total
! radiation is available for photosynthesis. The user should make
! sure that this is consistent with the light class given in the
! {\tt extinct} namelist of the {\tt obs.nml} file.
! The self-shading effect is also calculated in this subroutine,
! which may be used to consider the effect of bio-turbidity also
! in the temperature equation (if {\tt bioshade\_feedback} is set
! to true in {\tt bio.nml}). For details, see section \ref{sec:do-bio}.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: nlev
   logical, intent(in)                 :: bioshade_feedback
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
! !LOCAL VARIABLES:
   integer                   :: i
   REALTYPE                  :: zz,add
!EOP
!-----------------------------------------------------------------------
!BOC

   return
   end subroutine light_mangan
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Right hand sides of NPZD model
!
! !INTERFACE:
   subroutine do_bio_mangan(first,numc,nlev,cc,pp,dd)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   logical, intent(in)                  :: first
   integer, intent(in)                  :: numc,nlev
   REALTYPE, intent(in)                 :: cc(1:numc,0:nlev)
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)                :: pp(1:numc,1:numc,0:nlev)
   REALTYPE, intent(out)                :: dd(1:numc,1:numc,0:nlev)
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard, Karsten Bolding
!
! !LOCAL VARIABLES:
  integer                    :: i,j,ci
  REALTYPE                   :: rat(0:nlev,0:nlev)
  REALTYPE                   :: taub,r_sedim=0.,r_resus=0.
  REALTYPE                   :: mn_tot,spm_tot

!EOP
!-----------------------------------------------------------------------
!BOC

   pp = _ZERO_
   dd = _ZERO_

   rat=_ONE_
   rat(mn2,mn2_sed)=h(1)/1000.
   rat(mno,mno_sed)=h(1)/1000.
   rat(spm,spm_sed)=h(1)
   rat(mn2_sed,mn2)=1000./h(1)
   rat(mno_sed,mno)=1000./h(1)
   rat(spm_sed,spm)=1.0/h(1)

   taub=u_taub**2*1000.
   if (tau_bc.gt.taub) then
      r_sedim=+abs(w_s)*(tau_bc-taub)/tau_bc
   else
      r_resus=-r_ero*(tau_bc-taub)/tau_bc
   end if

   do ci=1,nlev
      dd(mn2,mno,ci)=r_ox*cc(mn2,ci)*cc(spm,ci)/(cc(spm,ci)+spm12)

      if (ci.eq.1) then
         dd(mno_sed,mn2_sed,ci)=r_red*cc(mno_sed,ci)
         dd(mn2_sed,mn2    ,ci)=v_t*(1000.*cc(mn2_sed,ci)/d_s-cc(mn2,ci))/1000. 
         dd(mno_sed,mno    ,ci)=r_resus*cc(mno_sed,ci)*mno_sed12/            &
                                      (cc(mno_sed,ci)+mno_sed12)
         dd(spm_sed,spm    ,ci)=r_resus*cc(spm_sed,ci)*spm_sed12/            &
                                      (cc(spm_sed,ci)+spm_sed12)
         dd(mno,mno_sed    ,ci)=r_sedim*cc(mno,ci)
         dd(spm,spm_sed    ,ci)=r_sedim*cc(spm,ci) 
      end if

      do i=1,numc
         do j=1,numc
            pp(i,j,ci)=rat(j,i)*dd(j,i,ci)
         end do
      end do
   end do

   mn_tot=1000.*(cc(mno_sed,1)+cc(mn2_sed,1))
   do ci=1,nlev
      mn_tot=mn_tot+h(ci)*(cc(mno,ci)+cc(mn2,ci))
   end do
   spm_tot=cc(spm_sed,1)
   do ci=1,nlev
      spm_tot=spm_tot+h(ci)*cc(spm,ci)
   end do
   

   return
   end subroutine do_bio_mangan
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finish the bio calculations
!
! !INTERFACE:
   subroutine end_bio_mangan
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
   end subroutine end_bio_mangan
!EOC

!-----------------------------------------------------------------------

   end module bio_mangan

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------

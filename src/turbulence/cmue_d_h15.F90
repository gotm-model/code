#include"cppdefs.h"
!-----------------------------------------------------------------------
!
! !ROUTINE: The Langmuir turbulence quasi-equilibrium stability functions  after Harcourt (2015)\label{sec:cmueDH15}
!
! !INTERFACE:
   subroutine cmue_d_h15(nlev)
!
! !DESCRIPTION:
!
!  Old Description from GTOM: This subroutine updates the explicit solution of
!  \eq{bijVertical} and \eq{giVertical} under the same assumptions
!  as those discussed in \sect{sec:cmueC}. Now, however, an additional
!  equilibrium assumption is invoked. With the help of \eq{PeVertical},
!  one can write the equilibrium condition for the TKE as
! \begin{equation}
!  \label{quasiEquilibrium}
!     \dfrac{P+G}{\epsilon} =
!    \hat{c}_\mu(\alpha_M,\alpha_N) \alpha_M
!    - \hat{c}'_\mu(\alpha_M,\alpha_N) \alpha_N = 1
!   \comma
! \end{equation}
! where \eq{alphaIdentities} has been used. This is an implicit relation
! to determine $\alpha_M$ as a function of $\alpha_N$.
! With the definitions given in \sect{sec:cmueC}, it turns out that
! $\alpha_M(\alpha_N)$ is a quadratic polynomial that is easily solved.
! The resulting value for $\alpha_M$ is substituted into the stability
! functions described in \sect{sec:cmueC}. For negative $\alpha_N$
! (convection) the shear number $\alpha_M$ computed in this way may
! become negative. The value of $\alpha_N$ is limited such that this
! does not happen, see \cite{UmlaufBurchard2005a}.
! This Langmuir turbulence version includes the CL Vortex force in
! the algebraic models as well as in the vortex production of TKE and L or epsilon
!
! !USES:
   use turbulence, only: an,as,at
! nondimensional forcing functions for Eulerian shear dot Stokes shear, and Stokes shear squared:
! Also, surface proximity function SPF=(1-fzs), goes to zero at surface as tanh(0.25*z/l_S) where l_S
! the vortex-production-weighted dissipation length scale
   use turbulence, only: av, aw, SPF
   use turbulence, only: tke, L
   use turbulence, only: cmue1,cmue2
   use turbulence, only: cmue3
   use turbulence, only: sq, sl, sq_var, sl_var
   use turbulence, only: length_lim

   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer, intent(in)       :: nlev

! !DEFINED PARAMETERS:
   REALTYPE, parameter       :: small       = 1.0D-8

!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!  Converted by Ramsey Harcourt, last updated 31 July 2018.  This version uses the Harcourt(2015)
!  stability functions from the quasi-equilibrium Second Moment closure (SMC) with Craik-Leibovich
!  terms, but it has been further modified by replacing the crude limiters applied
!  individually to Gh, Gv and Gs in Harcourt(2015) under unstable/positive production conditions
!  with a combinations of limitations on (L/q)^2 applied consistently across Gm, Gs, Gh, Gv,
!  as a function of Gh and Gv input to the ARSM. This ARSM also applies the Galperin limit to
!  L going into the ARSM (algebraic) diagnosis of Sm, Sh, Ss, regardless of whether it/s
!  being enforced within the dyanamic model.
!
!  Recomend running with e3=5 & length.lim=false, but e3=1.2 & length.lim=true is similar
!  When length.lim=false, length scale or at least L/q is still limited within the ARSM for
!  Stability fcns cmue1,cmue2,cmue3. length.lim=false allows the elevated length scale within
!  the mixed layer to impact the transition zone, while restraining the Stability functions
!  to the stability-limited length scale regime.
!
!EOP
!-----------------------------------------------------------------------
! !LOCAL VARIABLES:
!
   integer, parameter :: rk = kind(_ONE_)
     integer                 ::   i
     REALTYPE            ::   Gv, Gs, Gh, Gm
     REALTYPE            ::   Sm, Ss, Sh

     REALTYPE, parameter :: my_A1 = 0.92D0
     REALTYPE, parameter :: my_A2 = 0.74D0
     REALTYPE, parameter :: my_B1 = 16.6D0
     REALTYPE, parameter :: my_B2 = 10.1D0
     REALTYPE, parameter :: my_C1 = 0.08D0
     REALTYPE, parameter :: my_C2 = 0.7D0
     REALTYPE, parameter :: my_C3 = 0.2D0
     REALTYPE, parameter :: h15_Ghmin = -0.28D0
     REALTYPE, parameter :: h15_Ghoff = 0.003D0
     REALTYPE, parameter :: h15_Gvoff = 0.006D0
     REALTYPE, parameter :: h15_Sxmax = 2.12D0

     REALTYPE :: h15_Shn0, h15_Shnh, h15_Shns, h15_Shnv
     REALTYPE :: h15_Shdah, h15_Shdav, h15_Shdbh
     REALTYPE :: h15_Shdv, h15_Shdvh, h15_Shdvv
     REALTYPE :: h15_Ssn0, h15_Ssdh, h15_Ssdv
     REALTYPE :: h15_Smn0, h15_SmnhSh, h15_SmnsSs
     REALTYPE :: h15_Smdh, h15_Smdv

     REALTYPE :: tmp0,tmp1,tmp2,tmp3,tmp4
     REALTYPE :: Ghcrit, Gvcrit

!-----------------------------------------------------------------------
! These constants  above & below could all be set or computed elsewhere in advance,
! subject to adjustments in A's, B's & C's. Just sticking them all in here for now.

     h15_Shn0=my_A2*(1.D0-6.D0*my_A1/my_B1)
     h15_Shnh=-9.D0*my_A1*my_A2*( my_A2*(1.D0-6.D0*my_A1/my_B1))
     h15_Shns=9.D0*my_A1*my_A2*(1.D0-6.D0*my_A1/my_B1)*                                 &
              (2.D0*my_A1+my_A2)
     h15_Shnv=9.D0*my_A1*my_A2*                                                         &
              (my_A2*(1.D0-6.D0*my_A1/my_B1-3.D0*my_C1)                                 &
               -2.D0*my_A1*(1.D0-6.D0*my_A1/my_B1+3.D0*my_C1))
     h15_Shdah=-9.D0*my_A1*my_A2
     h15_Shdav=-36.D0*my_A1*my_A1
     h15_Shdbh=-3.D0*my_A2*(6.D0*my_A1+my_B2*(1.D0-my_C3))
     h15_Shdv=-9.D0*my_A2*my_A2*(1.D0-my_C2)
     h15_Shdvh=-162.D0*my_A1*my_A1*my_A2*(2.D0*my_A1+(2.D0-my_C2)*my_A2)
     h15_Shdvv=324.D0*my_A1*my_A1*my_A2*my_A2*(1.D0-my_C2)
     h15_Ssn0=my_A1*(1.D0-6.D0*my_A1/my_B1)
     h15_Ssdh=-9.D0*my_A1*my_A2
     h15_Ssdv=-9.D0*my_A1*my_A1
     h15_Smn0 = my_A1*(1.D0-6.D0*my_A1/my_B1-3.D0*my_C1)
     h15_SmnhSh = 9.D0*my_A1*(2.D0*my_A1+my_A2*(1.D0-my_C2))
     h15_SmnsSs = 27.D0*my_A1*my_A1
     h15_Smdh = -9.D0*my_A1*my_A2
     h15_Smdv = -36.D0*my_A1*my_A1

     do i=1,nlev-1
! convert nondimensional forcing functions to q2-q2l formulation,
! at least until this is rederived in k-epsilon formulation
        tmp0 = 4.D0/my_B1**2.
        Gh = -tmp0*an(i)
        Gm =  tmp0*as(i)
        Gv =  tmp0*av(i)
        Gs =  tmp0*aw(i)

! Limitation on the stable side
! if length scale is not strictly limited by stratification, limit it here within the ARSM:

        if (.not.(length_lim)) then
           tmp1=1.D0

           tmp2=h15_Ghmin/min(h15_Ghmin,Gh)

           tmp1=min(tmp1,tmp2)

           if (tmp1.lt.1.D0) then
              Gh=Gh*tmp1
              Gv=Gv*tmp1
              Gs=Gs*tmp1
           endif
        endif

! Limitation on the unstable side
! Before SPF: Limit to less than l/q corresponding to equilibrium
! tke eqn soln for (l/q) in limit of gs=gm=0
!

! Find ratio to rescale to critical Sh & l/q with offset of -Gvoff and -Ghoff from critical to equilibrium curve for Gm=Gs=0
! Substituting Gh=R*Gh+Ghoff, Gv=R*Gv+Gvoff to find R such that R*Gh, R*Gv are offset by [-Ghoff, -Gvoff] from critical [Gh, Gv]

         tmp0=2.D0

         if(Gv.gt._ZERO_) then

! Coefficient of linear R with contributions from offsets in terms ~Gx*Gy
        tmp1= (h15_Shdah+h15_Shdbh)*Gh+(h15_Shdav+h15_Shdv)*Gv
        tmp1=tmp1+(h15_Shdah*h15_Ghoff+h15_Shdav*h15_Gvoff)*(h15_Shdbh*Gh) +           &
             (h15_Shdvh*h15_Ghoff+h15_Shdvv*h15_Gvoff)*Gv
        tmp1=tmp1+(h15_Shdah*Gh+h15_Shdav*Gv)*(h15_Shdbh*h15_Ghoff) +                  &
             (h15_Shdvh*Gh+h15_Shdvv*Gv)*h15_Gvoff
! Coefficient of R^2
        tmp2=(h15_Shdah*Gh+h15_Shdav*Gv)*(h15_Shdbh*Gh) +                              &
             (h15_Shdvh*Gh+h15_Shdvv*Gv)*Gv
! Constant term, something a bit smaller than 1 because of offset
        tmp4=1.D0+(h15_Shdah+h15_Shdbh)*h15_Ghoff+(h15_Shdav+h15_Shdv)*h15_Gvoff       &
             +(h15_Shdah*h15_Ghoff+h15_Shdav*h15_Gvoff)*(h15_Shdbh*h15_Ghoff) +        &
              (h15_Shdvh*h15_Ghoff+h15_Shdvv*h15_Gvoff)*h15_Gvoff

! Solve for ratio rescaling to critical a*r^2+b*r+c=0; c=1, b=tmp1, a=tmp2
! Check determinant

        tmp3=tmp1*tmp1-4.D0*tmp2*tmp4

! We need the smallest positive root R
        if ((tmp3.ge._ZERO_).and.(tmp2.lt._ZERO_)) then
               tmp3=(-tmp1+sqrt(tmp3))/(2.D0*tmp2)
        elseif ((tmp3.ge._ZERO_).and.(tmp3.gt._ZERO_)) then
               tmp3=(-tmp1-sqrt(tmp3))/(2.D0*tmp2)
        else
! there is no root i.e. direction of [Gh, Gv] is in stable sector. Set tmp3>1 for no rescaling
               tmp3=2.D0
        endif

        if ((tmp3.gt.0.D0).and.(tmp3.lt.1.D0)) then
           tmp0=min(tmp0,tmp3)
        endif

        endif

! Apply SPF *after* limiting l/q to equilibrium in limit of gm=gs=0
! because tke equilibrium does not involve the near-surface pressure-strain
! terms effected by SPF. Apply SPF *before* the limitation on monotonicity of
! Sh/Gh because that might actually be affected by pressure-strain terms.
        Gv =  Gv*SPF(i)
        Gs =  Gs*SPF(i)**2.

! Second limitation after Umlauf & Burchard (2005), Eq 47, approximating -d( Sh/Gh )/d(Gh) > 0
         if(Gh.gt._ZERO_) then

! This curve is approximated on the Gs=0 plane using the critical Den{Sh}=0 curve but with
! Sh(Gh->2*Gh) when Gh>0, no offsets needed. Bit of a coincidence that hasn't yet been worked out.
! tmp1 is coefficient (b) of linear R with contributions from offsets in terms ~Gx*Gy
        tmp1= 2.D0*(h15_Shdah+h15_Shdbh)*Gh+(h15_Shdav+h15_Shdv)*Gv
! Coefficient (a) of R^2
        tmp2=(2.D0*h15_Shdah*Gh+h15_Shdav*Gv)*(2.D0*h15_Shdbh*Gh) +                              &
             (2.D0*h15_Shdvh*Gh+h15_Shdvv*Gv)*Gv
! Constant term is c=1
        tmp4=1.D0

! Solve for ratio rescaling to critical a*r^2+b*r+c=0; c=1, b=tmp1, a=tmp2
! Check determinant

        tmp3=tmp1*tmp1-4.D0*tmp2*tmp4

        if ((tmp3.ge._ZERO_).and.(tmp2.lt._ZERO_)) then
               tmp3=(-tmp1+sqrt(tmp3))/(2.D0*tmp2)
        elseif ((tmp3.ge._ZERO_).and.(tmp3.gt._ZERO_)) then
               tmp3=(-tmp1-sqrt(tmp3))/(2.D0*tmp2)
        else
! there is no root i.e. direction of [Gh, Gv] is in stable sector say tmp3>1 for no rescaling
               tmp3=2.D0
        endif

        if ((tmp3.gt.0.D0).and.(tmp3.lt.1.D0)) then
           tmp0=min(tmp0,tmp3)
        endif

        endif

! only rescale if R*Gh is less than Gh and same sign, i.e. tmp0 between 0 & 1
        if ((tmp0.gt.0.D0).and.(tmp0.lt.1.D0)) then
           Gh = tmp0*Gh
           Gm = tmp0*Gm
           Gv = tmp0*Gv
           Gs = tmp0*Gs
        endif

        tmp1=(h15_Shn0+h15_Shnh*Gh+h15_Shns*Gs+h15_Shnv*Gv)
        if (tmp1.lt._ZERO_) then
           Sh=small
        else
           tmp2=(1.D0+h15_Shdah*Gh+h15_Shdav*Gv)*                                       &
                (1.D0+h15_Shdbh*Gh)+                                                    &
                (h15_Shdv+h15_Shdvh*Gh+h15_Shdvv*Gv)*Gv
           if (tmp2.le._ZERO_) then
              Sh=h15_Sxmax
           else
              Sh=min(max(small,tmp1/tmp2),h15_Sxmax)
           endif
        endif

        tmp2=(1.D0+h15_Ssdh*Gh+h15_Ssdv*Gv)
        if (tmp2.lt._ZERO_) then
           Ss=h15_Sxmax
        else
           Ss=min(max(small,h15_Ssn0/tmp2),h15_Sxmax)
        endif

        tmp1 = (h15_Smn0+h15_SmnhSh*Gh*Sh+h15_SmnsSs*Gs*Ss)
! avoid singularity in Sm from denominator that would cancel -- still need to derive the expression but for now just sidestep
        if ((tmp1.lt.small).and.(tmp1.ge._ZERO_)) then
           Gh=Gh+small
           Gv=Gv+small
           tmp1 = (h15_Smn0+h15_SmnhSh*Gh*Sh+h15_SmnsSs*Gs*Ss)
        elseif((tmp1.gt.-small).and.(tmp1.lt._ZERO_)) then
           Gh=Gh-small
           Gv=Gv-small
           tmp1 = (h15_Smn0+h15_SmnhSh*Gh*Sh+h15_SmnsSs*Gs*Ss)
        endif

        if (tmp1.lt._ZERO_) then
           Sm=small
        else
           tmp2=(1.D0+h15_Smdh*Gh+h15_Smdv*Gv)
           if (tmp2.le._ZERO_) then
              Sm=h15_Sxmax
           else
              Sm=min(max(small,tmp1/tmp2),h15_Sxmax)
           endif
        endif

        Ss=Ss*SPF(i)

        cmue1(i) =  sqrt(2.D0)*Sm
        cmue2(i) =  sqrt(2.D0)*Sh
        cmue3(i) =  sqrt(2.D0)*Ss

        sq_var(i) = sqrt(sq**2 + (0.41_rk*Sh)**2)
        sl_var(i) = sqrt(sl**2 + (0.41_rk*Sh)**2)

     end do

     return
   end subroutine cmue_d_h15


!-----------------------------------------------------------------------

!$Id: cmue_gen.F90,v 1.1 2004-01-27 08:28:28 lars Exp $
#include"cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: The equilibrium, linear stability function\label{sec:cmue_gen}
! 
! !INTERFACE:
   subroutine cmue_gen(nlev)
!
! !DESCRIPTION:
!
!  This a trial version of the generic stability function. 
!  Different sets of model parameters are available by choosing
!  the parameter scnd_coeff.
!  \begin{equation}
!   \label{vdng}
!    \begin{array}{rcl}
!    D         &=& d_0 
!               +  d_1 \overline{N}^2  + d_2 \overline{S}^2      
!               +  d_3 \overline{N}^2 \overline{S}^2  
!               + d_4 \overline{N}^4   + d_5 \overline{S}^4                  \comma \\[3mm]
!    N_n        &=& n_0 
!               +  n_1 \overline{N}^2  + n_2 \overline{S}^2                  \comma \\[3mm]
!    N_b       &=& n_{b0} 
!               +  n_{b1} \overline{N}^2 + n_{b2} \overline{S}^2             
!   \point  
!   \end{array}
!  \end{equation}
!  
!  The coefficients of $D$ are given by
!  \begin{equation}
!   \label{vdi}
!   \begin{array}{rcl}
!     d_0 &=& 36 {\cal N}^3 {\cal N}_b^2                                     \comma \\[3mm]
!     d_1 &=& 84 a_5 a_{b3} {\cal N}^2 {\cal N}_b  
!          +  36 a_{b5} {\cal N}^3 {\cal N}_b                                \comma \\[3mm]
!     d_2 &=&  9 (a_{b2}^2 - a_{b1}^2) {\cal N}^3 
!           + 12 ( 3 a_3^2 - a_2^2) {\cal N}   {\cal N}_b^2                  \comma \\[3mm]
!     d_3 &=& 12 (a_2 a_{b1} - 3 a_3 a_{b2} ) a_5 a_{b3}  {\cal N}
!           + 12 ( a_3^2 - a_2^2)  a_5 a_{b3} {\cal N}_b                            \\[3mm]
!         &+& 12 ( 3 a_3^2 - a_2^2) a_{b5} {\cal N} {\cal N}_b               \comma \\[3mm]
!     d_4 &=& 48 a_5^2 a_{b3}^2 {\cal N} + 36 a_5 a_{b3}  a_{b5} {\cal N}^2  \comma \\[3mm]
!     d_5 &=&  3 ( 3 a_3^2 - a_2^2)(a_{b2}^2 - a_{b1}^2) {\cal N} 
!     \comma 
!   \end{array}
!  \end{equation}
!  and the coefficients of the numerators are 
!  \begin{equation}
!   \label{vni}
!   \begin{array}{rcl}
!     n_0 &=& 36 a_1 {\cal N}^2 {\cal N}_b^2                                 \comma \\[3mm]
!     n_1 &=&-12 a_5 a_{b3} (a_{b1}+a_{b2}) {\cal N}^2  
!           -  8 a_5 a_{b3} (-6 a_1+a_2+3 a_3) {\cal N} {\cal N}_b                  \\[3mm]
!         &+& 36 a_1 a_{b5} {\cal N}^2 {\cal N}_b                            \comma \\[3mm]        
!     n_2 &=& 9 a_1 (a_{b2}^2 - a_{b1}^2){\cal N}^2               
!   \end{array}
!  \end{equation}
!  \begin{equation}
!   \label{vnbi}
!   \begin{array}{rcl}
!     n_{b0} &=& 12 a_{b3} {\cal N}^3 {\cal N}_b                             \comma \\[3mm]
!     n_{b1} &=& 12 a_5 a_{b3}^2 {\cal N}^2                                  \comma \\[3mm]
!     n_{b2} &=&  9 a_1 a_{b3} (a_{b1}-a_{b2}) {\cal N}^2  
!              +    a_{b3} (6 a_1 (a_2-3 a_3) 
!              -    4 (a_2^2-3 a_3^2) ) {\cal N} {\cal N}_b                  \comma
!   \end{array}
!  \end{equation}
!  
!
! !USES:
   use turbulence, only: an,as
   use turbulence, only: cmue1,cmue2
   use turbulence, only: cm0 
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)       :: nlev

!
! !DEFINED PARAMETERS:
   REALTYPE,  parameter                :: cc1GL78     =  3.6000
   REALTYPE,  parameter                :: cc2GL78     =  0.8000
   REALTYPE,  parameter                :: cc3GL78     =  1.2000
   REALTYPE,  parameter                :: cc4GL78     =  1.2000
   REALTYPE,  parameter                :: cc5GL78     =  0.0000
   REALTYPE,  parameter                :: cc6GL78     =  0.5000
   REALTYPE,  parameter                :: ct1GL78     =  3.0000
   REALTYPE,  parameter                :: ct2GL78     =  0.3333
   REALTYPE,  parameter                :: ct3GL78     =  0.3333
   REALTYPE,  parameter                :: ct4GL78     =  0.0000
   REALTYPE,  parameter                :: ct5GL78     =  0.3333
   REALTYPE,  parameter                :: cttGL78     =  0.8000

   REALTYPE,  parameter                :: cc1MY82     =  6.0000
   REALTYPE,  parameter                :: cc2MY82     =  0.3200
   REALTYPE,  parameter                :: cc3MY82     =  0.0000
   REALTYPE,  parameter                :: cc4MY82     =  0.0000
   REALTYPE,  parameter                :: cc5MY82     =  0.0000
   REALTYPE,  parameter                :: cc6MY82     =  0.0000
   REALTYPE,  parameter                :: ct1MY82     =  3.7280
   REALTYPE,  parameter                :: ct2MY82     =  0.0000
   REALTYPE,  parameter                :: ct3MY82     =  0.0000
   REALTYPE,  parameter                :: ct4MY82     =  0.0000
   REALTYPE,  parameter                :: ct5MY82     =  0.0000
   REALTYPE,  parameter                :: cttMY82     =  0.6102

   REALTYPE,  parameter                :: cc1KC94     =  6.0000
   REALTYPE,  parameter                :: cc2KC94     =  0.3200
   REALTYPE,  parameter                :: cc3KC94     =  0.0000
   REALTYPE,  parameter                :: cc4KC94     =  0.0000
   REALTYPE,  parameter                :: cc5KC94     =  0.0000
   REALTYPE,  parameter                :: cc6KC94     =  0.0000
   REALTYPE,  parameter                :: ct1KC94     =  3.7280
   REALTYPE,  parameter                :: ct2KC94     =  0.7000
   REALTYPE,  parameter                :: ct3KC94     =  0.7000
   REALTYPE,  parameter                :: ct4KC94     =  0.0000
   REALTYPE,  parameter                :: ct5KC94     =  0.2000
   REALTYPE,  parameter                :: cttKC94     =  0.6102

   REALTYPE,  parameter                :: cc1LDOR96   =  3.0000
   REALTYPE,  parameter                :: cc2LDOR96   =  0.8000
   REALTYPE,  parameter                :: cc3LDOR96   =  2.0000
   REALTYPE,  parameter                :: cc4LDOR96   =  1.1180
   REALTYPE,  parameter                :: cc5LDOR96   =  0.0000
   REALTYPE,  parameter                :: cc6LDOR96   =  0.5000
   REALTYPE,  parameter                :: ct1LDOR96   =  3.0000
   REALTYPE,  parameter                :: ct2LDOR96   =  0.3333
   REALTYPE,  parameter                :: ct3LDOR96   =  0.3333
   REALTYPE,  parameter                :: ct4LDOR96   =  0.0000
   REALTYPE,  parameter                :: ct5LDOR96   =  0.3333
   REALTYPE,  parameter                :: cttLDOR96   =  0.8000

   REALTYPE,  parameter                :: cc1CHCD01A  =  5.0000
   REALTYPE,  parameter                :: cc2CHCD01A  =  0.8000
   REALTYPE,  parameter                :: cc3CHCD01A  =  1.9680
   REALTYPE,  parameter                :: cc4CHCD01A  =  1.1360
   REALTYPE,  parameter                :: cc5CHCD01A  =  0.0000
   REALTYPE,  parameter                :: cc6CHCD01A  =  0.4000
   REALTYPE,  parameter                :: ct1CHCD01A  =  5.9500
   REALTYPE,  parameter                :: ct2CHCD01A  =  0.6000
   REALTYPE,  parameter                :: ct3CHCD01A  =  1.0000
   REALTYPE,  parameter                :: ct4CHCD01A  =  0.0000
   REALTYPE,  parameter                :: ct5CHCD01A  =  0.3333
   REALTYPE,  parameter                :: cttCHCD01A  =  0.7200

   REALTYPE,  parameter                :: cc1CHCD01B  =  5.0000
   REALTYPE,  parameter                :: cc2CHCD01B  =  0.6983
   REALTYPE,  parameter                :: cc3CHCD01B  =  1.9664
   REALTYPE,  parameter                :: cc4CHCD01B  =  1.0940
   REALTYPE,  parameter                :: cc5CHCD01B  =  0.0000
   REALTYPE,  parameter                :: cc6CHCD01B  =  0.4950
   REALTYPE,  parameter                :: ct1CHCD01B  =  5.6000
   REALTYPE,  parameter                :: ct2CHCD01B  =  0.6000
   REALTYPE,  parameter                :: ct3CHCD01B  =  1.0000
   REALTYPE,  parameter                :: ct4CHCD01B  =  0.0000
   REALTYPE,  parameter                :: ct5CHCD01B  =  0.3333
   REALTYPE,  parameter                :: cttCHCD01B  =  0.4770

   REALTYPE,  parameter                :: cc1CCH02    =  5.0000
   REALTYPE,  parameter                :: cc2CCH02    =  0.7983
   REALTYPE,  parameter                :: cc3CCH02    =  1.9680
   REALTYPE,  parameter                :: cc4CCH02    =  1.1360
   REALTYPE,  parameter                :: cc5CCH02    =  0.0000
   REALTYPE,  parameter                :: cc6CCH02    =  0.5000
   REALTYPE,  parameter                :: ct1CCH02    =  5.5200
   REALTYPE,  parameter                :: ct2CCH02    =  0.2134
   REALTYPE,  parameter                :: ct3CCH02    =  0.3570
   REALTYPE,  parameter                :: ct4CCH02    =  0.0000
   REALTYPE,  parameter                :: ct5CCH02    =  0.3333
   REALTYPE,  parameter                :: cttCCH02    =  0.8200

   integer, parameter                  :: GL78        = 1
   integer, parameter                  :: MY82        = 2
   integer, parameter                  :: KC94        = 3
   integer, parameter                  :: LDOR96      = 4
   integer, parameter                  :: CHCD01A     = 5
   integer, parameter                  :: CHCD01B     = 6
   integer, parameter                  :: CCH02       = 7

   integer, parameter                  :: scnd_coeff  = GL78




!
! !REVISION HISTORY: 
!  Original author(s): Lars Umlauf
!
!  $Log: cmue_gen.F90,v $
!  Revision 1.1  2004-01-27 08:28:28  lars
!  trial version of generic stability function
!
!
!EOP
!
!-----------------------------------------------------------------------
! !LOCAL VARIABLES:
!
     integer                 ::   i
     REALTYPE                ::   N,Nt
     REALTYPE                ::   d0,d1,d2,d3,d4,d5
     REALTYPE                ::   n0,n1,n2,nt0,nt1,nt2
     REALTYPE                ::   dCm,nCm,nCmp,cm3_inv,cm6_inv
     REALTYPE                ::   tmp0,tmp1,tmp2
     REALTYPE                ::   cc1,cc2,cc3,cc4,cc5,cc6
     REALTYPE                ::   ct1,ct2,ct3,ct4,ct5,ct6,ctt
     REALTYPE                ::   a1,a2,a3,a4,a5
     REALTYPE                ::   at1,at2,at3,at4,at5
     REALTYPE                ::   anKE,asKE
     REALTYPE                ::   asMax,asMaxNum,asMaxDen
     REALTYPE                ::   anMin,anMinNum,anMinDen
     
!-----------------------------------------------------------------------
!BOC


     select case (scnd_coeff)
     case (GL78)
        cc1     =    cc1GL78
        cc2     =    cc2GL78
        cc3     =    cc3GL78
        cc4     =    cc4GL78
        cc5     =    cc5GL78
        cc6     =    cc6GL78
        ct1     =    ct1GL78
        ct2     =    ct2GL78
        ct3     =    ct3GL78
        ct4     =    ct4GL78
        ct5     =    ct5GL78
        ctt     =    cttGL78
     case (MY82)
        cc1     =    cc1MY82
        cc2     =    cc2MY82
        cc3     =    cc3MY82
        cc4     =    cc4MY82
        cc5     =    cc5MY82
        cc6     =    cc6MY82
        ct1     =    ct1MY82
        ct2     =    ct2MY82
        ct3     =    ct3MY82
        ct4     =    ct4MY82
        ct5     =    ct5MY82
        ctt     =    cttMY82
     case (KC94)
        cc1     =    cc1LDOR96
        cc2     =    cc2LDOR96
        cc3     =    cc3LDOR96
        cc4     =    cc4LDOR96
        cc5     =    cc5LDOR96
        cc6     =    cc6LDOR96
        ct1     =    ct1LDOR96
        ct2     =    ct2LDOR96
        ct3     =    ct3LDOR96
        ct4     =    ct4LDOR96
        ct5     =    ct5LDOR96
        ctt     =    cttLDOR96
     case (CHCD01A)
        cc1     =    cc1CHCD01A
        cc2     =    cc2CHCD01A
        cc3     =    cc3CHCD01A
        cc4     =    cc4CHCD01A
        cc5     =    cc5CHCD01A
        cc6     =    cc6CHCD01A
        ct1     =    ct1CHCD01A
        ct2     =    ct2CHCD01A
        ct3     =    ct3CHCD01A
        ct4     =    ct4CHCD01A
        ct5     =    ct5CHCD01A
        ctt     =    cttCHCD01A
     case (CHCD01B)
        cc1     =    cc1CHCD01B
        cc2     =    cc2CHCD01B
        cc3     =    cc3CHCD01B
        cc4     =    cc4CHCD01B
        cc5     =    cc5CHCD01B
        cc6     =    cc6CHCD01B
        ct1     =    ct1CHCD01B
        ct2     =    ct2CHCD01B
        ct3     =    ct3CHCD01B
        ct4     =    ct4CHCD01B
        ct5     =    ct5CHCD01B
        ctt     =    cttCHCD01B
     case (CCH02)
        cc1     =    cc1CCH02
        cc2     =    cc2CCH02
        cc3     =    cc3CCH02
        cc4     =    cc4CCH02
        cc5     =    cc5CCH02
        cc6     =    cc6CCH02
        ct1     =    ct1CCH02
        ct2     =    ct2CCH02
        ct3     =    ct3CCH02
        ct4     =    ct4CCH02
        ct5     =    ct5CCH02
        ctt     =    cttCCH02
     case default
        
        STDERR '... not a valid parameter set'
        STDERR 'Choose different value for scnd_coeff'
        STDERR 'Program execution stopped ...'
        stop 'turbulence.F90'
        
     end select
     
     
     !  compute the a_i's for the Algebraic Stress Model
     
     a1   =  2./3. - cc2/2.
     a2   =  1.    - cc3/2.
     a3   =  1.    - cc4/2.
     a4   =          cc5/2.
     a5   =  1./2. - cc6/2.
     
     at1  =           1. - ct2
     at2  =           1. - ct3
     at3  =  2. *   ( 1. - ct4)
     at4  =  2. *   ( 1. - ct5)
     at5  =  2.*ctt*( 1. - ct5)
     
     

     !  compute the numerator and denominator of stability functions
     
     N    =   cc1/2.   
     Nt   =   ct1
     
     d0   =   36.* N**3. * Nt**2.
     d1   =   84.*a5*at3 * N**2. * Nt  + 36.*at5 * N**3. * Nt
     d2   =   9.*(at2**2.-at1**2.) * N**3. - 12.*(a2**2.-3.*a3**2.) * N * Nt**2.
     d3   =   12.*a5*at3*(a2*at1-3.*a3*at2) * N + 12.*a5*at3*(a3**2.-a2**2.) * Nt       &
            + 12.*at5*(3.*a3**2.-a2**2.) * N * Nt
     d4   =   48.*a5**2.*at3**2. * N + 36.*a5*at3*at5 * N**2.
     d5   =   3.*(a2**2.-3.*a3**2.)*(at1**2.-at2**2.) * N
     

     n0   =   36.*a1 * N**2. * Nt**2.
     n1   = - 12.*a5*at3*(at1+at2) * N**2. + 8.*a5*at3*(6.*a1-a2-3.*a3) * N * Nt        &
            + 36.*a1*at5 * N**2. * Nt
     n2   =   9.*a1*(at2**2.-at1**2.) * N**2.
     
     nt0  =   12.*at3 * N**3. * Nt
     nt1  =   12*a5*at3**2. * N**2.
     nt2  =   9.*a1*at3*(at1-at2) * N**2. + (  6.*a1*(a2-3.*a3)                         &
            - 4.*(a2**2.-3.*a3**2.) )*at3 * N * Nt 
     

     cm3_inv = 1./cm0**3.
     cm6_inv = 1./cm0**6.



     do i=1,nlev

        anKE      = cm6_inv*an(i)
        asKE      = cm6_inv*as(i)
        
        asMaxNum  = d0*n0 + (d0*n1+d1*n0)*anKE + (d1*n1+d4*n0)*anKE*anKE + d4*n1*anKE*anKE*anKE
        asMaxDen  = d2*n0 + (d2*n1+d3*n0)*anKE + d3*n1*anKE*anKE
        asMax     = asMaxNum / asMaxDen

        anMinNum  = d1 + nt0 + sqrt((d1+nt0)**2. + 4.*d0*(d4+nt1))
        anMinDen  = 2.*(d4+nt1)
        anMin     = - anMinNum / anMinDen


        tmp0      = -d0 - (d1 + nt0)*anKE - (d4 + nt1)*anKE*anKE
        tmp1      = -d2 + n0 + (n1-d3-nt2)*anKE
        tmp2      =  n2-d5
        
! uncomment this for equilibrium version (causes bug in c3-computation, sorry)
!        asKE =  (-tmp1 + sqrt(tmp1*tmp1-4.*tmp0*tmp2) )/(2.*tmp2)

! uncomment this for convective limiter
!        if (anKe.lt.anMin) anKe = anMin

! uncomment this for shear limiter
!        if (asKE.gt.asMax) asKE = asMax

        
        dCm      = d0  +  d1*anKE +  d2*asKE + d3*anKE*asKE + d4*anKE*anKE + d5*asKE*asKE 
        nCm      = n0  +  n1*anKE +  n2*asKE
        nCmp     = nt0 + nt1*anKE + nt2*asKE
        
        cmue1(i) =  cm3_inv*nCm/dCm
        cmue2(i) =  cm3_inv*nCmp/dCm
     end do
     
     
     return
   end subroutine cmue_gen
   

!-----------------------------------------------------------------------
!EOC

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------- 

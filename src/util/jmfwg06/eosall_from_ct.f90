subroutine eosall_from_ct(s,ct,p,rho,rho_s,rho_ct,rho_p)

!   in-situ density and its derivatives as functions of
!   salinity, conservative temperature and pressure, as in
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                           (psu)
!   ct               : conservative temperature           (deg C, ITS-90)
!   p                : gauge pressure                     (dbar)
!                      (absolute pressure - 10.1325 dbar)
!
!   rho              : in-situ density                    (kg m^-3)
!   rho_s            : partial derivative wrt s           (kg m^-3 psu^-1)
!   rho_ct           : partial derivative wrt ct          (kg m^-3 deg C^-1)
!   rho_p            : partial derivative wrt p           (kg m^-3 dbar^-1)
!
!   check values     : eosall_from_ct(20,20,1000,...) gives
!
!                               rho =  1017.842890411975
!                               rho_s =   0.7445335606836513
!                               rho_ct = -0.2479523311966256
!                               rho_p =   4.326286157945491d-3
!
!   DRJ on 10/12/03


implicit real*8(a-h,o-z)


ct2 = ct*ct; sqrts = sqrt(s)

anum =       9.9983912878771446d+02 + 	       &
        ct*( 7.0687133522652896d+00 + 	       &
        ct*(-2.2746841916232965d-02 + 	       &
        ct*  5.6569114861400121d-04)) +	       &
         s*( 2.3849975952593345d+00 + 	       &
        ct*  3.1761924314867009d-04 + 	       &
         s*  1.7459053010547962d-03)

aden =       1.0000000000000000d+00 + 	       &
        ct*( 7.0051665739672298d-03 + 	       &
        ct*(-1.5040804107377016d-05 + 	       &
        ct*( 5.3943915288426715d-07 + 	       &
        ct*  3.3811600427083414d-10))) +       &
         s*( 1.5599507046153769d-03 - 	       &
        ct*( 1.8137352466500517d-06 + 	       &
       ct2*  3.3580158763335367d-10) +	       &
     sqrts*( 5.7149997597561099d-06 + 	       &
       ct2*  7.8025873978107375d-10))


anum_s =     2.3849975952593345d+00 + 	       &
        ct*  3.1761924314867010d-04 + 	       &
         s*  3.4918106021095924d-03

aden_s =     1.5599507046153770d-03 +	       &
        ct*(-1.8137352466500517d-06 - 	       &
       ct2*  3.3580158763335367d-10) +	       &
     sqrts*( 8.5724996396341660d-06 + 	       &
       ct2*  1.1703881096716107d-09)

anum_ct =    7.0687133522652900d+00 +	       &
        ct*(-4.5493683832465930d-02 +	       &
        ct*  1.6970734458420036d-03)+ 	       &
	 s*  3.1761924314867010d-04

aden_ct =    7.0051665739672300d-03 +  	       &
        ct*(-3.0081608214754030d-05 + 	       &
        ct*( 1.6183174586528013d-06 +  	       &
        ct*  1.3524640170833366d-09)) +	       &
         s*(-1.8137352466500517d-06 -  	       &
       ct2*  1.0074047629000610d-09 + 	       &
  ct*sqrts*  1.5605174795621475d-09)

anum_p =     1.2192536310173776d-02 +	       &
       ct2*  2.4643435731663950d-07 +	       &
         s*  4.0525405332794890d-06

aden_p =     7.1038052872522840d-06


if(p.ne.0.d0) then

  pct = p*ct

  anum = anum +         p*( 1.2192536310173776d-02 +     &
                      ct2*  2.4643435731663949d-07 +     &
                        s*  4.0525405332794888d-06 -     &
                        p*( 2.3890831309113187d-08 +     &
                      ct2*  5.9016182471196891d-12))

  aden = aden +         p*( 7.1038052872522844d-06 -     &
                 pct*(ct2*  2.1692301739460094d-17 +     &
                        p*  8.2564080016458560d-18))


  anum_s = anum_s +     p*  4.0525405332794890d-06

  anum_ct = anum_ct + pct*( 4.9286871463327900d-07 -     &
                        p*  1.1803236494239378d-11)

  aden_ct = aden_ct -					 &
                 p*p*(ct2*  6.5076905218380280d-17 +     &
                        p*  8.2564080016458560d-18)      

  anum_p = anum_p -     p*( 4.7781662618226370d-08 +     &
                      ct2*  1.1803236494239378d-11)

  aden_p = aden_p -					 &
                 pct*(ct2*  4.3384603478920190d-17 +     & 
                        p*  2.4769224004937570d-17)     

end if


rec_aden = 1.0d0/aden


rho = anum*rec_aden


rho_s = (anum_s-aden_s*rho)*rec_aden


rho_ct = (anum_ct-aden_ct*rho)*rec_aden


rho_p = (anum_p-aden_p*rho)*rec_aden


!      saline contraction coefficient is rho_s/rho
!
!      thermal expansion coefficient is -rho_ct/rho
!
!      sound speed is 1.0d2/sqrt(rho_p)
!
!      this subroutine should always be run in double precision
!      (since rho is returned rather than sigma = rho-1.0d3)


return
end
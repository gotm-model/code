subroutine eosall_from_theta(s,th,p,rho,rho_s,rho_th,rho_p)

!   in-situ density and its derivatives as functions of
!   salinity, potential temperature and pressure, as in
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                           (psu)
!   th               : potential temperature              (deg C, ITS-90)
!   p                : gauge pressure                     (dbar)
!                      (absolute pressure - 10.1325 dbar)
!
!   rho              : in-situ density                    (kg m^-3)
!   rho_s            : partial derivative wrt s           (kg m^-3 psu^-1)
!   rho_th           : partial derivative wrt th          (kg m^-3 deg C^-1)
!   rho_p            : partial derivative wrt p           (kg m^-3 dbar^-1)
!
!   check values     : eosall_from_theta(20,20,1000,...) gives
!
!                               rho =  1017.728868019642
!                               rho_s =   0.7510471164699279
!                               rho_th = -0.2570255211349140
!                               rho_p =   4.317589133273301d-3
!
!   DRJ on 10/12/03


implicit real*8(a-h,o-z)


th2 = th*th; sqrts = sqrt(s)

anum =       9.9984085444849347d+02 + 	       &
        th*( 7.3471625860981584d+00 + 	       &
        th*(-5.3211231792841769d-02 + 	       &
        th*  3.6492439109814549d-04)) +	       &
         s*( 2.5880571023991390d+00 - 	       &
        th*  6.7168282786692355d-03 + 	       &
         s*  1.9203202055760151d-03)

aden =       1.0000000000000000d+00 + 	       &
        th*( 7.2815210113327091d-03 + 	       &
        th*(-4.4787265461983921d-05 + 	       &
        th*( 3.3851002965802430d-07 + 	       &
        th*  1.3651202389758572d-10))) +       &
         s*( 1.7632126669040377d-03 - 	       &
        th*( 8.8066583251206474d-06 + 	       &
       th2*  1.8832689434804897d-10) +	       &
     sqrts*( 5.7463776745432097d-06 + 	       &
       th2*  1.4716275472242334d-09))


anum_s =     2.5880571023991390d+00 - 	       &
        th*  6.7168282786692355d-03 + 	       &
         s*  3.8406404111520300d-03

aden_s =     1.7632126669040377d-03 +	       &
        th*(-8.8066583251206470d-06 - 	       &
       th2*  1.8832689434804897d-10) +	       &
     sqrts*( 8.6195665118148150d-06 + 	       &
       th2*  2.2074413208363504d-09)

anum_th =    7.3471625860981580d+00 +	       &
        th*(-1.0642246358568354d-01 +	       &
        th*  1.0947731732944364d-03)- 	       &
	 s*  6.7168282786692355d-03

aden_th =    7.2815210113327090d-03 +  	       &
        th*(-8.9574530923967840d-05 + 	       &
        th*( 1.0155300889740728d-06 +  	       &
        th*  5.4604809559034290d-10)) +	       &
         s*(-8.8066583251206470d-06 -  	       &
       th2*  5.6498068304414700d-10 + 	       &
  th*sqrts*  2.9432550944484670d-09)

anum_p =     1.1798263740430364d-02 +	       &
       th2*  9.8920219266399120d-08 +	       &
         s*  4.6996642771754730d-06

aden_p =     6.7103246285651894d-06


if(p.ne.0.d0) then

  pth = p*th

  anum = anum +         p*( 1.1798263740430364d-02 +     &
                      th2*  9.8920219266399117d-08 +     &
                        s*  4.6996642771754730d-06 -     &
                        p*( 2.5862187075154352d-08 +     &
                      th2*  3.2921414007960662d-12))

  aden = aden +         p*( 6.7103246285651894d-06 -     &
                 pth*(th2*  2.4461698007024582d-17 +     &
                        p*  9.1534417604289062d-18))


  anum_s = anum_s +     p*  4.6996642771754730d-06

  anum_th = anum_th + pth*( 1.9784043853279823d-07 -     &
                        p*  6.5842828015921320d-12)

  aden_th = aden_th -                                    &
                 p*p*(th2*  7.3385094021073750d-17 +     &
                        p*  9.1534417604289060d-18)      

  anum_p = anum_p -     p*( 5.1724374150308704d-08 +     &
                      th2*  6.5842828015921320d-12)

  aden_p = aden_p -                                      &
                 pth*(th2*  4.8923396014049170d-17 +     & 
                        p*  2.7460325281286720d-17)     

end if


rec_aden = 1.0d0/aden


rho = anum*rec_aden


rho_s = (anum_s-aden_s*rho)*rec_aden


rho_th = (anum_th-aden_th*rho)*rec_aden


rho_p = (anum_p-aden_p*rho)*rec_aden


!      saline contraction coefficient is rho_s/rho
!
!      thermal expansion coefficient is -rho_th/rho
!
!      sound speed is 1.0d2/sqrt(rho_p)
!
!      this subroutine should always be run in double precision
!      (since rho is returned rather than sigma = rho-1.0d3)


return
end

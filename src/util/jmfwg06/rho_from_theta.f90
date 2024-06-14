function rho_from_theta(s,th,p)

!   in-situ density from potential temperature, as in 
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                           (psu)
!   th               : potential temperature              (deg C, ITS-90)
!   p                : gauge pressure                     (dbar)
!                      (absolute pressure - 10.1325 dbar)
!
!   rho_from_theta   : in-situ density                    (kg m^-3)
!
!   check value      : rho_from_theta(20,20,1000) = 1017.728868019642
!
!   DRJ on 10/12/03


implicit real*8(a-h,o-z)


th2 = th*th; sqrts = sqrt(s)

anum =          9.9984085444849347d+02 +    &
           th*( 7.3471625860981584d+00 +    &
           th*(-5.3211231792841769d-02 +    &
           th*  3.6492439109814549d-04)) +  &
            s*( 2.5880571023991390d+00 -    &
           th*  6.7168282786692355d-03 +    &
            s*  1.9203202055760151d-03) 

aden =          1.0000000000000000d+00 +    &
           th*( 7.2815210113327091d-03 +    &
           th*(-4.4787265461983921d-05 +    &
           th*( 3.3851002965802430d-07 +    &
           th*  1.3651202389758572d-10))) + &
            s*( 1.7632126669040377d-03 -    &
           th*( 8.8066583251206474d-06 +    &
          th2*  1.8832689434804897d-10) +   &
        sqrts*( 5.7463776745432097d-06 +    &
          th2*  1.4716275472242334d-09))


if(p.ne.0.d0) then

    pth = p*th
                                    
    anum = anum +        p*( 1.1798263740430364d-02 +   & 
                       th2*  9.8920219266399117d-08 +   & 
                         s*  4.6996642771754730d-06 -   & 
                         p*( 2.5862187075154352d-08 +   & 
                       th2*  3.2921414007960662d-12))    

    aden = aden +        p*( 6.7103246285651894d-06 -   &
                  pth*(th2*  2.4461698007024582d-17 +   &
                         p*  9.1534417604289062d-18))   

end if


rho_from_theta = anum/aden


!	Note:   this function should always be run in double precision
!               (since rho is returned rather than sigma = rho-1.0d3)


return
end

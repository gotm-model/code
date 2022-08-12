function fp_t(s,p,sat)

!   in situ temperature freezing point of seawater, as in
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                               (psu)
!   p                : gauge pressure                         (dbar)
!                      (absolute pressure - 10.1325 dbar)
!   sat              : string variable
!                       'air-sat'  - air saturated water       
!                       'air-free' - air free water
!
!   fp_t             : in-situ freezing temperature           (deg C, ITS-90)
!
!   check value      : fp_t(35,200,'air-sat')   = -2.072991753480427 deg C
!                      fp_t(35,200,'air-free') = -2.070973701805972 deg C

!   DRJ on 2/6/04


implicit real*8(a-h,o-z)

character*(*) sat


sqrts = sqrt(s); s2 = s*s  

tf_num =                   2.5180516744541290d-03     +      &
                       s*(-5.8946669548576310d-02     +      &
                   sqrts*( 2.4811422319110776d-03     +      &
                   sqrts*(-3.1930091631496098d-04     +      &
                      s2*( 1.5637174143955485d-08)))) -      &
                       p*( 7.4276961814810053d-04     +      &
                       p*  1.4312216596227918d-08)

tf_den =                   1.0000000000000000d+00     +      &
                       p*(-1.9625518786831890d-06     +      &
                       p*  7.0588565064816584d-11)    -      &
                s2*sqrts*  4.3301568126998630d-07 


fp_t = tf_num/tf_den;


if(sat.eq.'air-sat') then  
    fp_t = fp_t           -2.5180516744541290d-03     +      &
                      s *  1.4285714285714290d-05
elseif(sat.eq.'air-free') then  
    continue
else  
    print *, '***   Error in fp_t.f90: invalid third argument   ***'
    print *
    stop
endif


return
end
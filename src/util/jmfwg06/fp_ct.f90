function fp_ct(s,p,sat)

!   conservative temperature freezing point of seawater, as in
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                               (psu)
!   p                : gauge pressure                         (dbar)
!                      (absolute pressure - 10.1325 dbar)
!   sat              : string variable
!                       'air-sat'  - air saturated water       
!                       'air-free' - air free water
!
!   fp_ct            : conservative freezing temperature      (deg C, ITS-90)
!
!   check value      : fp_ct(35,2000,'air-sat')   = -2.073223432555101 deg C
!                      fp_ct(35,2000,'air-free') = -2.071222603621528 deg C
!
!   DRJ on 2/6/03


implicit real*8(a-h,o-z)

character*(*) sat


sqrts = sqrt(s) 

tf_num =                    1.7945004324529630d-02    +      &
                        s*(-5.8403584591688665d-02    +      &
                    sqrts*( 2.4573268704237757d-03    -      &
                    sqrts*  3.4327919114658586d-04))  +      &
                        p*(-7.3981255037990307d-04    +      &
                        p*(-7.3845034467503930d-09    +      &
                        s*  1.9069793902937708d-11))

tf_den =                    1.0000000000000000d+00    +      &
                        p*(-1.7509421027054954d-05    +      &
                        p*  5.2153095812720787d-10)   +      &
                s*s*sqrts*  1.4719680395528758d-06 


fp_ct = tf_num/tf_den


if(sat.eq.'air-sat') then  
    fp_ct = fp_ct          -2.661425530980574d-03     +      &
                       s *  1.887418849738127d-05;
elseif(sat.eq.'air-free') then  
    continue
else  
    print *, '***   Error in fp_ct.f90: invalid third argument   ***'
    print *
    stop
endif


return
end

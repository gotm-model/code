function rho_from_ct(s,ct,p)

!   in-situ density from conservative temperature, as in 
!   Jackett, McDougall, Feistel, Wright and Griffies (2003), submitted JAOT
!
!   s                : salinity                           (psu)
!   ct               : conservative temperature           (deg C, ITS-90)
!   p                : gauge pressure                     (dbar)
!                      (absolute pressure - 10.1325 dbar)
!
!   rho_from_ct      : in-situ density                    (kg/m^3)
!
!   check value      : rho_from_ct(20,20,1000) = 1017.842890411975
!
!   DRJ on 10/12/03


implicit real*8(a-h,o-z)


ct2 = ct*ct; sqrts = sqrt(s)

anum =          9.9983912878771446d+02 +    &
           ct*( 7.0687133522652896d+00 +    &
           ct*(-2.2746841916232965d-02 +    &
           ct*  5.6569114861400121d-04)) +  &
            s*( 2.3849975952593345d+00 +    &
           ct*  3.1761924314867009d-04 +    &
            s*  1.7459053010547962d-03) 

aden =          1.0000000000000000d+00 +    &
           ct*( 7.0051665739672298d-03 +    &
           ct*(-1.5040804107377016d-05 +    &
           ct*( 5.3943915288426715d-07 +    &
           ct*  3.3811600427083414d-10))) + &
            s*( 1.5599507046153769d-03 -    &
           ct*( 1.8137352466500517d-06 +    &
          ct2*  3.3580158763335367d-10) +   &
        sqrts*( 5.7149997597561099d-06 +    &
          ct2*  7.8025873978107375d-10))


if(p.ne.0.d0) then

    pct = p*ct

    anum = anum +                           &
             p*( 1.2192536310173776d-02 +   & 
           ct2*  2.4643435731663949d-07 +   & 
             s*  4.0525405332794888d-06 -   & 
             p*( 2.3890831309113187d-08 +   & 
           ct2*  5.9016182471196891d-12))    

    aden = aden +                           &
             p*( 7.1038052872522844d-06 -   &
      pct*(ct2*  2.1692301739460094d-17 +   &
             p*  8.2564080016458560d-18))   

end if


rho_from_ct = anum/aden


!	Note:   this function should always be run in double precision
!               (since rho is returned rather than sigma = rho-1.0d3)


return
end

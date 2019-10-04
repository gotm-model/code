function ct_from_t(s,t,p)

!   conservative temperature from in situ temperature, as in
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                           (psu)
!   t                : in-situ temperature                (deg C, ITS-90)
!   p                : gauge pressure                     (dbar)
!                      (absolute pressure - 10.1325 dbar)
!
!   ct_from_t        : conservative temperature           (deg C, ITS-90) 
!
!   calls            : ct_from_theta and theta_from_t
!
!   check value      : ct_from_t(20,20) = 20.27690206780196
!
!   DRJ on 10/12/03


implicit real*8(a-h,o-z)


pr = 0.d0

theta = theta_from_t(s,t,p,pr)


ct_from_t = ct_from_theta(s,theta)


return
end

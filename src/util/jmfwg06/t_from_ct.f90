function t_from_ct(s,ct,p)

!   in-situ temperature from conservative temperature
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                           (psu)
!   ct               : conservative temperature           (deg C, ITS-90)
!   p                : gauge pressure                     (dbar)
!                      (absolute pressure - 10.1325 dbar)
!
!   t                : in-situ temperature                (deg C, ITS-90)
!
!   calls            : theta_from_ct and theta_from_t
!
!   check value      : t_from_ct(20,20,1000) = 19.72671624220263 with 1
!                                                      loop in theta_from_ct
!                      t_from_ct(20,20,1000) = 19.72671627735695 with 2
!                                                      loops in theta_from_ct

!   DRJ on 11/10/03

implicit real*8(a-h,o-z)


pr0 = 0.d0 

theta = theta_from_ct(s,ct)


t_from_ct = theta_from_t(s,theta,pr0,p)


return
end

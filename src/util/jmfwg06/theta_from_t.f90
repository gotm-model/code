function theta_from_t(s,t,p,pr)

!   potential temperature from in-situ temperature, as in 
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                           (psu)
!   t                : in-situ temperature                (deg C, ITS-90)
!   p                : gauge pressure                     (dbar)
!                      (absolute pressure - 10.1325 dbar)
!   pr               : reference pressure                 (dbar)
!
!   theta_from_t     : potential temperature              (deg C, ITS-90)
!
!   calls            : de_dt_F and entropy_diff_F
!
!   check values     : theta_from_t(35,20,4000,0) = 19.21108374301637
!                                                           (with nloops=1)
!                      theta_from_t(35,20,4000,0) = 19.21108374301171
!                                                           (with nloops=2)
!
!   DRJ on 10/12/03


implicit real*8(a-h,o-z)
 

th0 = t+(p-pr)*( 8.65483913395442d-6   - &
             s*  1.41636299744881d-6   - &
        (p+pr)*  7.38286467135737d-9   + &
             t*(-8.38241357039698d-6   + &
             s*  2.83933368585534d-8   + &
             t*  1.77803965218656d-8   + &
        (p+pr)*  1.71155619208233d-10))


de_dt = 13.6d0

nloops = 2					! default

!    NOTE: nloops = 1 gives theta with a maximum error of 5.48x10^-06
!          nloops = 2 gives theta with a maximum error of 2.84x10^-14
	
n = 1 

do while(n.le.nloops)
  dentropy = entropy_diff_F(s,t,p,th0,pr)
  theta = th0-dentropy/de_dt
  theta = 0.5d0*(theta+th0)
  de_dt = de_dt_F(s,theta,pr)
  theta = th0-dentropy/de_dt
  n = n+1; th0 = theta
end do


theta_from_t = th0


return
end

function cp_F(s,t,p)

!   heat capacity (specific heat) from d(entropy)/dt, as in
!   Feistel (2003), Prog. Ocean. 58, 43-114
!
!   s                : salinity                           (psu)
!   t                : in-situ temperature                (deg C, ITS-90)
!   p                : gauge pressure                     (dbar)
!                      (absolute pressure - 10.1325 dbar)
!           
!   cp_F             : heat capacity                      (J/kgK)
!
!   calls            : de_dt_F
!
!   check value      : cp_F(20,20,1000) = 4047.390131368573
!
!   DRJ on 11/12/03


implicit real*8(a-h,o-z)
 

cp_F = (t+273.15d0)*de_dt_F(s,t,p)


return
end

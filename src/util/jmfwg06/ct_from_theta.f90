function ct_from_theta(s,theta)

!   conservative temperature from potential temperature, as in
!   Jackett, McDougall, Feistel, Wright and Griffies (2004), submitted JAOT
!
!   s                : salinity                  (psu)
!   theta            : potential temperature     (deg C, ITS-90)
!
!   ct_from_theta    : conservative temperature  (deg C, ITS-90) 
!
!   calls            : penthalpy_F
!
!   check value      : ct_from_theta(20,20) = 20.45274961282756
!
!   DRJ on 10/12/03


implicit real*8(a-h,o-z)


rec_Cp0 = 2.50494524832013d-4; !Cp0 = 3992.10322329649d0
	

ct_from_theta = rec_Cp0*penthalpy_F(s,theta)


return
end

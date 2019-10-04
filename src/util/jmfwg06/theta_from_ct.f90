function theta_from_ct(s,ct)

!   potential temperature from conservative temperature, as in
!   Jackett, McDougall, Feistel, Wright & Griffies (2004), submitted JAOT

!   s                : salinity                  (psu)
!   ct               : conservative temperature  (deg C, ITS-90)
 
!   theta_from_ct    : potential temperature     (deg C, ITS-90)

!   calls            : cp0_F and ct_from_theta

!   check value      : theta_from_ct(20,20) = 19.55627910604363 

!   DRJ on 03/06/05


implicit real*8(a-h,o-z)
 

a0 = -1.446013646344788d-2
a1 = -3.305308995852924d-3
a2 =  1.062415929128982d-4
a3 =  9.477566673794488d-1
a4 =  2.166591947736613d-3
a5 =  3.828842955039902d-3

b0 =  1.000000000000000d+0
b1 =  6.506097115635800d-4
b2 =  3.830289486850898d-3
b3 =  1.247811760368034d-6	

   
a5ct = a5*ct; b3ct = b3*ct

ct_factor = (a3+a4*s+a5ct)

th0_num = a0+s*(a1+a2*s)+ct*ct_factor

rec_th0_den = 1.0d0/(b0+b1*s+ct*(b2+b3ct))

th0 = th0_num*rec_th0_den

ct0 = ct_from_theta(s,th0) 

dth_dct = (ct_factor+a5ct-(b2+b3ct+b3ct)*th0)*rec_th0_den         

theta = th0-(ct0-ct)*dth_dct


rec_Cp0 = 2.50494524832013d-4; ! Cp0 = 3992.10322329649;
	
dct = ct_from_theta(s,theta)-ct;

dct_dth = (theta+273.15d0)*de_dt0_F(s,theta)*rec_Cp0


theta_from_ct = theta-dct/dct_dth


!       NOTE: theta has a maximum error of 6.0x10^-14


return                                                                  
end

function penthalpy_F(s,th)

!   potential enthalpy from differentiating the Gibbs potential in
!   Feistel (2003), Prog. Ocean. 58, 43-114
!
!   s                : salinity (psu)
!   th               : potential temperature (deg C, ITS-90)
!
!   penthalpy_F      : potential enthalpy (J/kg)
!
!   check value      : penthalpy_F(20,20) = 81649.4876546448
!
!   DRJ on 10/12/03


implicit real*8(a-h,o-z)


x2 = 2.5d-2*s; x = sqrt(x2); y = 2.5d-2*th;


penthalpy_F = 61.013624165232955d0 + y*(168776.46138048015d0        + &
                 y*(-2735.2785605119643d0 + y*(2574.2164453821442d0 + &
                   y*(-1536.6644434977545d0 + y*(545.734049793163d0 + &
             (-50.910917284743334d0 - 18.30489878927802d0*y)*y))))) + &
                  x2*(416.31512917743896d0 + x*(937.9793807560891d0 + &
                   x*(-3140.435779506947d0 + x*(2975.170149976973d0 + &
                 x*(-1760.137081144729d0 + x*414.5655751783703d0))) + &
                   y*(2167.72082596016d0 + y*(-1224.5772800562902d0 + &
                  y*(326.3074029273967d0 + 50.6703824689518d0*y)))) + &
                    y*(-12694.10018182362d0 + y*(4405.71847182968d0 + &
                 y*(-2132.9690185026416d0 + y*(303.91071982808035d0 + &
                                            69.74975368852d0*y)))))


return
end

function de_dt0_F(s,th)

!   d(entropy)/dt from twice differentiating the Gibbs potential in
!   Feistel (2003), Prog. Ocean. 58, 43-114
!
!   s                : salinity                           (psu)
!   th               : potential temperature              (deg C, ITS-90)
!
!   de_dt0_F         : d(entropy)/dt                      (J/kgK^2)
!
!   check value      : de_dt0_F(35,20) = 13.63256369213874
!
!   DRJ on 30/06/05


implicit real*8(a-h,o-z)


x2 = 2.5d-2*s; x = sqrt(x2); y = 2.5d-2*th

de_dt = 24715.571866078d0 + x2*(-1858.920033948178d0 + &
                x*(317.440355256842d0 + y*(-405.1392883572282d0 + &
               202.6815298758072d0*y)) + y*(1562.563716288858d0 + &
               y*(-1165.8752731900836d0 + 348.7487684426d0*y))) + &
              y*(-4420.4472249096725d0 + y*(1778.231237203896d0 + &
                 y*(-1160.5182516851419d0 + (569.531539542516d0 - &
                                   128.13429152494615d0*y)*y)))




de_dt0_F = 6.25d-4*de_dt


return
end

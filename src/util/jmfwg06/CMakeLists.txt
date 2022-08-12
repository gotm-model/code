# http://www.teos-10.org/preteos10_software/JMFWG06.html
add_library(jmfwg06
   jmfwg06/cp_F.f90
   jmfwg06/ct_from_t.f90
   jmfwg06/ct_from_theta.f90
   jmfwg06/de_dt0_F.f90
   jmfwg06/de_dt_F.f90
   jmfwg06/entropy_diff_F.f90
   jmfwg06/eosall_from_ct.f90
   jmfwg06/eosall_from_theta.f90
   jmfwg06/fp_ct.f90
   jmfwg06/fp_t.f90
   jmfwg06/fp_theta.f90
   jmfwg06/penthalpy_F.f90
   jmfwg06/rho_F.f90
   jmfwg06/rho_from_ct.f90
   jmfwg06/rho_from_theta.f90
   jmfwg06/t_from_ct.f90
   jmfwg06/theta_from_ct.f90
   jmfwg06/theta_from_t.f90
)
set_property(TARGET jmfwg06 PROPERTY FOLDER jmfwg06)

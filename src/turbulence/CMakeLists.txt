add_library(turbulence
   algebraiclength.F90
   alpha_mnb.F90
   cmue_a.F90
   cmue_b.F90
   cmue_c.F90
   cmue_d.F90
   cmue_ma.F90
   cmue_rf.F90
   cmue_sg.F90
   compute_cpsi3.F90
   compute_rist.F90
   dissipationeq.F90
   epsbalgebraic.F90
   fk_craig.F90
   genericeq.F90
   gotm_lib_version.F90
   internal_wave.F90
   ispralength.F90
   kbalgebraic.F90
   kbeq.F90
   kpp.F90
   lengthscaleeq.F90
   potentialml.F90
   production.F90
   q2over2eq.F90
   r_ratio.F90
   tkealgebraic.F90
   tkeeq.F90
   turbulence.F90
   algebraiclength.F90
   alpha_mnb.F90
   cmue_a.F90
   cmue_b.F90
   cmue_c.F90
   cmue_d.F90
   cmue_ma.F90
   cmue_rf.F90
   cmue_sg.F90
   compute_cpsi3.F90
   compute_rist.F90
   dissipationeq.F90
   epsbalgebraic.F90
   fk_craig.F90
   genericeq.F90
   gotm_lib_version.F90
   internal_wave.F90
   ispralength.F90
   kbalgebraic.F90
   kbeq.F90
   kpp.F90
   lengthscaleeq.F90
   potentialml.F90
   production.F90
   q2over2eq.F90
   r_ratio.F90
   tkealgebraic.F90
   tkeeq.F90
   turbulence.F90
   variances.F90
)
target_link_libraries(turbulence PRIVATE util yaml)
target_include_directories(turbulence PUBLIC $<BUILD_INTERFACE:$<TARGET_PROPERTY:turbulence,Fortran_MODULE_DIRECTORY>> $<INSTALL_INTERFACE:include>)
add_library(gotm::turbulence ALIAS turbulence)
set_property(TARGET turbulence PROPERTY FOLDER gotm)

install(TARGETS turbulence 
   EXPORT gotmConfig
   LIBRARY DESTINATION lib 
   ARCHIVE DESTINATION lib
)
install(FILES 
   $<TARGET_PROPERTY:turbulence,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/turbulence.mod
   DESTINATION include
)

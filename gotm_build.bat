@rem Script to build the gotm executable uing CMake

@set old=%cd%
@rem echo %old%

@echo Build directory:
@if "%build_dir%"=="" ( @set build_dir=%UserProfile%\build\gotm ) else ( @echo build_dir is set )
@echo %build_dir%
@chdir "%build_dir%"

@echo Default Fortran compiler is ifort
@set compiler=ifort

@chdir "%compiler%"
@echo Ready to build/compile:
@rem cmake --build . --clean-first --config Release --target INSTALL
cmake --build . --config Release --target INSTALL

@pause

@chdir ..
@chdir %old%

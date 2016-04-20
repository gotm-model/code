@rem Script to build the gotm executable uing CMake

@set old=%cd%

@echo Build directory:
@if "%build_dir%"=="" ( @set build_dir=%TEMP%\build\gotm ) else ( @echo build_dir is set )
@echo %build_dir%
@chdir "%build_dir%"

@echo Default Fortran compiler is ifort
@set compiler=ifort

@echo Ready to build/compile:
@rem cmake --build . --clean-first --config Release --target INSTALL
cmake --build . --config Release --target INSTALL

@pause

@chdir %old%

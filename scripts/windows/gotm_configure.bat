@rem Script to configure the gotm executable uing CMake

@set old=%cd%

@echo Build directory:
@if "%build_dir%"=="" ( @set build_dir=%TEMP%\build\gotm ) else ( @echo build_dir is set )
@echo %build_dir%
@if not EXIST "%build_dir%\." ( @mkdir "%build_dir%" )
@chdir "%build_dir%"

@echo Base directories:
@set GOTM_BASE=%USERPROFILE%\Documents\GOTM\code
@set FABM_BASE=%USERPROFILE%\Documents\FABM\code
@echo %GOTM_BASE%
@echo %FABM_BASE%

@echo Default Fortran compiler is ifort
@set compiler=ifort

@echo Install directory:
@set install_prefix=%APPDATA%\gotm
@echo %install_prefix%

@echo Ready to configure:
cmake "%GOTM_BASE%\src" ^
      -DGOTM_EMBED_VERSION=on ^
      -DGOTM_USE_FLEXIBLE_OUTPUT=on ^
      -DGOTM_USE_FABM=on ^
      -DFABM_BASE="%FABM_BASE%" ^
      -DCMAKE_Fortran_COMPILER=%compiler% ^
      -DCMAKE_INSTALL_PREFIX="%install_prefix%"

@pause

@chdir ..\
@chdir %old%

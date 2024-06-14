! This file is include in all .F90 files and contains very important
! definitions. Infact GOTM will not compile when this file is not
! in a correct format.
! KBK 20000220

#define PATH_MAX	255

#define stderr		0
#define stdout		6

! Handy for writing
#define STDOUT write(stdout,*)
#define STDERR write(stderr,*)
#define LEVEL0 STDERR
#define LEVEL1 STDERR '   ',
#define LEVEL2 STDERR '       ',
#define LEVEL3 STDERR '           ',
#define LEVEL4 STDERR '               ',
#define FATAL  STDERR 'FATAL ERROR: ',

#define LINE "------------------------------------------------------------------------"

! Shapes for variables
#define POINT           0
#define Z_SHAPE         1
#define T_SHAPE         2
#define XY_SHAPE        3
#define XYT_SHAPE       4
#define XYZT_SHAPE      5

#define RAWBINARY       0
#define ASCII           1
#define NETCDF          2
#define GRADS           3
#define OPENDX          4

! For easier reading
#define READING 0
#define WRITING 1

! To avoid dividing by zero
#define SMALL 1e-8

! What precision will we use in this compilation
#define SINGLE
#undef  SINGLE

#ifdef SINGLE
#define REALTYPE real(kind=selected_real_kind(6))
!#define MPI_REALTYPE	MPI_REAL
#define _ZERO_ 0.0
#define _HALF_ 0.5
#define _ONE_  1.0
#else
#define REALTYPE real(kind=selected_real_kind(13))
!#define MPI_REALTYPE	MPI_DOUBLE_PRECISION
#define _ZERO_ 0.0d0
#define _HALF_ 0.5d0
#define _ONE_  1.0d0
#endif

! Definition to write NetCDF output reals as single or double precision:
#ifdef _NCDF_SAVE_DOUBLE_
#define NCDF_FLOAT_PRECISION NF90_DOUBLE
#define NCDF_REAL real(kind=selected_real_kind(13))
#else
#define NCDF_FLOAT_PRECISION NF90_REAL
#define NCDF_REAL real(kind=selected_real_kind(6))
#endif

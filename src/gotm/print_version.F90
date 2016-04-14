#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: print_version() - prints GOTM and optional FABM versions
!
! !INTERFACE:
   subroutine print_version()
!
! !DESCRIPTION:
!  Use git to obtain latest git hashes for GOTM and FABM.
!  Also print compiler information from GOTM.
!
! !USES:
   use gotm_version, only: gotm_commit_id=>git_commit_id, &
                           gotm_branch_name=>git_branch_name
   use gotm_compilation
#ifdef _FABM_
   use fabm, only: fabm_initialize_library
   use fabm_types, only: type_version,first_module_version
   use fabm_version, only: fabm_commit_id=>git_commit_id, &
                           fabm_branch_name=>git_branch_name
#endif
#ifdef NETCDF_FMT
   use netcdf
#endif
   IMPLICIT NONE

#ifdef _FABM_
   type (type_version),pointer :: version
#endif
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL0 LINE
   LEVEL0 'GOTM version:   ',gotm_commit_id,' (',gotm_branch_name,' branch)'
#ifdef _FABM_
   LEVEL0 'FABM version:   ',fabm_commit_id,' (',fabm_branch_name,' branch)'
   call fabm_initialize_library()
   version => first_module_version
   do while (associated(version))
      LEVEL0 trim(version%module_name)//' version:   ',trim(version%version_string)
      version => version%next
   end do
#endif
#ifdef NETCDF_FMT
   LEVEL0 'NetCDF version: ',trim(NF90_INQ_LIBVERS())
#endif
   LEVEL0 LINE
   LEVEL0 'Compiler: ',compiler_id,' ',compiler_version
   return
   end subroutine print_version
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2016 - Karsten Bolding (BB)                            !
!-----------------------------------------------------------------------


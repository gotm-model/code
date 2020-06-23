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
   use yaml_version, only: yaml_commit_id=>git_commit_id, &
                           yaml_branch_name=>git_branch_name
   use flexout_version, only: flexout_commit_id=>git_commit_id, &
                              flexout_branch_name=>git_branch_name
#ifdef _ICE_
   use stim_version, only: stim_commit_id=>git_commit_id, &
                           stim_branch_name=>git_branch_name
#endif
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
   LEVEL0 'GOTM:    ',gotm_commit_id,' (',gotm_branch_name,' branch)'
   LEVEL0 'YAML:    ',yaml_commit_id,' (',yaml_branch_name,' branch)'
   LEVEL0 'flexout: ',flexout_commit_id,' (',flexout_branch_name,' branch)'
#ifdef _ICE_
   LEVEL0 'STIM:    ',stim_commit_id,' (',stim_branch_name,' branch)'
#endif
#ifdef _FABM_
   LEVEL0 'FABM:    ',fabm_commit_id,' (',fabm_branch_name,' branch)'
   call fabm_initialize_library()
   version => first_module_version
   do while (associated(version))
      LEVEL0 trim(version%module_name)//':  ',trim(version%version_string)
      version => version%next
   end do
#endif
#ifdef _CVMIX_
   LEVEL0 'CVMix:   included'
#endif
#ifdef NETCDF_FMT
   LEVEL0 'NetCDF:  ',trim(NF90_INQ_LIBVERS())
#endif
   LEVEL0 LINE
   LEVEL0 'Compiler: ',compiler_id,' ',compiler_version
   return
   end subroutine print_version
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2016 - Karsten Bolding (BB)                            !
!-----------------------------------------------------------------------


#include"cppdefs.h"
   subroutine deprecated_output(namlst,title,dt,list_fields)

   use register_all_variables, only: fm
   use output_manager_core, only:output_manager_host=>host, type_output_manager_host=>type_host,type_output_manager_file=>type_file,time_unit_second,type_output_item
   use output_manager
   use netcdf_output
   use text_output

   IMPLICIT NONE

   integer, intent(in) :: namlst
   character(len=*), intent(in) :: title
   REALTYPE, intent(in) :: dt
   logical, intent(inout) :: list_fields

   class (type_output_manager_file), pointer :: outfile
   type (type_output_item),          pointer :: output_item

   integer                             :: out_fmt
   character(len=PATH_MAX)             :: out_dir
   character(len=PATH_MAX)             :: out_fn
   integer                             :: nfirst,nsave
   integer                             :: sync_out
   logical                             :: diagnostics
   integer                             :: mld_method
   REALTYPE                            :: diff_k
   REALTYPE                            :: Ri_crit
   logical                             :: rad_corr
   character(len=*), parameter         :: fname='gotmrun.nml'
   logical                             :: file_exists=.false.

   namelist /output/      list_fields, &
                          out_fmt,out_dir,out_fn,nfirst,nsave,sync_out, &
                          diagnostics,mld_method,diff_k,Ri_crit,rad_corr

   LEVEL1 'deprecated_output'

   out_fmt     = ASCII
   out_dir     = '.'
   out_fn      = 'gotm'
   nfirst      = 0
   nsave       = 1
   sync_out    = 1
   diagnostics = .false.
   mld_method  = 1
   diff_k      = 1.e-5
   Ri_crit     = 0.5
   rad_corr    = .true.

   inquire(file=fname,exist=file_exists)
   if (file_exists) then
      open(namlst,file=fname,action='read',status='old',err=90)
      read(namlst,nml=output,err=94)
   else
      LEVEL2 'use default output parameters'
   end if

#ifndef NETCDF_FMT
   if (out_fmt == NETCDF) then
      LEVEL1 'Warning: out_fmt is set to 2 (NetCDF output), but GOTM has been compiled without NetCDF support. Forcing out_fmt=1 (text).'
      out_fmt = ASCII
   end if
#endif

   select case (out_fmt)
      case (ASCII)
         allocate(type_text_file::outfile)
      case (NETCDF)
#ifdef NETCDF_FMT
         allocate(type_netcdf_file::outfile)
         select type (outfile)
         class is (type_netcdf_file)
            if (sync_out <= 0) then
               outfile%sync_interval = -1
            else
               outfile%sync_interval = sync_out
            end if
         end select
#endif
   end select
   outfile%path = trim(out_dir)//'/'//trim(out_fn)
   outfile%title = trim(title)
   outfile%time_unit = time_unit_second
   outfile%time_step = dt*nsave
   call output_manager_add_file(fm,outfile)
   allocate(output_item)
   call outfile%append_item(output_item)

   LEVEL2 'done'
   return

90 FATAL 'I could not open ', trim(fname)
   stop 'deprecated_output'
94 FATAL 'I could not read the "output" namelist'
   stop 'deprecated_output'

end subroutine deprecated_output

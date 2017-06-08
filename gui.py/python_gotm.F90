#include"cppdefs.h"

module python_gotm

   use iso_c_binding, only: c_double, c_int, c_char, C_NULL_CHAR, c_f_pointer, c_loc, c_ptr

   implicit none

   private

   integer, parameter :: max_path_length = 256

   logical :: output_redirected = .false.

contains
   
   subroutine initialize() bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: initialize
      use gotm, only: init_gotm
      call init_gotm()
   end subroutine

   subroutine finalize() bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: finalize
      use gotm, only: clean_up
      call clean_up()
   end subroutine

   subroutine run() bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: run
      use gotm, only: time_loop
      call time_loop()
   end subroutine

   subroutine redirect_output(outfile,errfile) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: redirect_output
      character(kind=c_char), target, intent(in) :: outfile(*)
      character(kind=c_char), target, intent(in) :: errfile(*)

      character(len=max_path_length),pointer :: filepath
       
      call reset_output()
      call c_f_pointer(c_loc(outfile), filepath)
      open(unit=stdout,file=filepath(:index(filepath,C_NULL_CHAR)-1))
      call c_f_pointer(c_loc(errfile), filepath)
      open(unit=stderr,file=filepath(:index(filepath,C_NULL_CHAR)-1))
      output_redirected = .true.
   end subroutine

   subroutine reset_output() bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: reset_output
      if (output_redirected) then
         close(unit=stdout)
         close(unit=stderr)
         output_redirected = .false.
      end if
   end subroutine reset_output

   subroutine get_version(length,version_string) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: get_version
      use gotm_version, only: gotm_commit_id=>git_commit_id, &
                              gotm_branch_name=>git_branch_name

      integer(c_int),value,intent(in) :: length
      character(kind=c_char)          :: version_string(length)

      call copy_to_c_string(gotm_commit_id//' ('//gotm_branch_name//' branch)', version_string)
   end subroutine get_version

   subroutine get_time_bounds(istart,istop) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: get_time_bounds
      use time, only: minn,maxn

      integer(kind=c_int),intent(out) :: istart,istop

      istart = minn
      istop = maxn
   end subroutine get_time_bounds

   subroutine set_time_bounds(istart,istop) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: set_time_bounds
      use time, only: minn,maxn

      integer(c_int),intent(in),value :: istart,istop

      minn = istart
      maxn = istop
   end subroutine set_time_bounds

   subroutine copy_to_c_string(string,cstring)
      character(len=*),      intent(in)  :: string
      character(kind=c_char),intent(out) :: cstring(:)
      integer i,n
      n = min(len_trim(string),size(cstring)-1)
      do i=1,n
         cstring(i) = string(i:i)
      end do
      cstring(n+1) = C_NULL_CHAR
   end subroutine

end module python_gotm
!$Id: gui_util.F90,v 1.1 2006-11-24 15:52:14 kbk Exp $

#include"cppdefs.h"

module gui_util
    implicit none
    
    logical, private :: redirected = .false.

contains

    subroutine redirectoutput(outpath,errpath)
#ifdef WINDOWS
        !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_GUI_UTIL_mp_redirectoutput',NOMIXED_STR_LEN_ARG :: redirectoutput
#endif
        implicit none
        
        character(len=255), intent(in) :: outpath
        character(len=255), intent(in) :: errpath
        
        call resetoutput()
        open(unit=stdout,file=outpath)
        open(unit=stderr,file=errpath)
        redirected = .true.
    end subroutine redirectoutput

    subroutine resetoutput()
        implicit none
    
        if (redirected) then
            close(unit=stdout)
            close(unit=stderr)
            redirected = .false.
        end if
    end subroutine resetoutput
    
end module gui_util

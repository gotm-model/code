!$Id: main.F90,v 1.2 2001-05-31 12:00:52 gotm Exp $
#include<cppdefs.h>
!-----------------------------------------------------------------------
!BOI
!
! !TITLE: The implementation of the General Ocean Turbulence Model - \bf{GOTM}
!
! !AUTHORS: Hans Burchard and Karsten Bolding and Manuel R. Villarreal and Pierre-Philippe Mathieu and Georg Umgiesser.
!
! !AFFILIATION: Various places around the world. 
!
! !DATE: 
!  \date{}
!
! !INTRODUCTION:
!  \LaTeX
!  This is the main for the General Ocean Turbulence Model (GOTM).
!  The whole model is steered by a number of namelist-files.
!  For the physical background of GOTM, see the GOTM report.
!
!  This main contains some initializations and the time loop.
!  From here, all necessary subroutines are called.
!
!  \section{The philosophy behind GOTM}
!
!  General Ocean Turbulence Model (GOTM) is a very ambitious name for 
!  a one-dimensional water column model which simply allows for different 
!  combinations of momentum and tracer equations and a choice between some 
!  standard turbulence parameterizations.
!  A turbulence modeling expert might not find any new or sophisticated 
!  turbulence closure scheme in GOTM. Whoever is missing any feature in GOTM,
!  is invited to contribute to GOTM and add her or his personal preferences
!  or needs. GOTM is far from being complete and it will never be completed.
!  At some certain stage during the development of GOTM in winter 1998/99,
!  we decided to freeze innovations and to consolidate, comment and test what
!  was already included. The result of these efforts consists of the present
!  report, the GOTM source code, and the data and input files for some
!  test cases. All this is put together on this site of the world wide web.
!
!  For more information, see the GOTM report or http://www.gotm.net.
!
!  The authors of this code are:
!  \begin{itemize}
!    \item  Hans Burchard           (hans@gotm.net)
!    \item  Karsten Bolding         (karsten@gotm.net)
!    \item  Manuel R. Villarreal    (uscfmmrv@cesga.es)
!    \item  Pierre-Philippe Mathieu (pp.mathieu@altavista.net)
!    \item  Georg Umgiesser         (georg@lagoon.isdgm.ve.cnr.it)
!  \end{itemize}
!
!  Please report any bugs and suggestions to one of us.
!
!EOI
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: GOTM - the main program 
! 
! !INTERFACE:
   program main 
!
! !USES:
   use time
   use gotm
   IMPLICIT NONE
!
! !DESCRIPTION: 
!  This routine starts the model - and write a few diagnostics.
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!  $Log: main.F90,v $
!  Revision 1.2  2001-05-31 12:00:52  gotm
!  Correction in the calculation of the shear squared calculation - now according
!  to Burchard 1995 (Ph.D. thesis).
!  Also some cosmetics and cleaning of Makefiles.
!
!  Revision 1.1.1.1  2001/02/12 15:55:59  gotm
!  initial import into CVS
!
!
! !LOCAL VARIABLES:
   character(LEN=8)	:: datestr
   real 		:: t1,t2
! 
!EOP
!-----------------------------------------------------------------------
!BOC
   call CPU_Time(t1)
   call Date_And_Time(datestr,timestr)
   STDERR LINE 
   STDERR 'GOTM ver. ',RELEASE,': Started on  ',datestr,' ',timestr
   STDERR LINE

   call init_gotm()
   call time_loop()
   call clean_up()

   call CPU_Time(t2)
   call Date_And_Time(datestr,timestr)
   STDERR LINE
   STDERR 'GOTM ver. ',RELEASE,': Finished on ',datestr,' ',timestr
   STDERR 'CPU-time was in loop:  ',t2-t1,' seconds'
   STDERR 'Sim-time/CPU-time:     ',simtime/(t2-t1)
   STDERR LINE
!kbk   STDERR 'Copyright (C) Karsten Bolding & Hans Burchard'
   STDERR LINE

   end
!EOC

!-----------------------------------------------------------------------
!Copyright (C) 2000 - Karsten Bolding & Hans Burchard
!-----------------------------------------------------------------------

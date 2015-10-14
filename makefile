#Intel ifc version
#libraries = -L/usr/X11R6/lib/ -L/home/hutch/accis/ifc/ -laccisX -lXt -lX11 -lPEPCF90 -lSM -lICE
#G77=ifc
#COMPILE-SWITCHES = -O3 -w90 -cm

#g77 version.
libraries = -L/usr/X11R6/lib/ -L/home/hutch/accis/ -laccisX -lXt -lX11 -lGL -lGLU
G77=gfortran
COMPILE-SWITCHES = -Wall -O2

#MINGW=mingw-g77 -mwindows -mconsole
MINGWBIN=/home/hutch/local/xmingw32/bin/
MINGW=${MINGWBIN}mingw32-g77 -H -mwindows -mconsole 
WINLIB= -L/home/hutch/accis/win32/ -laccisWin

#pattern rule, compile using the external definitions of commons, no backslash.
%.o : %.f piccom.f makefile;
	$(G77) -c $(COMPILE-SWITCHES) $*.f

%.o : %.F piccom.f makefile;
	$(G77) -c $(COMPILE-SWITCHES) $*.F

% : %.f
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.f  $(libraries)

% : %.F
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.F  $(libraries)

%.exe : %.f
	if [ -f $*.coff ]; then $(MINGW) -o $*.exe $*.coff $*.f $(WINLIB);\
           else  $(MINGW) -o $*.exe $*.f $(WINLIB); fi


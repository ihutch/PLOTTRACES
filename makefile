libraries = -L/usr/X11R6/lib/ -L/home/hutch/accis/ -laccisX -lX11
G77=gfortran
COMPILE-SWITCHES = -Wall -O2 -Wno-unused-dummy-argument

#MINGW=mingw-g77 -mwindows -mconsole
MINGWBIN=/home/hutch/local/xmingw32/bin/
MINGW=${MINGWBIN}mingw32-g77 -H -mwindows -mconsole 
WINLIB= -L/home/hutch/accis/win32/ -laccisWin

#pattern rule, compile using the external definitions of commons, no backslash.
%.o : %.f cmdlncom.f makefile;
	$(G77) -c $(COMPILE-SWITCHES) $*.f

%.o : %.F cmdlncom.f makefile;
	$(G77) -c $(COMPILE-SWITCHES) $*.F

%.o : %.f90 cmdlncom.f makefile;
	$(G77) -c $(COMPILE-SWITCHES) $*.f90

% : %.f
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.f  $(libraries)

% : %.f90
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.f90  $(libraries)

% : %.F
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.F  $(libraries)

%.exe : %.f
	if [ -f $*.coff ]; then $(MINGW) -o $*.exe $*.coff $*.f $(WINLIB);\
           else  $(MINGW) -o $*.exe $*.f $(WINLIB); fi


#########################################################################
# Define the directories, variables, defaults that ACCIS uses
ifeq ("$(ACCISPARENT)","") 
  ACCISPARENT:= $(HOME)/src
endif
ACCISHOME:=$(realpath $(ACCISPARENT))/accis
ACCISX=$(ACCISHOME)/libaccisX.a
LIBPATH= -L$(ACCISHOME) -L.
LIBRARIES = -laccisX -lX11
LIBDEPS = $(ACCISHOME)/libaccisX.a
COMPILE-SWITCHES = -Wall -O2
# -fbounds-check
#########################################################################
# Unless turned off, check that the accis library is available and make it,
# unless we are in the ACCISHOME directory doing things explicitly.
ifeq ("$(NOACCISCHECK)","")
ACCISCHECK:=\
$(shell if [ "${CURDIR}" != "$(ACCISHOME)" ];\
 then   echo >&2 "${CURDIR}" is not ACCISHOME: "$(ACCISHOME)" ;\
        echo -n >&2 "Checking accis ... ";\
 if [ -f "${ACCISX}" ] ; then echo>&2 "Library ${ACCISX} exists."; else\
   if [ -d "${ACCISPARENT}" ] ; then echo>&2 -n "parent directory exists. ";\
     else mkdir ${ACCISPARENT} ; fi;\
   if [ -d "${ACCISHOME}" ] ; then echo>&2 "accis directory exists. ";\
     else if cd ${ACCISPARENT}; then\
        git clone https://github.com/ihutch/accis.git; cd - >/dev/null; fi;fi;\
   if cd ${ACCISHOME}; then\
         make ACCISHOME=$(ACCISHOME) >&2; cd - >/dev/null; fi;\
   if [ -f "${ACCISX}" ] ; then echo>&2 "Made ${ACCISX}";\
     else echo>&2 "Error making ${ACCISX}"; fi;\
 fi;fi;\
)
endif
#########################################################################
# To satisfy dependencies by building accis in the standard place, simply
# add the dependency $(LIBDEPS), and  $(LIBPATH) $(LIBRARIES) to executables
# If accis should be made in some non-standard $(ACCISHOME) location then
# the makefile should contain
#       ACCISPARENT:=$(realpath <ParentDirectory>)
#       export ACCISPARENT
# just before the include ACCIS.mk line. 
#########################################################################


libraries = $(LIBPATH) $(LIBRARIES)
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

% : $(LIBDEPS) %.f
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.f  $(libraries)

% : $(LIBDEPS) %.f90
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.f90  $(libraries)

% : $(LIBDEPS) %.F
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.F  $(libraries)

%.exe : %.f
	if [ -f $*.coff ]; then $(MINGW) -o $*.exe $*.coff $*.f $(WINLIB);\
           else  $(MINGW) -o $*.exe $*.f $(WINLIB); fi


SHELL=/bin/sh
#
SRCS=	grbit2.f90  wave_stat.f90
OBJS=	 grbit2.o  wave_stat.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
#----- used with XLF on AIX  ---
FC      =$(FCMP)
FFLAGS  =$(FFLAGSM)
#----- used with G95 on Linux ---
#FC =           g95
#FFLAGS =        $(INC)
#--------------------------------

##LDFLAGS = 
##INC = /nwprod/lib/incmod/g2_4
##LIBS = -L/nwprod/lib -lg2_4 -lw3nco_v2.0.3_4 -lbacio_4 \
##        -ljasper -lpng -lz
##
##CMD =   wave_stat	
##PROFLIB =  

LDFLAGS = 
INC = $(G2_INC4) 
LIBS = $(G2_LIB4) $(W3NCO_LIB4) $(BACIO_LIB4) $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)
#       /usrx/local/prod/packages/ips/19.0.5/serial/zlib/1.2.11/lib/libz.a
#$(Z_LIB) $(Z_LIBa)

CMD =   wave_stat
PROFLIB =  	
#
# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

wave_stat.o:	wave_stat.f90
	$(FC) -c $(FFLAGS) -I $(INC) wave_stat.f90

grbit2.o:   grbit2.f90 
	$(FC) -c $(FFLAGS) -I $(INC) grbit2.f90

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS) *.db *.lst *.mod

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile

install:
	mv $(CMD) ../../exec

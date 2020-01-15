###############################################################
#    Makefile for overenstr.f 
#    Use:
#      make         - build the executable
#      make clean   - start with a clean slate
###############################################################
# Tunable parameters :
# FC Name of the Fortran compiling system to use
# LDFLAGS Options of the loader
# FFLAGS Options of the compiler
# DEBUG Options of the compiler included for debugging
# LIBS List of libraries
# CMD Name of the executable

#  export W3NCO_LIB4=/nwprod/lib/w3nco/v2.0.6/libw3nco_v2.0.6_4.a
#  export BACIO_LIB4=/nwprod/lib/bacio/v2.0.1/libbacio_v2.0.1_4.a

FC      = ifort 
BINDIR  = ../../exec
LDFAGS  = -0 -convert big_endian
LIBS    = ${W3NCO_LIB4} ${BACIO_LIB4}
CMD     = overenstr.grib
OBJS    = overenstr.grib.o
DEBUG   =
FFLAGS  = -O3 $(DEBUG)

# Lines from here down should not need to be changed. They are
# the actual rules which make uses to build CMD

all:	$(CMD)

$(CMD): $(SRCS)
	$(FC) -o overenstr.grib overenstr.grib.f $(LIBS)
clean:
	-rm -f $(OBJS) $(CMD)

install:
	-mv $(CMD) $(BINDIR)

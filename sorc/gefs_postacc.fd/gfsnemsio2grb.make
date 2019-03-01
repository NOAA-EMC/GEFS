SHELL   =/bin/bash
EXEC    =../exec/gfsnemsio2grb
FC	=ftn
# FC	=ifort
#INCNEMSIO=/nwprod/lib/incmod/nemsio
#FOPTS	= -O -FR -I$(INCNEMSIO)
# FOPTS   = -O -I$(NEMSIO_INC)
#wgrib2api=/u/Wesley.Ebisuzaki/home/grib2/lib
wgrib2api=/u/Wesley.Ebisuzaki/home/grib2.v2.0.7/lib
FOPTS   = -O -I$(NEMSIO_INC)  -qopenmp
LOPTS	=
#LIBS	=-L/nwprod/lib -lnemsio -lbacio_4 -lw3emc_d  -lw3nco_d -L${wgrib2api} -lwgrib2
LIBS    = $(NEMSIO_LIB) $(BACIO_LIB4) ${W3NCO_LIB4} -L${wgrib2api} -lwgrib2
OBJS = gfsnemsio2grb.o
SRCS = gfsnemsio2grb.f90
# *************************************************************************
all: $(SRCS)
	$(FC) $(FOPTS) $(LOPTS) ${SRCS} -o $(EXEC) $(LIBS) -I${wgrib2api}


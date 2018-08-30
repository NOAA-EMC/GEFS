SHELL= 	/bin/bash
#INC=     /nwprod/lib/incmod/g2_4
#LIBS=    -L/nwprod/lib -lg2_4 -lw3nco_4 -lbacio_4 \
	 -ljasper -lpng -lz
INC_NSTGEN=-I ${SFCIO_INC4} -I ${NEMSIO_INC}
LIBS_NSTGEN=${NEMSIO_LIB} ${BACIO_LIB4} ${SFCIO_LIB4} ${W3NCO_LIB4}
SRCSf90= nstgen.f90
SRCSf=   
SRCS=    $(SRCSf90) $(SRCSf)
OBJS=    $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD=   	gefs_nstgen
FC= 	ifort
FFLAGS = -O3 -fp-model source -convert big_endian -assume byterecl -implicitnone $(INC_NSTGEN)
LDFLAGS=

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS_NSTGEN)
	rm -f $(OBJS)

$(OBJS):        $(SRCS)
	$(FC) $(FFLAGS) -c $(SRCS)

#$(OBJS):        makefile_wcoss

allclean:       clean all

clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD)
install:
	 mv $(CMD) ../../exec/$(CMD)


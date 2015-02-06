SHELL= 	/bin/bash
#INC=     /nwprod/lib/incmod/g2_4
#LIBS=    -L/nwprod/lib -lg2_4 -lw3nco_4 -lbacio_4 \
	 -ljasper -lpng -lz
SRCSf90= gefs_ensstat.f90 init_parm.f90 printinfr.f90 gtbits.f90 isrchne.f90
SRCSf=   EPDF.f SORTMM.f
SRCS=    $(SRCSf90) $(SRCSf)
OBJS=    $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD=   	gefs_ensstat
FC= 	ifort
FFLAGS = -O3 -g -convert big_endian -I $(INC)
LDFLAGS=

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)
	rm -f $(OBJS)

$(OBJS):        $(SRCS)
	$(FC) $(FFLAGS) -c $(SRCS)

$(OBJS):        makefile_wcoss

allclean:       clean all

clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD)
install:
	 mv $(CMD) ../../exec/$(CMD)



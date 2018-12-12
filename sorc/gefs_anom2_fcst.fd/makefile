SHELL= 	/bin/bash
#INC=     /nwprod/lib/incmod/g2_4
#LIBS=    -L/nwprod/lib -lg2_4 -lw3nco_4 -lbacio_4 \
	 -ljasper -lpng -lz
SRCSf90= anom2_fcst.f90
SRCSf=   
SRCS=    $(SRCSf90) $(SRCSf)
OBJS=    $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD=   	gefs_anom2_fcst
FC= 	ifort
INC=${G2_INC4} 
FFLAGS = -O3 -g -convert big_endian -I $(INC)
LDFLAGS=
LIBS1= ${W3NCO_LIB4} ${BACIO_LIB4} 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS1)
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


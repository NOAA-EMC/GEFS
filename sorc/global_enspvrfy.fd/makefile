SHELL=	/bin/bash
INC=${INC_d}
LIBS=${LIBS_d}
#INC= /nwprod/lib/incmod/g2_d
#LIBS= -L/nwprod/lib  -lg2_d -lw3nco_d -lbacio_4 -lip_d -lsp_d \
#        -lpng -ljasper -lz -lw3nco_d

SRCS=	PVRFY.f90 VRFY.f90 ANNATV.f90 INIT_PARM.f90 PRINTINFR.f90
OBJS=   $(SRCS:.f90=.o)
CMD=	global_enspvrfy    
#FC=	ifort
#FFLAGS =        -O3 -g -r8 -convert big_endian -auto -mkl -I $(INC)
FFLAGS =${FFLAGS_d}
LDFLAGS=
LOPTS=  -mkl

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LOPTS)  $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)
	rm -f $(OBJS)

$(OBJS):        $(SRCS)
	$(FC) $(FFLAGS) -c $(SRCS)

$(OBJS):        makefile

allclean:       clean all

clean:
	-rm -f $(OBJS)

install:
	mv $(CMD) ../../exec/$(CMD)


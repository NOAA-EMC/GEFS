SHELL=	/bin/bash
#INC= /nwprod/lib/incmod/g2_d
#LIBS= -L/nwprod/lib  -lg2_d -lw3nco_d -lbacio_4 -lip_d -lsp_d \
        -lpng -ljasper -lz -lw3nco_d

# LIBS= -L/nwprod/lib/ -lg2_4 -lw3nco_4 -lip_4 -lbacio_4 -lsp_4 \
#       -ljasper -lpng -lz 

SRCS=	PVRFY.f90 VRFY.f90 ANNATV.f90 INIT_PARM.f90 PRINTINFR.f90
OBJS=   $(SRCS:.f90=.o)
CMD=	global_enspvrfy    
FC=	ifort
#FFLAGS=	-O3 -qsuffix=f=f90 -qlist -qsource -qlanglvl=90PURE -bnoquiet 
#FFLAGS=	-O3 -qsuffix=f=f90 -qlist -qsource -bnoquiet 
#FFLAGS=	-O3 -list -traceback
# FFLAGS =        -O3 -g -convert big_endian -I $(INC)
FFLAGS =        -O3 -g -r8 -convert big_endian -auto -mkl -I $(INC_d)
LDFLAGS=
LOPTS=  -mkl


#$(CMD):	$(SRCS)
#	$(FC) $(SRCS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LOPTS)  $(LDFLAGS) -o $(@) $(OBJS) $(LIBS_d)
	rm -f $(OBJS)

$(OBJS):        $(SRCS)
	$(FC) $(FFLAGS) -c $(SRCS)

$(OBJS):        makefile

allclean:       clean all

clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD)
install:
	        mv $(CMD) ../../exec/$(CMD)



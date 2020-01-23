#INCS   = -I$(MKLROOT)/include -I/nwprod/lib/incmod/sigio_v2.0.1_4
#FC     = mpfort
#FFLAGS = -openmp -convert big_endian -assume byterecl -g -traceback -list
FFLAGS = -convert big_endian -assume byterecl -g -traceback -list
#LIBS   = -L/nwprod/lib -lsp_v2.0.2_4 -lsigio_v2.0.1_4 -lw3nco_v2.0.6_4 \
#         -L$(MKLROOT)/lib/intel64 -lmkl_scalapack_lp64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_intelmpi_lp64 -lpthread -lm
FC      = mpif90

CMD    = gefs_init_et_p
SRCS   = gefs_init_et_p.f90
OBJS   = $(SRCS:.f90=.o)

LDFLAGS = -mkl -lmkl_scalapack_lp64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_intelmpi_lp64 -lpthread -lm -CB -${OPENMPFFLAG}

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD): $(OBJS)
	$(FC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS_INIT_ET)

$(OBJS):        $(SRCS)
	$(FC) $(FFLAGS) -I $(INCS) -c $(SRCS)

$(OBJS):        makefile

allclean:       clean all

clean:
	-rm -f $(OBJS) *.mod *.lst

clobber:        clean
	-rm -f $(CMD)

install:
	mv $(CMD) ../../exec/$(CMD)


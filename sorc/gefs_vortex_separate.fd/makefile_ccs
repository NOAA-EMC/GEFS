
SHELL = /bin/sh

LIBSIGIO=/nwprod/lib/incmod/sigio_4

SRCS=	modules.f gefs_vortex_separate.f BOUND.f fft99.f sig_p_convt1.f\
	SEPAR.f WNLIT.f FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f\
	WNLSM.f WNNLS.f XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f \
	XERSAV.f srotm.f srotmg.f amatrix.f rodist.f landcmsk.f

OBJS=	modules.o gefs_vortex_separate.o BOUND.o fft99.o sig_p_convt1.o\
	SEPAR.o WNLIT.o FDUMP.o H12.o I1MACH.o J4SAVE.o XGETUA.o\
	WNLSM.o WNNLS.o XERABT.o XERCTL.o XERPRT.o XERROR.o XERRWV.o \
	XERSAV.o srotm.o srotmg.o amatrix.o rodist.o landcmsk.o

CF =    ncepxlf
LDFLAGS =
LIBS  = -L/nwprod/lib -lsp_d -lw3_d -lbufr_d_64 -lsigio_4 -lbacio_4

CMD =   gefs_vortex_separate
PROFLIB =   -lessl    -lprof

#FFLAGS= -qsmp=noauto -p -O -NS10000 -qintsize=4 -qrealsize=8 -qmaxmem=-1 -qinitauto -qsource -qflttrap
FFLAGS= -qsmp=noauto -p -O -NS10000 -qintsize=4 -qrealsize=8 -qmaxmem=-1 -qinitauto -qsource -qflttrap\
-I$(LIBSIGIO)
LDFLAGS=-l essl

all:  $(CMD)

$(CMD): $(OBJS)
	$(CF) -I $(LIBSIGIO) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:    $(OBJS)
	$(CF) -I $(LIBSIGIO) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD) $(CMD).prof

void:   clobber
	-rm -f $(SRCS) makefile

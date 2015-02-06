SHELL = /bin/bash

#LIBSIGIO=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib/incmod/sigio_4
#LIBSIGIO=/nwprod/lib/incmod/sigio_4
#INCS= -I/nwprod/lib/incmod/sigio_4

SRCS=	modules.f gefs_vortex_separate.f BOUND.f fft99.f sig_p_convt1.f\
	SEPAR.f WNLIT.f FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f\
	WNLSM.f WNNLS.f XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f \
	XERSAV.f srotm.f srotmg.f amatrix.f rodist.f landcmsk.f

#OBJS=	modules.o gefs_vortex_separate.o BOUND.o fft99.o sig_p_convt1.o\
#	SEPAR.o WNLIT.o FDUMP.o H12.o I1MACH.o J4SAVE.o XGETUA.o\
#	WNLSM.o WNNLS.o XERABT.o XERCTL.o XERPRT.o XERROR.o XERRWV.o \
#	XERSAV.o srotm.o srotmg.o amatrix.o rodist.o landcmsk.o
OBJS=   $(SRCS:.f=.o)

FC =    ifort

#LIBS  = -L/nwprod/lib -lsp_d -lw3_d -lbufr_d_64 -lsigio_4 -lbacio_4
#LIBS    = -L/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib -lbacio_4 -lw3lib-2.0_4 -lip_4 -lsp_d -lsigio_4
#LIBS  = -L/nwprod/lib -lsp_d -lw3_d -lbufr_v10.1.0_d_64 -lsigio_4 -lbacio_4
#LIBS  = -L/nwprod/lib -lsp_d -lsigio_4 -lw3nco_d

CMD =   gefs_vortex_separate
#PROFLIB =   -lessl    -lprof

#FFLAGS= -qsmp=noauto -p -O -NS10000 -qintsize=4 -qrealsize=8 -qmaxmem=-1 -qinitauto -qsource -qflttrap
#FFLAGS= -openmp -p -O -i4 -r8 -I$(LIBSIGIO)
#FFLAGS= -O3 -xHOST -assume byterecl -warn all -traceback -openmp -I$(LIBSIGIO)
#-O2 -xHOST -assume byterecl -warn all -implicitnone -traceback -openmp
#LDFLAGS=-l essl
#FFLAGS= -openmp -p  -xHOST -r8 -O0 -i4 -traceback -assume byterecl  -I$(LIBSIGIO) -list
FFLAGS=  -p   -r8 -O0 -i4 -traceback -assume byterecl -list
LDFLAGS = -mkl  -openmp

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD): $(OBJS)
	$(FC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS_INIT)

#$(OBJS):        $(SRCS)
#	$(FC) $(FFLAGS) $(INCS) -c $(SRCS)
.f.o:
	$(FC) $(FFLAGS) -I $(INCS)  -c $<


allclean:       clean all

clean:
	-rm -f $(OBJS) *.lst

clobber:        clean
	-rm -f $(CMD)
install:
	mv $(CMD) ../../exec/$(CMD)


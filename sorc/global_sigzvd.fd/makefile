SHELL	=/bin/ksh
#INCMOD1	=/nwprod/lib/incmod/sigio_4 
#INCMOD1	=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib/incmod/sigio_4
#INCMOD1	=/nwprod/lib/incmod/sigio_4
#INCS	= -I/nwprod/lib/incmod/sigio_4
#SRCM	=global_sigzvd.f
#SRCS	=
SRCS	=global_sigzvd.f
OBJS	=$(SRCS:.f=.o)
#FC	=ncepxlf
FC	=ifort
#FFLAGS	=-qnosave -qsmp=noauto -O2 -qmaxmem=-1
#LDFLAGS	= -lesslsmp
#LIBS	= -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 -lsigio_4 -lsfcio_4
FFLAGS= -mkl -O2  -${OPENMPFFLAG} -p  -xHOST -r8  -convert big_endian -i4 -list -traceback -assume byterecl 

#LIBS    = -L//scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 -lsigio_4 -lsfcio_4
#LIBS    = -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 -lsigio_4 -lsfcio_4
#LIBS    = -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 -lsigio_4 -lsfcio_4
#LIBS    = -L/nwprod/lib -lsigio_4

CMD	=global_sigzvd

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD): $(OBJS)
	$(FC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS_INIT)

$(OBJS):        $(SRCS)
	$(FC) $(FFLAGS) -I $(INCS) -c $(SRCS)

$(OBJS):        makefile_wcoss

allclean:       clean all

clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD)
install:
	 mv $(CMD) ../../exec/$(CMD)




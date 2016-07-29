SHELL = /bin/bash

SRCS=	gefs_vortex_combine.f

#OBJS=	gefs_vortex_combine.o
OBJS=   $(SRCS:.f=.o)


CF =    ifort
FC =    ifort
LDFLAGS =

CMD =   gefs_vortex_combine
PROFLIB =   
FFLAGS=  -p   -r8 -O0 -i4  -traceback -assume byterecl   -list

LDFLAGS= -openmp

#
all:    $(CMD)

$(CMD): $(OBJS)
	$(FC) $(LDFLAGS) $(FFLAGS) -I $(INCGFS) -I $(INCG)  -I $(INCS) -o $(@) $(OBJS) $(LIBS_INIT)

$(OBJS):        $(SRCS)
	$(FC) $(FFLAGS) -I $(INCGFS) -I $(INCG) -I $(INCS) -c $(SRCS)


allclean:       clean all

clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD)
install:
	 mv $(CMD) ../../exec/$(CMD)



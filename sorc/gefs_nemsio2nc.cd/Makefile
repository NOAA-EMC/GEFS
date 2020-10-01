SHELL=  /bin/bash

FC  	=$(FCMP)
FC90    =$(FCMP95)
LDR 	=$(FCMP)
CC      =$(CCMP)

INCS    =$(INCSM)
LIBS    =$(LIBSM)

CXX_FLAGS =$(CXX_FLAGSM)
FFLAGS  =$(FFLAGSM)
OMPFLAG	=$(OMPFLAGM)
LDFLAGS =$(LDFLAGSM)

SRCSf90	= nemsio_interface.f90
SRCSf	=
SRCS_F	= $(SRCSf90) $(SRCSf)
OBJS_F	= $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD		= gefs_nemsio2nc

SRCS_CC	= nemsio.cc gfsnc.cc nems2nc.cc nemsio2nc.cc
OBJS_CC = $(SRCS_CC:.cc=.o)

OBJS    = $(OBJS_F) $(OBJS_CC)

$(CMD): $(OBJS_F) $(OBJS_CC)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)

$(OBJS):  $(OBJS_F) $(OBJS_CC)

$(OBJS_CC):  $(SRCS_CC)
	$(CC) $(CXX_FLAGS) $(OMPFLAG) $(INCS) -c $(SRCS_CC)

$(OBJS_F):	$(SRCS_F)
	$(FC) $(FFLAGS) $(OMPFLAG) $(INCS) -c $(SRCS_F)

allclean:  	clean all

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)

install:
	cp $(CMD) ../../exec/$(CMD)


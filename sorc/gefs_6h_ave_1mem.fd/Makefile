SHELL=  /bin/bash

FC  	=$(FCMP)
FC90    =$(FCMP95)
LDR 	=$(FCMP)

INCS    =$(INCSM)
LIBS    =$(LIBSM)

FFLAGS  =$(FFLAGSM)
OMPFLAG	=$(OMPFLAGM)
LDFLAGS =$(LDFLAGSM)

SRCSf90	= gefs_6h_ave_1mem.f90 printinfr.f90 
SRCSf	= prlevel.f 
SRCS	= $(SRCSf90) $(SRCSf)
OBJS	= $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD		= gefs_6h_ave_1mem.exe

$(CMD):		$(OBJS)
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)

$(OBJS):	$(SRCS)
	$(FC) $(FFLAGS) $(OMPFLAG) $(INCS) -c $(SRCS)

allclean:  	clean all

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)

install:
	cp $(CMD) ../../exec/$(CMD)



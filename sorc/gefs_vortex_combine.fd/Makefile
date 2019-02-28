SHELL=  /bin/bash

FC  	=$(FCMP)
FC90    =$(FCMP95)
LDR 	=$(FCMP)

INCS    =$(INCSM)
LIBS    =$(LIBSM)

FFLAGS  =$(FFLAGSM)
OMPFLAG	=$(OMPFLAGM)
LDFLAGS =$(LDFLAGSM)

SRCSf90	= gefs_vortex_combine.f90
SRCSf	= 

SRCS	= $(SRCSf90) $(SRCSf)
OBJS	= $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD		= gefs_vortex_combine

$(CMD):		$(OBJS)
	# This is the new method by Xianwu Xue to remove the INCS and its corresponding options [$(FFLAGS) $(OMPFLAG) $(INCS)]
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)
	# Using the below method will generate the same exec file size as the one before changing to EE2
	#$(LDR) $(LDFLAGS) $(FFLAGS) $(OMPFLAG) $(INCS) $(OBJS) $(LIBS) -o $(CMD)

$(OBJS):	$(SRCS)
	$(FC) $(FFLAGS) $(OMPFLAG) $(INCS) -c $(SRCS)

allclean:  	clean all

clean:
	-rm -f $(OBJS) *.lst *.mod

clobber:	clean
	-rm -f $(CMD)

install:
	cp $(CMD) ../../exec/$(CMD)



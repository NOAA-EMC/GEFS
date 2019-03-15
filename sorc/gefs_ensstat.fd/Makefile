SHELL=  /bin/bash

FC  	=$(FCMP)
FC90    =$(FCMP95)
LDR 	=$(FCMP)

INCS    =$(INCSM)
LIBS    =$(LIBSM)

FFLAGS  =$(FFLAGSM)
OMPFLAG	=$(OMPFLAGM)
LDFLAGS =$(LDFLAGSM)

SRCSf90	= gefs_ensstat.f90 init_parm.f90 printinfr.f90 gtbits.f90 isrchne.f90
SRCSf	= EPDF.f SORTMM.f

SRCS	= $(SRCSf90) $(SRCSf)
OBJS	= $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD		= gefs_ensstat

$(CMD):		$(OBJS)
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)
	#$(LDR) $(LDFLAGS) $(FFLAGS) $(OMPFLAG) $(OBJS) $(LIBS) -o $(CMD)

$(OBJS):	$(SRCS)
	$(FC) $(FFLAGS) $(OMPFLAG) $(INCS) -c $(SRCS)

allclean:  	clean all

clean:
	-rm -f $(OBJS) *.lst *.mod

clobber:	clean
	-rm -f $(CMD)

install:
	cp $(CMD) ../../exec/$(CMD)



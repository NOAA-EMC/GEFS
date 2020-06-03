SHELL=  /bin/bash

FC  	=$(FCMP)
FC90    =$(FCMP95)
LDR 	=$(FCMP)

INCS    =$(INCSM)
LIBS    =$(LIBSM)

FFLAGS  =$(FFLAGSM)
OMPFLAG	=$(OMPFLAGM)
LDFLAGS =$(LDFLAGSM)
ifdef cwdm
    cwdmP = $(cwdm)
else
    cwdmP = ../
endif

SRCSf90	= CVT24H.f90 INIT_PARM.f90 PRINTINFR.f90
SRCSf	= 

SRCS	= $(SRCSf90) $(SRCSf)
OBJS	= $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD		= global_enscvt24h

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
	cp $(CMD) $(cwdmP)../exec/$(CMD)



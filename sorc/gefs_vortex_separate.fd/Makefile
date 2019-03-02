SHELL=  /bin/bash

FC  	=$(FCMP)
FC90    =$(FCMP95)
LDR 	=$(FCMP)

INCS    =$(INCSM)
LIBS    =$(LIBSM)

FFLAGS  =$(FFLAGSM)
OMPFLAG	=$(OMPFLAGM)
LDFLAGS =$(LDFLAGSM)

SRCSf90	= 
SRCSf	= modules.f gefs_vortex_separate.f BOUND.f fft99.f sig_p_convt1.f\
    	  SEPAR.f WNLIT.f FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f\
    	  WNLSM.f WNNLS.f XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f \
    	  XERSAV.f srotm.f srotmg.f amatrix.f rodist.f landcmsk.f

SRCS	= $(SRCSf90) $(SRCSf)
OBJS	= $(SRCSf:.f=.o) $(SRCSf90:.f90=.o)
CMD		= gefs_vortex_separate 

$(CMD):		$(OBJS)
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)                             # New and concise method
	#$(LDR) $(LDFLAGS) $(FFLAGS) $(OMPFLAG) $(INCS) $(OBJS) $(LIBS) -o $(CMD) # To get the original size of executable file

$(OBJS):	$(SRCS)
	$(FC) $(FFLAGS) $(OMPFLAG) $(INCS) -c $(SRCS)

allclean:  	clean all

clean:
	-rm -f $(OBJS) *.lst *.mod

clobber:	clean
	-rm -f $(CMD)

install:
	cp $(CMD) ../../exec/$(CMD)



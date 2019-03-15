SHELL=  /bin/bash

FC  	=$(FCMP)
FC90    =$(FCMP95)
LDR 	=$(FCMP)

INCS    =$(INCSM)
LIBS    =$(LIBSM)

OMPFLAG	=$(OMPFLAGM)
LDFLAGS =$(LDFLAGSM)

FFLAGSf = -nofree -r8 -assume byterecl -g -traceback -fp-model strict
FFLAGSf90 = -O2 -assume byterecl -warn all -implicitnone -g -traceback -fp-model strict

SRCSf90	= sig2p.f90
SRCSf_m	= gettrk_main.f
SRCSf1  = gettrk_modules.f

SRCS	= $(SRCSf90) $(SRCSf1) $(SRCSf_m)
OBJS    = $(SRCSf90:.f90=.o) $(SRCSf_m:.f=.o) $(SRCSf1:.f=.o)
OBJSf_m = $(SRCSf_m:.f=.o)
OBJSf1  = $(SRCSf1:.f=.o)
OBJSf90 = $(SRCSf90:.f90=.o)

CMD		= gettrk


$(CMD):		$(OBJSf1) $(OBJSf90) $(OBJSf_m)
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD) 		# New and concise method
	#$(LDR) $(INCS) -o $(CMD) $(OBJS) $(LIBS) $(LDFLAGS) # To get the original size of executable file

$(OBJSf1):   $(SRCSf1)
	$(FC) $(FFLAGSf) -c $(SRCSf1)	

$(OBJSf90):  $(OBJSf1)  $(SRCSf90)
	$(FC) $(FFLAGSf90) $(OMPFLAG) $(INCS) -c $(SRCSf90)	 # New and concise method
	#$(FC) $(FFLAGSf90) -c  $(OMPFLAG) $(INCS) -c $(SRCSf90) # To get the original size of executable file

$(OBJSf_m):   $(SRCSf_m) $(OBJSf1) $(OBJSf90)
	$(FC) $(FFLAGSf) -c $(SRCSf_m) 

allclean:  	clean all

clean:
	-rm -f $(OBJS) *.lst *.mod

clobber:	clean
	-rm -f $(CMD)

install:
	cp $(CMD) ../../exec/$(CMD)



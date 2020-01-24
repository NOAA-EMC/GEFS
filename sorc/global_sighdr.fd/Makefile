SHELL   =/bin/sh
INCMOD  =$(SIGIO_INC4)               
SRCM    =sighdr.f
OBJS    =
FC      =$(FCMP)
LIBS    =  -L$(SIGIO_DIR) -l$(SIGIO_LIB4) -L$(BACIO_DIR) -l$(BACIO_LIB4) -L$(W3NCO_DIR) -l$(W3NCO_LIB4)
CMD     =global_sighdr
$(CMD): $(SRCM) $(OBJS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
install:
	-cp -p $(CMD) ../../exec/.
clean:
	-rm -f $(OBJS) $(CMD)

SHELL   =/bin/sh
INCMOD  =$(SFCIO_INC4)               
SRCM    =sfchdr.f
OBJS    =
FC      =$(FCMP)
LIBS    =  -L$(SFCIO_DIR) -l$(SFCIO_LIB4) -L$(BACIO_DIR) -l$(BACIO_LIB4) -L$(W3NCO_DIR) -l$(W3NCO_LIB4)
CMD     =global_sfchdr
$(CMD): $(SRCM) $(OBJS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
install:
	-cp -p $(CMD) ../../exec/.
clean:
	-rm -f $(OBJS) $(CMD)

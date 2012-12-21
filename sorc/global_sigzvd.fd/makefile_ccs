SHELL	=/bin/sh
INCMOD1	=/nwprod/lib/incmod/sigio_4 
SRCM	=global_sigzvd.f
SRCS	=
OBJS	=
FC	=ncepxlf
FFLAGS	=-qnosave -qsmp=noauto -O2 -qmaxmem=-1
LDFLAGS	= -lesslsmp
LIBS	= -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 -lsigio_4 -lsfcio_4
CMD	=global_sigzvd
$(CMD):	$(SRCM) $(OBJS)
	$(FC) -I $(INCMOD1) $(FFLAGS) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS)
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) makefile

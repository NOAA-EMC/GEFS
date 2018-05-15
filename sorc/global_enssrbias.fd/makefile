SHELL=  /bin/bash
CMD=	global_enssrbias 
FC=     ifort
FFLAGS=  -list -traceback -convert big_endian -assume byterecl
LDFLAGS=
INCS=
SRCS=	STAT_RM_BIAS_30D.f
OBJS=   $(SRCS:.f=.o)
#LIBS    = -l essl -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 
LIBS    =

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:    $(CMD)

$(CMD): $(OBJS)
	$(FC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

$(OBJS):        $(SRCS)
	$(FC) $(FFLAGS) $(INCS) -c $(SRCS)

$(OBJS):        makefile

allclean:       clean all

clean:
	-rm -f $(OBJS)

install:
	mv $(CMD) ../../exec/$(CMD)


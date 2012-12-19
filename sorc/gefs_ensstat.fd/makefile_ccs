SHELL=  /bin/sh
CMD=	gefs_ensstat
FC=     ncepxlf
FOPTS=  -qsmp=noauto
LOPTS=
INCS=
SRCS=	gefs_ensstat.f 
OBJS=   gefs_ensstat.o
LIBS    = -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 \
	-l essl
$(CMD): $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)

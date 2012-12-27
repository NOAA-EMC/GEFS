SHELL=  /bin/sh
CMD=	global_ensadd
FC=     ncepxlf
FOPTS=  -qsmp=noauto
LOPTS=
INCS=
SRCS=	ENSADD.f 
LIBS    = -l essl -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4
$(CMD): $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
global_ensadd: $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o ensadd

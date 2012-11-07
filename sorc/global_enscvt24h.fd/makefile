SHELL=  /bin/sh
CMD=	global_enscvt24h   
FC=     ncepxlf
FOPTS=  -qsmp=noauto
LOPTS=
INCS=
SRCS=	CVT24H.f          
LIBS    = -l essl -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 
$(CMD): $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
global_enscvt24h: $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o cvt24h

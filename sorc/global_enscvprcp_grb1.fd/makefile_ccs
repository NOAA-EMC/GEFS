SHELL=  /bin/sh
CMD=	global_enscvprcp 
FC=     ncepxlf
FOPTS=  -qsmp=noauto
LOPTS=
INCS=
SRCS=	CVPRCP.f            
LIBS    = -l essl -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 
$(CMD): $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
global_enscvprcp: $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o cvprcp

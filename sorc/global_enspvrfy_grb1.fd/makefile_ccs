SHELL=  /bin/sh
CMD=	global_enspvrfy  
FC=     ncepxlf
FOPTS=  -qsmp=noauto
LOPTS=
INCS=
SRCS=	PVRFY.f VRFY.f ANNATV.f GRANGE.f

LIBS    = -l essl -L/nwprod/lib -lbacio_4 -lw3_4 -lip_4 -lsp_4 
$(CMD): $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
global_enspvrfy: $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o pvrfy

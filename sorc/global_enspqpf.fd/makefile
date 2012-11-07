SHELL=  /bin/sh
CMD=	global_enspqpf
FC=     ncepxlf
FOPTS=  -qsmp=noauto
LOPTS=
INCS=
SRCS=	PQPF.f 
LIBS=	-L/nwprod/lib/ -lbacio_4 -lw3_4 -lip_4 -lsp_4 \
	-l essl
$(CMD): $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)

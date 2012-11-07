SHELL=  /bin/sh
CMD=	global_ensprcpcv
FC=     xlf_r
FOPTS=  -qsmp=noauto
LOPTS=
INCS=
SRCS=	PRCPCV.f 
LIBS=	/nwprod/w3lib90/bacio_4_pwr3 \
	/nwprod/w3lib90/w3lib_4_pwr3 \
	/nwprod/w3lib90/iplib_4_pwr3 \
	/nwprod/w3lib90/splib_4_pwr3 \
	-l essl
$(CMD): $(SRCS)
	$(FC) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
global_ensprcpcv: $(SRCS)
	$(FC) $(FOPTS) $(SRCS) /nwprod/w3lib90/iplib_4_pwr3 $(LIBS) -o prcpcv

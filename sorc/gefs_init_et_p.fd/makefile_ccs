INCMOD1=/nwprod/lib/incmod/sigio_4
BYTES=4

LIBS= -L /nwprod/lib/ -lsigio_4 -lw3_$(BYTES) \
	-lsp_$(BYTES) \
	-lip_$(BYTES) \
	-lbacio_$(BYTES) \
	-lsp_d -lw3_d \
	-lbufr_d_64 -L /usr/lib -lpesslsmp -lblacssmp -lessl

#	-lsigio_$(BYTES) \
#	-lsfcio_$(BYTES) \
#LIB2=-L /nfsuser/g01.1/weather/wx20wm/d/nwgeiu/sorc/gefs_init_et.fd -lsigio_4.a

OPTS= -qnosave -qarch=auto -qintsize=4 \
-qcheck -qsmp=noauto -I $(INCMOD1) #-g -bnoquiet 

gefs_init_et_p:	gefs_init_et_p.f90 
	mpxlf95_r $(OPTS) -o $@ gefs_init_et_p.f90 $(LIBS)


test:
	echo $(LIBS)



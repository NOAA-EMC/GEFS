#ARCH=pwr3
INCMOD1=/nwprod/lib/incmod/sigio_4
ARCH=pwr4
BYTES=4
#LIBS=	-L /nwprod/lib/ -lsigio_4 -L /nwprod/lib/ -lw3_$(BYTES) \
#
LIBS=	-L /global/save/wx20rw/s/nwgeja/sorc/gefs_init_et.fd -lsigio_4 -L /nwprod/lib/ -lw3_$(BYTES) \
	-lsp_$(BYTES) \
	-lip_$(BYTES) \
	-lbacio_$(BYTES) \
	-lsp_d -lw3_d \
	-lbufr_d_64 \

#	-lsigio_$(BYTES) \
#	-lsfcio_$(BYTES) \
#LIB2=-L /nfsuser/g01.1/weather/wx20rw/d/nwgeiu/sorc/gefs_init_et.fd -lsigio_4.a

OPTS= -qnosave -qarch=$(ARCH) -qrealsize=$(BYTES) -qintsize=$(BYTES) \
-qcheck -g -bnoquiet -qfixed -qsmp=noauto -lessl -I $(INCMOD1)
#OPTS= -qnosave -qarch=$(ARCH) -qrealsize=$(BYTES) -qintsize=$(BYTES) \
#-qcheck -g -bnoquiet -qfixed -qsmp=noauto -lessl -I /global/save/wx20rw/s/nwgeja/sorc/gefs_init_et.fd
#OPTS= -qnosave -qarch=$(ARCH) -qrealsize=$(BYTES) -qintsize=$(BYTES) \
#-qcheck -g -bnoquiet -qfixed -qsmp=noauto -lessl -I /global/noscrub/wx20rw/d/nwgeja/sorc/gefs_init_et.fd
# -L /nwprod/lib -lsigio_4 -I /nwprod/lib/incmod
# -bmaxdata:2000000000 -bmaxstack:256000000 \
# -L /nfsuser/ibmapp/gwv/lib -l lapack
#  -bnoquiet -qfixed -qsmp=noauto -lessl
gefs_init_et:	gefs_init_et.f 
	xlf_r $(OPTS) -o gefs_init_et gefs_init_et.f $(LIBS)

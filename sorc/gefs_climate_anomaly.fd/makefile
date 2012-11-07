SHELL=	/bin/sh
LIBS=   -L/nwprod/lib/ -lbacio_4 -lw3_4 -lbufr_4_32 -lip_4 -lsp_4
SRCS=	gefs_climate_anomaly.f90 CDFNOR.f90 
CMD=	gefs_climate_anomaly
FC=	ncepxlf90
FFLAGS=	-O3 -qsuffix=f=f90 -qlist -qsource -qlanglvl=90PURE -bnoquiet 
#FFLAGS=	-O3 -qsuffix=f=f90 -qlist -qsource -bnoquiet 

$(CMD):	$(SRCS)
	$(FC) $(SRCS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

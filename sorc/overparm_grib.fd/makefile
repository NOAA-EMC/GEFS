BACIO_LIB4=/gpfs/dell1/nco/ops/nwprod/lib/bacio/v2.0.2/ips/18.0.1/libbacio_v2.0.2_4.a
W3NCO_LIB4=/gpfs/dell1/nco/ops/nwprod/lib/w3nco/v2.0.6/ips/18.0.1/libw3nco_v2.0.6_4.a
#LIBS="/gpfs/dell1/nco/ops/nwprod/lib -l${BACIO_LIB4} ${W3NCO_LIB4}"
#LIBS="-L/gpfs/dell1/nco/ops/nwprod/lib -lbacio/v2.0.2/ips/18.0.1/libbacio_v2.0.2_4.a -lw3nco/v2.0.6/ips/18.0.1/libw3nco_v2.0.6_4.a"
#LIBS =    -L/gpfs/dell1/nco/ops/nwprod/lib -lw3nco_v2.0.3_4 -lbacio_4
LIBS='${BACIO_LIB4} ${W3NCO_LIB4}'
overparm_grib:overparm_grib.f
#	ifort -o overparm_grib overparm_grib.f $(LIBS)
	ifort -o overparm_grib overparm_grib.f $(BACIO_LIB4) $(W3NCO_LIB4)


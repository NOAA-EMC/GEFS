#!/bin/ksh
#
#  UTILITY SCRIPT NAME :  exgefs_wafs.sh.ecf
#               AUTHOR :  Boi Vuong
#         DATE WRITTEN :  10/17/2006
#
# This script sets up a poe script to run in parallel to 
#      create WAFS (grid 37-44) from the Global Ensemble data. 
# History log:
#   8/2015: Modified for WCOSS phase2
#   4/2021: Dingchen Hou, 
#           Modified and renamed exgefs_atmos_makesbn.sh 
#           Skipped the WAFS (grid 37-44) part 
#           Focus on grib2 files,using the gefs_mmefs_awips.sh ush scrip
#           Including variable selctio, cutting to latlon regional grid,i
#              and adding WMO headers
#
########################################

### need pass the values of CYC, YMD, DATA, COMIN and COMOUT

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
    # Turn on strict bash error checking
    set -eu
fi

########################################

cd $DATA

echo " ------------------------------------------"
echo " BEGIN MAKING WMO-HEADED GEFS GRIB2 PRODUCTS"
echo " ------------------------------------------"

gridp25="-170:441:0.25 75:241:-0.25"
gridp5="-170:221:0.50 75:121:-0.50"

if [ ${FORECAST_SEGMENT} = hr ]; then
    ${USHgefs}/gefs_atmos_getsbn.sh avg apcp APCP pgrb2sp25 pgrb2s.0p25 0p25 003 240 3 "$gridp25"
    ${USHgefs}/gefs_atmos_getsbn.sh avg tmax TMAX pgrb2sp25 pgrb2s.0p25 0p25 003 240 3 "$gridp25"
    ${USHgefs}/gefs_atmos_getsbn.sh avg tmin TMIN pgrb2sp25 pgrb2s.0p25 0p25 003 240 3 "$gridp25"
    ${USHgefs}/gefs_atmos_getsbn.sh avg apcp APCP pgrb2ap5 pgrb2a.0p50 0p50 246 384 6 "$gridp5"
    ${USHgefs}/gefs_atmos_getsbn.sh avg tmax TMAX pgrb2ap5 pgrb2a.0p50 0p50 246 384 6 "$gridp5"
    ${USHgefs}/gefs_atmos_getsbn.sh avg tmin TMIN pgrb2ap5 pgrb2a.0p50 0p50 246 384 6 "$gridp5"
else
    ${USHgefs}/gefs_atmos_getsbn.sh avg apcp APCP pgrb2ap5 pgrb2a.0p50 0p50 390 840 6 "$gridp5"
    ${USHgefs}/gefs_atmos_getsbn.sh avg tmax TMAX pgrb2ap5 pgrb2a.0p50 0p50 390 840 6 "$gridp5"
    ${USHgefs}/gefs_atmos_getsbn.sh avg tmin TMIN pgrb2ap5 pgrb2a.0p50 0p50 390 840 6 "$gridp5"
fi

#####################################################################
# GOOD RUN
set +x
echo "*********JOB ${.sh.file} HAS COMPLETED NORMALLY******"
set -x
#####################################################################

echo "$(date -u) end ${.sh.file}"
exit 0


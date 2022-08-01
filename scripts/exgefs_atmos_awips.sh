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
#   07/2022: Xianwu Xue
#           Improve, optimize and rename exgefs_atmos_awips.sh
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

for var in apcp tmax tmin
do
    nvar=$(echo $var|tr '[a-z]' '[A-Z]')
    if [ ${FORECAST_SEGMENT} = hr ]; then
        ${USHgefs}/gefs_atmos_getsbn.sh avg ${var} ${nvar} pgrb2sp25 pgrb2s.0p25 0p25 006 240 6 "$gridp25"
        export err=$?; if [[ $err != 0 ]]; then exit $err; fi
        ${USHgefs}/gefs_atmos_getsbn.sh avg ${var} ${nvar} pgrb2ap5 pgrb2a.0p50 0p50 246 384 6 "$gridp5"
        export err=$?; if [[ $err != 0 ]]; then exit $err; fi
    else
        ${USHgefs}/gefs_atmos_getsbn.sh avg ${var} ${nvar} pgrb2ap5 pgrb2a.0p50 0p50 390 840 6 "$gridp5"
        export err=$?; if [[ $err != 0 ]]; then exit $err; fi
    fi
done


#####################################################################
# GOOD RUN
set +x
echo "*********JOB ${.sh.file} HAS COMPLETED NORMALLY******"
set -x
#####################################################################

echo "$(date -u) end ${.sh.file}"
exit 0


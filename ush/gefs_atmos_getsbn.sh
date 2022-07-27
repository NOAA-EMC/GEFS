#!/bin/ksh
######################################################################
#  UTILITY SCRIPT NAME :  gefs_mmefs_awips.sh
#         DATE WRITTEN :  08/30/2013
#
#  Abstract:  This utility script produces Meteorological Model Ensemble
#             Forecast System (MMEFS) of selected variables (Accumulated 
#             precipitation and 2 meter Temperature) for grid id 3 (Global
#             Lat/Lon 1 degree resolution) in GRIB2 format for AWIPS
#             from the GEFS model.
# History log:
#   8/2015: Modified for WCOSS phase2
#   4/2021: Dingchen Hou
#	Substantial modified and converted to a utility for collecting a variable,
#		Cutting to regional latlon grid abd adding WMO header
#           (1) for a single variable (APCP, TMAX or TMIN)
#           (2) for a single grid (and lead times) of GEFS v12, 0p25 (0-240h)  
#		and 0p50 (246-384h and 390-840h)
#           (3) for one ensemble member (ge*$mem, mem can be avg or spr) only
#           (4) for a single output grid ($cutgrid) only
#   07/22/2022: Xianwu Xue
#               Port it to WCOSS2 and make some improvement
######################################################################

  msg="HAS BEGUN!"
  echo "$msg"
########################################

set +x
echo " "
echo "#########################################"
echo "                                         "
echo "   Process GEFS (MMEFS) GRIB2 PRODUCTS   "
echo "   FOR FORECAST HOURS $7 - $8            "
echo "                                         "
echo "#########################################"
echo " "
set -x

export SLEEP_TIME=900
export SLEEP_INT=5
export SLEEP_LOOP_MAX=`expr $SLEEP_TIME / $SLEEP_INT`

mem=$1
var=$2
nvar=$3
type=$4
fntype=$5
gdtype=$6
bhr=$7
ehr=$8
ihr=$9
cutgrid=${10}

mkdir $DATA/$var
cd $DATA/$var
# Selecting requested variable
file=grib2.gefs.t${cyc}z.${var}.f${bhr}_${ehr}
hr=$bhr
while (( hr <= $ehr )); do
    hr3=`printf %03d $hr`
    export pgm="postcheck"
    set -x
    file_temp=ge${mem}.t${cyc}z.${type}f${hr3}
    ln -s ${COMIN}/atmos/${type}/ge${mem}.t${cyc}z.${fntype}.f${hr3} ${file_temp}
    $WGRIB2 ${file_temp} | grep "$nvar" | $WGRIB2 -i ${file_temp} -grib ${file_temp}_$var
    cat  ${file_temp}_$var >> $file
    rm  ${file_temp}*
    (( hr = hr + $ihr ))
done

#Cutting the files to latlon CONUS (170W-60W,75N-15N)
$WGRIB2 $file $option1 $option21 $option22 $option23 $option24 \
	    $option25 $option26 $option27 $option28 \
	    -new_grid latlon $cutgrid \
	    ${file}.conus_notoc

############################################
# Processing GRIB2 GEFS grid 3 for MMEFS
############################################

pgm=tocgrib2
. prep_step
startmsg

# Processing GEFS (MMEFS) GRIB2 
export FORT11=${file}.conus_notoc
export FORT51=${file}.conus
$TOCGRIB2 < $PARMgefs/wmo/grib2_awips_gefs_f${bhr}_${ehr}_${var}_conus >> $pgmout 2>errfile
err=$?; export err; err_chk
echo " error from tocgrib2=",$err

if [ $? -ne 0 ]; then
	msg="WARNING: WMO header not added to $FORT11"
	echo "$msg"
fi 
mv $FORT51 $DATA

cd $DATA

# Sending files to COM area
fileout=${file}.conus
if [ -s $fileout ]; then
	if [ "$SENDCOM" = YES ]; then
        ##############################
        # Post Files to COMOUTwmo
        ##############################
        cpfs $fileout $COMOUTwmo

        if [ "$SENDDBN_NTC" = YES ]; then
            ##########################
            # Distribute Data to NCF
            #########################
            $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $$COMOUTwmo/${fileout}
        fi
	fi
else
	err_exit "file $fileout was not generated"
fi

msg=" $var for $bhr to $ehr hours HAS COMPLETED NORMALLY!"
echo "$msg"

echo "$(date -u) end ${.sh.file}"

exit 0
############## END OF SCRIPT #######################

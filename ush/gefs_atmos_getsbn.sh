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
#	Substantial modified and coverted to a utility for colleting a variable,
#		Cutting to regional latlon grid abd adding WMO header
#           (1) for a single variable (APCP, TMAX or TMIN)
#           (2) for a single grid (and lead times) of GEFS v12, 0p25 (0-240h)  
#		and 0p50 (246-384h and 390-840h)
#           (3) for one ensemble member (ge*$mem, mem can be avg or spr) only
#           (4) for a single output grid ($cutgrid) only
######################################################################

  msg="HAS BEGUN!"
  postmsg "$jlogfile" "$msg"
########################################

set -x 

echo " ------------------------------------------"
echo "  BEGIN MAKING GEFS (MMEFS) GRIB2 PRODUCTS "
echo " ------------------------------------------"

set +x
echo " "
echo "#########################################"
echo "                                         "
echo "   Process GEFS (MMEFS) GRIB2 PRODUCTS   "
echo "   FOR FORECAST HOURS 00 - 180.          "
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
#Selecting requested variable
id=ge$mem
hr=$bhr
while (( hr <= $ehr )); do
 
	hr3=`printf %03d $hr`
	export pgm="postcheck"
	set -x
	ln -s ${COMIN}/atmos/${type}/${id}.t${cyc}z.${fntype}.f${hr3} ${id}.t${cyc}z.${type}f${hr3}
	$WGRIB2 ${id}.t${cyc}z.${type}f${hr3} | grep "$nvar" | \
		$WGRIB2 -i ${id}.t${cyc}z.${type}f${hr3} -grib ${id}.t${cyc}z.${type}f${hr3}_$var
	cat  ${id}.t${cyc}z.${type}f${hr3}_$var >> ${id}.t${cyc}z.pgrb2.$gdtype.f${bhr}-${ehr}_$var
	rm  ${id}.t${cyc}z.${type}f${hr3}*
	(( hr = hr + $ihr ))
done

#Cutting the files to latlon CONUS (170W-60W,75N-15N)
file=ge${mem}.t${cyc}z.pgrb2.$gdtype.f${bhr}-${ehr}_$var
$WGRIB2 $file $option1 $option21 $option22 $option23 $option24 \
	$option25 $option26 $option27 $option28 \
	-new_grid latlon $cutgrid \
	$file\_conus

############################################
# Processing GRIB2 GEFS grid 3 for MMEFS
############################################

pgm=tocgrib2
. prep_step
startmsg

# Processing GEFS (MMEFS) GRIB2 
file=${file}_conus
export FORT11=$file
export FORT51=$FORT11\_TOC
$TOCGRIB2 < $PARMgefs/gefs_sbn_grib2_${bhr}-${ehr}_$var >> $pgmout 2>errfile
err=$?;export err ;err_chk
echo " error from tocgrib2=",$err

if [ $? -ne 0 ]; then
	msg="WARNING: WMO header not added to $FORT11"
	postmsg $jlogfile "$msg"
	echo "$msg"
fi 
mv $FORT51 $DATA

cd $DATA

# Sending files to COM area
#fileout=ge${mem}.t${cyc}z.pgrb2.${gdtype}.f${bhr}-${ehr}_${var}_conus_TOC
fileout=${file}_TOC
if [ -s $fileout ]; then
	if [ "$SENDCOM" = YES ]; then
            ##############################
            # Post Files to PCOM
            ##############################
            cpfs $fileout $PCOM

            if [ "$SENDDBN_NTC" = YES ]; then
               ##########################
               # Distribute Data to NCF
               #########################
               $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $PCOM/${fileout}
            fi
	fi
else
	err_exit "file $fileout was not generated"
fi

msg=" $var for $bhr to $ehr hours HAS COMPLETED NORMALLY!"
postmsg "$jlogfile" "$msg"

############## END OF SCRIPT #######################

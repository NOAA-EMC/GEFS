######################### CALLED BY EXENSCQPF ##########################
echo "------------------------------------------------"
echo "Ensemble CQPF -> global_enspvrfysh              "
echo "------------------------------------------------"
echo "History: Feb 2004 - First implementation of this new script."
echo "AUTHOR: Yuejian Zhu (wx20yz)"
echo "History: Nov 2014 - Grib2 code conversion"
echo "AUTHOR: Yan Luo (wx22lu)"

echo "         ######################################### "
echo "         ####  RUN PRECIPTATION VERIFICATION  #### "
echo "         ####  RUN PRECIPTATION VERIFICATION  #### "
echo "         ####  RUN PRECIPTATION VERIFICATION  #### "
echo "         ######################################### "

set -x

export CVT24H=$USHgefs/global_enscvt24h.sh
export WGREP=$USHgefs/global_enswgrp.sh

INITIME=0

export APRUN=${gefsmpexec:-mpirun}

###
### forecast precipitation file name like this
###
### gfs_2000101000_12_24
### gfs_2000101000_24_36
### above two file verified to 2000101012-2000101112(usa-dlyprcp-20001011)

export hourlist="00 36 60 84 108 132 156 180 204 228 252 276 300 324 348 372"

>cvt24h.cmdfile
for fhour in $hourlist; do
	if [ $fhour -eq 00 ]; then
		CYMDH=`$NDATE -$fhour $OBSYMD\00`
	else
		CYMDH=`$NDATE -$fhour $OBSYMD\12`
	fi
	echo "$CVT24H $CYMDH" >>cvt24h.cmdfile
done # for fhour in $hourlist

cat cvt24h.cmdfile
chmod 775 cvt24h.cmdfile
export MP_CMDFILE=$DATA/cvt24h.cmdfile
export MP_PGMMODEL=mpmd
$APRUN

for fhour in $hourlist; do
	$WGREP $OBSYMDH $fhour 1 gfs    
	$WGREP $OBSYMDH $fhour 1 ctl
done

for RUNID in gfs ctl; do
	PRECIP_DATA_FILE=usa-dlyprcp-${OBSYMD}
	PRECIP_ANALYSIS_FILE=precip.$RUNID

	# 1   PATH TO MODEL INFO FILE
	# 2   PATH TO MODEL READIN REGIONAL MASK
	# 3   PATH TO MODEL ON TMP DIRECTORY
	# 4   PATH TO PCP ANALYSIS FILE
	# 5   PATH TO PCP DATA FILE
	# 6   FACTER FOR MRF CTL FORECAST ( Default:1.0 )

	cat <<-nameEOF >input_runv
		model_info_file
		$FIXgefs/pcpmask                                
		$DATA                   
		$PRECIP_ANALYSIS_FILE
		$PRECIP_DATA_FILE
		1.0
		nameEOF

	# 1   MODEL NAME, SEE GETARCH FOR PROPER SPELLING AND PATH TO ARCHIVE
	# 2   GRID # TO TAKE FROM MODEL GRIB FILE FOR VERIFICATION (-1 MEANS 1ST)
	#     126 for t126 Gaussian Grid. 98 for t62 Gaussian Grid.
	#     144*73 resolution is 2                               
	# 3   NUMBER OF CYCLES AVAILABLE  (ex, 1 for ecmwf, possibly 4 for gfs)
	# 4   CYCLE #1
	# 5   CYCLE #2
	# ...
	# 6   OUTPUT FREQUENCY IN HOURS 
	# 7   FORECAST DURATION IN HOURS 
	# ...REPEAT

	cat <<-modelEOF >model_info_file
		$RUNID
		2   
		1
		$INITIME
		24
		372
		done
		modelEOF

	### obs_box.dat contains observation analysis at each grid points
	### stat.out contains verification output

	if [ -s obs_box.dat ]; then
		rm obs_box.dat
	fi

	export pgm=global_enspvrfy
	. prep_step

	startmsg

	$EXECgefs/global_enspvrfy  <input_runv   >> $pgmout 2>errfile
	#export err=$?;err_chk

	cat  stat.out 
	mv stat.out    $DATA/rain_$RUNID.$OBSYMD
	mv obs_box.dat $DATA/obs_box_$RUNID.$YMD
done # for RUNID in gfs ctl


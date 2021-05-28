#! /bin/ksh

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

##############################################
# set last forecast hour to save sfcsig files
# set cycles to save sfcsig or master files
##############################################
fhsave=${fhsave:-"f000 f192 f384 f390 f840"}
cycsavelistsfcsig=${cycsavelistsfcsig:-""}
cycsavelistmaster=${cycsavelistmaster:-""}
echo "fhsave=$fhsave"
echo "cycsavelistsfcsig=$cycsavelistsfcsig"
echo "cycsavelistmaster=$cycsavelistmaster"

FHINC=${FHINC:-0}

# Test GRIB files before cleaning up
echo "$(date) GRIB file test before cleanup begin"
((nmissing = 0))

for member in $memberlist; do
	####################################
	# Specify Forecast Hour Range
	####################################
	SHOUR=0

	# Analysis files
	# filelist="\
	# 	$COMIN/$COMPONENT/master/ge${member}.$cycle.master.grb2anl \
	# 	$COMIN/$COMPONENT/master/ge${member}.$cycle.master.grb2ianl \
	# 	$COMIN/$COMPONENT/pgrb22p5/ge${member}.$cycle.pgrb2.2p50.anl \
	# 	$COMIN/$COMPONENT/pgrb22p5/ge${member}.$cycle.pgrb2.2p50.anl.idx \
	# 	$COMIN/$COMPONENT/pgrb2ap5/ge${member}.$cycle.pgrb2a.0p50.anl \
	# 	$COMIN/$COMPONENT/pgrb2ap5/ge${member}.$cycle.pgrb2a.0p50.anl.idx \
	# 	$COMIN/$COMPONENT/pgrb2bp5/ge${member}.$cycle.pgrb2b.0p50.anl \
	# 	$COMIN/$COMPONENT/pgrb2bp5/ge${member}.$cycle.pgrb2b.0p50.anl.idx \
	# 	$COMIN/$COMPONENT/pgrb2p25/ge${member}.$cycle.pgrb2.0p25.anl \
	# 	$COMIN/$COMPONENT/pgrb2p25/ge${member}.$cycle.pgrb2.0p25.anl.idx \
	# 	$COMIN/$COMPONENT/pgrb2sp25/ge${member}.$cycle.pgrb2s.0p25.anl \
	# 	$COMIN/$COMPONENT/pgrb2sp25/ge${member}.$cycle.pgrb2s.0p25.anl.idx \
	# "
	# for file in $filelist; do
	# 	if [[ ! -s $file ]]; then
	# 		((nmissing = nmissing + 1))
	# 		echo "nmissing=$nmissing file=$file IS MISSING"
	# 	fi
	# done # file in $filelist

	fhr=$SHOUR
	while (( fhr <= fhmaxh )); do

		ffhr="f$(printf %03i $fhr)"

		filelist=" \
			$COMIN/$COMPONENT/master/ge${member}.$cycle.master.grb2${ffhr} \
			$COMIN/$COMPONENT/master/ge${member}.$cycle.master.grb2i${ffhr} \
			$COMIN/$COMPONENT/pgrb2ap5/ge${member}.$cycle.pgrb2a.0p50.${ffhr} \
			$COMIN/$COMPONENT/pgrb2ap5/ge${member}.$cycle.pgrb2a.0p50.${ffhr}.idx \
			$COMIN/$COMPONENT/pgrb2bp5/ge${member}.$cycle.pgrb2b.0p50.${ffhr} \
			$COMIN/$COMPONENT/pgrb2bp5/ge${member}.$cycle.pgrb2b.0p50.${ffhr}.idx \
			$COMIN/$COMPONENT/sfcsig/ge$member.$cycle.atm${ffhr}.nemsio \
			$COMIN/$COMPONENT/sfcsig/ge$member.$cycle.sfc${ffhr}.nemsio \
		"

		# 2p50 files only at FHOUTLF frequency
		if (( fhr%FHOUTLF == 0 )); then
			filelist="$filelist \
				$COMIN/$COMPONENT/pgrb22p5/ge${member}.$cycle.pgrb2.2p50.${ffhr} \
				$COMIN/$COMPONENT/pgrb22p5/ge${member}.$cycle.pgrb2.2p50.${ffhr}.idx \
			"
		fi

		# 0p25 only during high-frequency period
		if (( fhr <= FHMAXHF )); then
			filelist="$filelist \
				$COMIN/$COMPONENT/pgrb2p25/ge${member}.$cycle.pgrb2.0p25.${ffhr} \
				$COMIN/$COMPONENT/pgrb2p25/ge${member}.$cycle.pgrb2.0p25.${ffhr}.idx \
				$COMIN/$COMPONENT/pgrb2sp25/ge${member}.$cycle.pgrb2s.0p25.${ffhr} \
				$COMIN/$COMPONENT/pgrb2sp25/ge${member}.$cycle.pgrb2s.0p25.${ffhr}.idx \
			"
		fi

		for file in $filelist; do
			if [[ ! -s $file ]]; then
				((nmissing = nmissing + 1))
				echo "nmissing=$nmissing file=$file IS MISSING"
			fi
		done # file in $filelist

		if (( fhr < FHMAXHF )); then
			(( fhr = fhr + FHOUTHF ))
		else
			(( fhr = fhr + FHOUTLF ))
		fi
	done # (( fhr <= fhmaxh ))
done # member in $memberlist

echo "nmissing=$nmissing"
echo "$(date) GRIB file test before cleanup end"
if ((nmissing > 0)); then
	cat  > msg <<-EOF
		WARNING: in ${.sh.file}: GRIB file test before cleanup IDENTIFIED $nmissing MISSING FILES
		Will skip this cycle's cleanup.
		If this is the first run or first 00z run after machine switch, then you can safely ignore it.
		Otherwise please double check if the model missing any file.
	EOF
	cat msg |mail.py  -s "Skip $cycle GEFS cleanup." nco.spa@noaa.gov
	exit
else # (( nmissing > 0 ))
	echo "$(date) sfcsig sflux cleanup begin"

	savecycle=false
	for cycsave in $cycsavelistsfcsig; do
		if ((cyc == cycsave)); then
			savecycle=true
		fi
	done # cycsave in $cycsavelistsfcsig

	echo "cyc=$cyc savecycle=$savecycle"
	if [[ $savecycle == false ]]; then
		for member in $memberlist; do
			if [[ -z ${HOUTSPS:-""} ]]; then
				export FHOUR=$fhmaxh
			else
				((FHOUR = fhmaxh - HOUTSPS - FHINC))
			fi
			export FHOUR
			export member
			###########################################################
			# remove sigma, surface, and flux files
			###########################################################
			$HOMEgefs/ush/gefs_post_cleanup.sh
            ########################
            # remove /nwges RESTART directory
            ########################
            rm -rf $GESOUT/init/$member/RESTART
		done # member in $memberlisttest
	else # [[ $savecycle = false ]]
		echo "sfcsig files are saved for cyc=$cyc"
	fi # [[ $savecycle = false ]]

	echo "$(date) sfcsig cleanup end"

	########################################################
	# remove master post subdirectory
	########################################################
	echo "$(date) before master cleanup"
	savecycle=false
	for cycsave in $cycsavelistmaster; do
		if ((cyc == cycsave)); then
			savecycle=true
		fi
	done # cycsave in $cycsavelistmaster
	echo "cyc=$cyc savecycle=$savecycle"

	if [[ $savecycle == false ]]; then
		if [[ $SENDCOM == "YES" ]]; then
			rm -rf $COMOUT/$COMPONENT/master
		fi
	else
		echo "master files are saved for cyc=$cyc"
	fi
	echo "$(date) after master cleanup"
fi # (( nmissing > 0 ))

echo "$(date -u) end ${.sh.file}"

exit 0


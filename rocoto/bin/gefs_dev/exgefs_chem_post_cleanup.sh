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
fhsave=${fhsave:-240}
cycsavelistsfcsig=${cycsavelistsfcsig:-"00"}
cycsavelistmaster=${cycsavelistmaster:-""}
echo "fhsave=$fhsave"
echo "cycsavelistsfcsig=$cycsavelistsfcsig"
echo "cycsavelistmaster=$cycsavelistmaster"

# Test GRIB files before cleaning up
echo "$(date) GRIB file test before cleanup begin"
((nmissing = 0))

export member="aer"

####################################
# Specify Forecast Hour Range
####################################
SHOUR=0

# filelist="\
# 	$COMIN/$COMPONENT/master/ge${member}.$cycle.master.grb2anl \
# 	$COMIN/$COMPONENT/master/ge${member}.$cycle.master.grb2ianl \
# "
# for file in $filelist; do
# 	if [[ ! -s $file ]]; then
# 		((nmissing = nmissing + 1))
# 		echo "nmissing=$nmissing file=$file IS MISSING"
# 	fi
# done # file in $filelist

fhr=$SHOUR
while (( fhr <= fhmax_aer )); do

	ffhr="f$(printf %03i $fhr)"

	filelist="\
		$COMIN/$COMPONENT/master/ge${member}.$cycle.master.grb2${ffhr} \
		$COMIN/$COMPONENT/master/ge${member}.$cycle.master.grb2i${ffhr} \
		$COMIN/$COMPONENT/pgrb2ap25_aer/ge${member}.$cycle.pgrb2a.0p25.${ffhr} \
		$COMIN/$COMPONENT/pgrb2ap25_aer/ge${member}.$cycle.pgrb2a.0p25.${ffhr}.idx \
		$COMIN/$COMPONENT/pgrb2ap50_aer/ge${member}.$cycle.pgrb2a.0p50.${ffhr} \
		$COMIN/$COMPONENT/pgrb2ap50_aer/ge${member}.$cycle.pgrb2a.0p50.${ffhr}.idx \
		$COMIN/$COMPONENT/sfcsig/ge$member.$cycle.atm${ffhr}.nemsio \
		$COMIN/$COMPONENT/sfcsig/ge$member.$cycle.sfc${ffhr}.nemsio \
	"

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
done # (( fhr <= fhmax_aer ))

echo "nmissing=$nmissing"
echo "$(date) GRIB file test before cleanup end"
if ((nmissing > 0)); then
	echo "FATAL ERROR in ${.sh.file}: GRIB file test before cleanup IDENTIFIED $nmissing MISSING FILES"
	export err=99
	err_chk
	exit $err
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
		if [[ -z ${HOUTSPS:-""} ]]; then
			export FHOUR=$fhmax_aer
		else
			((FHOUR = fhmax_aer - HOUTSPS - FHINC))
		fi
		export FHOUR
		###########################################################
		# remove atm and sfc files
		###########################################################
		$HOMEgefs/ush/gefs_post_cleanup.sh
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


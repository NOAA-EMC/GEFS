#!/bin/ksh

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

# Define MASTERRES for master file grid, default being Gaussian with alternatives of  p125/p25/p5 
export MASTERRES=        
export res=$MASTERRES
export SUBJOB=${SUBJOB:-""}
export mem=$(echo $RUNMEM|cut -c3-5)

export RERUN=${RERUN:-NO}

###############################################################
# Specify locations of the following scripts and executables
###############################################################
export CHGRESSH=$USHgfs/global_chgres.sh
export CHGRESEXEC=$EXECgfs/global_chgres

export POSTGPSH=${POSTGPSH:-$USHpost/gfs_nceppost.sh}
export POSTGPEXEC=${POSTGPEXEC:-$EXECpost/gfs_ncep_post}
export nemsioget=$EXECgfs/nemsio_get
export SIGHDR=$nemsioget

export ERRSCRIPT=${ERRSCRIPT:-err_chk}
export LOGSCRIPT=startmsg
export REDOUT='1>>'
export REDERR='2>'

if [[ $SENDCOM == "YES" ]]; then
	mkdir -m 775 -p $COMOUT/$COMPONENT/master
	mkdir -m 775 -p $COMOUT/$COMPONENT/misc/post
fi

if [[ $REMAP_GRID = latlon ]]; then
	case $FORECAST_SEGMENT in
		(hr) 
			master_grid=0p25deg
			LEVS=$LEVSHR
			;;
		(lr)
			master_grid=0p50deg
			LEVS=$LEVSLR
			;;
		(*)
			echo "FATAL ERROR in ${.sh.file}: FORECAST_SEGMENT $FORECAST_SEGMENT unsupported!"
			export err=100
			exit $err
			;;
	esac

	case $master_grid in
		(0p25deg) LATB=720; LONB=1440;;
		(0p50deg) LATB=360; LONB=720;;
	esac
	export LATB
	export LONB

else  #REMAP_GRID
	export JCAP=$JCAPFV
	export LATB=$LATBFV
	export LONB=$LONBFV
fi
	
echo "LEVS = $LEVS  LATB = $LATB  LONB = $LONB"

####################################
# Specify Restart File Name to Key Off
####################################
restart_file=$COMIN/$COMPONENT/sfcsig/${RUNMEM}.t${cyc}z.logf

case $FORECAST_SEGMENT in
	(hr)
		export SHOUR=00;
		export FHOUR=$fhmaxh;;
	(lr)
		if (( FHMAXHF > fhmaxh )); then
			export SHOUR=$(( $fhmaxh + $FHOUTHF ))
		else
			export SHOUR=$(( $fhmaxh + $FHOUTLF ))
		fi
		export FHOUR=$fhmax;;
esac

export SHOUR=$SHOUR
export FHOUR=$FHOUR
export FHMAX_HF=$FHMAXHF
export FHOUT_HF=$FHOUTHF
export FHOUT_LF=$FHOUTLF
export SHOUR_LF=$FHMAX_HF

if [[ -n $SUBJOB ]]; then
	J1=$(echo $SUBJOB | cut -c1-1)
	J2=$(echo $SUBJOB | cut -c2-2)
	iJ2=$(printf '%d\n' "'$J2")
	iJ2=$((iJ2-65))

	export FHOUT_HF=$((FHOUTHF*J1))
	export FHOUT_LF=$((FHOUTLF*J1))

	export SHOUR=$(($SHOUR+$FHOUTHF*iJ2))
	export SHOUR_LF=$(($FHMAXHF+$FHOUTLF*iJ2))
fi

echo <<- EOF
	FORECAST_SEGMENT=$FORECAST_SEGMENT
	SUBJOB=$SUBJOB
	DOANALYSIS=$DOANALYSIS
	SHOUR=$SHOUR
	FHOUR=$FHOUR
	FHOUT_HF=$FHOUT_HF
	FHOUT_LF=$FHOUT_LF
	FHMAX_HF=$FHMAX_HF
	SHOUR_LF=$SHOUR_LF
EOF

export FHOUR=$FHOUR
export POSTGPVARS=$POSTGPVARS_HIGH

export post_log=$DATA/post.$FORECAST_SEGMENT.log$SUBJOB\_${RUNMEM}
postsh="$HOMEgefs/ush/gefs_atmos_post.sh $SHOUR $FHOUR $FHOUT_HF $FHOUT_LF $FHMAX_HF $SHOUR_LF"

$postsh
export err=$?
if [ $err -ne 0 ]; then
	echo "FATAL ERROR in ${.sh.file}: received a non-zero return code from $postsh"
	exit $err
fi

if [[ $SENDCOM == "YES" && -z $post_log ]]; then
	mv $post_log $COMOUT/$COMPONENT/misc/post
fi

echo "$(date -u) end ${.sh.file}"

exit 0

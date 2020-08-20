#!/bin/bash

echo "$(date -u) begin $(basename $BASH_SOURCE)"
export PS4="${PS4}${1}: "

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export mem=$1
export nmem=$(echo $mem|cut -c 2-)
nmem=${nmem#0}

export DATA=$DATA/$mem
export INIDIR=$DATA
export OUTDIR=$GESOUT/enkf/$mem
INITDIR=$GESOUT/init/$mem
mkdir -p $INIDIR
mkdir -p $OUTDIR
mkdir -p $INITDIR

cd $INIDIR

if [[ $mem = c00 ]] ;then
	# Control intial conditions from current GFS cycle
	ATMFILE=$COMINgfs/gfs.t${cyc}z.atmanl.nc
	if [[ -f $ATMFILE ]]; then
		$NCP $ATMFILE $INIDIR
		export ATM_FILES_INPUT="gfs.t${cyc}z.atmanl.nc"
	else
		msg="FATAL ERROR in $(basename $BASH_SOURCE): GFS atmospheric analysis file $ATMFILE not found!"
		echo "$msg"
		export err=101
		err_chk || exit $err
	fi
	export CONVERT_SFC=".true."

else
	i=0
	success="NO"
	(( cmem = nmem + memshift ))
	while [[ $success == "NO" && $i < $MAX_ENKF_SEARCHES ]]; do
		if (( cmem > 80 )); then
			(( cmem = cmem - 80 ))
		fi

		memchar="mem"$(printf %03i $cmem)
		ATMFILE="$COMINenkf$pdyp/$cycp/$memchar/gdas.t${cycp}z.atmf006.nc"

		if [[ -f $ATMFILE ]]; then
			$NCP $ATMFILE $INIDIR
			export ATM_FILES_INPUT="gdas.t${cycp}z.atmf006.nc"
			success="YES"

		else
			(( i = i + 1 ))
			if [[ $i < $MAX_ENKF_SEARCHES ]]; then
				echo "EnKF atmospheric file $ATMFILE not found, trying different member"
				(( cmem = cmem + ENKF_SEARCH_LEAP ))

			else
				msg="FATAL ERROR in $(basename $BASH_SOURCE): Unable to find EnKF atmospheric file after $MAX_ENKF_SEARCHES attempts"
				echo $msg
				export err=102
				err_chk || exit $err
			fi
		fi # [[ -f $ATMFILE ]]
	done # [[ success == "NO" && $i < MAX_ENKF_SEARCHES ]]
	export CONVERT_SFC=".false."
fi

if [[ $CONVERT_SFC == ".true." ]]; then
	export SFC_FILES_INPUT="gfs.t${cyc}z.sfcanl.nc"
	SFCFILE="$COMINgfs/$SFC_FILES_INPUT"
	if [[ -f $SFCFILE ]]; then
		$NCP $SFCFILE $INIDIR
	else
		msg="FATAL ERROR in $(basename $BASH_SOURCE): GFS surfce analysis $SFCFILE not found!"
		echo $msg
		export err=100
		err_chk || exit $err
	fi
fi

export CRES=$(echo $CASEHR |cut -c2-5)
export COMIN=$INIDIR
export INPUT_TYPE="gaussian_netcdf"
export FIXfv3=$FIXgfs/fix_fv3_gmted2010/C$CRES
export FIXsfc=$FIXfv3/fix_sfc
#############################################################
# Execute the script
$USHgfs/chgres_cube.sh
export err=$?
if [[ $err != 0 ]]; then
	echo "FATAL ERROR in $(basename $BASH_SOURCE): chgres_cube failed!"
	exit $err
fi
#############################################################

# Move files to the guess directory
for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
	mv ${DATA}/out.atm.${tile}.nc $OUTDIR/gfs_data.${tile}.nc
done
mv ${DATA}/gfs_ctrl.nc $OUTDIR/.

touch ${OUTDIR}/chgres_atm.log  # recenter can start now

if [[ $CONVERT_SFC == ".true." ]]; then
	# Copy sfc files to the guess directory for all members
	for mem2 in $memberlist; do
		INITDIR2=$GESOUT/init/$mem2
		mkdir -p $INITDIR2
		for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
			$NCP ${DATA}/out.sfc.${tile}.nc $INITDIR2/sfc_data.${tile}.nc
		done
	done
fi

# Copy control file to init
$NCP $OUTDIR/gfs_ctrl.nc $INITDIR

if [[ $SENDCOM == "YES" ]]; then
	MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
	DBNTYP=${MODCOM}_INIT	
	COMDIR=$COMOUT/init/$mem
	mkdir -p $COMDIR
	$NCP $OUTDIR/gfs_ctrl.nc $COMDIR
	if [[ $SENDDBN = YES ]];then
		$DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMDIR/gfs_ctrl.nc
	fi
	if [[ $mem == "c00" ]]; then
		$NCP $OUTDIR/gfs_data*.nc $COMDIR
		if [[ $SENDDBN = YES ]];then
			for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
				$DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMDIR/gfs_data.${tile}.nc
			done
		fi
	fi
	if [[ $CONVERT_SFC == ".true." ]]; then
		for mem2 in $memberlist; do
			COMDIR2=$COMOUT/init/$mem2
			mkdir -p $COMDIR2
			for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
				$NCP $GESOUT/init/$mem/sfc_data.${tile}.nc $COMDIR2
				if [[ $SENDDBN = YES ]];then
					$DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMDIR2/sfc_data.${tile}.nc
				fi
			done
		done
	fi
fi

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit $err


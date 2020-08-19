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
mkdir -p $INIDIR
mkdir -p $OUTDIR

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
fi
export CONVERT_SFC=".false."
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
mv ${DATA}/out.atm.tile1.nc $OUTDIR/gfs_data.tile1.nc
mv ${DATA}/out.atm.tile2.nc $OUTDIR/gfs_data.tile2.nc
mv ${DATA}/out.atm.tile3.nc $OUTDIR/gfs_data.tile3.nc
mv ${DATA}/out.atm.tile4.nc $OUTDIR/gfs_data.tile4.nc
mv ${DATA}/out.atm.tile5.nc $OUTDIR/gfs_data.tile5.nc
mv ${DATA}/out.atm.tile6.nc $OUTDIR/gfs_data.tile6.nc
mv ${DATA}/gfs_ctrl.nc $OUTDIR/.

touch ${OUTDIR}/chgres_atm.log  # recenter can start now

# Copy control file to init
mkdir -p $GESOUT/init/$mem
$NCP $OUTDIR/gfs_ctrl.nc $GESOUT/init/$mem

if [[ $SENDCOM == "YES" ]]; then
	MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
	DBNTYP=${MODCOM}_INIT
	mkdir -p $COMOUT/init/$mem
	$NCP $OUTDIR/gfs_ctrl.nc $COMOUT/init/$mem
	if [[ $SENDDBN = YES ]];then
		$DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMOUT/init/$mem/gfs_ctrl.nc
	fi
	if [[ $mem == "c00" ]]; then
		$NCP $OUTDIR/gfs_data*.nc $COMOUT/init/$mem
		if [[ $SENDDBN = YES ]];then
			for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
				$DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMOUT/init/$mem/gfs_data.${tile}.nc
			done
		fi
	fi
fi

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit $err


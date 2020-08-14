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

# Surface file must always be from current GFS cycle
SFCFILE=$COMINgfs/gfs.t${cyc}z.sfcanl.nc
if [[ -f $SFCFILE ]]; then
	$NCP $SFCFILE $INIDIR
else
	msg="FATAL ERROR in $(basename $BASH_SOURCE): GFS surfce analysis $SFCFILE not found!"
	echo $msg
	export err=100
	err_chk || exit $err
fi

if [[ $mem = c00 ]] ;then
	# Control intial conditions must be from current GFS cycle
	ATMFILE=$COMINgfs/gfs.t${cyc}z.atmanl.nc
	if [[ -f $ATMFILE ]]; then
		$NCP $ATMFILE $INIDIR
		export ATM_FILES_INPUT="gfs.t${cyc}z.atmanl.nc"
		export SFC_FILES_INPUT="gfs.t${cyc}z.sfcanl.nc"
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
	while [[ $success == "NO" && $i < MAX_ENKF_SEARCHES ]]; do
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
			if [[ $ < MAX_ENKF_SEARCHES ]]; then
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

mv ${DATA}/out.atm.tile1.nc $OUTDIR/gfs_data.tile1.nc
mv ${DATA}/out.atm.tile2.nc $OUTDIR/gfs_data.tile2.nc
mv ${DATA}/out.atm.tile3.nc $OUTDIR/gfs_data.tile3.nc
mv ${DATA}/out.atm.tile4.nc $OUTDIR/gfs_data.tile4.nc
mv ${DATA}/out.atm.tile5.nc $OUTDIR/gfs_data.tile5.nc
mv ${DATA}/out.atm.tile6.nc $OUTDIR/gfs_data.tile6.nc

if [[ $mem = c00 ]] ;then
	mv ${DATA}/out.sfc.tile1.nc $OUTDIR/sfc_data.tile1.nc
	mv ${DATA}/out.sfc.tile2.nc $OUTDIR/sfc_data.tile2.nc
	mv ${DATA}/out.sfc.tile3.nc $OUTDIR/sfc_data.tile3.nc
	mv ${DATA}/out.sfc.tile4.nc $OUTDIR/sfc_data.tile4.nc
	mv ${DATA}/out.sfc.tile5.nc $OUTDIR/sfc_data.tile5.nc
	mv ${DATA}/out.sfc.tile6.nc $OUTDIR/sfc_data.tile6.nc
fi

mv ${DATA}/gfs_ctrl.nc $OUTDIR/.
mkdir -p $GESOUT/init/$mem
$NCP $OUTDIR/gfs_ctrl.nc $GESOUT/init/$mem
# if [[ $DOSFC = "NO" ]]; then
#     $NCP $GESOUT/init/p01/sfc* $GESOUT/init/$mem
# else
#     $NCP $OUTDIR/sfc* $GESOUT/init/$mem
# fi

# Copy control surface files for all members
if [[ $mem == "c00" ]]; then
	for mem2 in $memberlist; do
		mkdir -p $GESOUT/init/$mem2
		for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
			$NCP $OUTDIR/sfc_data.${tile}.nc $GESOUT/init/$mem2
		done
	done
fi

if [[ $SENDCOM == "YES" ]]; then
	MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
	DBNTYP=${MODCOM}_INIT
	mkdir -p $COMOUT/init/$mem
	$NCP $OUTDIR/gfs_ctrl.nc $COMOUT/init/$mem
	# if [[ $DOSFC = NO ]]; then
	# 	$NCP $GESOUT/init/p01/sfc* $COMOUT/init/$mem
	# else
	# 	$NCP $OUTDIR/sfc* $COMOUT/init/$mem
	# fi
	if [[ $SENDDBN = YES ]];then
		$DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMOUT/init/$mem/gfs_ctrl.nc
	fi
	if [[ $mem == "c00" ]]; then
		for mem2 in $memberlist; do
			mkdir -p $COMOUT/init/$mem2
			for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
				$NCP $GESOUT/init/$mem2/sfc_data.${tile}.nc $COMOUT/init/$mem2
				if [[ $SENDDBN = YES ]];then
					$DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMOUT/init/$mem2/sfc_data.${tile}.nc
				fi
			done
		done

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


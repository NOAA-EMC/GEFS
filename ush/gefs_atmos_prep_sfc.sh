#!/bin/bash

echo "$(date -u) begin $(basename $BASH_SOURCE)"
export PS4="${PS4}${1}: "

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export DATA=$DATA/sfc
export INIDIR=$DATA
mkdir -p $INIDIR

cd $INIDIR

# Surface file must always be from current GFS cycle
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

export CRES=$(echo $CASEHR |cut -c2-5)
export COMIN=$INIDIR
export INPUT_TYPE="gaussian_netcdf"
export FIXfv3=$FIXgfs/fix_fv3_gmted2010/C$CRES
export FIXsfc=$FIXfv3/fix_sfc

export CONVERT_ATM=".false."
export CONVERT_SFC=".true."
export CONVERT_NST=".false."

# Execute the script
$USHgfs/chgres_cube.sh
export err=$?
if [[ $err != 0 ]]; then
	echo "FATAL ERROR in $(basename $BASH_SOURCE): chgres_cube failed!"
	exit $err
fi

for mem in $memberlist; do
	OUTDIR=$GESOUT/init/$mem
	mkdir -p $OUTDIR
	for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
		$NCP ${DATA}/out.sfc.${tile}.nc $OUTDIR/sfc_data.${tile}.nc
	done
done

if [[ $SENDCOM == "YES" ]]; then
	MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
	DBNTYP=${MODCOM}_INIT
	for mem in $memberlist; do
		mkdir -p $COMOUT/init/$mem
		for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
			$NCP $GESOUT/init/$mem/sfc_data.${tile}.nc $COMOUT/init/$mem
			if [[ $SENDDBN = YES ]];then
				$DBNROOT/bin/dbn_alert MODEL $DBNTYP $job $COMOUT/init/$mem/sfc_data.${tile}.nc
			fi
		done
	done
fi

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit $err


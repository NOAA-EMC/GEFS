#!/bin/ksh
#########################################################################
#                                           #
# Script:  gefs_bfr2gpk form gfs_bfr2gpk.sh #
#                                           #
#  This script reads GEFS BUFR output and transfers it into GEMPAK   #
#  surface and sounding data files.         #
#                                           #
# Log:                                      #
# K. Brill/HPC      04/12/05                #
#########################################################################  
echo "$(date -u) begin ${0}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

# Set GEMPAK paths.

#. /nwprod/gempak/.gempak

#  Go to a working directory.

cd $DATA

#  Set input directory name.

if [[ $SENDCOM == "YES" ]]; then
	export BPATH=$COMOUT/$COMPONENT/bufr/${mem}
	OUTDIR=$COMOUT/$COMPONENT/gempak
else
	export BPATH=$DATA/${mem}
	OUTDIR=$DATA/gempak
fi

#  Set output directory:
outfilbase=${RUNMEM}_${PDY}${cyc}

cat $BPATH/bufr.*.${PDY}${cyc} > bufr.combined

date
namsnd <<- EOF > /dev/null
	SNBUFR   = bufr.combined
	SNOUTF   = ${outfilbase}.snd
	SFOUTF   = ${outfilbase}.sfc
	SNPRMF   = sngfs.prm
	SFPRMF   = sfgfs.prm
	TIMSTN   = 170/2150
	r

	ex
	EOF

export err=$?
if [[ $err != 0 ]]; then
	echo <<- EOF
		FATAL ERROR in ${0}: namsnd failed with the following settings:
			SNBUFR   = bufr.combined
			SNOUTF   = ${outfilbase}.snd
			SFOUTF   = ${outfilbase}.sfc
			SNPRMF   = sngfs.prm
			SFPRMF   = sfgfs.prm
			TIMSTN   = 170/2150
			r

			ex
		EOF
	err_chk
	exit $err
fi
date

/bin/rm *.nts

snd=${outfilbase}.snd
sfc=${outfilbase}.sfc
if [[ $SENDCOM == "YES" ]]; then
	cp $snd $OUTDIR/.$snd
	cp $sfc $OUTDIR/.$sfc
	mv $OUTDIR/.$snd $OUTDIR/$snd
	mv $OUTDIR/.$sfc $OUTDIR/$sfc
fi

if [ $SENDDBN = "YES" ]; then
    MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
    DBNTYP=${MODCOM}_PTYP
    $DBNROOT/bin/dbn_alert MODEL ${DBNTYP}_SFC $job $OUTDIR/$sfc
    $DBNROOT/bin/dbn_alert MODEL ${DBNTYP}_SND $job $OUTDIR/$snd
fi
echo done > $DATA/gembufr.done

exit 0


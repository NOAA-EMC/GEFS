#!/bin/ksh
###############################################################
# This script generates the 6-hourly pqpf pqif pqrf pqff pqsf grib files
# 10/31/2014, script is adopted to process grib2 files
###############################################################
echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

export CDATE=$CDATE
ICYC=$(echo $CDATE | cut -c9-10)
YYYYMMDD=$(echo $CDATE | cut -c1-8)

export enspqpf=$EXECgefs/global_enspqpf
export ext_h=${ext_h:-""}

cd $DATA

# Specify the input/output file names:
export CPGB=$COMIN/$COMPONENT/ensstat/enspost_grb2${ext_h}.t${ICYC}z.prcp
export CPGO=$DATA/pqpf   
export CRAIN=$COMIN/$COMPONENT/ensstat/enspost_grb2${ext_h}.t${ICYC}z.rain
export CRAINO=$DATA/pqrf
export CFRZR=$COMIN/$COMPONENT/ensstat/enspost_grb2${ext_h}.t${ICYC}z.frzr
export CFRZRO=$DATA/pqff
export CICEP=$COMIN/$COMPONENT/ensstat/enspost_grb2${ext_h}.t${ICYC}z.icep
export CICEPO=$DATA/pqif 
export CSNOW=$COMIN/$COMPONENT/ensstat/enspost_grb2${ext_h}.t${ICYC}z.snow
export CSNOWO=$DATA/pqsf  

cat <<- EOF >inputpqpf
	&namin
		icyc=$ICYC
		cpgb='$CPGB',cpge='$CPGO'
		crain='$CRAIN',craino='$CRAINO'
		cfrzr='$CFRZR',cfrzro='$CFRZRO'
		cicep='$CICEP',cicepo='$CICEPO'
		csnow='$CSNOW',csnowo='$CSNOWO'
	/
	EOF

cat inputpqpf

rm $CPGO $CRAINO $CFRZRO $CICEPO $CSNOWO

export pgm=$enspqpf
source prep_step

startmsg
$enspqpf < inputpqpf 

export err=$?;
if [[ $err != 0 ]]; then
	echo <<- EOF
		FATAL ERROR in ${.sh.file}: $enspqpf returned a non-zero error!"
		  Namelist file inputpqpf contained:
		  	$(cat inputpqpf)
		EOF
	err_chk
	exit $err
fi

echo "$(date -u) end ${.sh.file}"

exit $err

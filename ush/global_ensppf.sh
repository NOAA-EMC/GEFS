#!/bin/ksh
#######################################
# Child script: ensppf.sh
# ABSTRACT:  This script produces a gribindex of
#            files
#            Theses files are then feed into the
#            executable ensaddx.$xc
#
#######################################
echo "$(date -u) begin ${0}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

cd $DATA

if [[ $# != 3 ]]; then
	echo "Usage: ${0} gribin gribout npert"
	exit 1
fi

export pgm=global_ensppf
source prep_step

startmsg
$EXECgefs/global_ensppf <<-EOF 2>/dev/null
	&namin
		cpgb='$1',cpge='$2',npert=$3 /
	EOF

export err=$?;
if [[ $err != 0 ]]; then
	echo <<- EOF
		FATAL ERROR in ${0}: $EXECgefs/global_ensppf returned a non-zero error!"
		  Namelist file was provided directly and contained:
		  		&namin
					cpgb='$1',cpge='$2',npert=$3 /
		EOF
	err_chk
	exit $err
fi

echo "$(date -u) end ${0}"

exit $err

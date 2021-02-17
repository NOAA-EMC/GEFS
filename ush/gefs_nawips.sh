#!/bin/ksh
###################################################################
# echo "----------------------------------------------------"
# echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
# echo "----------------------------------------------------"
# echo "History: Mar 2000 - First implementation of this new script."
# echo "S Lilly: May 2008 - add logic to make sure that all of the "
# echo "                    data produced from the restricted ECMWF"
# echo "                    data on the CCS is properly protected."
# echo "C. Magee: 10/2013 - swap X and Y for rtgssthr Atl and Pac."
#####################################################################

echo "$(date -u) begin ${.sh.file}"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

cd $DATA

RUNM=${1:-${RUNM}}
member=${2:-${member}}
resolution=${3:-${resolution}}
GEMPAKgefs=${4:-${GEMPAKgefs}}
gempak_in=${5:-${gempak_in}}
gempak_out=${6:-${gempak_out}}
fstart=${7:-${fstart}}
fend=${8:-${fend}}
FHMAXHF=${9:-${FHMAXHF}}
FHOUTHF=${10:-${FHOUTHF}}
FHOUTLF=${11:-${FHOUTLF}}
pgrdbType=${12:-${pdgrbType}} #:-pgrb2a}

DATA_member=$DATA/${member}/$resolution
mkdir -p $DATA_member
cd $DATA_member

mkdir -p $gempak_out

#
# Now set up GEMPAK/NTRANS environment
#
#
# Copy model specific GEMPAK tables into working directory
cp $GEMPAKgefs/fix/*.tbl .

NAGRIB_TABLE=${NAGRIB_TABLE:-${GEMPAKgefs}/fix/nagrib.tbl}
NAGRIB=nagrib2_nc
#

entry=$(grep "^$RUNM " $NAGRIB_TABLE | awk 'index($1,"#") != 1 {print $0}')

if [ "$entry" != "" ] ; then
	cpyfil=$(echo $entry  | awk 'BEGIN {FS="|"} {print $2}')
	garea=$(echo $entry   | awk 'BEGIN {FS="|"} {print $3}')
	gbtbls=$(echo $entry  | awk 'BEGIN {FS="|"} {print $4}')
	maxgrd=$(echo $entry  | awk 'BEGIN {FS="|"} {print $5}')
	kxky=$(echo $entry    | awk 'BEGIN {FS="|"} {print $6}')
	grdarea=$(echo $entry | awk 'BEGIN {FS="|"} {print $7}')
	proj=$(echo $entry    | awk 'BEGIN {FS="|"} {print $8}')
	output=$(echo $entry  | awk 'BEGIN {FS="|"} {print $9}')
else
	cpyfil=gds
	garea=dset
	gbtbls=
	maxgrd=4999
	kxky=
	grdarea=
	proj=
	output=T
fi
pdsext=no

# for gefs
gempak_out_hold=$gempak_out

SLEEP_LOOP_MAX=$(expr $SLEEP_TIME / $SLEEP_INT)\

fhcnt=$fstart
while [ $fhcnt -le $fend ] ; do

	fhr=$(printf %03i $fhcnt)
	
	finc1=${FHOUTHF}
	if [ $fhcnt -ge ${FHMAXHF} ]; then
		finc1=${FHOUTLF}
	fi
	case $RUNM in
	ens*)  
		GRIBIN=$gempak_in/${model}.${member}.${PDY}.${cyc}
		GEMGRD=${RUNM}_${PDY}${cyc} 
		;;
	ge*)  
		if [ "$model" = "bc" -o "$model" = "an" -o "$model" = "wt" -o "$model" = "me" -o "$model" = "anv" ]; then
			GRIBIN=$gempak_in/${RUNM}.${cycle}.pgrb2a_${model}f${fhr}
			GEMGRD=${RUNM}${model}_${PDY}${cyc}f${fhr3}

			# create subdirectory for the bc and an gefs files, -- 05/16/2013
			# so that the mag system can only take the expected gefs files
			# gempak_out_hold=$gempak_out
			if [ "$model" = "bc" -o "$model" = "an" ]; then
				gempak_out=${gempak_out_hold}/${model}
				mkdir -p -m 775 $gempak_out
			 fi
		elif [ "$model" = "glbanl" ]; then
			GRIBIN=$gempak_in/${model}.${cycle}.pgrb2a_mdf${fhr}
			GEMGRD=${model}_${PDYm2}${cyc}f${fhr3}
		elif [ "$model" = "ndgd" ]; then
			GRIBIN=$gempak_in/${RUNM}.${cycle}.${model}_conusf${fhr}
			GEMGRD=${RUNM}${model}_${PDY}${cyc}f${fhr3}
		elif [ "$model" = "ndgd_alaska" ]; then
			GRIBIN=$gempak_in/${RUNM}.${cycle}.${model}f${fhr}
			GEMGRD=${RUNM}${model}_${PDY}${cyc}f${fhr3}
		else
			# This is for gefs
			GRIBIN=${gempak_in}/${RUNM}.${cycle}.${pgrdbType}.${resolution}.f${fhr}
			if [ $resolution = "1p00" ]; then
			#if [ $resolution = "0p50" ]; then
				GEMGRD=${RUNM}_${PDY}${cyc}f${fhr}
			else
				GEMGRD=${RUNM}_${resolution}_${PDY}${cyc}f${fhr}
			fi
		fi
		;;

	*) 
		GRIBIN=$gempak_in/${model}.${cycle}.${GRIB}${fhr}${EXT}
		GEMGRD=${RUNM}_${PDY}${cyc}f${fhr3} 
	esac

	GRIBIN_chk=${GRIBIN}.idx

	icnt=1
	while [ $icnt -lt $SLEEP_LOOP_MAX ]; do
		if [ -r $GRIBIN_chk ]; then
			break
		else
			let "icnt=icnt+1"
			if [ $icnt -eq $SLEEP_LOOP_MAX ]; then
				cat <<-EOF
					FATAL ERROR in ${.sh.file}: GRIB index file $GRIBIN_chk still not found at $(date) after waiting ${SLEEP_TIME}s.
					EOF
				export err=1;
				err_chk
				exit $err
			else
				sleep $SLEEP_INT
			fi
		fi

	done

	ln -s $GRIBIN grib$fhr

	$GEMEXE/$NAGRIB <<- EOF
		GBFILE   = grib$fhr
		INDXFL   = 
		GDOUTF   = $GEMGRD
		PROJ     = $proj
		GRDAREA  = $grdarea
		KXKY     = $kxky
		MAXGRD   = $maxgrd
		CPYFIL   = $cpyfil
		GAREA    = $garea
		OUTPUT   = $output
		GBTBLS   = $gbtbls
		GBDIAG   = 
		PDSEXT   = $pdsext
		l
		r
		EOF
	
	export err=$?
	if [ $err -ne 0 ]; then
		echo <<- EOF
			FATAL ERROR in ${.sh.file}: Gempak failed creating $GEMGRD for f${fhr} using the following settings:
				GBFILE   = grib$fhr
				INDXFL   = 
				GDOUTF   = $GEMGRD
				PROJ     = $proj
				GRDAREA  = $grdarea
				KXKY     = $kxky
				MAXGRD   = $maxgrd
				CPYFIL   = $cpyfil
				GAREA    = $garea
				OUTPUT   = $output
				GBTBLS   = $gbtbls
				GBDIAG   = 
				PDSEXT   = $pdsext
				l
				r
			EOF
		err_chk
		exit $err
	fi

	#####################################################
	# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
	# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
	# FOR THIS CASE HERE.
	#####################################################
	ls -l $GEMGRD

	# export pgm="GEMPAK CHECK FILE"

	if [ "$NAGRIB" = "nagrib2_nc" ]; then
		gpend
	fi
	if [ $SENDCOM = "YES" ]; then
		cpfs $GEMGRD $gempak_out/$GEMGRD
		if [ $SENDDBN = "YES" ]; then
			$DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
			$gempak_out/$GEMGRD
		else
			echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
		fi
	fi

	let fhcnt=fhcnt+finc1
done

if [[ $err != 0 ]]; then
	echo "FATAL ERROR in ${.sh.file}: GRIB conversion failed!"
	err_chk
	exit $err    
fi

echo "$(date -u) end ${.sh.file}"

exit $err

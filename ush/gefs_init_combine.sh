#!/bin/ksh  -l
###testb
# lines between ###testb and ###teste not needed for production
###teste
################################################################################
#   Script:	gefs_init_pair.sh
#
################################################################################

set +xa
echo " ------------------------------------------------------------"
echo "  "
echo "            GLOBAL ENSEMBLE INITIALIZATION "
echo "  "
echo "                `date`     "
echo "  "
echo "                   JOB  $job  "
echo "  "
echo "  "
echo "               FORECAST cycle TIME is $cycle"
echo "  "
echo " ------------------------------------------------------------"
echo "          processing info for this execution"
echo " Home directory is ............................ $HOMEGLOBAL"
echo " Processing directory for files.. ............. $DATA"
echo "  "
echo " Executable file directory is ................. $EXECgefs"
echo " Fixed field directory is ..................... $FIXGLOBAL"
echo " Unix control language file is ................ $USHGLOBAL"
echo "  "
echo " Network id is ................................ $NET"
echo " Run id for $com processing is ................ $RUN"
echo "  "
echo " standard output in file ...................... $pgmout"
echo " unique processing id for run ................. $pid"
echo " YES SENDCOM means save com files ............. $SENDCOM"
echo " ------------------------------------------------------------"
echo " Argument list:"
echo "   1  ipair=number of this instance .............. $1"
echo "   2  npair=number of pairs ...................... $2"
echo "   3  fhrp=preferred input forecast length ....... $3"
echo "   4  inflag=(0=FV3NEMSIO, 2=GFS NEMSIO, 3=SIGIO). ${4}"
echo "   5  relocfact=relocation fraction .............. ${5}"
echo " ------------------------------------------------------------"
set -xa

cd $DATA

################################################################################

echo
echo `date` combin begin mem=$2
echo
if (( $# < 5 )); then
        echo $0 needs 5 arguments
        exit
fi

imem=${1}
imem0=${2}
fhrp=${3}
inflag=${4}
relocfact=${5}

cmem=$(printf %03i $imem)
cmem0=$(printf %02i $imem0)
cmem1=$(printf %03i $imem0)
memchars="m$cmem"
memchar="mem$cmem"
memchar1="mem$cmem1"


# make the local work directory

echo DATA=$DATA
DATALOCAL=$DATA/$memchar
mkdir -p $DATALOCAL
cd $DATALOCAL
pwd
# this is done for the benefit of operational scripts that use DATA
DATAPARENT=$DATA
DATA=$DATALOCAL
echo DATALOCAL=$DATALOCAL
echo DATA=$DATA
echo DATAPARENT=$DATAPARENT

#sh $utilscript/setup.sh

echo
echo `date` restore from 2nd to 3rd ush script begin
echo

for file in finuse_env finuse_strm finuse_presep save.relocpertflag save.ifhruse; do
	fils=../$file.$memchar
	echo file=$file fils=$fils
	if [[ -f $fils ]]; then
		ls -al $fils
		mv -f $fils $file
		ls -al $file
	else
		echo file=$file DOES NOT EXIST
	fi # [[ -f $fils ]]
done # file in finuse finuse_env finuse_storm et al
	file=ratmanl_$memchar1
	fils=../ratmanl_$memchar1
	if [[ -f $fils ]]; then
		ls -al $fils
		mv -f $fils .
		ls -al ratmanl_$memchar1
	else
		echo file=$file DOES NOT EXIST
	fi # [[ -f $fils ]]
export relocpertflag=`cat save.relocpertflag`
export ifhruse=`cat save.ifhruse`
echo
echo `date` restore from 2nd to 3rd ush script end
echo

rm fort.??

#  Combine the storm and environment forecast fields
#
echo relocflag=$relocflag relocpertflag=$relocpertflag
echo relocfact=$relocfact

execcombine=$EXECgefs/gefs_vortex_combine

if (( relocpertflag == 1 )); then
	echo Combine the storm and environment forecast fields for mem $memchar in EnKF saved as $memchar1 start

	# need to have already processed c0 member

	ln -s -f ratmanl_${memchar1}  fort.54
	# ln -s -f ../gec00_strm  fort.71
	# cp -f ../gec00_strm  fort.71
	ln -s -f ../gec00_strm.c2  fort.71

	# ln -s -f finp_strm  fort.75
	ln -s -f ../gec00_strm  fort.75
	ln -s -f finuse_strm  fort.74

	ln -s -f ratmanl_add fort.58

	export gesfhr=$ifhruse
	export ensm=$cmem1

	echo `date` $execcombine before
	echo $gesfhr $ensm $relocfact | $execcombine
	filtrccan=$?
	echo `date` $execcombine after


	# check for success
	echo before move
	ls -al ratmanl*
	echo
	if (( filtrccan == 0 )); then
		if (( filtrccap == 0 )); then
			if [[ -s ratmanl_add ]]; then
					echo gefs_vortex_combine succeeded
					mv -f ratmanl_${memchar1} ratmanl_env
					mv -f ratmanl_add ratmanl_${memchar1}
			fi # ratmanl.${memchar1}_add
		else
			echo filtrccap=$filtrccap
		fi # (( filtrccap == 0 ))
	else
		echo filtrccan=$filtrccan
	fi # (( filtrccan == 0 ))
	echo after move
	ls -al ratmanl*
	echo

	echo Combine the storm and environment forecast fields for mem $memchar in EnKF saved as $memchar1 end
fi # (( relocpertflag == 1 ))
echo relocflag=$relocflag relocpertflag=$relocpertflag
#

# guess copies moved here from scripts/exenstr.sh.sms
################################################################################
#  Put output files.


if [[ $SENDCOM = YES ]];then
	if (( imem0 == 1 )) ; then
		cp -f ../sfcanl.in $COMOUT/$cyc/init/gec00.${cycle}.sfcanl
		cp -f ../nsnanl.in $COMOUT/$cyc/init/gec00.${cycle}.nsnanl
		testfile=$COMOUT/$cyc/init/gec00.${cycle}.sfcanl
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		cp -f ../sanl.in $COMOUT/$cyc/init/gec00.${cycle}.sanl
		testfile=$COMOUT/$cyc/init/gec00.${cycle}.sanl
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
	fi # (( imem0==1 )) 
		cp -f ratmanl_${memchar1} $COMOUT/$cyc/init/gep${cmem0}.${cycle}.sanl
		testfile=$COMOUT/$cyc/init/gep${cmem0}.${cycle}.sanl
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
fi # [[ $SENDCOM = YES ]]

################################################################################
#  Put model initial files.

(( missingcount = 0 ))

if (( imem0 == 1 )) ; then
	if [[ -s ../sanl.in ]]; then
		mv -f ../sanl.in $GESOUT/gec00.${cycle}.sanl
		testfile=$GESOUT/gec00.${cycle}.sanl
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		cp -f ../sfcanl.in $GESOUT/gec00.${cycle}.sfcanl
		cp -f ../nsnanl.in $GESOUT/gec00.${cycle}.nsnanl
		testfile=$GESOUT/gec00.${cycle}.sfcanl
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
	else
		(( missingcount = missingcount + 1 ))
	fi # [[ -s sanl.in ]]
fi # (( ipair == 1 )) && (( cyc == cyc_fcst ))

	if [[ -s ratmanl_${memchar1} ]]; then
		mv -f ratmanl_${memchar1} $GESOUT/gep${cmem0}.${cycle}.sanl
		testfile=$GESOUT/gep${cmem0}.${cycle}.sanl
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
		cp -f ../sfcanl.in $GESOUT/gep${cmem0}.${cycle}.sfcanl
		cp -f ../nsnanl.in $GESOUT/gep${cmem0}.${cycle}.nsnanl
		testfile=$GESOUT/gep${cmem0}.${cycle}.sfcanl
		if [[ ! -s $testfile ]]; then
			msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
			echo "`date`    $msg"
			postmsg "$jlogfile" "$msg"
			export err=1
			err_chk
		fi # [[ ! -s $testfile ]]
	else
		(( missingcount = missingcount + 1 ))
	fi # [[ -s sanlgm${ipair}n && -s sanlgm${ipair}p ]]

##############################
# kill this job if data was missing
##############################
if (( missingcount > 0 )); then
	export pgm=gefs_vortex_combine
	export err=9
	err_chk
fi # (( missingcount > 0 ))

# DBN alerts moved here from scripts/exenstr.sh.sms
################################################################################
#  Send DBN alerts

if [[ $SENDDBN = YES ]];then
	  $DBNROOT/bin/dbn_alert MODEL ENS_SA_P${imem0} $job $COMOUT/$cyc/init/gep${cmem0}.${cycle}.sanl
fi # [[ $SENDDBN = YES ]]
echo
echo `date` combine end mem=$imem0
echo

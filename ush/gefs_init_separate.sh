#!/bin/ksh -l
###testb
# lines between ###testb and ###teste not needed for production
###teste
################################################################################
#   Script:	gefs_init_pair.sh
#
#   Author:	Xiaqiong Zhou
#   Date:	2018 May 08
#
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
echo " Executable file directory is ................. $EXECGLOBAL"
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
echo `date` separate begin ipair=$1
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
memchars="m$cmem"
memchar="mem$cmem"

echo cyc_fcst=$cyc_fcst
echo cyc=$cyc

export cycle_fcst=t${cyc_fcst}z
echo cycle_fcst=$cycle_fcst

echo cycsuffix=$cycsuffix
echo fcstlong=$fcstlong
echo cfsuffix=$cfsuffix

#ipai=`echo $ipair|cut -c2,2`
#echo one digit ipai=$ipai
echo inflag=$inflag 
echo cyc_fcst=$cyc_fcst
echo cycle_fcst=$cycle_fcst

echo inflag=$inflag
echo outflag=$outflag

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


	cyc=`echo $cycle|cut -c2-3`
	haveinput=no
	eminflaguse=0

	inflagt=$inflag

   	 	(( fhr = $fhrp ))
		while (( fhr < $fhrpend )); do
			if [[ $haveinput = no ]]; then
				if (( fhr < 10 )); then
				fhr=0$fhr
				fi
				echo "begin loop for nflagt=$inflagt fhr=$fhr"
			pdycycp0=`$NDATE -$fhr $PDY$cyc`
			pdyp0=`echo $pdycycp0|cut -c1-8`
			cycp0=`echo $pdycycp0|cut -c9-10`
                     if (( imem == 0 )) ; then
                        fcstin=$DATAPARENT/gec00_presep
                        else
			if (( inflagt == 0 )) ; then
				fcstin=${COMINenkf}${pdyp0}/$cycp0//$memchar/gdas.t${cycp0}z.atmf0${fhr}.nemsio
			elif (( inflagt == 1 )); then
				fcstin=${COMINenkf}${pdyp0}/$cycp0/gdas.t${cycp0}z.atmf0${fhr}.${memchar}.nemsio
			elif (( inflagt == 2 )); then
				fcstin=$comfcstin/$cycstart/sfcsig/gep${cmem0}.$cyclestart.sf${fhr}
			fi
                      fi  
			if [[ -e $fcstin ]]; then
				echo fcstin=$fcstin found
				found=true                                
				haveinput=yes                                
                                ifhruse=$fhr
                                cp -pr $fcstin finuse
			fi                        
		       echo end loop for fhr=$fhr
		fi # [[ $haveinput = no ]]
			(( fhr = fhr + fhrp ))
	done # while (( fhr <= $fhrpend ))

	wait

if (( ifhruse == fhrp )) then
	echo we are cycling at the preferred forecast length $ifhruse
       echo relocapertflat=$relocpertflag
else
	echo we are cycling at a different forecast length $ifhruse
	echo turn relocation off
	export relocpertflag=0
fi # (( ifhruse == fhrp ))


#
#  Separate the storm and environment forecast fields
#
echo `date` relocflag=$relocflag relocpertflag=$relocpertflag
execseparate=$EXECgefs/gefs_vortex_separate
if (( relocpertflag == 1 )); then
	echo `date` Separate the storm and environment forecast fields for n$ipair begin

	yy=`echo $PDY|cut -c3-4`
	echo yy=$yy

	# this file is created once for all in the exenstr script

	ln -s -f ../tcvitals.as fort.11

	ln -s -f ../tracks.atcfunix.$cyc      fort.40
	ln -s -f finuse               fort.24

	ln -s -f finuse_env              fort.54
	ln -s -f finuse_strm              fort.74

	export gesfhr=$ifhruse
	export ensm=${cmem}

	echo `date` $execseparate before
	echo $gesfhr $ensm | $execseparate
	filtrccn=$?
	echo `date` $execseparate after

	# substitute files and continue only if separation succeeded 
	export relocpertflag=0
	if (( filtrccn == 0 )); then
		if [[ -s finuse_env ]]; then
			if [[ -s finuse_strm ]]; then
				mv -f finuse finuse_presep
				cp -f finuse_env finuse
				export relocpertflag=1
                                if (( cmem == 000 )); then
                                   cp -pr finuse_strm ../gec00_strm
                                   cp -pr finuse_strm ../gec00_strm.c2
                                fi
			fi # [[ -s finuse_strm ]]
		fi # [[ -s finuse_env ]]
	else
		echo SEPARATION FAILED FOR mem=$cmem filtrccn=$filtrccn
	fi # (( filtrccn == 0 ))

	echo `date` Separate the storm and environment forecast fields for $cmem end
fi # (( relocpertflag == 1 ))
#
# end relocation splitting section
#



echo
echo `date` save from 1st to 2nd ush script begin
echo
echo $relocpertflag>save.relocpertflag
echo $ifhruse>save.ifhruse
for file in finuse finuse_presep finuse_env  finuse_strm save.relocpertflag  save.ifhruse; do
	fils=../$file.${memchar}
	echo file=$file fils=$fils
	if [[ -f $file ]]; then
		ls -al $file
		mv -f $file $fils
		ls -al $fils
	else
		echo file=$file DOES NOT EXIST
	fi # [[ -f $file ]]
done # for file in use
echo
echo `date` save from 1st to 2nd ush script end
echo
echo
echo `date` separate end mem=$memchar
echo

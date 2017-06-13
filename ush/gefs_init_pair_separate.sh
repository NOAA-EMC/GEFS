#!/bin/ksh -l
###testb
# lines between ###testb and ###teste not needed for production
###teste
################################################################################
#   Script:	gefs_init_pair.sh
#
#   Author:	Mark Iredell
#   Date:	1997 January 08
#
#   Steward:	Richard Wobus
#   Center:	Environmental Modeling Center
#   Phone:	(301) 763-8000 x7712
#
#   Abstract:	Creates initial conditions for the global ensemble
#       	by perturbing the analysis with growing modes
#               extracted from the previous day's ensemble forecasts.
#       	This script is called for a single pair from the
#               script exenstr.sh.sms.
#
#   Modified:   12/11/98	replace hardwired globampl with fixed file
#                               save growth rate output file
#   Modified:   06/08/99	dummy script made to hold over on IBM until real
#                               exenstr.sh.sms script is ready
#   Modified:   09/09/99	modified for IBM SP
#   Modified:   04/17/00	increase to 5 pairs at 12z
#   Modified:   05/19/00	create perturbation at any two resolutions,
#                               currently set to create 5 pairs and a 
#                               low-resolution control run at T126
#   Modified:   04/30/01	generalize to run with create any number of
#                               levels and any number of tracer variables
#   Modified:   05/xx/01	separate into outer and inner (single pair)
#                               scripts 
#   Modified:   06/07/05        add tropical cyclone relocation and fix energy
#   Modified:   09/09/05        add 2-sided breeding with 1-sided names
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
echo "   3  nhrpair=number of high-resolution pairs .... $3"
echo "   4  jcap=horizontal truncation ................. $4"
echo "   5  levs=number of vertical levels ............. $5"
echo "   6  latb=number of gaussian latitudes .......... $6"
echo "   7  lonb=number of gaussian longitudes ......... $7"
echo "   8  ntrac=number of tracer variables ........... $8"
echo "   9  fhrp=preferred input forecast length ....... $9"
echo "  10  inflag=(1=n,2=p,3=both,4=both n,5=both p) .. ${10}"
echo "  11  outflag=(1=n,2=p,3=both,4=both n,5=both p) . ${11}"
echo "  12  relocfact=relocation fraction .............. ${12}"
echo " ------------------------------------------------------------"
set -xa

cd $DATA

################################################################################

echo
echo `date` separate begin ipair=$1
echo
if (( $# < 12 )); then
	echo $0 needs 12 arguments
	exit
fi

ipairi=$1
npairi=$2
nhrpair=$3
jcap=$4
levs=$5
latb=$6
lonb=$7
ntrac=$8
fhrp=$9
inflag=${10}
outflag=${11}
relocfact=${12}

echo ipairi=$ipairi
echo npairi=$npairi
echo
(( npairc = npairi / 4 ))
echo npairc=$npairc
if (( ipairi <= npairc )); then
	ipair=ipairi
	cyc_fcst=00
elif (( ipairi <= 2 * npairc )); then
	(( ipair = ipairi - npairc ))
	cyc_fcst=06
elif (( ipairi <= 3 * npairc )); then
	(( ipair = ipairi - 2 * npairc ))
	cyc_fcst=12
else
	(( ipair = ipairi - 3 * npairc ))
	cyc_fcst=18
fi # (( ipairi <= npairc ))
(( ipair = ipair + 0 ))
if (( ipair < 10 )); then
	ipair=0$ipair
fi

echo ipair=$ipair
echo cyc_fcst=$cyc_fcst
echo cyc=$cyc

export cycle_fcst=t${cyc_fcst}z
echo cycle_fcst=$cycle_fcst

if (( cyc == cyc_fcst )); then
	export cycsuffix=false
	export fcstlong=true
	export cfsuffix=""
else
	export cycsuffix=true
	export fcstlong=false
	export cfsuffix=".cycfs$cyc_fcst"
fi # (( cyc == cyc_fcst ))
echo cycsuffix=$cycsuffix
echo fcstlong=$fcstlong
echo cfsuffix=$cfsuffix

echo two digit ipair=$ipair
#ipai=`echo $ipair|cut -c2,2`
#echo one digit ipai=$ipai
echo inflag=$inflag outflag=$outflag
if (( inflag > 3 )) || (( outflag > 3 )); then
	(( ipairp = 2 * ipair ))
	(( ipairn = ipairp - 1 ))
	if (( ipairp < 10 )); then
		ipairp=0$ipairp
	fi
	if (( ipairn < 10 )); then
		ipairn=0$ipairn
	fi
	echo two digit ipairn=$ipairn ipairp=$ipairp 
fi # (( inflag > 3 )) || (( outflag > 3 ))
### YOTA 12/21/2012 for EnKF inputs begin
if (( inflag == 6 )); then
	(( ipairip = 2 * ipairi ))
	(( ipairin = ipairip - 1 ))
	if (( ipairip < 10 )); then
		ipairip=0$ipairip
	fi
	if (( ipairin < 10 )); then
		ipairin=0$ipairin
	fi
fi # (( inflag == 6 ))
### YOTA 12/21/2012 for EnKF inputs end

# cfsuffix identifies long forecast cycle 
# associated with this breeding job

# cfsuffixp identifies long forecast cycle
# associated with the previous forecast cycle
# from which tracking information will be used

echo cfsuffix=$cfsuffix
echo ensshort=$ensshort
echo cfsuffixp=$cfsuffixp
echo ensshortp=$ensshortp
echo cyc_fcst=$cyc_fcst
echo cycle_fcst=$cycle_fcst

echo inflag=$inflag
echo outflag=$outflag

# these variables specify the resolution of perturbation output
JCAP=$jcap
LEVS=$levs
LONB=$lonb
LATB=$latb
NTRAC=$ntrac

# make the local work directory

echo DATA=$DATA
DATALOCAL=$DATA/pair.$ipairi
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

# check for cold start
coldstartflag=0
for coldstartcycle in $coldstartcycles; do
	echo $PDY$cyc | grep $coldstartcycle >/dev/null 2>&1
	rc=$?
	if (( rc == 0 )); then
		coldstartflag=1
		echo cold start specified by $coldstartcycle
	fi
done # for coldstartcycle in $coldstartcycles

if (( coldstartflag == 0 )); then
	#  find the previous forecasts
	cyc=`echo $cycle|cut -c2-3`
	haveinput=no
	inflaguse=0

	inflagt=$inflag

	(( fhr = $fhrpstart ))
	while (( fhr <= $fhrpend )); do
		if (( fhr < 10 )); then
			fhr=0$fhr
		fi
		echo "begin loop for nflagt=$inflagt fhr=$fhr"
		pdycycp0=`$NDATE -$fhr $PDY$cyc`
		pdyp0=`echo $pdycycp0|cut -c1-8`
		cycp0=`echo $pdycycp0|cut -c9-10`
		if [[ $haveinput = no ]]; then
			(( cycstart = cyc - fhr ))
			comfcstin=$COMIN
			comfcstinm1=$COMINm1
			comfcstinm2=$COMINm2
			comfcstinm3=$COMINm3
			comfcstinm4=$COMINm4
			pdyin=$PDY
			pdyinm1=$PDYm1
			pdyinm2=$PDYm2
			pdyinm3=$PDYm3
			pdyinm4=$PDYm4
			while (( cycstart < 0 )); do
				(( cycstart=cycstart+24))
				comfcstin=$comfcstinm1
				comfcstinm1=$comfcstinm2
				comfcstinm2=$comfcstinm3
				comfcstinm3=$comfcstinm4
				comfcstinm4=null
				pdyin=$pdyinm1
				pdyinm1=$pdyinm2
				pdyinm2=$pdyinm3
				pdyinm3=$pdyinm4
				pdyinm4=00000000
			done # while (( cycstart < 0 ))
			if (( cycstart < 10 )); then
				cycstart="0$cycstart"
			fi
			cyclestart=t${cycstart}z
			datein=$pdyin$cycstart

			# cfsuffixstart identifies long forecast cycle
			# associated with previous forecast used in
			# breeding which may be 6 or more hours previous

			if (( cycstart == cyc_fcst )); then
				cfsuffixstart=""
			else
				cfsuffixstart=".cycfs$cyc_fcst"
			fi
			echo "fhr=$fhr cyc=$cyc cycstart=$cycstart cyclestart=$cyclestart datein=$datein"
			echo "cfsuffixstart=$cfsuffixstart"
			found=false
			if (( inflagt == 1 )) || (( inflagt == 2 )) || (( inflagt == 3 )) || (( inflagt == 4 )) || (( inflagt == 5 )) || ((inflagt == 6 )); then
				found=true
			fi
			#   check for existence and correct headers of input files
			if (( inflagt == 1 )) || (( inflagt == 3 )); then
				fcstinn=$comfcstin/$cycstart/sfcsig/gen${ipair}.$cyclestart.sf$fhr$cfsuffixstart
			elif (( inflagt == 4 )); then
				fcstinn=$comfcstin/$cycstart/sfcsig/gen${ipairn}.$cyclestart.sf$fhr$cfsuffixstart
			elif (( inflagt == 5 )); then
				fcstinn=$comfcstin/$cycstart/sfcsig/gep${ipairn}.$cyclestart.sf$fhr$cfsuffixstart
			elif (( inflagt == 6 )); then
				fcstinn=${COMINenkf}${pdyp0}/$cycp0/gdas.t${cycp0}z.atmf0${fhrp}.mem0${ipairin}.nemsio
			else
				fcstinn=$comfcstin/$cycstart/sfcsig/gec00.$cyclestart.sf$fhr$cfsuffixstart
			fi # (( inflagt == 1 )) || (( inflagt == 3 ))
			if [[ -e $fcstinn ]]; then
				echo fcstinn=$fcstinn found
                                echo fcstinn=$fcstinn found
                               if [ $NEMSIO_IN = .true. ]; then
                                onifhr=$($nemsioget $fcstinn nfhour |grep -i "nfhour" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
                               else

				onifhr=`$sighdrexec $fcstinn ifhr`
                               fi
				rc=$?
				echo rc=$rc onifhr=$onifhr
				if (( rc == 0 )); then
					if (( onifhr == fhrp )); then
                                                if [ $NEMSIO_IN = .true. ];then
                                                   export OCDATE_NEMS=$($nemsioget ${fcstinn} idate | grep -i "idate" |awk -F= '{print $2}')
                                                   INI_YEAR=$(echo $OCDATE_NEMS | awk -F" " '{print $1}')
                                                   INI_MONTH=$(echo $OCDATE_NEMS | awk -F" " '{print $2}')
                                                   INI_DAY=$(echo $OCDATE_NEMS | awk -F" " '{print $3}')
                                                   INI_HOUR=$(echo $OCDATE_NEMS | awk -F" " '{print $4}')
                                                   oyyyy=$INI_YEAR
                                                   omm=$INI_MONTH; if [ $omm -lt 10 ]; then omm=0$omm ; fi
                                                   odd=$INI_DAY; if [ $odd -lt 10 ]; then odd=0$odd ; fi
                                                   ohh=`expr $INI_HOUR + 0 `; if [ $ohh -lt 10 ]; then ohh=0$ohh ; fi
                                                   onidate=${oyyyy}${omm}${odd}${ohh}
                                                else

						onidate=`$sighdrexec $fcstinn idate`
                                               fi
						rc=$?
						echo rc=$rc onidate=$onidate
						if (( rc == 0 )); then
							if (( onidate == datein )); then
								echo "fcstinn=$fcstinn appears to be usable"
								fcstinnuse=$fcstinn
							else
								found=false
							fi # (( onidate == datein ))
						else
							found=false
						fi # (( rc == 0 ))
					else
						found=false
					fi # (( onifhr == fhrp ))
				else
					found=false
				fi # (( rc == 0 ))
			else
				echo fcstinn=$fcstinn not found
				found=false
			fi # [[ -e $fcstinn ]]
			echo found=$found
			if (( inflagt == 2 )) || (( inflagt == 3 )); then
				fcstinp=$comfcstin/$cycstart/sfcsig/gep${ipair}.$cyclestart.sf$fhr$cfsuffixstart
			elif (( inflagt == 4 )); then
				fcstinp=$comfcstin/$cycstart/sfcsig/gen${ipairp}.$cyclestart.sf$fhr$cfsuffixstart
			elif (( inflagt == 5 )); then
				fcstinp=$comfcstin/$cycstart/sfcsig/gep${ipairp}.$cyclestart.sf$fhr$cfsuffixstart
			elif (( inflagt == 6 )); then
				fcstinp=${COMINenkf}${pdyp0}/$cycp0/gdas.t${cycp0}z.atmf0${fhrp}.mem0${ipairip}.nemsio
			else
				fcstinp=$comfcstin/$cycstart/sfcsig/gec00.$cyclestart.sf$fhr$cfsuffixstart
			fi # (( inflagt == 2 )) || (( inflagt == 3 ))
			if [[ -e $fcstinp ]]; then
				echo fcstinp=$fcstinp found
                               if [ $NEMSIO_IN = .true. ]; then
                                opifhr=$($nemsioget $fcstinp nfhour |grep -i "nfhour" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
                               else
				opifhr=`$sighdrexec $fcstinp ifhr`
                               fi
				rc=$?
				echo opifhr=$opifhr
				if (( rc == 0 )); then
					if (( opifhr == fhrp )); then
                                            if [ $NEMSIO_IN = .true. ];then
                                                   export OCDATE_NEMS=$($nemsioget ${fcstinp} idate | grep -i "idate" |awk -F= '{print $2}')
                                                   INI_YEAR=$(echo $OCDATE_NEMS | awk -F" " '{print $1}')
                                                   INI_MONTH=$(echo $OCDATE_NEMS | awk -F" " '{print $2}')
                                                   INI_DAY=$(echo $OCDATE_NEMS | awk -F" " '{print $3}')
                                                   INI_HOUR=$(echo $OCDATE_NEMS | awk -F" " '{print $4}')
                                                   oyyyy=$INI_YEAR
                                                   omm=$INI_MONTH; if [ $omm -lt 10 ]; then omm=0$omm ; fi
                                                   odd=$INI_DAY; if [ $odd -lt 10 ]; then odd=0$odd ; fi
                                                   ohh=`expr $INI_HOUR + 0 `; if [ $ohh -lt 10 ]; then ohh=0$ohh ; fi
                                                   opidate=${oyyyy}${omm}${odd}${ohh}
                                                else
						opidate=`$sighdrexec $fcstinp idate`
                                                fi
						rc=$?
						echo rc=$rc opidate=$opidate
						if (( rc == 0 )); then
							if (( opidate == datein )); then
								echo fcstinp=$fcstinp appears to be usable
								fcstinpuse=$fcstinp
							else
								found=false
							fi # (( opidate == datein ))
						else
							found=false
						fi # (( rc == 0 ))
					else
						found=false
					fi # (( opifhr == fhrp ))
				else
					found=false
				fi # (( rc == 0 ))
			else
				echo fcstinp=$fcstinp not found
				found=false
			fi # [[ -e $fcstinp ]]
			echo found=$found
			if [[ $found = true ]]; then
				ifhruse=$fhr
				echo all forecasts found for ifhruse=$ifhruse
				if (( inflagt == 1 )) || (( inflagt == 3 )) || (( inflagt == 4 )) || (( inflagt == 5 )) || (( inflagt == 6 )) ; then
					fcstinnuse=$fcstinn
					comfcstinuse=$comfcstin
				fi
				if (( inflagt == 2 )) || (( inflagt == 3 )) || (( inflagt == 4 )) || (( inflagt == 5 )) || (( inflagt == 6 )) ; then
					fcstinpuse=$fcstinp
					comfcstinuse=$comfcstin
				fi
				haveinput=yes
				inflaguse=$inflagt
				if (( inflaguse > 3 )); then
					inflaguse=3
				fi
			else
				echo some forecasts missing for fhr=$fhr
			fi # [[ $found = true ]]
		fi # [[ $haveinput = no ]]
		echo end loop for fhr=$fhr
		(( fhr = fhr + fhrp ))
	done # while (( fhr <= $fhrpend ))

	wait
else
	haveinput=no
fi # (( coldstartflag == 0 ))

# change the resolution of input files as necessary

if [[ $haveinput = yes ]]; then
	echo we have chosen ifhruse=$ifhruse com=$comfcstinuse
	if (( inflaguse == 1 )) || (( inflaguse == 3 )); then
          if [ $NEMSIO_IN = .true. ];then
        ojcap=$($nemsioget $fcstinn jcap |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        olevs=$($nemsioget $fcstinn levs |grep -i "levs" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        ontrac=$($nemsioget $fcstinn NTRAC|grep -i "NTRAC" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        oidvc=$($nemsioget $fcstinn IDVC|grep -i "IDVC" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
         else

		ojcap=`$sighdrexec $fcstinn jcap` 
		olevs=`$sighdrexec $fcstinn levs` 
		ontrac=`$sighdrexec $fcstinn ntrac` 
		oidvc=`$sighdrexec $fcstinn idvc` 
        fi
		export JCAP=$jcap
		export LEVS=$levs
		export LONB=$lonb
		export LATB=$latb
		export NTRAC=$ntrac
		export IDVC=$IDVC
		export NVCOORD=$IDVC
		export CHGRESVARS="NTRAC=$NTRAC,NVCOORD=$NVCOORD"
		if (( IDVC == 1 )); then
			export SIGLEVEL=$FIXgsm/global_siglevel.l${LEVS}.txt
		fi
		if (( IDVC == 2 )); then
			export SIGLEVEL=$FIXgsm/global_hyblev.l${LEVS}.txt
		fi
		export SIGINP=$fcstinnuse
		export SFCINP=NULL
		export SIGOUT=$DATALOCAL/finn
		export SFCOUT=sfcout
		runchgres=yes
		if (( JCAP == ojcap )); then
			if (( LEVS == olevs )); then
				if (( NTRAC == ontrac )); then
					if (( IDVC == oidvc )); then
						runchgres=no
					fi
				fi # (( NTRAC == ontrac ))
			fi # (( LEVS == olevs ))
		fi # (( JCAP == ojcap ))
		if [[ $runchgres = yes ]]; then
			#     if [[ $IDVC = 2 ]]; then
			#	if [[ $oidvc != 2 ]] || [[ LEVS != olevs ]]; then
			#	  export SIGLEVEL=$siglevelfile
			#	  export NVCOORD=2
			#	fi
			#     fi
			###testb
			echo
			echo `date` env before chgres begin
			env | sort
			echo `date` env before chgres end
			echo
			###teste
			$chgresush >>$pgmout
		else
			cp -fp $SIGINP $SIGOUT
		fi # [[ $runchgres = yes ]]
	else
		touch $DATALOCAL/finn
	fi # (( inflaguse == 1 )) || (( inflaguse == 3 ));
	if (( inflaguse == 2 )) || (( inflaguse == 3 )); then
          if [ $NEMSIO_IN = .true. ];then
        ojcap=$($nemsioget $fcstinp jcap |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        olevs=$($nemsioget $fcstinp levs |grep -i "levs" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        ontrac=$($nemsioget $fcstinp NTRAC|grep -i "NTRAC" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        oidvc=$($nemsioget $fcstinp IDVC|grep -i "IDVC" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
         else
		ojcap=`$sighdrexec $fcstinp jcap` 
		olevs=`$sighdrexec $fcstinp levs` 
		ontrac=`$sighdrexec $fcstinp ntrac` 
		oidvc=`$sighdrexec $fcstinp idvc` 
          fi
		export JCAP=$jcap
		export LEVS=$levs
		export LONB=$lonb
		export LATB=$latb
		export NTRAC=$ntrac
		export IDVC=$IDVC
		export NVCOORD=$IDVC
		export CHGRESVARS="NTRAC=$NTRAC,NVCOORD=$NVCOORD"
		if (( IDVC == 1 )); then
			export SIGLEVEL=$FIXgsm/global_siglevel.l${LEVS}.txt
		fi
		if (( IDVC == 2 )); then
			export SIGLEVEL=$FIXgsm/global_hyblev.l${LEVS}.txt
		fi
		export SIGINP=$fcstinpuse
		export SFCINP=NULL
		export SIGOUT=$DATALOCAL/finp
		export SFCOUT=sfcout
		runchgres=yes
		if (( JCAP == ojcap )); then
			if (( LEVS == olevs )); then
				if (( NTRAC == ontrac )); then
					if (( IDVC == oidvc )); then
						runchgres=no
					fi
				fi # (( NTRAC == ontrac ))
			fi # (( LEVS == olevs ))
		fi # (( JCAP == ojcap ))
		if [[ $runchgres = yes ]]; then
			#     if [[ $IDVC = 2 ]]; then
			#	if [[ $oidvc != 2 ]] || [[ LEVS != olevs ]]; then
			#	  export SIGLEVEL=$siglevelfile
			#	  export NVCOORD=2
			#	fi
			#     fi
			###testb
			echo
			echo `date` env before chgres begin
			env | sort
			echo `date` env before chgres end
			echo
			###teste
			$chgresush >>$pgmout
		else
			cp -fp $SIGINP $SIGOUT
		fi # [[ $runchgres = yes ]]
	else
		touch $DATALOCAL/finp
	fi # (( inflaguse == 2 )) || (( inflaguse == 3 ))
else # [[ $haveinput = yes ]]
	echo data was not found, use null files
	ifhruse=$fhrp
	echo ifhruse=$ifhruse
	touch $DATALOCAL/finn
	touch $DATALOCAL/finp
	echo turn relocation off
	export relocpertflag=0
fi # [[ $haveinput = yes ]]

wait

if (( ifhruse == fhrp )); then
	echo we are cycling at the preferred forecast length $ifhruse
else
	echo we are cycling at a different forecast length $ifhruse
	echo turn relocation off
	export relocpertflag=0
fi # (( ifhruse == fhrp ))

if [ $NEMSIO_IN != .true. ];then
ln -sf finn sig_zvdi
if [[ "$sigzvd" = "yes" ]]; then
	ln -sf finn sig_zvdo
else
	ln -sf /dev/null sig_zvdo
fi
echo ifinn$ipair > sig_zvdl

## kate 04/26/2012
$EXECgefs/global_sigzvd

ret_sigzvd=$?

ln -sf finp sig_zvdi
if [[ "$sigzvd" = "yes" ]]; then
	ln -sf finp sig_zvdo
else
	ln -sf /dev/null sig_zvdo
fi
echo ifinp$ipair > sig_zvdl

## kate 04/26/2012
$EXECgefs/global_sigzvd

ret_sigzvd=$?
fi

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

	ln -s -f $FIXgsm/global_slmask.t126.grb    fort.12


	ln -s -f ../tracks.atcfunix.$cyc_fcst      fort.40
	ln -s -f finn               fort.24

	ln -s -f finn_env              fort.54
	ln -s -f finn_strm              fort.74

	export gesfhr=$ifhruse
	export ensm=p${ipairn}

	echo `date` $execseparate before
	echo $gesfhr $ensm | $execseparate
	filtrccn=$?
	echo `date` $execseparate after

	# substitute files and continue only if separation succeeded 
	export relocpertflag=0
	if (( filtrccn == 0 )); then
		if [[ -s finn_env ]]; then
			if [[ -s finn_strm ]]; then
				mv -f finn finn_presep
				cp -f finn_env finn
				export relocpertflag=1
			fi # [[ -s finn_strm ]]
		fi # [[ -s finn_env ]]
	else
		echo SEPARATION FAILED FOR nipair=n$ipair pipairn=p$ipairn filtrccn=$filtrccn
	fi # (( filtrccn == 0 ))

	echo `date` Separate the storm and environment forecast fields for n$ipair end
fi # (( relocpertflag == 1 ))
if (( relocpertflag == 1 )); then
	echo `date` Separate the storm and environment forecast fields for p$ipair begin

	ln -s -f ../tcvitals.as fort.11

	ln -s -f $FIXgsm/global_slmask.t126.grb    fort.12

	ln -s -f ../tracks.atcfunix.$cyc_fcst      fort.40
	ln -s -f finp               fort.24

	ln -s -f finp_env              fort.54
	ln -s -f finp_strm              fort.74

	export gesfhr=$ifhruse
	export ensm=p${ipairp}

	echo `date` $execseparate before
	echo $gesfhr $ensm | $execseparate
	filtrccp=$?
	echo `date` $execseparate after

	# substitute files and continue only if separation succeeded 
	export relocpertflag=0
	if (( filtrccp == 0 )); then
		if [[ -s finp_env ]]; then
			if [[ -s finp_strm ]]; then
				mv -f finp finp_presep
				cp -f finp_env finp
				export relocpertflag=1
			fi # [[ -s finp_strm ]]
		fi # [[ -s finp_env ]]
	else
		echo SEPARATION FAILED FOR pipair=p$ipair pipairp=p$ipairp filtrccp=$filtrccp
	fi # (( filtrccp == 0 ))


	echo `date` Separate the storm and environment forecast fields for p$ipair end
fi # (( relocpertflag == 1 ))
wait
echo `date` relocflag=$relocflag relocpertflag=$relocpertflag
#
# end relocation splitting section
#

ln -sf finn sig_zvdi
if [[ "$sigzvd" = "yes" ]]; then
	ln -sf finn sig_zvdo
else
	ln -sf /dev/null sig_zvdo
fi
echo sfinn$ipair > sig_zvdl
## kate 04/26/2012
$EXECgefs/global_sigzvd

ret_sigzvd=$?

ln -sf finp sig_zvdi
if [[ "$sigzvd" = "yes" ]]; then
	ln -sf finp sig_zvdo
else
	ln -sf /dev/null sig_zvdo
fi
echo sfinp$ipair > sig_zvdl
## kate 04/26/2012
$EXECgefs/global_sigzvd

ret_sigzvd=$?

if (( ipair > nhrpair )); then
	export SIGINP=../sanl.lr.in
	export SFCINP=../sfcanl.lr.in
else
	export SIGINP=../sanl.hr.in
	export SFCINP=../sfcanl.hr.in
fi

export SIGOUT=$DATALOCAL/sanl.in
export SFCOUT=$DATALOCAL/sfcanl.in

cp -fp $SIGINP $SIGOUT
cp -fp $SFCINP $SFCOUT

        if [[ $nstinit == no ]]; then
        export NSTINP=NULL
 	export NSNOUT=NULL
        else
 	 if (( ipair > nhrpair )); then
         export NSTINP=NULL
	 export NSTOUT=NULL
 	 else
 	 export NSTINP=../nsnanl.hr.in
 	 export NSNOUT=$DATALOCAL/nsnanl.in
 	 cp -fp $NSTINP $NSNOUT
         fi
	fi
wait

unset SFCINP
unset SFCOUT
unset NSTINP
unset NSNOUT

for meml in n p; do
	if [[ ! -s fin${meml}  ]]; then
		export JCAP=$jcap
		export LEVS=$levs
		export LONB=$lonb
		export LATB=$latb
		export NTRAC=$ntrac
		export IDVC=$IDVC
		export NVCOORD=$IDVC
		export CHGRESVARS="NTRAC=$NTRAC,NVCOORD=$NVCOORD"
		if (( IDVC == 1 )); then
			export SIGLEVEL=$FIXgsm/global_siglevel.l${LEVS}.txt
		fi
		if (( IDVC == 2 )); then
			export SIGLEVEL=$FIXgsm/global_hyblev.l${LEVS}.txt
		fi

		export SIGINP=$FIXgefs/gefs.pertback.$cycle_fcst.${meml}${ipair}

		export SFCINP=NULL
		#  export SIGOUT=$DATALOCAL/gefs.pertback.$cycle_fcst.${meml}${ipair}
		export SIGOUT=$DATALOCAL/fin${meml}
		export SFCOUT=sfcout
                          if [ $NEMSIO_IN = .true. ];then
        ojcap=$($nemsioget ${SIGINP} jcap |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        olevs=$($nemsioget ${SIGINP} levs |grep -i "levs" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        ontrac=$($nemsioget ${SIGINP} NTRAC|grep -i "NTRAC" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
        oidvc=$($nemsioget ${SIGINP} IDVC|grep -i "IDVC" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')
         else

		ojcap=`$sighdrexec $SIGINP jcap` 
		olevs=`$sighdrexec $SIGINP levs` 
		ontrac=`$sighdrexec $SIGINP ntrac` 
		oidvc=`$sighdrexec $SIGINP idvc` 
         fi
		runchgres=yes
		if (( JCAP == ojcap )); then
			if (( LEVS == olevs )); then
				if (( NTRAC == ontrac )); then
					if (( IDVC == oidvc )); then
						runchgres=no
					fi
				fi # (( NTRAC == ontrac ))
			fi # (( LEVS == olevs ))
		fi # (( JCAP == ojcap ))
		if [[ $runchgres = yes ]]; then
			#   if [[ $IDVC = 2 ]]; then
			#     if [[ $oidvc != 2 ]] || [[ LEVS != olevs ]]; then
			#	export SIGLEVEL=$siglevelfile
			#	export NVCOORD=2
			#     fi
			#   fi
			###testb
			echo
			echo `date` env before chgres begin
			env | sort
			echo `date` env before chgres end
			echo
			###teste
			$chgresush >>$pgmout
		else
			cp -fp $SIGINP $SIGOUT
		fi # [[ $runchgres = yes ]]
	fi # [[ ! -s fin${meml}  ]]
done # for meml in n p

wait

echo
echo `date` save from 1st to 2nd ush script begin
echo
echo $relocpertflag>save.relocpertflag
echo $inflaguse>save.inflaguse
echo $ifhruse>save.ifhruse
for file in finn finp finn_env finp_env finn_strm finp_strm finn_presep finp_presep gefs.pertback.$cycle_fcst.n${ipair} gefs.pertback.$cycle_fcst.p${ipair} sanl.in sfcanl.in nsnanl.in save.relocpertflag save.inflaguse save.ifhruse; do
	fils=../$file.pair$ipairi
	echo file=$file fils=$fils
	if [[ -f $file ]]; then
		ls -al $file
		mv -f $file $fils
		ls -al $fils
	else
		echo file=$file DOES NOT EXIST
	fi # [[ -f $file ]]
done # for file in finn finp finn_env finp_env finn_strm finp_strm finn_presep finp_presep gefs.pertback.$cycle_fcst.n${ipair} gefs.pertback.$cycle_fcst.p${ipair} sanl.in sfcanl.in save.relocpertflag save.inflaguse save.ifhruse
echo
echo `date` save from 1st to 2nd ush script end
echo
echo
echo `date` separate end ipair=$ipairi
echo

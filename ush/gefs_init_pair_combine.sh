#!/bin/ksh  -l
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
echo `date` combine begin ipair=$1
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
fi
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
fi
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
fi

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

sh $utilscript/setup.sh

echo
echo `date` restore from 2nd to 3rd ush script begin
echo

for file in finn finp finn_env finp_env finn_strm finp_strm finn_presep finp_presep gefs.pertback.$cycle_fcst.n${ipair} gefs.pertback.$cycle_fcst.p${ipair} sanl.in sfcanl.in save.relocpertflag save.inflaguse sanlgm${ipair}n sanlgm${ipair}p sanl.c0 save.ifhruse
do
  fils=../$file.pair$ipairi
  echo file=$file fils=$fils
  if [[ -f $fils ]]; then
    ls -al $fils
    mv -f $fils $file
    ls -al $file
  else
    echo file=$file DOES NOT EXIST
  fi
done
export relocpertflag=`cat save.relocpertflag`
export inflaguse=`cat save.inflaguse`
export ifhruse=`cat save.ifhruse`
echo
echo `date` restore from 2nd to 3rd ush script end
echo

rm fort.??

ln -sf sanlgm${ipair}n sig_zvdi
if [[ "$sigzvd" = "yes" ]]; then
  ln -sf sanlgm${ipair}n sig_zvdo
else
  ln -sf /dev/null sig_zvdo
fi
echo rsann$ipair > sig_zvdl
$EXECGLOBAL/global_sigzvd
ret_sigzvd=$?

ln -sf sanlgm${ipair}p sig_zvdi
if [[ "$sigzvd" = "yes" ]]; then
  ln -sf sanlgm${ipair}p sig_zvdo
else
  ln -sf /dev/null sig_zvdo
fi
echo rsanp$ipair > sig_zvdl
$EXECGLOBAL/global_sigzvd
ret_sigzvd=$?

#
#  Combine the storm and environment forecast fields
#
echo relocflag=$relocflag relocpertflag=$relocpertflag
echo relocfact=$relocfact
###testb
if [[ $envir = prod ]]; then
###teste
execcombine=$EXECGLOBAL/gefs_vortex_combine
###testb
elif [[ $envir = para ]] || [[ $envir = test ]]; then
# RLW 20141008 modify to obtain and use version for vertical structure
execcombine=/nw$envir/gefs.${gefs_ver}/exec/gefs_vortex_combine
else
# RLW 20141008 modify to obtain and use version for vertical structure
execcombine=$basesource/nw$envir/gefs.${gefs_ver}/exec/gefs_vortex_combine
fi
###teste
if (( relocpertflag == 1 )); then
  echo Combine the storm and environment forecast fields for pair $ipair begin

# need to have already processed c0 member

  ln -s -f sanlgm${ipair}n  fort.54
# ln -s -f ../gec00_strm  fort.71
# cp -f ../gec00_strm  fort.71
  ln -s -f ../gec00_strm.c2  fort.71

# ln -s -f finp_strm  fort.75
  ln -s -f ../gec00_strm  fort.75
  ln -s -f finn_strm  fort.74

  ln -s -f sanlgm${ipair}n_add fort.58

  export gesfhr=$ifhruse
  export ensm=$ipair

  echo `date` $execcombine before
  echo $gesfhr $ensm $relocfact | $execcombine
  filtrccan=$?
  echo `date` $execcombine after

  ln -s -f sanlgm${ipair}p  fort.54
# ln -s -f ../gec00_strm  fort.71
# cp -f ../gec00_strm  fort.71
  ln -s -f ../gec00_strm.c2  fort.71

# ln -s -f finn_strm  fort.75
  ln -s -f ../gec00_strm  fort.75
  ln -s -f finp_strm  fort.74

  ln -s -f sanlgm${ipair}p_add fort.58

  export gesfhr=$ifhruse
  export ensm=$ipair

  echo `date` $execcombine before
  echo $gesfhr $ensm $relocfact | $execcombine
  filtrccap=$?
  echo `date` $execcombine after

# check for success
  echo before move
  ls -al sanlgm${ipair}*
  echo
  if (( filtrccan == 0 )); then
    if (( filtrccap == 0 )); then
      if [[ -s sanlgm${ipair}n_add ]]; then
	if [[ -s sanlgm${ipair}p_add ]]; then
	  echo gefs_vortex_combine succeeded
	  mv -f sanlgm${ipair}n sanlgm${ipair}n_env
	  mv -f sanlgm${ipair}p sanlgm${ipair}p_env
	  mv -f sanlgm${ipair}n_add sanlgm${ipair}n
	  mv -f sanlgm${ipair}p_add sanlgm${ipair}p
	fi
      fi
    else
      echo filtrccap=$filtrccap
    fi
  else
    echo filtrccan=$filtrccan
  fi
  echo after move
  ls -al sanlgm${ipair}*
  echo

  echo Combine the storm and environment forecast fields for pair $ipair end
fi
echo relocflag=$relocflag relocpertflag=$relocpertflag
#
# end relocation combining section
#

ln -sf sanlgm${ipair}n sig_zvdi
if [[ "$sigzvd" = "yes" ]]; then
  ln -sf sanlgm${ipair}n sig_zvdo
else
  ln -sf /dev/null sig_zvdo
fi
echo csann$ipair > sig_zvdl
$EXECGLOBAL/global_sigzvd
ret_sigzvd=$?

ln -sf sanlgm${ipair}p sig_zvdi
if [[ "$sigzvd" = "yes" ]]; then
  ln -sf sanlgm${ipair}p sig_zvdo
else
  ln -sf /dev/null sig_zvdo
fi
echo csanp$ipair > sig_zvdl
$EXECGLOBAL/global_sigzvd
ret_sigzvd=$?

# guess copies moved here from scripts/exenstr.sh.sms
################################################################################
#  Put output files.

if [[ $SENDCOM = YES ]];then

  if (( ipair == 1 )) && (( cyc == cyc_fcst )); then
    cp -f sfcanl.in $COMOUT/$cyc/init/gec00.${cycle}.sfcanl$cfsuffix
           testfile=$COMOUT/$cyc/init/gec00.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    cp -f sanl.c0 $COMOUT/$cyc/init/gec00.${cycle}.sanl$cfsuffix
         testfile=$COMOUT/$cyc/init/gec00.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  fi
  if (( outflag == 1 )) || (( outflag == 3 )); then
    cp -f sanlgm${ipair}n $COMOUT/$cyc/init/gen${ipair}.${cycle}.sanl$cfsuffix
                 testfile=$COMOUT/$cyc/init/gen${ipair}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  fi
  if (( outflag == 2 )) || (( outflag == 3 )); then
    cp -f sanlgm${ipair}p $COMOUT/$cyc/init/gep${ipair}.${cycle}.sanl$cfsuffix
                 testfile=$COMOUT/$cyc/init/gep${ipair}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  fi
  if (( outflag == 4 )); then
    cp -f sanlgm${ipair}n $COMOUT/$cyc/init/gen${ipairn}.${cycle}.sanl$cfsuffix
                 testfile=$COMOUT/$cyc/init/gen${ipairn}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    cp -f sanlgm${ipair}p $COMOUT/$cyc/init/gen${ipairp}.${cycle}.sanl$cfsuffix
                 testfile=$COMOUT/$cyc/init/gen${ipairp}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  fi
  if (( outflag == 5 )); then
    cp -f sanlgm${ipair}n $COMOUT/$cyc/init/gep${ipairn}.${cycle}.sanl$cfsuffix
                 testfile=$COMOUT/$cyc/init/gep${ipairn}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    cp -f sanlgm${ipair}p $COMOUT/$cyc/init/gep${ipairp}.${cycle}.sanl$cfsuffix
                 testfile=$COMOUT/$cyc/init/gep${ipairp}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  fi

fi

################################################################################
#  Put model initial files.

(( missingcount = 0 ))

if (( ipair == 1 )) && (( cyc == cyc_fcst )); then
  if [[ -s sanl.c0 ]]; then
    mv -f sanl.c0 $GESdir/gec00.${cycle}.sanl$cfsuffix
         testfile=$GESdir/gec00.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    cp -f sfcanl.in $GESdir/gec00.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gec00.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  else
    (( missingcount = missingcount + 1 ))
  fi
fi
if (( outflag == 1 )); then
  if [[ -s sanlgm${ipair}n ]]; then
    mv -f sanlgm${ipair}n $GESdir/gen${ipair}.${cycle}.sanl$cfsuffix
                 testfile=$GESdir/gen${ipair}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    mv -f sfcanl.in $GESdir/gen${ipair}.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gen${ipair}.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  else
    (( missingcount = missingcount + 1 ))
  fi
fi
if (( outflag == 2 )); then
  if [[ -s sanlgm${ipair}p ]]; then
    mv -f sanlgm${ipair}p $GESdir/gep${ipair}.${cycle}.sanl$cfsuffix
                 testfile=$GESdir/gep${ipair}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    mv -f sfcanl.in $GESdir/gep${ipair}.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gep${ipair}.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  else
    (( missingcount = missingcount + 1 ))
  fi
fi
if (( outflag == 3 )); then
  if [[ -s sanlgm${ipair}n && -s sanlgm${ipair}p ]]; then
    mv -f sanlgm${ipair}n $GESdir/gen${ipair}.${cycle}.sanl$cfsuffix
                 testfile=$GESdir/gen${ipair}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    cp -f sfcanl.in $GESdir/gen${ipair}.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gen${ipair}.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    mv -f sanlgm${ipair}p $GESdir/gep${ipair}.${cycle}.sanl$cfsuffix
                 testfile=$GESdir/gep${ipair}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    mv -f sfcanl.in $GESdir/gep${ipair}.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gep${ipair}.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  else
    (( missingcount = missingcount + 1 ))
  fi
fi
if (( outflag == 4 )); then
  if [[ -s sanlgm${ipair}n && -s sanlgm${ipair}p ]]; then
    mv -f sanlgm${ipair}n $GESdir/gen${ipairn}.${cycle}.sanl$cfsuffix
                 testfile=$GESdir/gen${ipairn}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    cp -f sfcanl.in $GESdir/gen${ipairn}.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gen${ipairn}.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    mv -f sanlgm${ipair}p $GESdir/gen${ipairp}.${cycle}.sanl$cfsuffix
                 testfile=$GESdir/gen${ipairp}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    mv -f sfcanl.in $GESdir/gen${ipairp}.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gen${ipairp}.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  else
    (( missingcount = missingcount + 1 ))
  fi
fi
if (( outflag == 5 )); then
  if [[ -s sanlgm${ipair}n && -s sanlgm${ipair}p ]]; then
    mv -f sanlgm${ipair}n $GESdir/gep${ipairn}.${cycle}.sanl$cfsuffix
                 testfile=$GESdir/gep${ipairn}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    cp -f sfcanl.in $GESdir/gep${ipairn}.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gep${ipairn}.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    mv -f sanlgm${ipair}p $GESdir/gep${ipairp}.${cycle}.sanl$cfsuffix
                 testfile=$GESdir/gep${ipairp}.${cycle}.sanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
    mv -f sfcanl.in $GESdir/gep${ipairp}.${cycle}.sfcanl$cfsuffix
           testfile=$GESdir/gep${ipairp}.${cycle}.sfcanl$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
  else
    (( missingcount = missingcount + 1 ))
  fi
fi

##############################
# kill this job if data was missing
##############################
if (( missingcount > 0 )); then
  export pgm=gefs_vortex_combine
  export err=9
  err_chk
fi

# DBN alerts moved here from scripts/exenstr.sh.sms
################################################################################
#  Send DBN alerts

if [[ $SENDDBN = YES ]];then
  #if (( ipair == 1 )) && (( cyc == cyc_fcst )); then
  #  $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/init/gec00.${cycle}.sanl$cfsuffix
  #  $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/init/gec00.${cycle}.sfcanl$cfsuffix
  #fi
  if (( outflag == 1 )) || (( outflag == 3 )); then
    $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/init/gen${ipair}.${cycle}.sanl$cfsuffix
  fi
  #if (( outflag == 2 )) || (( outflag == 3 )); then
  #  $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/init/gep${ipair}.${cycle}.sanl$cfsuffix
  #fi
  if (( outflag == 4 )); then
    $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/init/gen${ipairn}.${cycle}.sanl$cfsuffix
    $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/init/gen${ipairp}.${cycle}.sanl$cfsuffix
  fi
  #if (( outflag == 5 )); then
  #  $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/init/gep${ipairn}.${cycle}.sanl$cfsuffix
  #  $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/init/gep${ipairp}.${cycle}.sanl$cfsuffix
  #fi
fi
echo
echo `date` combine end ipair=$ipairi
echo

#!/bin/ksh
echo "entering extrkr_2015.sh"

loopnum=$1
cmodel=$2
ymdh=$3
pert=$4
DATA=$5

userid=$LOGNAME
export trkrtype=tracker
export gribver=2

USE_OPER_VITALS=YES
# USE_OPER_VITALS=NO
# USE_OPER_VITALS=INIT_ONLY

set +x
##############################################################################
echo " "
echo "------------------------------------------------"
echo "xxxx - Track vortices in model GRIB2 output     "
echo "------------J.Peng Sep.10, 2014 ----------------"
echo "Current time is: `date`"
echo " "
##############################################################################
set -x

##############################################################################
#
#    FLOW OF CONTROL
#
# 1. Define data directories and file names for the input model 
# 2. Process input starting date/cycle information
# 3. Update TC Vitals file and select storms to be processed
# 4. Cut apart input GRIB files to select only the needed parms and hours
# 5. Execute the tracker
# 6. Copy the output track files to various locations
#
##############################################################################

########################################
#msg="has begun for ${cmodel} at ${CYL}z"
#postmsg "$jlogfile" "$msg"
########################################

# This script runs the hurricane tracker using operational GRIB model output.  
# This script makes sure that the data files exist, it then pulls all of the 
# needed data records out of the various GRIB forecast files and puts them 
# into one, consolidated GRIB file, and then runs a program that reads the TC 
# Vitals records for the input day and updates the TC Vitals (if necessary).
# It then runs gettrk, which actually does the tracking.
# 
# Environmental variable inputs needed for this scripts:
#  PDY   -- The date for data being processed, in YYYYMMDD format
#  CYL   -- The numbers for the cycle for data being processed (00, 06, 12, 18)
#  cmodel -- Model being processed (gfs, mrf, ukmet, ecmwf, nam, ngm, ngps,
#                                   gdas, gfdl, ens (ncep ensemble))
#  envir -- 'prod' or 'test'
#  SENDCOM -- 'YES' or 'NO'
#  stormenv -- This is only needed by the tracker run for the GFDL model.
#              'stormenv' contains the name/id that is used in the input
#              grib file names.
#  pert  -- This is only needed by the tracker run for the NCEP ensemble.
#           'pert' contains the ensemble member id (e.g., n2, p4, etc.)
#           which is used as part of the grib file names.
#
# For testing script interactively in non-production set following vars:
#     gfsvitdir  - Directory for GFS Error Checked Vitals
#     namvitdir  - Directory for NAM Error Checked Vitals
#     gltrkdir   - Directory for output tracks
#     homesyndir - Directory with syndir scripts/exec/fix 
#     archsyndir - Directory with syndir scripts/exec/fix 
#

qid=$$
#----------------------------------------------#
#   Get input date information                 #
#----------------------------------------------#

export PDY=` echo $ymdh | cut -c1-8`
export CYL=` echo $ymdh | cut -c9-10`
export CYCLE=t${CYL}z

export jobid=${jobid:-testjob}
export envir=${envir:-prod}
export SENDCOM=${SENDCOM:-NO}
export PARAFLAG=${PARAFLAG:-NO}
export PHASEFLAG=n
export WCORE_DEPTH=1.0
#export PHASE_SCHEME=vtt
#export PHASE_SCHEME=cps
export PHASE_SCHEME=both
export STRUCTFLAG=n
export IKEFLAG=n

# Define tracker working directory for this ensemble member
export TRKDATA=${TRKDATA:-$DATA}

if [ ! -d $DATA ]
then
   mkdir -p $DATA
   cd $DATA
   /nwprod/util/ush/setup.sh
fi
cd $DATA

if [ ${PARAFLAG} = 'YES' ]
then 
  /nwprod/util/ush/setup.sh
fi

if [ ${#PDY} -eq 0 -o ${#CYL} -eq 0 -o ${#cmodel} -eq 0 ]
then
  set +x
  echo
  echo "Something wrong with input data.  One or more input variables has length 0"
  echo "PDY= ${PDY}, CYL= ${CYL}, cmodel= ${cmodel}"
  echo "EXITING...."
  set -x
  err_exit " FAILED ${jobid} -- BAD INPUTS AT LINE $LINENO IN TRACKER SCRIPT - ABNORMAL EX
IT"
else
  set +x
  echo " "
  echo " #-----------------------------------------------------------------#"
  echo " At beginning of tracker script, the following imported variables "
  echo " are defined: "
  echo "   PDY ................................... $PDY"
  echo "   CYL ................................... $CYL"
  echo "   CYCLE ................................. $CYCLE"
  echo "   cmodel ................................ $cmodel"
  echo "   jobid ................................. $jobid"
  echo "   envir ................................. $envir"
  echo "   SENDCOM ............................... $SENDCOM"
  echo " "
  set -x
fi

scc=`echo ${PDY} | cut -c1-2`
syy=`echo ${PDY} | cut -c3-4`
smm=`echo ${PDY} | cut -c5-6`
sdd=`echo ${PDY} | cut -c7-8`
shh=${CYL}
symd=`echo ${PDY} | cut -c3-8`
syyyy=`echo ${PDY} | cut -c1-4`
symdh=${PDY}${CYL}

export gfsvitdir=${gfsvitdir:-/com2/gfs/prod/gfs.$PDY}
export namvitdir=${namvitdir:-/com/nam/prod/nam.$PDY}
export gltrkdir=${gltrkdir:-/com/hur/${envir}/global}
export TPCATdir=/tpcprd/atcf

export homesyndir=${homesyndir:-/nwprod/util}
# RLW 20141010 modify exectrkdir to generalize
#export exectrkdir=${exectrkdir:-${homesyndir}/exec}
#export exectrkdir=/ensemble/save/Jiayi.Peng/sub_jiayi/exec
export exectrkdir=${exectrkdir:-${homesyndir}/exec}
# RLW 20141010 add parm location so leadtimes file can be stored there
export parmtrkdir=${parmtrkdir:-${homesyndir}/parm}
echo parmtrkdir=$parmtrkdir

export ushtrkdir=${ushtrkdir:-${homesyndir}/ush}
export archsyndir=${archsyndir:-/com/arch/prod/syndat}

cp /com/date/t${CYL}z ncepdate
export CENT=` cut -c7-8 ncepdate `

#if [ -s /nwprod/util/exec/wgrib2 ]
if [ -s $WGRIB2 ]
then
  #wgrib2=/nwprod/util/exec/wgrib2
  wgrib2=$WGRIB2
else
  set +x
  echo " "
  echo "!!! ERROR: wgrib2 is not available, script will crash.  Exiting...."
  echo " "
  set -x
  err_exit " FAILED ${jobid} -- line= $LINENO IN TRACKER SCRIPT - ABNORMAL EXIT"
fi

#J.Peng---03-05-2014
#wgrib_parmlist=" HGT:925 HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 MSLET HGT:900 HGT:800 HGT:750 HGT:650 HGT:600 HGT:550 HGT:500 HGT:450 HGT:400 HGT:350 HGT:300 HGT:250 TMP:500 TMP:450 TMP:400 TMP:350 TMP:300 TMP:250"
#wgrib_parmlist=" HGT:925 HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 PRMSL HGT:900 HGT:800 HGT:750 HGT:650 HGT:600 HGT:550 HGT:500 HGT:450 HGT:400 HGT:350 HGT:300 HGT:250 TMP:500 TMP:450 TMP:400 TMP:350 TMP:300 TMP:250"

#wgrib_parmlist=" HGT:925 HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 HGT:500 HGT:400 HGT:300 HGT:250 TMP:500 TMP:400 TMP:300 TMP:250"

export maxtime=65    # Max number of forecast time levels

#----------------------------------------------------------------#
#
#    --- Define data directories and data file names ---
#               
# Convert the input model to lowercase letters and check to see 
# if it's a valid model, and assign a model ID number to it.  
# This model ID number is passed into the Fortran program to 
# let the program know what set of forecast hours to use in the 
# ifhours array.  Also, set the directories for the operational 
# input GRIB data files and create templates for the file names.
# While only 1 of these sets of directories and file name 
# templates is used during a particular run of this script, 
# "gfsvitdir" is used every time, because that is the directory 
# that contains the error-checked TC vitals file that Steve Lord 
# produces, and so it is included after the case statement.
#
#----------------------------------------------------------------#

cmodel=`echo ${cmodel} | tr "[A-Z]" "[a-z]"`

case ${cmodel} in

  gfs) set +x                                       ;
       echo " "; echo " ++ operational GFS chosen"  ;
       echo " "                                     ;
       set -x                                       ;
# RLW 20141010 modify gfsdir to generalize
       #gfsdir=/com/gfs/$envir/gfs.${PDY}              ;
       gfsdir=/com2/gfs/prod/gfs.${PDY}              ;
#       gfsgfile=gfs.t${CYL}z.master.grbf            ;
#       gfsifile=gfs.t${CYL}z.master.grbif           ;

#      gfsdir=/global/noscrub/emc.glopara/com/gfs/para/gfs.${PDY};
       gfsdir=${GFSGRIBDIR:-$gfsdir};
       gfsgfile=gfs.t${CYL}z.pgrb2.0p25.f            ;

#J.Peng----2011-05-27----------------------------------------
#       COM=/com/gfs/${envir}/gfs.${PDY}             ;
# RLW 20141010 allow environment variable to modify fcstlen
        fcstlen=${TRACKFCSTLENGTH:-240}               ;
#        fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
#                  84 90 96 102 108 114 120 126 132 138 144
#                  150 156 162 168 174 180 186 192  99  99
#                  99  99  99  99  99  99  99  99  99  99
#                  99  99  99  99  99  99  99  99  99  99
#                  99  99  99  99  99  99  99  99  99  99' ;
#csthrs=' 000 006 012 018 024 030 036 042 048 054 060 066 072 078
#         084 090 096 102 108 114 120 126 132 138 144 150 156 162
#	  168 174 180 186 192 198 204 210 216 222 228 234 240';
fcsthrs=' 000  99  99  99  99  99  99  99  99  99  99  99  99  99
           99  99  99  99  99  99  99  99  99  99  99  99  99  99
           99  99  99  99  99  99  99  99  99  99  99  99  99';
# RLW 20141010 adjust fcsthrs to match fcstlen using fcstint
          fcstint=${TRACKFCSTINT:-6}
	  (( cutlength = 4 + ( fcstlen * 4 ) / fcstint )) ;
	  fcsthrs=`echo "$fcsthrs"|cut -c1-$cutlength`;
        atcfnum=15                                   ;
	 COM=${COM:-/com/gens/${envir}/gefs.${PDY}/$cyc/track} ;
# RLW 20141010 modify atcf names to generalize
        atcfname="${ATCFNAMEIN:-gfso}"                              ;
        atcfout="${ATCFNAMEIN:-gfso}"                                ;
        atcffreq=600                                 ;
        mslpthresh=0.0015                            ;
        v850thresh=1.5000                            ;
        modtyp='global'                              ;
        file_sequence="onebig"                       ;
        lead_time_units='hours'                      ;
        g2_jpdtn=0                                   ;
        model=1                                     ;;

# g2_jpdtn sets the variable that will be used as "JPDTN" for
# the call to getgb2, if gribver=2.  jpdtn=1 for ens data,
# jpdtn=0 for deterministic data.

  ens) set +x                                           ;
       echo " "; echo " ++ operational ensemble member ${pert} chosen"     ;
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
#       ensdir=/com/gens/prod/gefs.${PDY}/${CYL}/pgrba   ;
#       ensgfile=ge${pert}.t${CYL}z.pgrbaf               ;
#       ensifile=ge${pert}.t${CYL}z.pgrbaif              ;
#       ensdir=/ensemble/noscrub/Jiayi.Peng/gefs_grib2    ;
       ensdir=/ptmpd2/Dingchen.Hou/o/tfFs/com/gens/dev/gefs.20130910/00/pgrb2ap5;
       ensgfile=ge${pert}.t${CYL}z.pgrb2ap5f

       fcstlen=240                                      ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                 84 90 96 102 108 114 120 126 132 138 144
                 150 156 162 168 174 180 186 192 198 204 210
                 216 222 228 234 240  99  99  99  99  99  99
                 99  99  99  99  99  99  99  99  99  99  99
                 99  99  99  99  99  99  99';
       atcfnum=91                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       atcfname="a${pert_posneg}${pert_num}"            ;
       atcfout="a${pert_posneg}${pert_num}"             ;

       atcffreq=600                                     ;
       mslpthresh=0.0015                                ;
       v850thresh=1.5000                                ;
       file_sequence="onebig"                           ;
       lead_time_units='hours'                          ;
       g2_jpdtn=1                                       ;
       modtyp='global'                                  ;
       model=10                                         ;;

# g2_jpdtn sets the variable that will be used as "JPDTN" for
# the call to getgb2, if gribver=2.  jpdtn=1 for ens data,
# jpdtn=0 for deterministic data.

  *) set +x; echo " "; echo " !!! Model selected is not recognized."             ;
     echo " Model= ---> ${cmodel} <--- ..... Please submit the script again...."  ;
     echo " ";  set -x;
     err_exit " FAILED ${jobid} -- UNKNOWN cmodel IN TRACKER SCRIPT - ABNORMAL EXIT";;

esac

if [ ${PHASEFLAG} = 'y' ]; then
wgrib_parmlist=" HGT:925 HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 MSLET HGT:500 HGT:400 HGT:300 HGT:250 TMP:500 TMP:400 TMP:300 TMP:250"

#  wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 MSLET HGT:925 HGT:900 HGT:800 HGT:750 HGT:650 HGT:600 HGT:550 HGT:500 HGT:450 HGT:400 HGT:350 HGT:300 HGT:250 TMP:500 TMP:450 TMP:400 TMP:350 TMP:300 TMP:250"
#  wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 PRMSL HGT:925 HGT:900 HGT:800 HGT:750 HGT:650 HGT:600 HGT:550 HGT:500 HGT:450 HGT:400 HGT:350 HGT:300 HGT:250 TMP:500 TMP:450 TMP:400 TMP:350 TMP:300 TMP:250"

else
  wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 MSLET "
#  wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 PRMSL "
fi

#---------------------------------------------------------------#
#
#      --------  TC Vitals processing   --------
#
# Check Steve Lord's operational tcvitals file to see if any 
# vitals records were processed for this time by his system.  
# If there were, then you'll find a file in /com/gfs/prod/gfs.yymmdd 
# with the vitals in it.  Also check the raw TC Vitals file in
# /com/arch/prod/syndat , since this may contain storms that Steve's 
# system ignored (Steve's system will ignore all storms that are 
# either over land or very close to land);  We still want to track 
# these inland storms, AS LONG AS THEY ARE NHC STORMS (don't 
# bother trying to track inland storms that are outside of NHC's 
# domain of responsibility -- we don't need that info).
# UPDATE 3/27/09 MARCHOK: The SREF is run at off-synoptic times
#   (03,09,15,21Z).  There are no tcvitals issued at these offtimes,
#   so the updating of the "old" tcvitals is critical for running
#   the tracker on SREF.  For updating the old tcvitals for SREF,
#   we need to look 3h back, not 6h as for the other models that
#   run at synoptic times.  Therefore, we've introduced a
#   variable called "vit_incr" here.
#--------------------------------------------------------------#

if [ ${cmodel} = 'sref' ]; then
  vit_incr=3
else
  vit_incr=6
fi

# First check to see if the vitals file is in gfsvitdir or not.  If 
# it's not, then run Hua-Lu's ftp script to get the file from one
# of the other machines.  If it's still not there, then no big 
# deal; this script will exit just a little further down once it
# realizes there are not any storms to process.

old_ymdh=` /nwprod/util/exec/ndate -${vit_incr} ${PDY}${CYL}`
old_4ymd=` echo ${old_ymdh} | cut -c1-8`
old_ymd=` echo ${old_ymdh} | cut -c3-8`
old_hh=`  echo ${old_ymdh} | cut -c9-10`
old_str="${old_ymd} ${old_hh}00"

future_ymdh=` /nwprod/util/exec/ndate ${vit_incr} ${PDY}${CYL}`
future_4ymd=` echo ${future_ymdh} | cut -c1-8`
future_ymd=` echo ${future_ymdh} | cut -c3-8`
future_hh=`  echo ${future_ymdh} | cut -c9-10`
future_str="${future_ymd} ${future_hh}00"

# NOTE: Change synvitdir to point to /com/nam for regional models.

synvitdir=/com2/gfs/prod/gfs.${PDY}
synvitfile=gfs.t${CYL}z.syndata.tcvitals.tm00
synvitold_dir=/com2/gfs/prod/gfs.${old_4ymd}
synvitold_file=gfs.t${old_hh}z.syndata.tcvitals.tm00
synvitfuture_dir=/com2/gfs/prod/gfs.${future_4ymd}
synvitfuture_file=gfs.t${future_hh}z.syndata.tcvitals.tm00

#J.Peng-03-14-2014--------------------------------------------------
#ynvitdir=/ensemble/noscrub/Jiayi.Peng/tcvital_2013/gfs.${PDY}
#synvitfile=gfs.t${CYL}z.syndata.tcvitals.tm00
#synvitold_dir=/ensemble/noscrub/Jiayi.Peng/tcvital_2013/gfs.${old_4ymd}
#synvitold_file=gfs.t${old_hh}z.syndata.tcvitals.tm00
#synvitfuture_dir=/ensemble/noscrub/Jiayi.Peng/tcvital_2013/gfs.${future_4ymd}
#synvitfuture_file=gfs.t${future_hh}z.syndata.tcvitals.tm00

set +x
echo " "
echo "              -----------------------------"
echo " "
echo " Now sorting and updating the TC Vitals file.  Please wait...."
echo " "
set -x

current_str="${symd} ${CYL}00"

if [ -s ${synvitdir}/${synvitfile} -o\
     -s ${synvitold_dir}/${synvitold_file} -o\
     -s ${synvitfuture_dir}/${synvitfuture_file} ]
then
  grep "${old_str}" ${synvitold_dir}/${synvitold_file}        \
                  >${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
  grep "${current_str}"  ${synvitdir}/${synvitfile}                  \
                 >>${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
  grep "${future_str}" ${synvitfuture_dir}/${synvitfuture_file}  \
                 >>${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
else
  set +x
  echo " "
  echo " There is no (synthetic) TC vitals file for ${CYL}z in ${synvitdir},"
  echo " nor is there a TC vitals file for ${old_hh}z in ${synvitold_dir}."
  echo " nor is there a TC vitals file for ${future_hh}z in ${synvitfuture_dir},"
  echo " Checking the raw TC Vitals file ....."
  echo " "
  set -x
fi

# Take the vitals from Steve Lord's /com/gfs/prod tcvitals file,
# and cat them with the NHC-only vitals from the raw, original
# /com/arch/prod/synda_tcvitals file.  Do this because the nwprod
# tcvitals file is the original tcvitals file, and Steve runs a
# program that ignores the vitals for a storm that's over land or
# even just too close to land, and for tracking purposes for the
# US regional models, we need these locations.  Only include these
# "inland" storm vitals for NHC (we're not going to track inland 
# storms that are outside of NHC's domain of responsibility -- we 
# don't need that info).  
# UPDATE 5/12/98 MARCHOK: awk logic is added to screen NHC 
#   vitals such as "89E TEST", since TPC 
#   does not want tracks for such storms.

grep "${old_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | awk 'substr($0,6,1) !~ /8/ {print $0}' \
      >${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
grep "${current_str}"  ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | awk 'substr($0,6,1) !~ /8/ {print $0}' \
      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
grep "${future_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy} | \
      grep -v TEST | awk 'substr($0,6,1) !~ /8/ {print $0}' \
      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}


# IMPORTANT:  When "cat-ing" these files, make sure that the vitals
# files from the "raw" TC vitals files are first in order and Steve's
# TC vitals files second.  This is because Steve's vitals file has
# been error-checked, so if we have a duplicate tc vitals record in
# these 2 files (very likely), program supvit.x below will
# only take the last vitals record listed for a particular storm in
# the vitals file (all previous duplicates are ignored, and Steve's
# error-checked vitals records are kept).

cat ${DATA}/tmprawvit.${atcfout}.${PDY}${CYL} ${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL} \
        >${DATA}/vitals.${atcfout}.${PDY}${CYL}

#--------------------------------------------------------------#
# Now run a fortran program that will read all the TC vitals
# records for the current dtg and the dtg from 6h ago, and
# sort out any duplicates.  If the program finds a storm that
# was included in the vitals file 6h ago but not for the current
# dtg, this program updates the 6h-old first guess position
# and puts these updated records as well as the records from
# the current dtg into a temporary vitals file.  It is this
# temporary vitals file that is then used as the input for the
# tracking program.
#--------------------------------------------------------------#

oldymdh=` /nwprod/util/exec/ndate -${vit_incr} ${PDY}${CYL}`
oldyy=`echo ${oldymdh} | cut -c3-4`
oldmm=`echo ${oldymdh} | cut -c5-6`
olddd=`echo ${oldymdh} | cut -c7-8`
oldhh=`echo ${oldymdh} | cut -c9-10`
oldymd=${oldyy}${oldmm}${olddd}

futureymdh=` /nwprod/util/exec/ndate 6 ${PDY}${CYL}`
futureyy=`echo ${futureymdh} | cut -c3-4`
futuremm=`echo ${futureymdh} | cut -c5-6`
futuredd=`echo ${futureymdh} | cut -c7-8`
futurehh=`echo ${futureymdh} | cut -c9-10`
futureymd=${futureyy}${futuremm}${futuredd}

#echo "&datenowin   dnow%yy=${syy}, dnow%mm=${smm},"       >${DATA}/suv_input.${atcfout}.${PDY}${CYL}
#echo "             dnow%dd=${sdd}, dnow%hh=${CYL}/"      >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
#echo "&dateoldin   dold%yy=${oldyy}, dold%mm=${oldmm},"    >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
#echo "             dold%dd=${olddd}, dold%hh=${oldhh}/"    >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
#echo "&datefuturein  dfuture%yy=${futureyy}, dfuture%mm=${futuremm},"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
#echo "               dfuture%dd=${futuredd}, dfuture%hh=${futurehh}/"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}

echo "&datenowin   dnow%yy=${syy}, dnow%mm=${smm},"       >${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "             dnow%dd=${sdd}, dnow%hh=${CYL}/"      >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "&dateoldin   dold%yy=${oldyy}, dold%mm=${oldmm},"    >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "             dold%dd=${olddd}, dold%hh=${oldhh}/"    >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "&datefuturein  dfuture%yy=${futureyy}, dfuture%mm=${futuremm},"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "               dfuture%dd=${futuredd}, dfuture%hh=${futurehh}/"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "&hourinfo  vit_hr_incr=${vit_incr}/"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}

numvitrecs=`cat ${DATA}/vitals.${atcfout}.${PDY}${CYL} | wc -l`
if [ ${numvitrecs} -eq 0 ]
then

  if [ ${trkrtype} = 'tracker' ]
  then
    set +x
    echo " "
    echo "!!! NOTE -- There are no vitals records for this time period."
    echo "!!! File ${DATA}/vitals.${atcfout}.${PDY}${CYL} is empty."
    echo "!!! It could just be that there are no storms for the current"
    echo "!!! time.  Please check the dates and submit this job again...."
    echo " "
    set -x
    exit 1
  fi

fi

# For tcgen cases, filter to use only vitals from the ocean 
# basin of interest....

if [ ${trkrtype} = 'tcgen' ]
  then

  if [ ${numvitrecs} -gt 0 ]
  then
    
    fullvitfile=${DATA}/vitals.${atcfout}.${PDY}${CYL}
    cp $fullvitfile ${DATA}/vitals.all_basins.${atcfout}.${PDY}${CYL}
    basin=` echo $regtype | cut -c1-2`

    if [ ${basin} = 'al' ]; then
      cat $fullvitfile | awk '{if (substr($0,8,1) == "L") print $0}' \
               >${DATA}/vitals.tcgen_al_only.${atcfout}.${PDY}${CYL}
      cp ${DATA}/vitals.tcgen_al_only.${atcfout}.${PDY}${CYL} \
         ${DATA}/vitals.${atcfout}.${PDY}${CYL}
    fi
    if [ ${basin} = 'ep' ]; then
      cat $fullvitfile | awk '{if (substr($0,8,1) == "E") print $0}' \
               >${DATA}/vitals.tcgen_ep_only.${atcfout}.${PDY}${CYL}
      cp ${DATA}/vitals.tcgen_ep_only.${atcfout}.${PDY}${CYL} \
         ${DATA}/vitals.${atcfout}.${PDY}${CYL}
    fi
    if [ ${basin} = 'wp' ]; then
      cat $fullvitfile | awk '{if (substr($0,8,1) == "W") print $0}' \
               >${DATA}/vitals.tcgen_wp_only.${atcfout}.${PDY}${CYL}
      cp ${DATA}/vitals.tcgen_wp_only.${atcfout}.${PDY}${CYL} \
         ${DATA}/vitals.${atcfout}.${PDY}${CYL}
    fi

    cat ${DATA}/vitals.${atcfout}.${PDY}${CYL}

  fi
    
fi

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Before running the program to read, sort and update the vitals,
# first run the vitals through some awk logic, the purpose of 
# which is to convert all the 2-digit years into 4-digit years.
# We need this logic to ensure that all the vitals going
# into supvit.f have uniform, 4-digit years in their records.
#
# 1/8/2000: sed code added by Tim Marchok due to the fact that 
#       some of the vitals were getting past the syndata/qctropcy
#       error-checking with a colon in them; the colon appeared
#       in the character immediately to the left of the date, which
#       was messing up the "(length($4) == 8)" statement logic.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

sed -e "s/\:/ /g"  ${DATA}/vitals.${atcfout}.${PDY}${CYL} > ${DATA}/tempvit
mv ${DATA}/tempvit ${DATA}/vitals.${atcfout}.${PDY}${CYL}

awk '
{
  yycheck = substr($0,20,2)
  if ((yycheck == 20 || yycheck == 19) && (length($4) == 8)) {
    printf ("%s\n",$0)
  }
  else {
    if (yycheck >= 0 && yycheck <= 50) {
      printf ("%s20%s\n",substr($0,1,19),substr($0,20))
    }
    else {
      printf ("%s19%s\n",substr($0,1,19),substr($0,20))
    }
  }
} ' ${DATA}/vitals.${atcfout}.${PDY}${CYL} >${DATA}/vitals.${atcfout}.${PDY}${CYL}.y4

mv ${DATA}/vitals.${atcfout}.${PDY}${CYL}.y4 ${DATA}/vitals.${atcfout}.${PDY}${CYL}

if [ ${numvitrecs} -gt 0 ]
then

  export pgm=supvit
#  . prep_step

  ln -s -f ${DATA}/vitals.${atcfout}.${PDY}${CYL}         fort.31
  ln -s -f ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}     fort.51

  msg="$pgm start for $atcfout at ${CYL}z"
  postmsg "$jlogfile" "$msg"

  ${exectrkdir}/supvit <${DATA}/suv_input.${atcfout}.${PDY}${CYL}
  suvrcc=$?

  if [ ${suvrcc} -eq 0 ]
  then
    msg="$pgm end for $atcfout at ${CYL}z completed normally"
    postmsg "$jlogfile" "$msg"
  else
    set +x
    echo " "
    echo "!!! ERROR -- An error occurred while running supvit.x, "
    echo "!!! which is the program that updates the TC Vitals file."
    echo "!!! Return code from supvit.x = ${suvrcc}"
    echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
    echo "!!! Exiting...."
    echo " "
    set -x
    err_exit " FAILED ${jobid} - ERROR RUNNING SUPVIT IN TRACKER SCRIPT- ABNORMAL EXIT"
  fi

else

  touch ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}

fi

#-----------------------------------------------------------------
# In this section, check to see if the user requested the use of 
# operational TC vitals records for the initial time only.  This 
# option might be used for a retrospective medium range forecast
# in which the user wants to initialize with the storms that are
# currently there, but then let the model do its own thing for 
# the next 10 or 14 days....


if [ ${USE_OPER_VITALS} = 'INIT_ONLY' ]; then

  if [ ${init_flag} = 'yes' ]; then
    set +x
    echo " "
    echo "NOTE: User has requested that operational historical TC vitals be used,"
    echo "      but only for the initial time, which we are currently at."
    echo " "
    set -x
  else
    set +x
    echo " "
    echo "NOTE: User has requested that operational historical TC vitals be used,"
    echo "      but only for the initial time, which we are now *PAST*."
    echo " "
    set -x
    >${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}
  fi
    
elif [ ${USE_OPER_VITALS} = 'NO' ]; then
    
  set +x
  echo " "
  echo "NOTE: User has requested that historical vitals not be used...."
  echo " "
  set -x
  >${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}
    
fi

#------------------------------------------------------------------#
# Now select all storms to be processed, that is, process every
# storm that's listed in the updated vitals file for the current
# forecast hour.  If there are no storms for the current time,
# then exit.
#------------------------------------------------------------------#

numvitrecs=`cat ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`
if [ ${numvitrecs} -eq 0 ]
then
  if [ ${trkrtype} = 'tracker' ]
  then
    set +x
    echo " "
    echo "!!! NOTE -- There are no vitals records for this time period "
    echo "!!! in the UPDATED vitals file."
    echo "!!! It could just be that there are no storms for the current"
    echo "!!! time.  Please check the dates and submit this job again...."
    echo " "
    set -x
    exit 1
  fi
fi

set +x
echo " "
echo " *--------------------------------*"
echo " |        STORM SELECTION         |"
echo " *--------------------------------*"
echo " "
set -x

ict=1
while [ $ict -le 15 ]
do
  stormflag[${ict}]=3
  let ict=ict+1
done

dtg_current="${symd} ${CYL}00"
stormmax=` grep "${dtg_current}" ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`

if [ ${stormmax} -gt 15 ]
then
  stormmax=15
fi

sct=1
while [ ${sct} -le ${stormmax} ]
do
  stormflag[${sct}]=1
  let sct=sct+1
done


#---------------------------------------------------------------#
#
#    --------  "Genesis" Vitals processing   --------
#
# May 2006:  This entire genesis tracking system is being
# upgraded to more comprehensively track and categorize storms.
# One thing that has been missing from the tracking system is
# the ability to keep track of storms from one analysis cycle
# to the next.  That is, the current system has been very
# effective at tracking systems within a forecast, but we have
# no methods in place for keeping track of storms across
# difference initial times.  For example, if we are running
# the tracker on today's 00z GFS analysis, we will get a
# position for various storms at the analysis time.  But then
# if we go ahead and run again at 06z, we have no way of
# telling the tracker that we know about the 00z position of
# this storm.  We now address that problem by creating
# "genesis" vitals, that is, when a storm is found at an
# analysis time, we not only produce "atcfunix" output to
# detail the track & intensity of a found storm, but we also
# produce a vitals record that will be used for the next
# run of the tracker script.  These "genesis vitals" records
# will be of the format:
#
#  YYYYMMDDHH_AAAH_LLLLX_TYP
#
#    Where:
#
#      YYYYMMDDHH = Date the storm was FIRST identified
#                   by the tracker.
#             AAA = Abs(Latitude) * 10; integer value
#               H = 'N' for norther hem, 'S' for southern hem
#            LLLL = Abs(Longitude) * 10; integer value
#               X = 'E' for eastern hem, 'W' for western hem
#             TYP = Tropical cyclone storm id if this is a
#                   tropical cyclone (e.g., "12L", or "09W", etc).
#                   If this is one that the tracker instead "Found
#                   On the Fly (FOF)", we simply put those three
#                   "FOF" characters in there.

genvitdir=/gpfs/gd2/emc/hwrf/save/${userid}/gen/scripts
genvitfile=${genvitdir}/genesis.vitals.${cmodel}.${atcfout}.${CENT}${syy}

d6ago_ymdh=` /nwprod/util/exec/ndate -6 ${PDY}${CYL}`
d6ago_4ymd=` echo ${d6ago_ymdh} | cut -c1-8`
d6ago_ymd=` echo ${d6ago_ymdh} | cut -c3-8`
d6ago_hh=`  echo ${d6ago_ymdh} | cut -c9-10`
d6ago_str="${d6ago_ymd} ${d6ago_hh}00"

d6ahead_ymdh=` /nwprod/util/exec/ndate 6 ${PDY}${CYL}`
d6ahead_4ymd=` echo ${d6ahead_ymdh} | cut -c1-8`
d6ahead_ymd=` echo ${d6ahead_ymdh} | cut -c3-8`
d6ahead_hh=`  echo ${d6ahead_ymdh} | cut -c9-10`
d6ahead_str="${d6ahead_ymd} ${d6ahead_hh}00"

syyyym6=` echo ${d6ago_ymdh} | cut -c1-4`
smmm6=`   echo ${d6ago_ymdh} | cut -c5-6`
sddm6=`   echo ${d6ago_ymdh} | cut -c7-8`
shhm6=`   echo ${d6ago_ymdh} | cut -c9-10`

syyyyp6=` echo ${d6ahead_ymdh} | cut -c1-4`
smmp6=`   echo ${d6ahead_ymdh} | cut -c5-6`
sddp6=`   echo ${d6ahead_ymdh} | cut -c7-8`
shhp6=`   echo ${d6ahead_ymdh} | cut -c9-10`

set +x
echo " "
echo " d6ago_str=    --->${d6ago_str}<---"
echo " current_str=  --->${current_str}<---"
echo " d6ahead_str=  --->${d6ahead_str}<---"
echo " "
echo " Listing and contents of ${genvitdir}/genesis.vitals.${atcfout}.${CENT}${syy} follow "
echo " for the times 6h ago, current and 6h ahead:"
echo " "
set -x

ls -la ${genvitdir}/genesis.vitals.${atcfout}.${CENT}${syy}
cat ${genvitdir}/genesis.vitals.${atcfout}.${CENT}${syy}

set +x
echo " "
echo " "
set -x

grep "${d6ago_str}" ${genvitfile}                           \
       >${DATA}/genvitals.${cmodel}.${atcfout}.${PDY}${CYL}
grep "${current_str}"  ${genvitfile}                           \
      >>${DATA}/genvitals.${cmodel}.${atcfout}.${PDY}${CYL}
grep "${d6ahead_str}" ${genvitfile}                         \
      >>${DATA}/genvitals.${cmodel}.${atcfout}.${PDY}${CYL}

grep "${d6ago_str}"     ${genvitfile}
grep "${current_str}"   ${genvitfile}
grep "${d6ahead_str}"   ${genvitfile}


echo "&datenowin   dnow%yy=${syyyy}, dnow%mm=${smm},"          >${DATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "             dnow%dd=${sdd}, dnow%hh=${CYL}/"           >>${DATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "&date6agoin  d6ago%yy=${syyyym6}, d6ago%mm=${smmm6},"   >>${DATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "             d6ago%dd=${sddm6}, d6ago%hh=${shhm6}/"     >>${DATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "&date6aheadin  d6ahead%yy=${syyyyp6}, d6ahead%mm=${smmp6}," >>${DATA}/sgv_input.${atcfout}.${PDY}${CYL}
echo "               d6ahead%dd=${sddp6}, d6ahead%hh=${shhp6}/"   >>${DATA}/sgv_input.${atcfout}.${PDY}${CYL}

num_gen_vits=`cat ${DATA}/genvitals.${cmodel}.${atcfout}.${PDY}${CYL} | wc -l`

if [ ${num_gen_vits} -gt 0 ]
then

  export pgm=supvit_gen
#  . prep_step

  ln -s -f ${DATA}/genvitals.${cmodel}.${atcfout}.${PDY}${CYL}      fort.31
  ln -s -f ${DATA}/genvitals.upd.${cmodel}.${atcfout}.${PDY}${CYL}  fort.51

  msg="$pgm start for $atcfout at ${CYL}z"
  postmsg "$jlogfile" "$msg"

  ${exectrkdir}/supvit_gen <${DATA}/sgv_input.${atcfout}.${PDY}${CYL}
  sgvrcc=$?

  if [ ${sgvrcc} -eq 0 ]
  then
    msg="$pgm end for $atcfout at ${CYL}z completed normally"
    postmsg "$jlogfile" "$msg"
  else
    set +x
    echo " "
    echo "!!! ERROR -- An error occurred while running supvit_gen, "
    echo "!!! which is the program that updates the genesis vitals file."
    echo "!!! Return code from supvit_gen = ${sgvrcc}"
    echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
    echo "!!! Exiting...."
    echo " "
    set -x
    err_exit " FAILED ${jobid} - ERROR RUNNING SUPVIT_GEN IN TRACKER SCRIPT- ABNORMAL EXIT"
    exit 8
  fi
    
else
   
  touch ${DATA}/genvitals.upd.${cmodel}.${atcfout}.${PDY}${CYL}
    
fi


#-----------------------------------------------------------------#
#
#         ------  CUT APART INPUT GRIB FILES  -------
#
# For the selected model, cut apart the GRIB input files in order
# to pull out only the variables that we need for the tracker.  
# Put these selected variables from all forecast hours into 1 big 
# GRIB file that we'll use as input for the tracker.
# 
#-----------------------------------------------------------------#

set +x
echo " "
echo " -----------------------------------------"
echo "   NOW CUTTING APART INPUT GRIB FILES TO "
echo "   CREATE 1 BIG GRIB INPUT FILE "
echo " -----------------------------------------"
echo " "
set -x

gix=/nwprod/util/exec/grbindex
g2ix=/nwprod/util/exec/grb2index
cgb=/nwprod/util/exec/copygb
cgb2=/nwprod/util/exec/copygb2

regflag=`grep NHC ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`

# --------------------------------------------------
#   Process GFS data
# --------------------------------------------------

if [ ${model} -eq 1 ]
then

  if [ $loopnum -eq 1 ]
  then

    if [ -s ${DATA}/gfsgribfile.${PDY}${CYL} ]
    then
      rm ${DATA}/gfsgribfile.${PDY}${CYL}
    fi

    rm ${DATA}/master.gfsgribfile.${PDY}${CYL}.f*
    rm ${DATA}/gfsgribfile.${PDY}${CYL}.f*
    >${DATA}/gfsgribfile.${PDY}${CYL}

    set +x
    echo " "
    echo "Time before gfs wgrib loop is `date`"
    echo " "
    set -x
  
    for fhour in ${fcsthrs}
    do
  
      if [ ${fhour} -eq 99 ]
      then
        continue
      fi
  
      if [ ! -s ${gfsdir}/${gfsgfile}${fhour} ]
      then
        set +x
        echo " "
        echo " "
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " !!! GFS file is missing                              !!!!!!!!!!!!!!"
        echo " !!! Check for the existence of this file:            !!!!!!!!!!!!!!"
        echo " !!!    GFS File: ${gfsdir}/${gfsgfile}${fhour}"
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        set -x
        continue
      fi

      gfile=${gfsdir}/${gfsgfile}${fhour}
      $wgrib2 -s $gfile >gfs.ix

      for parm in ${wgrib_parmlist}
      do
        case ${parm} in
          "SurfaceU")
           grep "UGRD:10 m " gfs.ix | $wgrib2 -i $gfile -append -grib \
                              ${DATA}/master.gfsgribfile.${PDY}${CYL}.f${fhour} ;;
            "SurfaceV")
           grep "VGRD:10 m " gfs.ix | $wgrib2 -i $gfile -append -grib \
                              ${DATA}/master.gfsgribfile.${PDY}${CYL}.f${fhour} ;;
                     *)
           grep "${parm}" gfs.ix | $wgrib2 -i $gfile -append -grib \
                              ${DATA}/master.gfsgribfile.${PDY}${CYL}.f${fhour} ;;
        esac
      done

      gfs_master_file=${DATA}/master.gfsgribfile.${PDY}${CYL}.f${fhour}
      gfs_cat_file=${DATA}/gfsgribfile.${PDY}${CYL}
      $g2ix ${gfs_master_file} ${gfs_master_file}.ix

      g1=${gfs_master_file}
      x1=${gfs_master_file}.ix
      cat ${gfs_master_file} >>${gfs_cat_file}

    done
  
    $g2ix ${DATA}/gfsgribfile.${PDY}${CYL} ${DATA}/gfsixfile.${PDY}${CYL}

#   --------------------------------------------
    if [ ${PHASEFLAG} = 'y' ]; then

    catfile=${DATA}/gfs.${PDY}${CYL}.catfile
    >${catfile}

    for fhour in ${fcsthrs}
    do

      if [ ${fhour} -eq 99 ]
      then
        continue
      fi

      set +x
      echo " "
      echo "Date in interpolation for model= $cmodel and fhour= $fhour before = `date`"
      echo " "
      set -x

      gfile=${DATA}/gfsgribfile.${PDY}${CYL}
      ifile=${DATA}/gfsixfile.${PDY}${CYL}
      $g2ix $gfile $ifile

      gparm=7
      namelist=${DATA}/vint_input.${PDY}${CYL}.z
      echo "&timein ifcsthour=${fhour},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}
  
      ln -s -f ${gfile}                                       fort.11
      ln -s -f ${rundir}/gfs_hgt_levs.txt                     fort.16
      ln -s -f ${ifile}                                       fort.31
      ln -s -f ${DATA}/${cmodel}.${PDY}${CYL}.z.f${fhour}     fort.51
  
      /usrx/local/bin/getrusage -a ${exectrkdir}/vint.x <${namelist}
      rcc1=$?
  
  
      gparm=11
      namelist=${DATA}/vint_input.${PDY}${CYL}.t
      echo "&timein ifcsthour=${fhour},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}
  
      ln -s -f ${gfile}                                       fort.11
      ln -s -f ${rundir}/gfs_tmp_levs.txt                     fort.16
      ln -s -f ${ifile}                                       fort.31 
      ln -s -f ${DATA}/${cmodel}.${PDY}${CYL}.t.f${fhour}     fort.51
  
      /usrx/local/bin/getrusage -a ${exectrkdir}/vint.x <${namelist}
      rcc2=$?
  
      namelist=${DATA}/tave_input.${PDY}${CYL}
      echo "&timein ifcsthour=${fhour},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}
  
      ffile=${DATA}/${cmodel}.${PDY}${CYL}.t.f${fhour}
      ifile=${DATA}/${cmodel}.${PDY}${CYL}.t.f${fhour}.i

      $g2ix ${ffile} ${ifile}
  
      ln -s -f ${ffile}                                          fort.11
      ln -s -f ${ifile}                                          fort.31
      ln -s -f ${DATA}/${cmodel}.tave.${PDY}${CYL}.f${fhour}     fort.51
      ln -s -f ${DATA}/${cmodel}.tave92.${PDY}${CYL}.f${fhour}     fort.92
    
      /usrx/local/bin/getrusage -a ${exectrkdir}/tave.x <${namelist}
      rcc3=$?

      if [ $rcc1 -eq 0 -a $rcc2 -eq 0 -a $rcc3 -eq 0 ]; then
        echo " "
      else
        mailfile=${rundir}/errmail.${cmodel}.${PDY}${CYL}
        echo "CPS/WC interp failure for $cmodel ${PDY}${CYL}" >${mailfile}
#        mail -s "GFS Failure (CPS/WC int) $cmodel ${PDY}${CYL}" ${userid} <${mailfile}
        exit 8
      fi
    
      tavefile=${DATA}/${cmodel}.tave.${PDY}${CYL}.f${fhour}
      zfile=${DATA}/${cmodel}.${PDY}${CYL}.z.f${fhour}
      cat ${zfile} ${tavefile} >>${catfile}
      rm $tavefile $zfile
    
      set +x
      echo " "
      echo "Date in interpolation for cmodel= $cmodel and fhour= $fhour after = `date`"
      echo " "
      set -x
    
    done
    fi

  fi

  gfile=${DATA}/gfsgribfile.${PDY}${CYL}
#  cat ${catfile} >>${gfile}
    
  ifile=${DATA}/gfsixfile.${PDY}${CYL}
  $g2ix ${gfile} ${ifile}

  gribfile=${DATA}/gfsgribfile.${PDY}${CYL}
  ixfile=${DATA}/gfsixfile.${PDY}${CYL}

fi

# --------------------------------------------------
#   Process NCEP Ensemble perturbation, if selected
# --------------------------------------------------

if [ ${model} -eq 10 ]
then

  if [ $loopnum -eq 1 ]
  then

    if [ -s ${DATA}/ens${pert}gribfile.${PDY}${CYL} ]
    then
      rm ${DATA}/ens${pert}gribfile.${PDY}${CYL}
    fi

    for fhour in ${fcsthrs}
    do

      if [ ${fhour} -eq 99 ]
      then
        continue
      fi

      if [ ! -s ${ensdir}/${ensgfile}${fhour} ]
      then
        set +x
        echo " "
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " !!! ENSEMBLE ${PERT} File missing: ${ensdir}/${ensgfile}${fhour}"
        echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        set -x
        continue
      fi

      gfile=${ensdir}/${ensgfile}${fhour}
      $wgrib2 -s $gfile >ens.ix

      for parm in ${wgrib_parmlist}
      do
        case ${parm} in
	  "SurfaceU")
	  grep "UGRD:10 m " ens.ix | $wgrib2 -i $gfile -append -grib \
	                   ${DATA}/ens${pert}gribfile.${PDY}${CYL} ;;
	  "SurfaceV")
	  grep "VGRD:10 m " ens.ix | $wgrib2 -i $gfile -append -grib \
	                   ${DATA}/ens${pert}gribfile.${PDY}${CYL} ;;
	                *)
	  grep "${parm}" ens.ix | $wgrib2 -i $gfile -append -grib  \
	                  ${DATA}/ens${pert}gribfile.${PDY}${CYL} ;;
	esac
      done
    done
    $g2ix ${DATA}/ens${pert}gribfile.${PDY}${CYL} ${DATA}/ens${pert}ixfile.${PDY}${CYL}

#   --------------------------------------------

    if [ ${PHASEFLAG} = 'y' ]; then
    catfile=${DATA}/ens${pert}.${PDY}${CYL}.catfile
     >${catfile}

    for fhour in ${fcsthrs}
    do

      if [ ${fhour} -eq 99 ]
      then
        continue
      fi

      set +x
      echo " "
      echo "Date in interpolation for pert= $pert and fhour= $fhour before = `date`"
      echo " "
      set -x

      gfile=${DATA}/ens${pert}gribfile.${PDY}${CYL}
      ifile=${DATA}/ens${pert}ixfile.${PDY}${CYL}
      $g2ix $gfile $ifile

      gparm=7
      namelist=${DATA}/vint_input.${PDY}${CYL}.z
      echo "&timein ifcsthour=${fhour},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}

      ln -s -f ${gfile}                                       fort.11
      ln -s -f ${rundir}/nce_hgt_levs.txt                     fort.16
      ln -s -f ${ifile}                                       fort.31
      ln -s -f ${DATA}/${cmodel}_${pert}.${PDY}${CYL}.z.f${fhour}     fort.51

      /usrx/local/bin/getrusage -a ${exectrkdir}/vint.x <${namelist}
      rcc1=$?

      gparm=11
      namelist=${DATA}/vint_input.${PDY}${CYL}.t
      echo "&timein ifcsthour=${fhour},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}

      ln -s -f ${gfile}                                       fort.11
      ln -s -f ${rundir}/nce_tmp_levs.txt                     fort.16
      ln -s -f ${ifile}                                       fort.31
      ln -s -f ${DATA}/${cmodel}_${pert}.${PDY}${CYL}.t.f${fhour}     fort.51

      /usrx/local/bin/getrusage -a ${exectrkdir}/vint.x <${namelist}
      rcc2=$?

      namelist=${DATA}/tave_input.${PDY}${CYL}
      echo "&timein ifcsthour=${fhour},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}

      ffile=${DATA}/${cmodel}_${pert}.${PDY}${CYL}.t.f${fhour}
      ifile=${DATA}/${cmodel}_${pert}.${PDY}${CYL}.t.f${fhour}.i
      $g2ix ${ffile} ${ifile}

      ln -s -f ${ffile}                                          fort.11
      ln -s -f ${ifile}                                          fort.31
      ln -s -f ${DATA}/${cmodel}_${pert}.tave.${PDY}${CYL}.f${fhour}     fort.51
      ln -s -f ${DATA}/${cmodel}_${pert}.tave92.${PDY}${CYL}.f${fhour}     fort.92

      /usrx/local/bin/getrusage -a ${exectrkdir}/tave.x <${namelist}
      rcc3=$?

      if [ $rcc1 -eq 0 -a $rcc2 -eq 0 -a $rcc3 -eq 0 ]; then
        echo " "
      else
        mailfile=${rundir}/errmail.${cmodel}.${pert}.${PDY}${CYL}
        echo "CPS/WC interp failure for $cmodel $pert ${PDY}${CYL}" >${mailfile}
#        mail -s "GEFS Failure (CPS/WC int) $cmodel $pert ${PDY}${CYL}" ${userid} <${mailfile}
        exit 8
      fi

      tavefile=${DATA}/${cmodel}_${pert}.tave.${PDY}${CYL}.f${fhour}
      zfile=${DATA}/${cmodel}_${pert}.${PDY}${CYL}.z.f${fhour}
      cat ${zfile} ${tavefile} >>${catfile}
      rm $tavefile $zfile

      set +x
      echo " "
      echo "Date in interpolation for pert= $pert and fhour= $fhour after = `date`"
      echo " "
      set -x

    done
    fi 

  fi

  gfile=${DATA}/ens${pert}gribfile.${PDY}${CYL}
#  cat ${catfile} >>${gfile}

  ifile=${DATA}/ens${pert}ixfile.${PDY}${CYL}
  $g2ix ${gfile} ${ifile}

  gribfile=${DATA}/ens${pert}gribfile.${PDY}${CYL}
  ixfile=${DATA}/ens${pert}ixfile.${PDY}${CYL}

fi

#------------------------------------------------------------------------#
#                         Now run the tracker                            #
#------------------------------------------------------------------------#

ist=1
while [ $ist -le 15 ]
do
  if [ ${stormflag[${ist}]} -ne 1 ]
  then
    set +x; echo "Storm number $ist NOT selected for processing"; set -x
  else
    set +x; echo "Storm number $ist IS selected for processing...."; set -x
  fi
  let ist=ist+1
done

namelist=${DATA}/input.${atcfout}.${PDY}${CYL}
ATCFNAME=` echo "${atcfname}" | tr '[a-z]' '[A-Z]'`

if [ ${cmodel} = 'sref' ]; then
  export atcfymdh=` /nwprod/util/exec/ndate -3 ${scc}${syy}${smm}${sdd}${shh}`
else
  export atcfymdh=${scc}${syy}${smm}${sdd}${shh}
fi

contour_interval=100.0
write_vit=n
want_oci=.TRUE.

echo "&datein inp%bcc=${scc},inp%byy=${syy},inp%bmm=${smm},"      >${namelist}
echo "        inp%bdd=${sdd},inp%bhh=${shh},inp%model=${model}," >>${namelist}
echo "        inp%modtyp='${modtyp}',"                           >>${namelist}
echo "        inp%lt_units='${lead_time_units}',"                >>${namelist}
echo "        inp%file_seq='${file_sequence}',"                  >>${namelist}
echo "        inp%nesttyp='${nest_type}'/"                       >>${namelist}
echo "&atcfinfo atcfnum=${atcfnum},atcfname='${ATCFNAME}',"      >>${namelist}
echo "          atcfymdh=${atcfymdh},atcffreq=${atcffreq}/"      >>${namelist}
echo "&trackerinfo trkrinfo%westbd=${trkrwbd},"                  >>${namelist}
echo "      trkrinfo%eastbd=${trkrebd},"                         >>${namelist}
echo "      trkrinfo%northbd=${trkrnbd},"                        >>${namelist}
echo "      trkrinfo%southbd=${trkrsbd},"                        >>${namelist}
echo "      trkrinfo%type='${trkrtype}',"                        >>${namelist}
echo "      trkrinfo%mslpthresh=${mslpthresh},"                  >>${namelist}
echo "      trkrinfo%v850thresh=${v850thresh},"                  >>${namelist}
echo "      trkrinfo%gridtype='${modtyp}',"                      >>${namelist}
echo "      trkrinfo%contint=${contour_interval},"               >>${namelist}
echo "      trkrinfo%want_oci=${want_oci},"                      >>${namelist}
echo "      trkrinfo%out_vit='${write_vit}',"                    >>${namelist}
echo "      trkrinfo%gribver=${gribver},"                        >>${namelist}
echo "      trkrinfo%g2_jpdtn=${g2_jpdtn}/"                      >>${namelist}
echo "&phaseinfo phaseflag='${PHASEFLAG}',"                      >>${namelist}
echo "           phasescheme='${PHASE_SCHEME}',"                 >>${namelist}
echo "           wcore_depth=${WCORE_DEPTH}/"                    >>${namelist}
echo "&structinfo structflag='${STRUCTFLAG}',"                   >>${namelist}
echo "            ikeflag='${IKEFLAG}'/"                         >>${namelist}
echo "&fnameinfo  gmodname='${atcfname}',"                       >>${namelist}
echo "            rundescr='${rundescr}',"                       >>${namelist}
echo "            atcfdescr='${atcfdescr}'/"                     >>${namelist}
echo "&verbose verb=3/"                                          >>${namelist}
echo "&waitinfo use_waitfor='n',"                                >>${namelist}
echo "          wait_min_age=10,"                                >>${namelist}
echo "          wait_min_size=100,"                              >>${namelist}
echo "          wait_max_wait=1800,"                             >>${namelist}
echo "          wait_sleeptime=5,"                               >>${namelist}
echo "          per_fcst_command=''/"                            >>${namelist}

export pgm=gettrk
#. prep_step

ln -s -f ${gribfile}                                               fort.11
ln -s -f ${DATA}/vitals.upd.${atcfout}.${PDY}${shh}                fort.12
ln -s -f ${DATA}/genvitals.upd.${cmodel}.${atcfout}.${PDY}${CYL}   fort.14
# RLW 20141010 add parm location so leadtimes file can be stored there
ln -s -f ${parmtrkdir}/${cmodel}.tracker_leadtimes                     fort.15
ln -s -f ${ixfile}                                                 fort.31

if [ ${trkrtype} = 'tracker' ]; then
  if [ ${atcfout} = 'gfdt' -o ${atcfout} = 'gfdl' -o \
       ${atcfout} = 'hwrf' -o ${atcfout} = 'hwft' ]; then
    ln -s -f ${DATA}/trak.${atcfout}.all.${stormenv}.${PDY}${CYL}       fort.61
    ln -s -f ${DATA}/trak.${atcfout}.atcf.${stormenv}.${PDY}${CYL}      fort.62
    ln -s -f ${DATA}/trak.${atcfout}.radii.${stormenv}.${PDY}${CYL}     fort.63
    ln -s -f ${DATA}/trak.${atcfout}.atcfunix.${stormenv}.${PDY}${CYL}  fort.64
    ln -s -f ${DATA}/trak.${atcfout}.atcf_gen.${stormenv}.${PDY}${CYL}  fort.66
    ln -s -f ${DATA}/trak.${atcfout}.atcf_sink.${stormenv}.${PDY}${CYL} fort.68
    ln -s -f ${DATA}/trak.${atcfout}.atcf_hfip.${stormenv}.${PDY}${CYL} fort.69
  else
    ln -s -f ${DATA}/trak.${atcfout}.all.${PDY}${CYL}       fort.61
    ln -s -f ${DATA}/trak.${atcfout}.atcf.${PDY}${CYL}      fort.62
    ln -s -f ${DATA}/trak.${atcfout}.radii.${PDY}${CYL}     fort.63
    ln -s -f ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}  fort.64
    ln -s -f ${DATA}/trak.${atcfout}.atcf_gen.${PDY}${CYL}  fort.66
    ln -s -f ${DATA}/trak.${atcfout}.atcf_sink.${PDY}${CYL} fort.68
    ln -s -f ${DATA}/trak.${atcfout}.atcf_hfip.${PDY}${CYL} fort.69
  fi
else
  ln -s -f ${DATA}/trak.${atcfout}.all.${regtype}.${PDY}${CYL}       fort.61
  ln -s -f ${DATA}/trak.${atcfout}.atcf.${regtype}.${PDY}${CYL}      fort.62
  ln -s -f ${DATA}/trak.${atcfout}.radii.${regtype}.${PDY}${CYL}     fort.63
  ln -s -f ${DATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL}  fort.64
  ln -s -f ${DATA}/trak.${atcfout}.atcf_gen.${regtype}.${PDY}${CYL}  fort.66
  ln -s -f ${DATA}/trak.${atcfout}.atcf_sink.${regtype}.${PDY}${CYL} fort.68
  ln -s -f ${DATA}/trak.${atcfout}.atcf_hfip.${regtype}.${PDY}${CYL} fort.69
fi

if [ ${atcfname} = 'aear' ]
then
  ln -s -f ${DATA}/trak.${atcfout}.initvitl.${PDY}${CYL}           fort.65
fi

if [ ${write_vit} = 'y' ]
then
  ln -s -f ${DATA}/output_genvitals.${atcfout}.${PDY}${shh}        fort.67
fi

if [ ${PHASEFLAG} = 'y' ]; then
  if [ ${atcfout} = 'gfdt' -o ${atcfout} = 'gfdl' -o \
       ${atcfout} = 'hwrf' -o ${atcfout} = 'hwft' ]; then
    ln -s -f ${DATA}/trak.${atcfout}.cps_parms.${stormenv}.${PDY}${CYL}          fort.71
  else
    ln -s -f ${DATA}/trak.${atcfout}.cps_parms.${PDY}${CYL}          fort.71
  fi
fi

if [ ${STRUCTFLAG} = 'y' ]; then
  if [ ${atcfout} = 'gfdt' -o ${atcfout} = 'gfdl' -o \
       ${atcfout} = 'hwrf' -o ${atcfout} = 'hwft' ]; then
    ln -s -f ${DATA}/trak.${atcfout}.structure.${stormenv}.${PDY}${CYL}          fort.72
    ln -s -f ${DATA}/trak.${atcfout}.fractwind.${stormenv}.${PDY}${CYL}          fort.73
    ln -s -f ${DATA}/trak.${atcfout}.pdfwind.${stormenv}.${PDY}${CYL}            fort.76
  else
    ln -s -f ${DATA}/trak.${atcfout}.structure.${PDY}${CYL}          fort.72
    ln -s -f ${DATA}/trak.${atcfout}.fractwind.${PDY}${CYL}          fort.73
    ln -s -f ${DATA}/trak.${atcfout}.pdfwind.${PDY}${CYL}            fort.76
  fi
fi

if [ ${IKEFLAG} = 'y' ]; then
  if [ ${atcfout} = 'gfdt' -o ${atcfout} = 'gfdl' -o \
       ${atcfout} = 'hwrf' -o ${atcfout} = 'hwft' ]; then
    ln -s -f ${DATA}/trak.${atcfout}.ike.${stormenv}.${PDY}${CYL}                fort.74
  else
    ln -s -f ${DATA}/trak.${atcfout}.ike.${PDY}${CYL}                fort.74
  fi
fi

if [ ${trkrtype} = 'midlat' -o ${trkrtype} = 'tcgen' ]; then
  ln -s -f ${DATA}/trkrmask.${atcfout}.${regtype}.${PDY}${CYL}     fort.77
fi


set +x
echo " "
echo " -----------------------------------------------"
echo "           NOW EXECUTING TRACKER......"
echo " -----------------------------------------------"
echo " "
set -x

msg="$pgm start for $atcfout at ${CYL}z"
postmsg "$jlogfile" "$msg"

set +x
echo "+++ TIMING: BEFORE gettrk  ---> `date`"
set -x

set +x
echo " "
echo "TIMING: Before call to gettrk at `date`"
echo " "
set -x

/usrx/local/bin/getrusage -a ${exectrkdir}/gettrk <${namelist}
gettrk_rcc=$?

set +x
echo " "
echo "TIMING: After call to gettrk at `date`"
echo " "
set -x

set +x
echo "+++ TIMING: AFTER  gettrk  ---> `date`"
set -x

#--------------------------------------------------------------#
# Now copy the output track files to different directories
#--------------------------------------------------------------#

set +x
echo " "
echo " -----------------------------------------------"
echo "    NOW COPYING OUTPUT TRACK FILES TO COM  "
echo " -----------------------------------------------"
echo " "
set -x

#mkdir /ensemble/save/Jiayi.Peng/gfspara_track/gfs.${PDY}${CYL}
#cp /ptmpp1/Jiayi.Peng/trakout2/${PDY}${CYL}/gfs/trak.gfso.atcf* /ensemble/save/Jiayi.Peng/gfspara_track/gfs.${PDY}${CYL}/.
#rm -rf /ptmpp1/Jiayi.Peng/trakout2/${PDY}${CYL}/gfs/*

if [ ${gettrk_rcc} -eq 0 ]; then

  if [ -s ${DATA}/output_genvitals.${atcfout}.${PDY}${shh} ]; then
    cat ${DATA}/output_genvitals.${atcfout}.${PDY}${shh} >>${genvitfile}
  fi
 
  if [ ${PARAFLAG} = 'YES' ]
  then
    cp ${DATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL} ../.
    cat ${DATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL} >> \
           ${rundir}/${cmodel}.atcfunix.${syyyy}
#    cp ${DATA}/trak.${atcfout}.atcf_sink.${regtype}.${PDY}${CYL} ../.
#    cp ${DATA}/trak.${atcfout}.atcf_gen.${regtype}.${PDY}${CYL} ../.
  fi

  msg="$pgm end for $atcfout at ${CYL}z completed normally"
  postmsg "$jlogfile" "$msg"

# Now copy track files into various archives....

  if [ ${SENDCOM} = 'YES' ]
  then

    glatuxarch=${glatuxarch:-${gltrkdir}/tracks.atcfunix.${syy}}
    tmatuxarch=${tmatuxarch:-/gpfs/gd2/emc/hwrf/save/${userid}/trak/prod/tracks.atcfunix.${syy}}

       # disable arghive for legacy run
       if (( 0 == 1 )); then
    cat ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}  >>${glatuxarch}
    cat ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}  >>${tmatuxarch}
       fi

    if [ ${PARAFLAG} = 'YES' ]
    then
      echo " "
    else

      if [ ${cmodel} = 'gfdl' ]
      then
        cp ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL} ${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
      else
        cp ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL} ${COM}/${atcfout}.t${CYL}z.cyclone.trackatcfunix
      fi

       # disable arghive for legacy run
       if (( 0 == 1 )); then
      tmscrdir=/gpfs/gd2/emc/hwrf/save/${userid}/trak/prod

      tmtrakstat=${tmscrdir}/tracker.prod.status
      echo "${atcfout} tracker completed okay for ${PDY}${CYL}" >>${tmtrakstat}

      export SENDDBN=${SENDDBN:-YES}
      if [ ${SENDDBN} = 'YES' ]
      then
        if [ ${cmodel} = 'gfdl' ]
        then
          $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
        else
          $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/${atcfout}.t${CYL}z.cyclone.trackatcfunix
        fi
      fi

      # We need to parse apart the atcfunix file and distribute the forecasts to 
      # the necessary directories.  To do this, first sort the atcfunix records 
      # by forecast hour (k6), then sort again by ocean basin (k1), storm number (k2)
      # and then quadrant radii wind threshold (k12).  Once you've got that organized 
      # file, break the file up by putting all the forecast records for each storm 
      # into a separate file.  Then, for each file, find the corresponding atcfunix 
      # file in the /nhc/com/prod/atcf directory and dump the atcfunix records for that
      # storm in there. 

      if [ ${cmodel} = 'gfdl' ]
      then
        auxfile=${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
      else
        auxfile=${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}
      fi

      sort -k6 ${auxfile} | sort -k1 -k2 -k12  >atcfunix.sorted

      old_string="XX, XX"

      ict=0
      while read unixrec
      do
        storm_string=` echo "${unixrec}" | cut -c1-6`
        if [ "${storm_string}" = "${old_string}" ]
        then
          echo "${unixrec}" >>atcfunix_file.${ict}
        else
          let ict=ict+1
          echo "${unixrec}"  >atcfunix_file.${ict}
          old_string="${storm_string}"
        fi
      done <atcfunix.sorted

      if [ $ict -gt 0 ]
      then
        mct=0
        while [ $mct -lt $ict ]
        do
          let mct=mct+1
          at=` head -1 atcfunix_file.$mct | cut -c1-2 | tr '[A-Z]' '[a-z]'`
          NO=` head -1 atcfunix_file.$mct | cut -c5-6`
          if [ -d /com/nhc/prod/atcf/${at}${NO}${syyyy} ]
          then
            cat atcfunix_file.$mct >>/com/nhc/prod/atcf/${at}${NO}${syyyy}/a${at}${NO}${syyyy}.dat
            set +x
            echo " "
            echo "+++ Adding records to  TPC ATCFUNIX directory: /tpcprd/atcf_unix/${at}${NO}${syyyy}"
            echo " "
            set -x
          else
            set +x
            echo " "
            echo "There is no TPC ATCFUNIX directory for: /tpcprd/atcf_unix/${at}${NO}${syyyy}"
            set -x
          fi
        done
      fi
       fi

    fi

  fi

else

  if [ ${PARAFLAG} = 'YES' ]
  then
    echo " "
  else
    tmtrakstat=/gpfs/gd2/emc/hwrf/save/${userid}/trak/prod/tracker.prod.status
    echo "ERROR: ${atcfout} tracker FAILED for ${PDY}${CYL}" >>${tmtrakstat}
  fi

  set +x
  echo " "
  echo "!!! ERROR -- An error occurred while running gettrk.x, "
  echo "!!! which is the program that actually gets the track."
  echo "!!! Return code from gettrk.x = ${gettrk_rcc}"
  echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
  echo "!!! Exiting...."
  echo " "
  set -x
  err_exit " FAILED ${jobid} - ERROR RUNNING GETTRK IN TRACKER SCRIPT- ABNORMAL EXIT"

fi
echo "leaving extrkr_2015.sh"

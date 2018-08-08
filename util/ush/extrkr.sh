#!/bin/ksh

export PS4=' + extrkr.sh line $LINENO: '

set +x
echo "TIMING: Time at beginning of extrkr.sh for pert= $pert is `date`"
set -x

set +x
##############################################################################
echo " "
echo "------------------------------------------------"
echo "xxxx - Track vortices in model sig files"
echo "------------------------------------------------"
echo "History: Mar 1998 - Marchok - First implementation of this new script."
echo "         Apr 1999 - Marchok - Modified to allow radii output file and"
echo "                              to allow reading of 4-digit years from"
echo "                              TC vitals file."
echo "         Oct 2000 - Marchok - Fixed bugs: (1) copygb target grid scanning mode"
echo "                              flag had an incorrect value of 64 (this prevented"
echo "                              NAM, NGM and ECMWF from being processed correctly);" 
echo "                              Set it to 0.  (2) ECMWF option was using the "
echo "                              incorrect input date (today's date instead of "
echo "                              yesterday's)."
echo "         Jan 2001 - Marchok - Hours now listed in script for each model and "
echo "                              passed into program.  Script included to process"
echo "                              GFDL & Ensemble data.  Call to DBN included to "
echo "                              pass data to OSO and the Navy.  Forecast length"
echo "                              extended to 5 days for GFS & MRF."
echo "         Aug 2005 - Marchok - Added ability to process ECMWF ensemble, ECMWF"
echo "                              hires out to 240 (every 12h), CMC hires, CMC"
echo "                              ensemble, GFS extended from 126h to 180h."
echo "         May 2006 - Wobus -   For 2006 NCEP ensemble implementation, changed"
echo "                              directory names."
echo "         Jun 2006 - Marchok - Changed handling of NCEP ensemble files beyond"
echo "                              180h.  These are now 1-deg instead of 2.5-deg,"
echo "                              so there is no longer a need to interpolate "
echo "                              down to 1-deg for these files.  Also, changed"
echo "                              the COM directory for CMC."
echo "        Apr 2015 - Kate - This is only used to track TCs in EnKF forecasts from model sig files "
echo " "
echo "                    In the event of a crash, you can contact Tim "
echo "                    Marchok at GFDL at (609) 452-6534 or timothy.marchok@noaa.gov"
echo " "
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
msg="has begun for ${cmodel} at ${CYL}z"
postmsg "$jlogfile" "$msg"
########################################

# This script runs the hurricane tracker using operational sfcsig output.  
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
#                                   gdas, gfdl, ens (ncep ensemble), ensm (ncep
#                                   ensemble run off of the mean fields)
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

export PDY=${PDY:-$1}
export CYL=${CYL:-$cyc}
export CYCLE=t${CYL}z
export cmodel=${cmodel:-$3}
export jobid=${jobid:-testjob}
export envir=${envir:-test}
export SENDCOM=${SENDCOM:-NO}
export PARAFLAG=${PARAFLAG:-NO}
export TRKDATA=${TRKDATA:-$DATA}
#export ATCFdir=${ATCFdir:-$COMROOT/tpc/prod/atcf}

export DATA=${DATA:-/ptmp/wx20tm/trakout}
if [ ! -d $DATA ]
then
   mkdir -p $DATA
   cd $DATA
#  /nwprod/util/ush/setup.sh
#   $utilscript/setup.sh
fi
cd $DATA

#if [ ${PARAFLAG} = 'YES' ]
#then 
#  $utilscript/setup.sh
#else
#TM take out this else part for operations.....
#  /nwprod/util/ush/setup.sh
#   $utilscript/setup.sh
#fi

if [ ${#PDY} -eq 0 -o ${#CYL} -eq 0 -o ${#cmodel} -eq 0 ]
then
  set +x
  echo
  echo "Something wrong with input data.  One or more input variables has length 0"
  echo "PDY= ${PDY}, CYL= ${CYL}, cmodel= ${cmodel}"
  echo "EXITING...."
  set -x
  err_exit " FAILED ${jobid} -- BAD INPUTS AT LINE $LINENO IN TRACKER SCRIPT - ABNORMAL EXIT"
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

syy=`echo ${PDY} | cut -c3-4`
smm=`echo ${PDY} | cut -c5-6`
sdd=`echo ${PDY} | cut -c7-8`
shh=${CYL}
symd=`echo ${PDY} | cut -c3-8`
syyyy=`echo ${PDY} | cut -c1-4`

export gfsvitdir=${gfsvitdir:-$COMINgfs}

export homesyndir=${homesyndir:-$HOMEgefs/util}
export exectrkdir=${exectrkdir:-${homesyndir}/exec}
export archsyndir=${archsyndir:-$COMINatcfsyn}

#cp /com/date/t${CYL}z ncepdate
#export CENT=` cut -c7-8 ncepdate `
export CENT=20
wgrib=$WGRIB

wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 PRMSL:MSL "
wgrib_egrep_parmlist="HGT:850|HGT:700|UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m |VGRD:10 m |ABSV:850|ABSV:700|PRMSL:MSL"
wgrib_ec_hires_parmlist=" GH:850 GH:700 U:850 U:700 U:500 V:850 V:700 V:500 10U:sfc 10V:sfc MSL:sfc "

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
# NOTE: The varible PDY is now defined within the J-Jobs that
# call this script.  Therefore there is no reason to do this
# here.
#
# NOTE: The script that processes the ECMWF data defines PDY as
# the current day, and in this script we need PDY to be 
# yesterday's date (for the ecmwf ONLY).  So instead, the ecmwf
# script will pass the variable PDYm1 to this script, and in the
# case statement below we change that to PDY.
#
# NOTE: Do NOT try to standardize this script by changing all of 
# these various data directories' variable names to be all the 
# same, such as "datadir".  As you'll see in the data cutting 
# part of this script below, different methods are used to cut 
# apart different models, thus it is important to know the 
# difference between them....
#----------------------------------------------------------------#

cmodel=`echo ${cmodel} | tr "[A-Z]" "[a-z]"`

case ${cmodel} in 

  enkf) set +x                                          ;
       echo " "                                         ;
       echo " ++ EnKF 6hr fcst (relocation) ensemble member ${pert} chosen";
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       COM=${COM:-$COMROOT/gens/${envir}/gefs.${PDY}/$cyc/track}                                ;
       fcstlen=6                                        ;
       fcsthrs=' 00 06 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99'            ;
       atcfnum=72                                       ;
       atcfname=${pert}                                 ;
       atcfout=${pert}                                  ;
       modtyp='global'                                  ;
       SENDTRACKER=NO                                   ;
       SENDDBN=NO                                       ;
       model=21                                        ;;

  *) set +x; echo " "; echo " !!! Model selected is not recognized."             ;
     echo " Model= ---> ${cmodel} <--- ..... Please submit the script again...."  ;
     echo " ";  set -x;
     err_exit " FAILED ${jobid} -- UNKNOWN cmodel IN TRACKER SCRIPT - ABNORMAL EXIT";;

esac

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
# UPDATE 5/12/98 MARCHOK: The script is updated so that for the
#   global models, the gfs directory is checked for the error-
#   checked vitals file, while for the regional models, the 
#   nam directory is checked for that file.
#--------------------------------------------------------------#

# First check to see if the vitals file is in gfsvitdir or not.  If 
# it's not, then run Hua-Lu's ftp script to get the file from one
# of the other machines.  If it's still not there, then no big 
# deal; this script will exit just a little further down once it
# realizes there are not any storms to process.

d6ago_ymdh=` $NDATE -6 ${PDY}${CYL}`
d6ago_4ymd=` echo ${d6ago_ymdh} | cut -c1-8`
d6ago_ymd=` echo ${d6ago_ymdh} | cut -c3-8`
d6ago_hh=`  echo ${d6ago_ymdh} | cut -c9-10`
d6ago_str="${d6ago_ymd} ${d6ago_hh}00"

d6ahead_ymdh=` $NDATE 6 ${PDY}${CYL}`
d6ahead_4ymd=` echo ${d6ahead_ymdh} | cut -c1-8`
d6ahead_ymd=` echo ${d6ahead_ymdh} | cut -c3-8`
d6ahead_hh=`  echo ${d6ahead_ymdh} | cut -c9-10`
d6ahead_str="${d6ahead_ymd} ${d6ahead_hh}00"


  synvitdir=${synvitdir:-$COMINgfs}
  synvitfile=t${CYL}z/gfs.t${CYL}z.syndata.tcvitals.tm00
  synvit6ago_dir=${synvit6ago_dir:-$COMINgfs6ago}
  synvit6ago_file=t$t${d6ago_hh}z/gfs.t${d6ago_hh}z.syndata.tcvitals.tm00
  synvit6ahead_dir=${synvit6ahead_dir:-$COMINgfs6ahead}
  synvit6ahead_file=t${t${d6ahead_hh}z}z/gfs.t${d6ahead_hh}z.syndata.tcvitals.tm00
# synvitfile=gfs.t${CYL}z.syndata.tcvitals.tm00
# synvit6ago_dir=${synvit6ago_dir:-$COMINgfs6ago}
# synvit6ago_file=gfs.t${d6ago_hh}z.syndata.tcvitals.tm00
# synvit6ahead_dir=${synvit6ahead_dir:-$COMINgfs6ahead}
# synvit6ahead_file=gfs.t${d6ahead_hh}z.syndata.tcvitals.tm00

set +x
echo " "
echo "              -----------------------------"
echo " "
echo " Now sorting and updating the TC Vitals file.  Please wait...."
echo " "
set -x

dnow_str="${symd} ${CYL}00"

if [ -s ${synvitdir}/${synvitfile} -o\
     -s ${synvit6ago_dir}/${synvit6ago_file} -o\
     -s ${synvit6ahead_dir}/${synvit6ahead_file} ]
then
  grep "${d6ago_str}" ${synvit6ago_dir}/${synvit6ago_file}        \
                  >${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
  grep "${dnow_str}"  ${synvitdir}/${synvitfile}                  \
                 >>${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
  grep "${d6ahead_str}" ${synvit6ahead_dir}/${synvit6ahead_file}  \
                 >>${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
else
  set +x
  echo " "
  echo " There is no (synthetic) TC vitals file for ${CYL}z in ${synvitdir},"
  echo " nor is there a TC vitals file for ${d6ago_hh}z in ${synvit6ago_dir}."
  echo " nor is there a TC vitals file for ${d6ahead_hh}z in ${synvit6ahead_dir},"
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
# UPDATE 5/12/98 MARCHOK: nawk logic is added to screen NHC 
#   vitals such as "91L NAMELESS" or "89E NAMELESS", since TPC 
#   does not want tracks for such storms.
# UPDATE 11/30/12 FAFJ: no nawk during WCOSS transfer, using awk instead

grep "${d6ago_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | awk 'substr($0,6,1) !~ /[8]/ {print $0}' \
      >${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
grep "${dnow_str}"  ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | awk 'substr($0,6,1) !~ /[8]/ {print $0}' \
      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
grep "${d6ahead_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy} | \
      grep -v TEST | awk 'substr($0,6,1) !~ /[8]/ {print $0}' \
      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}

#PRODTEST#
#PRODTEST# Use the next couple lines to test the tracker on the SP.
#PRODTEST# These next couple lines use data from a test TC Vitals file that 
#PRODTEST# I generate.  When you are ready to test this system, call me and
#PRODTEST# I'll create one for the current day, and then uncomment the next
#PRODTEST# couple lines in order to access the test vitals file.
#
#ttrkdir=/nfsuser/g01/wx20tm/trak/prod/data
#ttrkdir=/nfsuser/g01/wx20tm/trak/prod/scripts
#grep "${dnow_str}" ${ttrkdir}/tcvit.test >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}


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

ymdh6ago=` $NDATE -6 ${PDY}${CYL}`
syy6=`echo ${ymdh6ago} | cut -c3-4`
smm6=`echo ${ymdh6ago} | cut -c5-6`
sdd6=`echo ${ymdh6ago} | cut -c7-8`
shh6=`echo ${ymdh6ago} | cut -c9-10`
symd6=${syy6}${smm6}${sdd6}

ymdh6ahead=` $NDATE 6 ${PDY}${CYL}`
syyp6=`echo ${ymdh6ahead} | cut -c3-4`
smmp6=`echo ${ymdh6ahead} | cut -c5-6`
sddp6=`echo ${ymdh6ahead} | cut -c7-8`
shhp6=`echo ${ymdh6ahead} | cut -c9-10`
symdp6=${syyp6}${smmp6}${sddp6}

echo "&datenowin   dnow%yy=${syy}, dnow%mm=${smm},"       >${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "             dnow%dd=${sdd}, dnow%hh=${CYL}/"      >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "&date6agoin  d6ago%yy=${syy6}, d6ago%mm=${smm6},"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "             d6ago%dd=${sdd6}, d6ago%hh=${shh6}/"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "&date6aheadin  d6ahead%yy=${syyp6}, d6ahead%mm=${smmp6},"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "               d6ahead%dd=${sddp6}, d6ahead%hh=${shhp6}/"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}

numvitrecs=`cat ${DATA}/vitals.${atcfout}.${PDY}${CYL} | wc -l`
if [ ${numvitrecs} -eq 0 ]
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

# - - - - - - - - - - - - -
# Before running the program to read, sort and update the vitals,
# first run the vitals through some awk logic, the purpose of 
# which is to convert all the 2-digit years into 4-digit years.
# Beginning 4/21/99, NHC and JTWC will begin sending the vitals
# with 4-digit years, however it is unknown when other global
# forecasting centers will begin using 4-digit years, thus we
# need the following logic to ensure that all the vitals going
# into supvit.f have uniform, 4-digit years in their records.
#
# 1/8/2000: sed code added by Tim Marchok due to the fact that 
#       some of the vitals were getting past the syndata/qctropcy
#       error-checking with a colon in them; the colon appeared
#       in the character immediately to the left of the date, which
#       was messing up the "(length($4) == 8)" statement logic.
# - - - - - - - - - - - - -

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

export pgm=supvit
. prep_step

export FORT31=${DATA}/vitals.${atcfout}.${PDY}${CYL}       
export FORT51=${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}  

msg="$pgm start for $atcfout at ${CYL}z"
postmsg "$jlogfile" "$msg"

$SUPVIT <${DATA}/suv_input.${atcfout}.${PDY}${CYL}
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

#------------------------------------------------------------------#
# Now select all storms to be processed, that is, process every
# storm that's listed in the updated vitals file for the current
# forecast hour.  If there are no storms for the current time,
# then exit.
#------------------------------------------------------------------#

numvitrecs=`cat ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`
if [ ${numvitrecs} -eq 0 ]
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

# Load the forecast hours for this particular model into an array 
# that will be passed into the executable via a namelist....

ifh=1
while [ $ifh -le ${maxtime} ]
do
  fh[${ifh}]=` echo ${fcsthrs} | awk '{print $n}' n=$ifh`
  let ifh=ifh+1
done

namelist=${DATA}/input.${atcfout}.${PDY}${CYL}
ATCFNAME=` echo "${atcfname}" | tr '[a-z]' '[A-Z]'`
  
echo "&datein inp%byy=${syy},inp%bmm=${smm},inp%bdd=${sdd},"    >${namelist}
echo "        inp%bhh=${shh}, inp%model=${model}/"             >>${namelist}
echo "&stormlist stswitch = ${stormflag[1]},${stormflag[2]},"  >>${namelist}
echo "      ${stormflag[3]},${stormflag[4]},${stormflag[5]},"  >>${namelist}
echo "      ${stormflag[6]},${stormflag[7]},${stormflag[8]},"  >>${namelist}
echo "    ${stormflag[9]},${stormflag[10]},${stormflag[11]},"  >>${namelist}
echo "   ${stormflag[12]},${stormflag[13]},${stormflag[14]},"  >>${namelist}
echo "   ${stormflag[15]}/"                                    >>${namelist}
echo "&fhlist itmphrs = ${fh[1]},${fh[2]},${fh[3]},"           >>${namelist}
echo "      ${fh[4]},${fh[5]},${fh[6]},${fh[7]},"              >>${namelist}
echo "      ${fh[8]},${fh[9]},${fh[10]},${fh[11]},"            >>${namelist}
echo "      ${fh[12]},${fh[13]},${fh[14]},"                    >>${namelist}
echo "      ${fh[15]},${fh[16]},${fh[17]},"                    >>${namelist}
echo "      ${fh[18]},${fh[19]},${fh[20]},"                    >>${namelist}
echo "      ${fh[21]},${fh[22]},${fh[23]},"                    >>${namelist}
echo "      ${fh[24]},${fh[25]},${fh[26]},"                    >>${namelist}
echo "      ${fh[27]},${fh[28]},${fh[29]},"                    >>${namelist}
echo "      ${fh[30]},${fh[31]},${fh[32]},"                    >>${namelist}
echo "      ${fh[33]},${fh[34]},${fh[35]},"                    >>${namelist}
echo "      ${fh[36]},${fh[37]},${fh[38]},"                    >>${namelist}
echo "      ${fh[39]},${fh[40]},${fh[41]},"                    >>${namelist}
echo "      ${fh[42]},${fh[43]},${fh[44]},"                    >>${namelist}
echo "      ${fh[45]},${fh[46]},${fh[47]},"                    >>${namelist}
echo "      ${fh[48]},${fh[49]},${fh[50]},"                    >>${namelist}
echo "      ${fh[51]},${fh[52]},${fh[53]},"                    >>${namelist}
echo "      ${fh[54]},${fh[55]},${fh[56]},"                    >>${namelist}
echo "      ${fh[57]},${fh[58]},${fh[59]},"                    >>${namelist}
echo "      ${fh[60]},${fh[61]},${fh[62]},"                    >>${namelist}
echo "      ${fh[63]},${fh[64]},${fh[65]}/"                    >>${namelist}
echo "&atcfinfo atcfnum=${atcfnum},atcfname='${ATCFNAME}'/"    >>${namelist}
### YOTA 12/21/2012 sigma file option begin
echo "${SIGVAL}"                                               >>${namelist}
### YOTA 12/21/2012 sigma file option end


export pgm=gettrk
. prep_step

ln -s -f ${gribfile}                                    fort.11
ln -s -f ${DATA}/vitals.upd.${atcfout}.${PDY}${shh}     fort.12
ln -s -f ${ixfile}                                      fort.31
ln -s -f ${DATA}/trak.${atcfout}.all.${PDY}${CYL}       fort.61
ln -s -f ${DATA}/trak.${atcfout}.atcf.${PDY}${CYL}      fort.62
ln -s -f ${DATA}/trak.${atcfout}.radii.${PDY}${CYL}     fort.63
ln -s -f ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}  fort.64

set +x
echo " "
echo " -----------------------------------------------"
echo "           NOW EXECUTING TRACKER......"
echo " -----------------------------------------------"
echo " "
set -x

msg="$pgm start for $atcfout at ${CYL}z"
postmsg "$jlogfile" "$msg"

${exectrkdir}/gettrk <${namelist}
gettrk_rcc=$?
export err=$?; err_chk

echo "TIMING: Time in extrkr.sh after gettrk for pert= $pert is `date`"
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

if [ ${gettrk_rcc} -eq 0 ]; then

  msg="$pgm end for $atcfout at ${CYL}z completed normally"
  postmsg "$jlogfile" "$msg"

# Copy atcf files to NHC archives. We'll use Steve Lord's original script,
# distatcf.sh, to do this, and that script requires the input atcf file to
# have the name "attk126", so first copy the file to that name, then call
# the distatcf.sh script.  After that's done, then copy the full 0-72h
# track into the /com/hur/prod/global track archive file.

  if [ ${SENDCOM} = 'YES' ]
  then

        cp ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL} ${COM}/${atcfout}.t${CYL}z.cyclone.trackatcfunix


      export SENDDBN=${SENDDBN:-YES}
      export SENDTRACKER=${SENDTRACKER:-NO}
      if [ ${SENDDBN} = 'YES' -o ${SENDTRACKER} = 'YES' ]
      then
            $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/${atcfout}.t${CYL}z.cyclone.trackatcfunix
      fi


  fi


else

  #if [ ${PARAFLAG} = 'YES' ]
  #then
  #  echo " "
  #else
  #  tmtrakstat=/nfsuser/g01/wx20tm/trak/prod/tracker.prod.status
  #  echo "ERROR: ${atcfout} tracker FAILED for ${PDY}${CYL}" >>${tmtrakstat}
  #fi

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

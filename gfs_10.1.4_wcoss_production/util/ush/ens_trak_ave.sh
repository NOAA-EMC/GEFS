#!/bin/ksh

export PS4=' + ens_trak_ave.sh line $LINENO: '

set +x
##############################################################################
echo " "
echo "-----------------------------------------------------"
echo " +++ - Compute ensemble mean cyclone forecast track"
echo "-----------------------------------------------------"
echo "History: Jul 2005 - Marchok - First implementation of this new script."
echo " "
echo "                    In the event of a crash, you can contact Tim "
echo "                    Marchok at GFDL at (609) 452-6534 or "
echo "                    via email at timothy.marchok@noaa.gov"
echo " "
echo " "
echo "Current time is: `date`"
echo " "
##############################################################################
set -x

########################################
msg="has begun for ${cmodel} at ${CYL}z"
postmsg "$jlogfile" "$msg"
########################################

# This script computes ensemble mean cyclone tracks.  It dumps all
# the cyclone tracks for a given ensemble forecast into one file,
# then runs a fortran program to compute the ensemble mean, spread
# and mode.  In addition, strike probabilities are produced for
# any storms in the Atlantic, EastPac and WestPac basins. 
#
# Environmental variable inputs needed for this scripts:
#  PDY   -- The date for data being processed, in YYYYMMDD format
#  cycle -- The cycle for data being processed (t00z, t06z, t12z, t18z)
#  cmodel -- Model being processed: ens (ncep ensemble), cens (CMC ensemble),
#                                   ece (ECMWF ensemble).
#  envir -- 'prod' or 'test'
#  SENDCOM -- 'YES' or 'NO'
#
# For testing script interactively in non-production, set following vars:
#     gltrkdir   - Directory for output tracks
#

export CYL=${CYL:-` echo ${cycle} | cut -c2-3`}
ymdh=${PDY}${CYL}

cent=`  echo ${ymdh} | cut -c1-2`
yy=`    echo ${ymdh} | cut -c3-4`
mm=`    echo ${ymdh} | cut -c5-6`
dd=`    echo ${ymdh} | cut -c7-8`
hh=`    echo ${ymdh} | cut -c9-10`
syyyy=` echo ${ymdh} | cut -c1-4`
ymd=${yy}${mm}${dd}
yyyymmdd=${cent}${yy}${mm}${dd}
export cyc=${hh}

#ndate=/nwprod/util/exec/ndate
#DHOU 04/13/2012  for ZEUS/WCOSS portability
export HOMEGLOBAL=${HOMEGLOBAL:-/nwprod}
ndate=$HOMEGLOBAL/util/exec/ndate

export exectrkdir=${exectrkdir:-/nwprod/util/exec}
export gltrkdir=${gltrkdir:-/com/hur/${envir}/global}
export ATCFdir=${ATCFdir:-/com/tpc/prod/atcf}

cd $DATA

#/nwprod/util/ush/setup.sh
#DHOU 04/13/2012   changed for ZEUS/WCOSS and portability
export utilscript=${utilscript:-/nwprod/util/exec}
ksh $utilscript/setup.sh

case ${cmodel} in

   ens) set +x                                                 ;
        echo " "                                               ;
        echo " ++ Input cmodel parameter = ${cmodel}...."      ;
        echo " ++ NCEP ensemble tracks will be averaged...."   ;
        echo " "                                               ;
        set -x                                                 ;
        AMODEL="AEMN"                                          ;
        amodel="aemn"                                          ;
        achar="a"                                              ;
        COM=${COM:-/com/gens/${envir}/gefs.${PDY}/${CYL}/track} ;
        SENDDBN=NO                                             ;
        SENDTRACKER=NO                                         ;;
 
   ece) set +x                                                 ;
        echo " "                                               ;
        echo " ++ Input cmodel parameter = ${cmodel}...."      ;
        echo " ++ ECMWF ensemble tracks will be averaged...."  ;
        echo " "                                               ;
        set -x                                                 ;
        AMODEL="EEMN"                                          ;
        amodel="eemn"                                          ;
        achar="e"                                              ;
        [ $CYL -eq 12 ] && PDY=$PDYm1                          ;
#        COM=${COM:-/com/mrf/${envir}/wsr.$PDY}                 ;
        COM=/com/mrf/${envir}/wsr.$PDY                         ;
        ymdh=${PDY}${CYL}                                      ;
        SENDDBN=NO                                             ;
        SENDTRACKER=NO                                        ;;
 
  cens) set +x                                                 ;
        echo " "                                               ;
        echo " ++ Input cmodel parameter = ${cmodel}...."      ;
        echo " ++ CMC ensemble tracks will be averaged...."    ;
        echo " "                                               ;
        set -x                                                 ;
        AMODEL="CEMN"                                          ;
        amodel="cemn"                                          ;
        achar="c"                                              ;
        COM=${COM:-/com/gens/prod/cmce.${PDY}/${CYL}/track}    ;
        SENDDBN=NO                                             ;
        SENDTRACKER=NO                                        ;;

  sref) set +x                                                 ;
        echo " "                                               ;
        echo " ++ Input cmodel parameter = ${cmodel}...."      ;
        echo " ++ SREF ensemble tracks will be averaged...."   ;
        echo " "                                               ;
        set -x                                                 ;
        AMODEL="SRMN"                                          ;
        amodel="srmn"                                          ;
        achar="s"                                              ;
        COM=${COM:-/com/sref/${envir}/sref.${PDY}/${CYL}/track};
        SENDDBN=NO                                             ;
        SENDTRACKER=NO                                        ;;

     *) set +x                                                 ;
        echo " "                                               ;
        echo " !! INPUT CMODEL PARAMETER IS NOT RECOGNIZED."   ;
        echo " !! Input cmodel parameter = ${cmodel}...."      ;
        echo " "                                               ;
        set -x                                                 ;
        err_exit " FAILED ${jobid} -- UNKNOWN cmodel IN TRACK-AVERAGING SCRIPT - ABNORMAL EXIT";;

esac

#---------------------------------------------------
# Run the program that calculates the ensemble mean
# track and generates the probability files....
#---------------------------------------------------

echo "TIMING: Time before any of the track-averaging stuff is `date`"

>trak.allperts.atcfunix.${amodel}.${ymdh}

# for tfile in `ls -1 ${COM}/trak.${achar}[np]*.atcfunix.${PDY}${cyc}`
for tfile in `ls -1 ${COM}/${achar}[np]*.t${cyc}z.cyclone.trackatcfunix`
do
  cat $tfile >>trak.allperts.atcfunix.${amodel}.${ymdh}
done

numrecs=` cat trak.allperts.atcfunix.${amodel}.${ymdh} | wc -l`
if [ ${numrecs} -gt 0 ]; then
  echo
else
  echo " "
  echo "+++ NOTE: NO MEMBER TRACKS EXIST FOR ${ymdh}"
  echo "+++ EXITING...."
  exit 0
fi

for dt in 65
do

  set +x
  echo "TIMING: Time just before call to trakave for dt= $dt is  `date`"
  set -x
  export pgm=ens_trak_ave
  . prep_step

  rm -rf fort.11 fort.51 fort.53 fort.54 fort.55 fort.56 fort.57

  ln -s -f ${DATA}/trak.allperts.atcfunix.${amodel}.${ymdh}     fort.11
  ln -s -f ${DATA}/${amodel}.trkprob.${ymdh}.${dt}.ctlinfo.txt  fort.51
#   Unit 52: Created internally, may be 1 to 15 files...

  ln -s -f ${DATA}/trak.${amodel}.atcfunix.${ymdh}              fort.53
  ln -s -f ${DATA}/trak.${amodel}.atcf.${ymdh}                  fort.54
  ln -s -f ${DATA}/trak.${amodel}.all.${ymdh}                   fort.55
  ln -s -f ${DATA}/trak.${amodel}.spread.${ymdh}                fort.56
  ln -s -f ${DATA}/trak.${amodel}.mode.${ymdh}                  fort.57

  namelist=${DATA}/input.${ymdh}.nlist
  echo "&datain dthresh=${dt}.0,cmodel='${cmodel}'/" >${namelist}

  ${exectrkdir}/ens_trak_ave <${namelist} >${DATA}/ens_trak_ave.${ymdh}.${dt}.fout
  ens_trak_ave_rcc=$?

  set +x
  echo "TIMING: Time just after call to trakave for dt= $dt is  `date`"
  set -x

done

set +x
echo "TIMING: Time after loop to get mean & probabilities for $ymdh is `date`"
set -x

if [ ${ens_trak_ave_rcc} -ne 0 ]
then
  set +x
  echo " "
  echo "!!! ERROR -- An error occurred while running ens_trak_ave.x, "
  echo "!!! which is the program that computes the mean track."
  echo "!!! Return code from ens_trak_ave.x = ${ens_trak_ave_rcc}"
  echo "!!! model= ${amodel}, forecast initial time = ${PDY}${CYL}"
  echo "!!! Exiting...."
  echo " "
  set -x
  err_exit " FAILED ${jobid} - ERROR RUNNING ENS_TRAK_AVE.X - ABNORMAL EXIT"
fi

#-----------------------------------------------------
# Parse out the atcfunix records and send them to the
# correct storm trackers directory and file.

if [ ${SENDCOM} = 'YES' ]
then

  glatuxarch=${glatuxarch:-${gltrkdir}/tracks.atcfunix.${yy}}
  cat ${DATA}/trak.${amodel}.atcfunix.${ymdh}           >>${glatuxarch}

  glmodearch=${glmodearch:-${gltrkdir}/tracks.ens_mode.atcfunix.${yy}}
  cat ${DATA}/trak.${amodel}.mode.${ymdh}               >>${glmodearch}

  cp ${DATA}/trak.${amodel}.atcfunix.${ymdh} ${COM}/${amodel}.t${CYL}z.cyclone.trackatcfunix
  cp ${DATA}/trak.${amodel}.spread.${ymdh}   ${COM}/${amodel}.t${CYL}z.cyclone.trackspread
  cp ${DATA}/trak.${amodel}.mode.${ymdh}     ${COM}/${amodel}.t${CYL}z.cyclone.trackmode
  cp ${DATA}/${amodel}.trkprob.*${ymdh}*.ieee             ${COM}/.
  cp ${DATA}/${amodel}.trkprob.${ymdh}.${dt}.ctlinfo.txt  ${COM}/.

  export SENDDBN=${SENDDBN:-NO}
  export SENDTRACKER=${SENDTRACKER:-NO}
  if [ ${SENDDBN} = 'YES' -o ${SENDTRACKER} = 'YES' ]
  then
    if [ $cmodel != 'ece' ]; then
      $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/${amodel}.t${CYL}z.cyclone.trackatcfunix
    fi
  fi  

  # We need to parse apart the atcfunix file and distribute the forecasts to
  # the necessary directories.  To do this, first sort the atcfunix records
  # by forecast hour (k6), then sort again by ocean basin (k1), storm number (k2)
  # and then quadrant radii wind threshold (k12).  Once you've got that organized
  # file, break the file up by putting all the forecast records for each storm
  # into a separate file.  Then, for each file, find the corresponding atcfunix
  # file in the storm trackers directory and dump the atcfunix records for that storm
  # in there.

  auxfile=${DATA}/trak.${amodel}.atcfunix.${PDY}${cyc}
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

  nhcct=0
  if [ $ict -gt 0 ]
  then
    mct=0
    while [ $mct -lt $ict ]
    do
      let mct=mct+1
      at=` head -1 atcfunix_file.$mct | cut -c1-2 | tr '[A-Z]' '[a-z]'`
      NO=` head -1 atcfunix_file.$mct | cut -c5-6`
      if [ ! -d $ATCFdir/${at}${NO}${syyyy} ]
      then
          mkdir -p $ATCFdir/${at}${NO}${syyyy}
      fi
      cat atcfunix_file.$mct >>$ATCFdir/${at}${NO}${syyyy}/ncep_a${at}${NO}${syyyy}.dat
      set +x
      echo " "
      echo "+++ Adding records to  TPC ATCFUNIX directory: $ATCFdir/${at}${NO}${syyyy}"
      echo " "
      set -x

      if [ $at = 'al' -o $at = 'ep' ]; then
        let nhcct=nhcct+1
      fi
    done
  fi

  cat ${DATA}/ens_trak_ave.${ymdh}.${dt}.fout

fi

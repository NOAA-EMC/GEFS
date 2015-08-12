#!/bin/sh
#
#  UTILITY SCRIPT NAME :  mkgefs_wafs.sh
#               AUTHOR :  Boi Vuong
#         DATE WRITTEN :  10/17/2006
#
#  Abstract:  This utility script uses COPYGB to interpolate to WAFS
#             (grid 37-44) from the Ensemble model (Grid 3) and uses
#             MKWMOENS to add a WMO header.
#
#     Input:  1 argument is passed to this script.
#             1st argument is the grid ID - format of 2I
# History log:
#   8/2015: Modified for WCOSS phase2
#

echo "------------------------------------------------------------"
echo "                 Ensemble WAFS processing                   "
echo "------------------------------------------------------------"

########################################
msg="Begin job for $job"
postmsg "$jlogfile" "$msg"
########################################

gid="$1"
sid="$2"
num=$#

if test $num -eq 2
then
   echo ""
   echo " Appropriate number of arguments were passed"
   echo ""
else
   echo ""
   echo " Number of arguments were not passed "
   echo ""
   echo "   This script creates WAFS (grids 37-44)"
   echo "   from global ensemble data."
   echo ""
   echo "     Usage:  mkgefs_wafs.sh \"37 38\" "
   echo ""
   exit 16
fi

cd $DATA
mkdir sub$sid
cd sub$sid

set +x
echo " "
echo "#####################################"
echo "    Process GRIB ENS-WAFS PRODUCTS   "
echo "#####################################"
echo " "
set -x


for hr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84  \
          90 96 102 108 114 120 126 132 138 144 150 156 \
          162 168 174 180 186 192;
do

   input_ens=../all_ens.t${cyc}z.pgrbaf$hr
   input_ensi=../ll_ens.t${cyc}z.pgrbaif$hr
   $GRBINDEX $input_ens $input_ensi
   for grid in $gid ; do
      $COPYGB -g${grid} -a $input_ens $input_ensi wafsens${grid}f$hr.${cycle}

      cp wafsens${grid}f$hr.${cycle} wafsens${grid}f$hr.${cycle}_temp
      ln -sf wafsens${grid}f$hr.${cycle}_temp  fort.11
      $UTILgefs/overenstr.grib
      mv fort.51   wafsens${grid}f$hr.${cycle}
      rm fort.11

      $GRBINDEX wafsens${grid}f$hr.${cycle} wafsens${grid}if${hr}.${cycle}
      export pgm=mkwmoens
      . prep_step
      export FORT11="wafsens${grid}f$hr.${cycle}"
      export FORT31="wafsens${grid}if$hr.${cycle}"
      export FORT51="wafs${grid}.${cycle}.ens${hr}"
#     $utilexec/mkwmoens < $PARMgefs/gefs_awips/grib_wafs${grid}.ens${hr} parm='KWBK' >> $pgmout 2>errfile
      $MKWMOENS < $PARMgefs/gefs_awips/grib_wafs${grid}.ens${hr} parm='KWBK' >> $pgmout 2>errfile
      err=$?;export err ;err_chk
      echo " error from tocgrib2=",$err

      if [ $? �-ne 0 ]; then
         msg="WARNING: WMO header not added to $FORT11"
         postmsg $jlogfile "$msg"
         echo "$msg"
      fi

      # Convert to GRIB2 format:
      $CNVGRIB21_GFS -g12 -p40 wafs${grid}.${cycle}.ens${hr} wafs${grid}.${cycle}.ens${hr}.grib2
      $GRB2INDEX wafs${grid}.${cycle}.ens${hr}.grib2 wafs${grid}.${cycle}.ens${hr}.grib2.idx

      if test $SENDCOM = 'YES'
      then
         cpfs wafs${grid}.${cycle}.ens${hr} $COMOUT/wafs${grid}.${cycle}.ens${hr}
         cpfs wafs${grid}.${cycle}.ens${hr}.grib2 $COMOUT/wafs${grid}.${cycle}.ens${hr}.grib2
         cpfs wafs${grid}.${cycle}.ens${hr}.grib2.idx $COMOUT/wafs${grid}.${cycle}.ens${hr}.grib2.idx
         if [[ "$grid" = "38" || "$grid" = "39" || "$grid" = "40" ]]
         then
	    cpfs wafs${grid}.${cycle}.ens${hr} $PCOMOUT/wafs${grid}.${cycle}.ens${hr}
         fi
      fi
   done
done

cd $DATA

if [ "$SENDDBN" = "YES" ]
then
   for hr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84  \
          90 96 102 108 114 120 126 132 138 144 150 156 \
          162 168 174 180 186 192;
   do
      for grid in $gid ; do
        $DBNROOT/bin/dbn_alert MODEL ENS_WAFS  $job $COMOUT/wafs${grid}.${cycle}.ens${hr}

        if [ $SENDDBN = YES ]
        then

        $DBNROOT/bin/dbn_alert MODEL ENS_WAFS_GB2 $job $COMOUT/wafs${grid}.${cycle}.ens${hr}.grib2

        fi

        if [[ "$grid" = "38" || "$grid" = "39" || "$grid" = "40" ]]
        then
           $DBNROOT/bin/dbn_alert GRIB_LOW ENS_XWAFS $job $PCOMOUT/wafs${grid}.${cycle}.ens${hr}
        fi
      done
   done 
fi

msg="HAS COMPLETED NORMALLY!"
postmsg "$jlogfile" "$msg"

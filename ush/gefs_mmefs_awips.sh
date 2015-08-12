#!/bin/sh
######################################################################
#  UTILITY SCRIPT NAME :  gefs_mmefs_awips.sh
#         DATE WRITTEN :  08/30/2013
#
#  Abstract:  This utility script produces Meteorological Model Ensemble
#             Forecast System (MMEFS) of selected variables (Accumulated 
#             precipitation and 2 meter Temperature) for grid id 3 (Global
#             Lat/Lon 1 degree resolution) in GRIB2 format for AWIPS
#             from the GEFS model.
# History log:
#   8/2015: Modified for WCOSS phase2
#
######################################################################
echo "---------------------------------------------------------------"
echo "JGEFS_AWIPS ( 000Z, 06Z, 12Z and 18Z) GEFS postprocessing      "
echo "---------------------------------------------------------------"
echo " "
######################################################################

########################################
  msg="HAS BEGUN!"
  postmsg "$jlogfile" "$msg"
########################################

set -x 

echo " ------------------------------------------"
echo "  BEGIN MAKING GEFS (MMEFS) GRIB2 PRODUCTS "
echo " ------------------------------------------"

set +x
echo " "
echo "#########################################"
echo "                                         "
echo "   Process GEFS (MMEFS) GRIB2 PRODUCTS   "
echo "   FOR FORECAST HOURS 00 - 180.          "
echo "                                         "
echo "#########################################"
echo " "
set -x

export SLEEP_TIME=900
export SLEEP_INT=5
export SLEEP_LOOP_MAX=`expr $SLEEP_TIME / $SLEEP_INT`
export type=pgrb2a

for id in  gec00 gep01 gep02 gep03 gep04 gep05 gep06 gep07 gep08  gep09 gep10 \
           gep11 gep12 gep13 gep14 gep15 gep16 gep17 gep18 gep19 gep20
do
    for  hr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 \
           108 114 120 126 132 138 144 150 156 162 168 174 180
    do
#
#    Process APCP  
#
    ############################################################
    # Start Looping for the
    # existence of the files (2m Temperatures - bias corrected)
    ############################################################
    
    set +x
    export pgm="postcheck"
    ic=1
    while [ $ic -le $SLEEP_LOOP_MAX ]
    do
       if test -f ${COMIN}/${type}_bc/${id}.t${cyc}z.${type}_bcf${hr}
       then
          break
       else
          ic=`expr $ic + 1`
          sleep $SLEEP_INT
       fi
       ###############################
       # If we reach this point assume
       # fcst job never reached restart
       # period and error exit
       ###############################
       if [ $ic -eq $SLEEP_LOOP_MAX ]
       then
          echo " FATAL ERROR ${COMIN}/${type}_bc/${id}.t${cyc}z.${type}_bcf${hr} is not available!!!"
          export err=9
          err_chk
       fi
     done 
 
     set -x

     cpfs ${COMIN}/${type}/${id}.t${cyc}z.${type}f${hr} .
     $WGRIB2 ${id}.t${cyc}z.${type}f${hr} | grep "APCP" | $WGRIB2 -i ${id}.t${cyc}z.${type}f${hr} -grib ${id}.t${cyc}z.${type}f${hr}_apcp

     cat  ${id}.t${cyc}z.${type}f${hr}_apcp >> ${id}.t${cyc}z.${type}_apcp
#
#    Process 2m Temperatures - bias corrected
# 
     cp ${COMIN}/${type}_bc/${id}.t${cyc}z.${type}_bcf${hr} .
     $WGRIB2 ${id}.t${cyc}z.${type}_bcf${hr} | grep "TMP:2 m" | $WGRIB2 -i ${id}.t${cyc}z.${type}_bcf${hr} -grib ${id}.t${cyc}z.${type}_bcf${hr}_tmp_2m
     cat ${id}.t${cyc}z.${type}_bcf${hr}_tmp_2m  >>  ${id}.t${cyc}z.${type}_bc_tmp_2m

   done
done

############################################
# Processing GRIB2 GEFS grid 3 for MMEFS
############################################

pgm=tocgrib2
. prep_step
startmsg

for type in pgrb2a_apcp pgrb2a_bc_tmp_2m
do
   for id in gec00 gep01 gep02 gep03 gep04 gep05 gep06 gep07 gep08 gep09 gep10 \
             gep11 gep12 gep13 gep14 gep15 gep16 gep17 gep18 gep19 gep20
   do

# Processing GEFS (MMEFS) GRIB2 

      export FORT11=${id}.t${cyc}z.${type}
      export FORT51=grib2_naefs_gefs_${id}.t${cyc}z.${type}
      $TOCGRIB2 < $PARMgefs/gefs_awips/grib2_naefs_gefs.${type} >> $pgmout 2>errfile

      err=$?;export err ;err_chk
      echo " error from tocgrib2=",$err

      if [ $? -ne 0 ]; then
         msg="WARNING: WMO header not added to $FORT11"
         postmsg $jlogfile "$msg"
         echo "$msg"
      fi 

      if [ -s grib2_naefs_gefs_${id}.t${cyc}z.${type} ]; then
         if [ "$SENDCOM" = YES ]; then
            ##############################
            # Post Files to PCOM
            ##############################
            cpfs grib2_naefs_gefs_${id}.t${cyc}z.${type} $PCOMOUT/grib2_naefs_gefs_${id}.t${cyc}z.${type}

            if [ "$SENDDBN" = YES ]; then
               ##########################
               # Distribute Data to NCF
               #########################
               $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $PCOMOUT/grib2_naefs_gefs_${id}.t${cyc}z.${type}
            fi
          fi
      else
          err_exit "grib2_naefs_gefs_${id}.t${cyc}z.${type} was not generated"
      fi

  done
done

################################################################################
# GOOD RUN
set +x
echo "**************JOB JGEFS_WAFS HAS COMPLETED NORMALLY ON THE WCOSS"
set -x
################################################################################

msg="HAS COMPLETED NORMALLY!"
postmsg "$jlogfile" "$msg"

############## END OF SCRIPT #######################

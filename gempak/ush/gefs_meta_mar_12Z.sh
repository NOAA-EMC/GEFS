#!/bin/ksh
#
# Metafile Script : gefs_meta_mar_12Z.sh
#
# Log :
# J. Carr/PMB      12/15/2004     Pushed into production
# Luke Lin         02/15/2006     point to new gefs
# C. Magee/NCO     10/06/2008     Changed to use COMINs and COMIN for input 
#                                 file locations (to make testing easier).
#
# Set Up Local Variables
#
set -x
export PS4='mar_12Z:$SECONDS + '
mkdir $DATA/mar_12Z
cd $DATA/mar_12Z
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gefs
MDL=GEFS
PDY2=`echo $PDY | cut -c3-`

if [ ${cyc} != "12" ] ; then
    echo " "
    echo "EXITING GEMPAK SCRIPT BECAUSE THIS SCRIPT DOES NOT EXECUTE"
    echo "AT ANY OTHER TIME EXCEPT 12Z."
    echo " "
    exit
fi

mdl=gefs
MDL=GEFS

# DEFINE YESTERDAY
yesterday=`${NDATE} -24 ${PDY}${cyc} | cut -c -8`
shrtyesterday=`${NDATE} -24 ${PDY}${cyc} | cut -c3-8`

fcsthrs="000 012 024 036 048 060 072 084 096 108 120"
levels="534 540 546 552 558 564 570"

for metaarea in pac atl
do
    if [ ${metaarea} == "pac" ] ; then
        garea="MPAC"
        proj=" "

    else
        garea="15;-100;70;5"
        proj="mer"
    fi
    metatype="${metaarea}_mar"
    metaname="${mdl}_${metatype}_${cyc}.meta"
    device="nc | ${metaname}"
    for level in ${levels}
    do
        for fcsthr in ${fcsthrs}
        do
            fcsthrsgfs=`expr ${fcsthr} + 12` 
            typeset -Z3 fcsthrsgfs

            export pgm=gdplot2_nc;. prep_step; startmsg

gdplot2_nc << EOF
GDFILE	= F-GEFSC00 | ${PDY2}/${cyc}00
GDATTIM	= F${fcsthr}
DEVICE	= ${device}
PANEL	= 0
TEXT	= m/22/1/1/hw
CONTUR	= 2
MAP	= 1
CLEAR	= yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = 1/10/1/2/10;10

GLEVEL  = 500 
GVCORD  = pres 
SKIP    = 0 
SCALE   = -1 
GDPFUN  = sm5s(hght)
TYPE    = c
CINT    = ${level}
LINE    = 6/1/2/0
FINT    =
FLINE   =
HILO    = 0
HLSYM   = 0
CLRBAR  = 0
WIND    = 0
REFVEC  =
TITLE   = 6/-1/~ ? C00 (CNTL)|~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP01 | ${PDY2}/${cyc}00
MAP     = 0
LATLON  = 0
CLEAR	= no
LINE    = 14/1/2/0
TITLE   = 14/-2/~ ? P01 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP02 | ${PDY2}/${cyc}00
LINE    = 2/1/2/0
TITLE   = 2/+1/~ ? P02 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP03 | ${PDY2}/${cyc}00
LINE    = 3/1/2/0
TITLE   = 3/+2/~ ? P03 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP04 | ${PDY2}/${cyc}00
LINE    = 4/1/2/0
TITLE   = 4/+3/~ ? P04 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP05 | ${PDY2}/${cyc}00
LINE    = 12/1/2/0
TITLE   = 12/+4/~ ? P05 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP06 | ${PDY2}/${cyc}00
LINE    = 11/1/2/0
TITLE   = 11/+5/~ ? P06 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP07 | ${PDY2}/${cyc}00
LINE    = 7/1/2/0
TITLE   = 7/+6/~ ? P07 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP08 | ${PDY2}/${cyc}00
LINE    = 8/1/2/0
TITLE   = 8/+7/~ ? P08 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP09 | ${PDY2}/${cyc}00
LINE    = 9/1/2/0
TITLE   = 9/+8/~ ? P09 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP10 | ${PDY2}/${cyc}00
LINE    = 10/1/2/0
TITLE   = 10/+9/~ ? P10 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP11 | ${PDY2}/${cyc}00
LINE    = 11/1/2/0
TITLE   = 11/+10/~ ? P11 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP12 | ${PDY2}/${cyc}00
LINE    = 12/1/2/0
TITLE   = 12/+11/~ ? P12 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP13 | ${PDY2}/${cyc}00
LINE    = 13/1/2/0
TITLE   = 13/+12/~ ? P13 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP14 | ${PDY2}/${cyc}00
LINE    = 14/1/2/0
TITLE   = 14/+13/~ ? P14 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP15 | ${PDY2}/${cyc}00
LINE    = 15/1/2/0
TITLE   = 15/+13/~ ? P15 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP16 | ${PDY2}/${cyc}00
LINE    = 16/1/2/0
TITLE   = 16/+13/~ ? P16 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP17 | ${PDY2}/${cyc}00
LINE    = 17/1/2/0
TITLE   = 17/+13/~ ? P17 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP18 | ${PDY2}/${cyc}00
LINE    = 18/1/2/0
TITLE   = 18/+13/~ ? P18 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP19 | ${PDY2}/${cyc}00
LINE    = 19/1/2/0
TITLE   = 19/+13/~ ? P19 |~${level} DM - ${metaarea}
run

GDFILE	= F-GEFSP20 | ${PDY2}/${cyc}00
LINE    = 20/1/2/0
TITLE   = 20/+13/~ ? P20 |~${level} DM - ${metaarea}
run

GDFILE	= \$COMINs_p1/nam.${PDY}/nam_${PDY}${cyc}f${fcsthr}
LINE    = 31/2/3/0
TITLE   = 31/+10/~ ? NAM (DASHED) |~${level} DM - ${metaarea}
run

#GDFILE	= \$COMINs/gfs.${PDY}/gfs_${PDY}${cyc}f${fcsthr}
GDFILE	= \$COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs_${PDY}${cyc}f${fcsthr}
LINE    = 5/2/3/0
TITLE   = 5/+12/~ ? GFS (DASHED) |~${level} DM - ${metaarea}
run

exit
EOF

            export err=$?;err_chk
        done
    done

    # GENERATE THE PMSL LOW CENTERS
    num=" "
    metashname="LOW CNTRS"

    for fcsthr in ${fcsthrs}
    do
        fcsthrsgfs=`expr ${fcsthr} + 12`
        typeset -Z3 fcsthrsgfs

        export pgm=gdplot2_nc;. prep_step; startmsg

gdplot2_nc << EOF
GDFILE	= F-GEFSC00 | ${PDY2}/${cyc}00
GDATTIM	= F${fcsthr}
DEVICE	= ${device}
PANEL	= 0
TEXT	= m/22/1/1/hw
CONTUR	= 1
MAP	= 1
CLEAR	= yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = 1/10/1/2/10;10

GLEVEL  = 0 
GVCORD  = none 
SKIP    = 0 
SCALE   = 0 
GDPFUN  = pmsl
TYPE    = c
CINT    = 4/1/8
LINE    = 0
HILO    = 6/L#/900-1016/5/50/y
HLSYM   = l/22/3/hw
FINT    =
FLINE   =
CLRBAR  = 0
WIND    = 0
REFVEC  =
TITLE   = 6/-1/~ ? C00 (CNTL)|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP01 | ${PDY2}/${cyc}00
MAP     = 0
LATLON  = 0
CLEAR	= no
LINE    = 14/1/2/0
HILO    = 14/L#/900-1016/5/50/y
TITLE   = 14/-2/~ ? P01 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP02 | ${PDY2}/${cyc}00
LINE    = 2/1/2/0
HILO    = 2/L#/900-1016/5/50/y
TITLE   = 2/+1/~ ? P02 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP03 | ${PDY2}/${cyc}00
LINE    = 3/1/2/0
HILO    = 3/L#/900-1016/5/50/y
TITLE   = 3/+2/~ ? P03 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP04 | ${PDY2}/${cyc}00
LINE    = 4/1/2/0
HILO    = 4/L#/900-1016/5/50/y
TITLE   = 4/+3/~ ? P04 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP05 | ${PDY2}/${cyc}00
LINE    = 12/1/2/0
HILO    = 12/L#/900-1016/5/50/y
TITLE   = 12/+4/~ ? P05 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP06 | ${PDY2}/${cyc}00
LINE    = 11/1/2/0
HILO    = 11/L#/900-1016/5/50/y
TITLE   = 11/+5/~ ? P06 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP07 | ${PDY2}/${cyc}00
LINE    = 7/1/2/0
HILO    = 7/L#/900-1016/5/50/y
TITLE   = 7/+6/~ ? P07 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP08 | ${PDY2}/${cyc}00
LINE    = 8/1/2/0
HILO    = 8/L#/900-1016/5/50/y
TITLE   = 8/+7/~ ? P08 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP09 | ${PDY2}/${cyc}00
LINE    = 9/1/2/0
HILO    = 9/L#/900-1016/5/50/y
TITLE   = 9/+8/~ ? P09 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP10 | ${PDY2}/${cyc}00
LINE    = 10/1/2/0
HILO    = 10/L#/900-1016/5/50/y
TITLE   = 10/+9/~ ? P10 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP11 | ${PDY2}/${cyc}00
LINE    = 11/1/2/0
HILO    = 11/L#/900-1016/5/50/y
TITLE   = 11/+10/~ ? P11 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP12 | ${PDY2}/${cyc}00
LINE    = 12/1/2/0
HILO    = 12/L#/900-1016/5/50/y
TITLE   = 12/+11/~ ? P12 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP13 | ${PDY2}/${cyc}00
LINE    = 13/1/2/0
HILO    = 13/L#/900-1016/5/50/y
TITLE   = 13/+12/~ ? P13 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP14 | ${PDY2}/${cyc}00
LINE    = 14/1/2/0
HILO    = 14/L#/900-1016/5/50/y
TITLE   = 14/+13/~ ? P14 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP15 | ${PDY2}/${cyc}00
LINE    = 15/1/2/0
HILO    = 15/L#/900-1016/5/50/y
TITLE   = 15/+13/~ ? P15 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP16 | ${PDY2}/${cyc}00
LINE    = 16/1/2/0
HILO    = 16/L#/900-1016/5/50/y
TITLE   = 16/+13/~ ? P16 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP17 | ${PDY2}/${cyc}00
LINE    = 17/1/2/0
HILO    = 17/L#/900-1016/5/50/y
TITLE   = 17/+13/~ ? P17 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP18 | ${PDY2}/${cyc}00
LINE    = 18/1/2/0
HILO    = 18/L#/900-1016/5/50/y
TITLE   = 18/+13/~ ? P18 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP19 | ${PDY2}/${cyc}00
LINE    = 19/1/2/0
HILO    = 19/L#/900-1016/5/50/y
TITLE   = 19/+13/~ ? P19 |~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP20 | ${PDY2}/${cyc}00
LINE    = 20/1/2/0
HILO    = 20/L#/900-1016/5/50/y
TITLE   = 20/+13/~ ? P20 |~${metaarea} ${metashname}
run

GDFILE	= \$COMINs_p1/nam.${PDY}/nam_${PDY}${cyc}f${fcsthr}
LINE    = 31/2/3/0
HILO    = 31/L#/900-1016/5/50/y
TITLE   = 31/+10/~ ? NAM |~${metaarea} ${metashname}
run

GDFILE	= \$COMINs/gfs.${PDY}/gfs_${PDY}${cyc}f${fcsthr}
LINE    = 5/2/3/0
HILO    = 5/L#/900-1016/5/50/y
TITLE   = 5/+12/~ ? GFS |~${metaarea} ${metashname}
run

exit
EOF

        export err=$?;err_chk
    done

    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################
    ls -l ${metaname}
    export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

    if [ $SENDCOM = "YES" ] ; then
        mv ${metaname} ${COMOUT}/gefs_${PDY}_${cyc}_${metatype}
        if [ $SENDDBN = "YES" ] ; then
            $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/gefs_${PDY}_${cyc}_${metatype}
        fi
    fi
done
exit

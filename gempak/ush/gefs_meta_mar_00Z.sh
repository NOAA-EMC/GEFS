#!/bin/sh
#
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
export PS4='mar_00Z:$SECONDS + '
mkdir $DATA/mar_00Z
cd $DATA/mar_00Z
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gefs
MDL=GEFS
PDY2=`echo $PDY | cut -c3-`

if [ ${cyc} != "00" ] ; then
    echo " "
    echo "EXITING GEMPAK SCRIPT BECAUSE THIS SCRIPT DOES NOT EXECUTE"
    echo "AT ANY OTHER TIME EXCEPT 00Z."
    echo " "
    exit
fi

# DEFINE YESTERDAY
yesterday=`${NDATE} -24 ${PDY}${cyc} | cut -c -8`
shrtyesterday=`${NDATE} -24 ${PDY}${cyc} | cut -c3-8`

# SET GFS PARAMETERS
gfscyc="12"
gfscyc2="06"
ecmwfcyc="12"
ecmwfdate="${yesterday}"
ukmetcyc="12"
ukmetdate="${yesterday}"

fcsthrs="000 012 024 036 048 060 072 084 096 108 120"
levels="528 534 540 546 552 564 576"

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
GDPFUN  = sm9s(hght)
TYPE    = c
CINT    = ${level}
LINE    = 6/1/1/0
FINT    =
FLINE   =
HILO    = 0
HLSYM   = 0
CLRBAR  = 0
WIND    = 0
REFVEC  =
TITLE   = 6/-2/~ ? C00 (CNTL)|~${metaarea} ${level} DM
run

CLEAR   = no
MAP     = 0
LATLON  = 0
GDFILE	= F-GEFSP01 | ${PDY2}/${cyc}00
LINE    = 17/1/1/0
TITLE   = 17/+1/~ ? P01|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP02 | ${PDY2}/${cyc}00
LINE    = 2/1/1/0
TITLE   = 2/+2/~ ? P02|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP03 | ${PDY2}/${cyc}00
LINE    = 4/1/1/0
TITLE   = 4/+3/~ ? P03|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP04 | ${PDY2}/${cyc}00
LINE    = 7/1/1/0
TITLE   = 7/+4/~ ? P04|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP05 | ${PDY2}/${cyc}00
LINE    = 8/1/1/0
TITLE   = 8/+5/~ ? P05|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP06 | ${PDY2}/${cyc}00
LINE    = 9/1/1/0
TITLE   = 9/+6/~ ? P06|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP07 | ${PDY2}/${cyc}00
LINE    = 10/1/1/0
TITLE   = 10/+7/~ ? P07|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP08 | ${PDY2}/${cyc}00
LINE    = 11/1/1/0
TITLE   = 11/+8/~ ? P08|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP09 | ${PDY2}/${cyc}00
LINE    = 12/1/1/0
TITLE   = 12/+9/~ ? P09|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP10 | ${PDY2}/${cyc}00
LINE    = 14/1/1/0
TITLE   = 14/+10/~ ? P10|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP11 | ${PDY2}/${cyc}00
LINE    = 15/1/1/0
TITLE   = 15/+11/~ ? P11|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP12 | ${PDY2}/${cyc}00
LINE    = 16/1/1/0
TITLE   = 16/+12/~ ? P12|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP13 | ${PDY2}/${cyc}00
LINE    = 17/1/1/0
TITLE   = 17/+13/~ ? P13|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP14 | ${PDY2}/${cyc}00
LINE    = 18/1/1/0
TITLE   = 18/+14/~ ? P14|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP15 | ${PDY2}/${cyc}00
LINE    = 18/1/1/0
TITLE   = 18/+15/~ ? P15|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP16 | ${PDY2}/${cyc}00
LINE    = 18/1/1/0
TITLE   = 18/+16/~ ? P16|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP17 | ${PDY2}/${cyc}00
LINE    = 18/1/1/0
TITLE   = 18/+17/~ ? P17|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP18 | ${PDY2}/${cyc}00
LINE    = 18/1/1/0
TITLE   = 18/+18/~ ? P18|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP19 | ${PDY2}/${cyc}00
LINE    = 18/1/1/0
TITLE   = 18/+19/~ ? P19|~${metaarea} ${level} DM
run

GDFILE	= F-GEFSP20 | ${PDY2}/${cyc}00
LINE    = 18/1/1/0
TITLE   = 18/+20/~ ? P20|~${metaarea} ${level} DM
run

#GDFILE	= \$COMINs/gfs.${yesterday}/gfs_${yesterday}${gfscyc}f${fcsthrsgfs}
GDFILE	= \$COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs_${yesterday}${gfscyc}f${fcsthrsgfs}
LINE    = 3/1/3/0
GDATTIM	= F${fcsthrsgfs}
TITLE   = 3/+11/~ ? GFS 12Z YEST|~${metaarea} ${level} DM
run

GDFILE	= \$COMINs/ecmwf.${ecmwfdate}/ecmwf_glob_${ecmwfdate}${ecmwfcyc}
LINE    = 31/1/2/0
GDATTIM	= F${fcsthrsgfs}
TITLE   = 31/+12/~ ? ECMWF 12Z YEST|~${metaarea} ${level} DM
run

GDFILE	= \$COMINs_p1/ukmet.${PDY}/ukmet_${PDY}${cyc}f${fcsthr}
LINE    = 26/2/2/0
GDATTIM	= F${fcsthr}
TITLE   = 26/+13/~ ? UKMET 00Z|~${metaarea} ${level} DM
run

exit
EOF

            #echo "before first err chk"
            export err=$?;err_chk
            echo "after first err chk"
        done
    done

    # GENERATE THE PMSL LOW CENTERS
    num=" "
    metashname="LOW CNTRS"
    for fcsthr in ${fcsthrs}
    do
        fcsthrsgfs=`expr ${fcsthr} + 12`
        typeset -Z3 fcsthrsgfs
        fcsthrsgfs2=`expr ${fcsthr} - 6`
        typeset -Z3 fcsthrsgfs2

        export pgm=gdplot2_nc;. prep_step; startmsg

gdplot2_nc << EOF
GDFILE	= F-GEFSC00 | ${PDY2}/${cyc}00
GDATTIM	= F${fcsthr}
DEVICE	= ${device}
PANEL	= 0
TEXT	= s/22/1/1/hw
CONTUR	= 2
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
FINT    =
FLINE   =
HILO    = 6/L${num}/900-1016/5/50/y
HLSYM   = l/22/3/hw
CLRBAR  = 0
WIND    = 0
REFVEC  =
TITLE   = 6/-2/~ ? C00 (CNTL) |~${metaarea} ${metashname}
run

CLEAR   = no
MAP     = 0
LATLON  = 0
GDFILE	= F-GEFSP01 | ${PDY2}/${cyc}00
HILO    = 17/L${num}/900-1016/5/50/y
TITLE   = 17/+1/~ ? P01|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP02 | ${PDY2}/${cyc}00
HILO    = 2/L${num}/900-1016/5/50/y
TITLE   = 2/+2/~ ? P02|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP03 | ${PDY2}/${cyc}00
HILO    = 4/L${num}/900-1016/5/50/y
TITLE   = 4/+3/~ ? P03|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP04 | ${PDY2}/${cyc}00
HILO    = 7/L${num}/900-1016/5/50/y
TITLE   = 7/+4/~ ? P04|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP05 | ${PDY2}/${cyc}00
HILO    = 8/L${num}/900-1016/5/50/y
TITLE   = 8/+5/~ ? P05|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP06 | ${PDY2}/${cyc}00
HILO    = 9/L${num}/900-1016/5/50/y
TITLE   = 9/+6/~ ? P06|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP07 | ${PDY2}/${cyc}00
HILO    = 10/L${num}/900-1016/5/50/y
TITLE   = 10/+7/~ ? P07|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP08 | ${PDY2}/${cyc}00
HILO    = 11/L${num}/900-1016/5/50/y
TITLE   = 11/+8/~ ? P08|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP09 | ${PDY2}/${cyc}00
HILO    = 12/L${num}/900-1016/5/50/y
TITLE   = 12/+9/~ ? P09|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP10 | ${PDY2}/${cyc}00
HILO    = 14/L${num}/900-1016/5/50/y
TITLE   = 14/+10/~ ? P10|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP11 | ${PDY2}/${cyc}00
HILO    = 15/L${num}/900-1016/5/50/y
TITLE   = 15/+11/~ ? P11|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP12 | ${PDY2}/${cyc}00
HILO    = 16/L${num}/900-1016/5/50/y
TITLE   = 16/+12/~ ? P12|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP13 | ${PDY2}/${cyc}00
HILO    = 17/L${num}/900-1016/5/50/y
TITLE   = 17/+13/~ ? P13|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP14 | ${PDY2}/${cyc}00
HILO    = 18/L${num}/900-1016/5/50/y
TITLE   = 18/+14/~ ? P14|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP15 | ${PDY2}/${cyc}00
HILO    = 18/L${num}/900-1016/5/50/y
TITLE   = 18/+15/~ ? P15|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP16 | ${PDY2}/${cyc}00
HILO    = 18/L${num}/900-1016/5/50/y
TITLE   = 18/+16/~ ? P16|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP17 | ${PDY2}/${cyc}00
HILO    = 18/L${num}/900-1016/5/50/y
TITLE   = 18/+17/~ ? P17|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP18 | ${PDY2}/${cyc}00
HILO    = 18/L${num}/900-1016/5/50/y
TITLE   = 18/+18/~ ? P18|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP19 | ${PDY2}/${cyc}00
HILO    = 18/L${num}/900-1016/5/50/y
TITLE   = 18/+19/~ ? P19|~${metaarea} ${metashname}
run

GDFILE	= F-GEFSP20 | ${PDY2}/${cyc}00
HILO    = 18/L${num}/900-1016/5/50/y
TITLE   = 18/+20/~ ? P20|~${metaarea} ${metashname}
run

GDFILE	= \$COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs_${yesterday}${gfscyc}f${fcsthrsgfs}
HILO    = 3/L${num}/900-1016/5/50/y
GDATTIM	= F${fcsthrsgfs}
TITLE   = 3/+11/~ ? GFS 12Z YEST|~${metaarea} ${metashname}
run

GDFILE	= \$COMINs/ecmwf.${ecmwfdate}/ecmwf_glob_${ecmwfdate}${ecmwfcyc}
HILO    = 31/L${num}/900-1016/5/50/y
GDATTIM	= F${fcsthrsgfs}
TITLE   = 31/+12/~ ? ECMWF 12Z YEST|~${metaarea} ${metashname}
run

GDFILE	= \$COMINs_p1/ukmet.${PDY}/ukmet_${PDY}${cyc}f${fcsthr}
HILO    = 26/L${num}/900-1016/5/50/y
GDATTIM	= F${fcsthr}
TITLE   = 26/+13/~ ? UKMET 00Z|~${metaarea} ${metashname}
run

exit
EOF

        echo "before 2nd err chk"
        export err=$?;err_chk
        echo "after 2nd err chk"
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

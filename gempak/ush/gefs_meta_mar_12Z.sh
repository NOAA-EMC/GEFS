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

########################################################
## Get member list
########################################################
export npert=${npert:-30}
memberlist=""
(( imem = 0 ))
while (( imem < npert+1 )); do
    if (( imem == 0 )); then
        smem=c$(printf %02i $imem)
    else
        smem=p$(printf %02i $imem)
    fi
    memberlist="$memberlist $smem"
    (( imem = imem + 1 ))
done # while (( imem < npert ))
echo memberlist=$memberlist
########################################################
## Get member list
########################################################

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

            grids=${memberlist}
            for fn in `echo $grids`
            do
                rm -rf $fn 
                if [ -r $COMIN/ge${fn}_${PDY}${cyc}f${fcsthr} ]; then
                    ln -s $COMIN/ge${fn}_${PDY}${cyc}f${fcsthr} $fn
                fi
            done

            fn=gfs
            rm -rf ${fn}
            if [ -r $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs_${PDY}${cyc}f${fcsthr} ]; then
                ln -s $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs_${PDY}${cyc}f${fcsthr} ${fn}
            fi

            fn=nam
            rm -rf ${fn}
            if [ -r $COMINs_p1/nam.${PDY}/nam_${PDY}${cyc}f${fcsthr} ]; then
                ln -s $COMINs_p1/nam.${PDY}/nam_${PDY}${cyc}f${fcsthr} ${fn}
            fi

            export pgm=gdplot2_nc;. prep_step; startmsg

cat > cmdfilemar  << EOF
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
FINT    =
FLINE   =
HILO    = 0
HLSYM   = 0
CLRBAR  = 0
WIND    = 0
REFVEC  =

EOF
            WrottenZERO=0
            grids=${memberlist}
            line_count=2
            color_number=9
            for gridl in ${grids}
            do
                # ----- gridl -----
                gdfn=${gridl} 
                
                if [ ${gdfn} == c00 ]; then
                    color_number=6
                    sline_count="-1"
                    sCNTL="(CNTL)"
                elif [[ ${gdfn} == p01 ]]; then
                    color_number=14
                    sline_count="-2"
                    sCNTL=""
                else
        
                    
                    if [[ ${gdfn} == p02 ]]; then
                        color_number=2
                        line_count=1
                    elif [[ ${gdfn} == p03 ]]; then
                        color_number=3
                        line_count=2
                    elif [[ ${gdfn} == p04 ]]; then
                        color_number=4
                        line_count=3
                    elif [[ ${gdfn} == p05 ]]; then
                        color_number=12
                        line_count=4
                    elif [[ ${gdfn} == p06 ]]; then
                        color_number=11
                        line_count=5
                    elif [[ ${gdfn} == p07 ]]; then
                        color_number=7
                        line_count=6
                    elif [[ ${gdfn} == p08 ]]; then
                        color_number=8
                        line_count=7
                    elif [[ ${gdfn} == p09 ]]; then
                        color_number=9
                        line_count=8
                    elif [[ ${gdfn} == p10 ]]; then
                        color_number=10
                        line_count=9
                    elif [[ ${gdfn} == p11 ]]; then
                        color_number=11
                        line_count=10
                    elif [[ ${gdfn} == p12 ]]; then
                        color_number=12
                        line_count=11
                    elif [[ ${gdfn} == p13 ]]; then
                        color_number=13
                        line_count=12
                    elif [[ ${gdfn} == p14 ]]; then
                        color_number=14
                        line_count=13
                    elif [[ ${gdfn} == p15 ]]; then
                        color_number=15
                        line_count=13
                    elif [[ ${gdfn} == p16 ]]; then
                        color_number=16
                        line_count=13
                    elif [[ ${gdfn} == p17 ]]; then
                        color_number=17
                        line_count=13
                    elif [[ ${gdfn} == p18 ]]; then
                        color_number=18
                        line_count=13
                    elif [[ ${gdfn} == p19 ]]; then
                        color_number=19
                        line_count=13
                    elif [[ ${gdfn} == p20 ]]; then
                        color_number=20
                        line_count=13
                    else
                        color_number=`echo $gdfn | cut -c2-`
                        line_count=13
                    fi

                    sline_count="+${line_count}"
                    sCNTL=""

                    #let line_count=$line_count+1
                fi

                # ----- gridl -----
                if [ -e ${gdfn} ]; then
cat >> cmdfilemar  << EOF
GDFILE  = ${gdfn}
LINE    = ${color_number}/1/2/0
TITLE   = ${color_number}/${sline_count}/~ ? ${gdfn} ${sCNTL} |~${level} DM - ${metaarea}
GDATTIM = F${fcsthr}
run

EOF
                    if [ $WrottenZERO -eq 0 ]; then            
cat >> cmdfilemar  << EOF
MAP     = 0
LATLON  = 0
CLEAR   = no

EOF
                    fi
                    WrottenZERO=1
                fi


            done

            # ----- nam -----
            gdfn=nam
            if [ -e ${gdfn} ]; then
cat >> cmdfilemar  << EOF
GDFILE  = ${gdfn}
LINE    = 31/2/3/0
TITLE   = 31/+10/~ ? ${gdfn} (DASHED) |~${level} DM - ${metaarea}
GDATTIM = F${fcsthr}
run

EOF
                if [ $WrottenZERO -eq 0 ]; then            
cat >> cmdfilemar  << EOF
MAP     = 0
LATLON  = 0
CLEAR   = no

EOF
                fi
                WrottenZERO=1
            fi

            # ----- gfs -----
            gdfn=gfs
            if [ -e ${gdfn} ]; then
cat >> cmdfilemar  << EOF
GDFILE  = ${gdfn}
LINE    = 5/2/3/0
TITLE   = 5/+12/~ ? ${gdfn} (DASHED) |~${level} DM - ${metaarea}
GDATTIM = F${fcsthr}
run

EOF
            fi

            cat cmdfilemar
            gdplot2_nc < cmdfilemar

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

        grids=${memberlist}
        for fn in `echo $grids`
        do
            rm -rf $fn 
            if [ -r $COMIN/ge${fn}_${PDY}${cyc}f${fcsthr} ]; then
                ln -s $COMIN/ge${fn}_${PDY}${cyc}f${fcsthr} $fn
            fi
        done

        fn=gfs
        rm -rf ${fn}
        if [ -r $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs_${PDY}${cyc}f${fcsthr} ]; then
            ln -s $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs_${PDY}${cyc}f${fcsthr} ${fn}
        fi

        fn=nam
        rm -rf ${fn}
        if [ -r $COMINs_p1/nam.${PDY}/nam_${PDY}${cyc}f${fcsthr} ]; then
            ln -s $COMINs_p1/nam.${PDY}/nam_${PDY}${cyc}f${fcsthr} ${fn}
        fi

        export pgm=gdplot2_nc;. prep_step; startmsg

cat > cmdfilemar_low  << EOF
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
HLSYM   = l/22/3/hw
FINT    =
FLINE   =
CLRBAR  = 0
WIND    = 0
REFVEC  =

EOF
        WrottenZERO = 0
        grids=${memberlist}
        line_count=2
        color_number=9
        for gridl in ${grids}
        do
            # ----- gridl -----
            gdfn=${gridl} 
            
            if [ ${gdfn} == c00 ]; then
                color_number=6
                sline_count="-1"
                sCNTL="(CNTL)"

            else
    
                if [ ${gdfn} == c00 ]; then
                    color_number=6
                    sline_count="-1"
                    sCNTL="(CNTL)"
                elif [[ ${gdfn} == p01 ]]; then
                    color_number=14
                    sline_count="-2"
                    sCNTL=""
                else
        
                    
                    if [[ ${gdfn} == p02 ]]; then
                        color_number=2
                        line_count=1
                    elif [[ ${gdfn} == p03 ]]; then
                        color_number=3
                        line_count=2
                    elif [[ ${gdfn} == p04 ]]; then
                        color_number=4
                        line_count=3
                    elif [[ ${gdfn} == p05 ]]; then
                        color_number=12
                        line_count=4
                    elif [[ ${gdfn} == p06 ]]; then
                        color_number=11
                        line_count=5
                    elif [[ ${gdfn} == p07 ]]; then
                        color_number=7
                        line_count=6
                    elif [[ ${gdfn} == p08 ]]; then
                        color_number=8
                        line_count=7
                    elif [[ ${gdfn} == p09 ]]; then
                        color_number=9
                        line_count=8
                    elif [[ ${gdfn} == p10 ]]; then
                        color_number=10
                        line_count=9
                    elif [[ ${gdfn} == p11 ]]; then
                        color_number=11
                        line_count=10
                    elif [[ ${gdfn} == p12 ]]; then
                        color_number=12
                        line_count=11
                    elif [[ ${gdfn} == p13 ]]; then
                        color_number=13
                        line_count=12
                    elif [[ ${gdfn} == p14 ]]; then
                        color_number=14
                        line_count=13
                    elif [[ ${gdfn} == p15 ]]; then
                        color_number=15
                        line_count=13
                    elif [[ ${gdfn} == p16 ]]; then
                        color_number=16
                        line_count=13
                    elif [[ ${gdfn} == p17 ]]; then
                        color_number=17
                        line_count=13
                    elif [[ ${gdfn} == p18 ]]; then
                        color_number=18
                        line_count=13
                    elif [[ ${gdfn} == p19 ]]; then
                        color_number=19
                        line_count=13
                    elif [[ ${gdfn} == p20 ]]; then
                        color_number=20
                        line_count=13
                    else
                        color_number=`echo $gdfn | cut -c2-`
                        line_count=13
                    fi

                sline_count="+${line_count}"
                sCNTL=""

                #let line_count=$line_count+1
            fi

            if [ -e ${gdfn} ]; then
cat >> cmdfilemar_low  << EOF
GDFILE  = ${gdfn}
LINE    = ${color_number}/1/2/0
HILO    = ${color_number}//L#/900-1016/5/50/y
TITLE   = ${color_number}/${sline_count}/~ ? ${gdfn} ${sCNTL} |~${metaarea} ${metashname}
GDATTIM	= F${fcsthr}
run

EOF
                if [ $WrottenZERO -eq 0 ]; then            
cat >> cmdfilemar_low  << EOF
MAP     = 0
LATLON  = 0
CLEAR   = no

EOF
                fi
                WrottenZERO=1
            fi

        done



        # ----- nam -----
        gdfn=nam
        if [ -e ${gdfn} ]; then
cat >> cmdfilemar_low  << EOF
GDFILE	= ${gdfn}
LINE    = 31/2/3/0
HILO    = 31/L#/900-1016/5/50/y
TITLE   = 31/+10/~ ? ${gdfn} |~${metaarea} ${metashname}
run

EOF
            if [ $WrottenZERO -eq 0 ]; then            
cat >> cmdfilemar_low  << EOF
MAP     = 0
LATLON  = 0
CLEAR   = no

EOF
            fi
            WrottenZERO=1
        fi

        # ----- gfs -----
        gdfn=gfs
        if [ -e ${gdfn} ]; then
cat >> cmdfilemar_low  << EOF
GDFILE	= ${gdfn}
LINE    = 5/2/3/0
HILO    = 5/L#/900-1016/5/50/y
TITLE   = 5/+12/~ ? ${gdfn} |~${metaarea} ${metashname}
run

EOF
        fi

        cat cmdfilemar_low
        gdplot2_nc < cmdfilemar_low

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

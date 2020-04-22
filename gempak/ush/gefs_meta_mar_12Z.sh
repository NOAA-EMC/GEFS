#!/bin/ksh
#
# Metafile Script : gefs_meta_mar_12Z.sh
#
# Log :
# J. Carr/PMB      12/15/2004     Pushed into production
# Luke Lin         02/15/2006     point to new gefs
# C. Magee/NCO     10/06/2008     Changed to use COMINs and COMIN for input 
#                                 file locations (to make testing easier).
# Xianwu Xue/EMC   04/06/2020     Modified for GEFS v12
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

sGrid=${sGrid} #:-"_0p50"}

mkdir $DATA/mar_12Z
cd $DATA/mar_12Z

PDY2=`echo $PDY | cut -c3-`

if [ ${cyc} != "12" ] ; then
    echo " "
    echo "EXITING GEMPAK SCRIPT BECAUSE THIS SCRIPT DOES NOT EXECUTE"
    echo "AT ANY OTHER TIME EXCEPT 12Z."
    echo " "
    exit
fi

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
    metatype="mar_${metaarea}"
    metaname="gefs${sGrid}_${PDY}_${cyc}_meta_${metatype}"
    device="nc | ${metaname}"
    for level in ${levels}
    do
        for fcsthr in ${fcsthrs}
        do
            fcsthrsgfs=`expr ${fcsthr} + 12` 
            #typeset -Z3 fcsthrsgfs
            fcsthrsgfs=$(printf %03i $fcsthrsgfs)

            grids=${memberlist}
            for fn in `echo $grids`
            do
                rm -rf $fn 
                if [ -r $COMIN/ge${fn}${sGrid}_${PDY}${cyc}f${fcsthr} ]; then
                    ln -s $COMIN/ge${fn}${sGrid}_${PDY}${cyc}f${fcsthr} $fn
                fi
            done

            fn=gfs
            rm -rf ${fn}
            if [ -r $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs${sGrid}_${PDY}${cyc}f${fcsthr} ]; then
                ln -s $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs${sGrid}_${PDY}${cyc}f${fcsthr} ${fn}
            fi

            fn=nam
            rm -rf ${fn}
            if [ -r $COMINnam/nam.${PDY}/gempak/nam_${PDY}${cyc}f${fcsthr} ]; then
                ln -s $COMINnam/nam.${PDY}/gempak/nam_${PDY}${cyc}f${fcsthr} ${fn}
            fi

			cat > cmdfilemar  <<- EOF
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
                    wLine=3
                    sCNTL="(CNTL)"

                else
                    color_number=`echo $gdfn | cut -c2-`
                    line_count=$color_number
                    wLine=1
                    sCNTL=""
                    sline_count="+${line_count}"

                    #let line_count=$line_count+1
                fi

                # ----- gridl -----
                if [ -e ${gdfn} ]; then
					cat >> cmdfilemar  <<- EOF
						GDFILE  = ${gdfn}
						LINE    = ${color_number}/1/${wLine}/0
						TITLE   = ${color_number}/${sline_count}/~ ? ${gdfn} ${sCNTL} |~${level} DM - ${metaarea}
						GDATTIM = F${fcsthr}
						run

						EOF
                    if [ $WrottenZERO -eq 0 ]; then            
						cat >> cmdfilemar  <<- EOF
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
				cat >> cmdfilemar  <<- EOF
					GDFILE  = ${gdfn}
					LINE    = 31/2/3/0
					TITLE   = 31/-3/~ ? ${gdfn} (DASHED) |~${level} DM - ${metaarea}
					GDATTIM = F${fcsthr}
					run

					EOF
                if [ $WrottenZERO -eq 0 ]; then            
					cat >> cmdfilemar  <<- EOF
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
				cat >> cmdfilemar  <<- EOF
					GDFILE  = ${gdfn}
					LINE    = 5/2/3/0
					TITLE   = 5/-4/~ ? ${gdfn} (DASHED) |~${level} DM - ${metaarea}
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
        #typeset -Z3 fcsthrsgfs
        fcsthrsgfs=$(printf %03i $fcsthrsgfs)

        grids=${memberlist}
        for fn in `echo $grids`
        do
            rm -rf $fn 
            if [ -r $COMIN/ge${fn}${sGrid}_${PDY}${cyc}f${fcsthr} ]; then
                ln -s $COMIN/ge${fn}${sGrid}_${PDY}${cyc}f${fcsthr} $fn
            fi
        done

        fn=gfs
        rm -rf ${fn}
        if [ -r $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs${sGrid}_${PDY}${cyc}f${fcsthr} ]; then
            ln -s $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs${sGrid}_${PDY}${cyc}f${fcsthr} ${fn}
        fi

        fn=nam
        rm -rf ${fn}
        if [ -r $COMINnam/nam.${PDY}/gempak/nam_${PDY}${cyc}f${fcsthr} ]; then
            ln -s $COMINnam/nam.${PDY}/gempak/nam_${PDY}${cyc}f${fcsthr} ${fn}
        fi

        #export pgm=gdplot2_nc;. prep_step; startmsg

		cat > cmdfilemar_low  <<- EOF
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
                wLine=3

            else
                color_number=`echo $gdfn | cut -c2-`
                line_count=$color_number
               
                sline_count="+${line_count}"
                sCNTL=""
                wLine=1

                #let line_count=$line_count+1
            fi

            if [ -e ${gdfn} ]; then
				cat >> cmdfilemar_low  <<- EOF
					GDFILE  = ${gdfn}
					LINE    = ${color_number}/1/${wLine}/0
					HILO    = ${color_number}//L#/900-1016/5/50/y
					TITLE   = ${color_number}/${sline_count}/~ ? ${gdfn} ${sCNTL} |~${metaarea} ${metashname}
					GDATTIM	= F${fcsthr}
					run

					EOF

                if [ $WrottenZERO -eq 0 ]; then            
					cat >> cmdfilemar_low  <<- EOF
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
			cat >> cmdfilemar_low  <<- EOF
				GDFILE	= ${gdfn}
				LINE    = 31/2/3/0
				HILO    = 31/L#/900-1016/5/50/y
				TITLE   = 31/-3/~ ? ${gdfn} |~${metaarea} ${metashname}
				run

				EOF
            if [ $WrottenZERO -eq 0 ]; then            
				cat >> cmdfilemar_low  <<- EOF
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
			cat >> cmdfilemar_low  <<- EOF
				GDFILE	= ${gdfn}
				LINE    = 5/2/3/0
				HILO    = 5/L#/900-1016/5/50/y
				TITLE   = 5/-4/~ ? ${gdfn} |~${metaarea} ${metashname}
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
        mv ${metaname} ${COMOUT}/
        if [ $SENDDBN = "YES" ] ; then
            $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/${metaname}
        fi
    fi
done
exit


#!/bin/sh
#
# Metafile Script : gefs_meta_mar_00Z.sh
#
# Log :
# J. Carr/PMB      12/15/2004     Pushed into production
# Luke Lin         02/15/2006     point to new gefs
# C. Magee/NCO     10/06/2008     Changed to use COMINs and COMIN for input
#                                 file locations (to make testing easier).
# Xianwu Xue/EMC   03/28/2020     Modify to use the new path and 0p50 gempak data
# Xianwu Xue/EMC   04/06/2020     Modified for GEFS v12
#
# Set Up Local Variables
#
echo "$(date -u) begin $(basename $BASH_SOURCE)"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

echo memberlist=$memberlist

sGrid=${sGrid} #:-"_0p50"}

mkdir $DATA/mar_00Z
cd $DATA/mar_00Z

PDY2=$(echo $PDY | cut -c3-)

if [ ${cyc} != "00" ] ; then
	echo " "
	echo "EXITING GEMPAK SCRIPT BECAUSE THIS SCRIPT DOES NOT EXECUTE"
	echo "AT ANY OTHER TIME EXCEPT 00Z."
	echo " "
	exit
fi

# DEFINE YESTERDAY
yesterday=$(${NDATE} -24 ${PDY}${cyc} | cut -c -8)
shrtyesterday=$(${NDATE} -24 ${PDY}${cyc} | cut -c3-8)

# SET GFS PARAMETERS
gfscyc="12"
gfscyc2="06"
ecmwfcyc="12"
ecmwfdate="${yesterday}"
ukmetcyc="12"
ukmetdate="${yesterday}"

fcsthrs="000 012 024 036 048 060 072 084 096 108 120"
levels="528 534 540 546 552 564 576"

for metaarea in pac atl; do
	if [ ${metaarea} == "pac" ] ; then
		garea="MPAC"
		proj=" "

	else
		garea="15;-100;70;5"
		proj="mer"
	fi
	metatype="meta_mar_${metaarea}"
	metaname="gefs${sGrid}_${PDY}_${cyc}_${metatype}"
	device="nc | ${metaname}"
	for level in ${levels}; do
		for fcsthr in ${fcsthrs}; do
			fcsthrsgfs=$((10#${fcsthr} + 12))
			fcsthrsgfs=$(printf %03i $fcsthrsgfs)

			grids=${memberlist}
			for fn in $(echo $grids); do
				rm -rf $fn
				if [ -r $COMIN/$COMPONENT/gempak/ge${fn}${sGrid}_${PDY}${cyc}f${fcsthr} ]; then
					ln -s $COMIN/$COMPONENT/gempak/ge${fn}${sGrid}_${PDY}${cyc}f${fcsthr} $fn
				fi
			done

			fn=gfs
			rm -rf ${fn}
			if [ -r $COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs${sGrid}_${yesterday}${gfscyc}f${fcsthrsgfs} ]; then
				ln -s $COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs${sGrid}_${yesterday}${gfscyc}f${fcsthrsgfs} ${fn}
			fi

			fn=ecmwf
			rm -rf ${fn}
			if [ -r $COMINecmwf.${ecmwfdate}/gempak/ecmwf_hr_${ecmwfdate}${ecmwfcyc}f${fcsthr} ]; then
				ln -s $COMINecmwf.${ecmwfdate}/gempak/ecmwf_hr_${ecmwfdate}${ecmwfcyc}f${fcsthr} ${fn}
			fi

			fn=ukmet
			rm -rf ${fn}
			if [ -r ${COMINukmet}.${PDY}/gempak/ukmet_hr_${PDY}${cyc}f${fcsthr} ]; then
				ln -s ${COMINukmet}.${PDY}/gempak/ukmet_hr_${PDY}${cyc}f${fcsthr} ${fn}
			fi


			export pgm=gdplot2_nc;. prep_step; startmsg

			cat > cmdfilemar  <<- EOF
				DEVICE  = ${device}
				PANEL   = 0
				TEXT    = m/22/1/1/hw
				CONTUR  = 2
				MAP = 1
				CLEAR   = yes
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
			for gridl in ${grids}; do
				# ----- gridl -----
				gdfn=${gridl}

				if [ ${gdfn} == c00 ]; then
					color_number=2
					sline_count="-1"
					wLine=3
					sCNTL="(CNTL)"

				else

					color_number=$(echo $gdfn | cut -c2-)
					line_count=$color_number

					sline_count="+${line_count}"

					wLine=1
					sCNTL=""

					#let line_count=$line_count+1
				fi

				if [ -e ${gdfn} ]; then
					cat >> cmdfilemar  <<- EOF
						GDFILE  = ${gdfn}
						LINE    = ${color_number}/1/${wLine}/0
						TITLE   = ${color_number}/${sline_count}/~ ? ${gdfn} ${sCNTL} |~${metaarea} ${level} DM
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

			# ----- gfs -----
			gdfn=gfs
			if [ -e ${gdfn} ]; then
				cat >> cmdfilemar  <<- EOF
					GDFILE  = ${gdfn}
					LINE    = 22/2/3/0
					GDATTIM = F${fcsthrsgfs}
					TITLE   = 22/-3/~ ? ${gdfn} 12Z YEST (DASHED)|~${metaarea} ${level} DM
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

			# ----- ecmwf -----
			gdfn=ecmwf
			if [ -e ${gdfn} ]; then
				cat >> cmdfilemar  <<- EOF
					GDFILE  = ${gdfn}
					LINE    = 6/2/3/0
					GDATTIM = F${fcsthrsgfs}
					TITLE   = 6/-5/~ ? ECMWF 12Z YEST (DASHED)|~${metaarea} ${level} DM
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

			# ----- ukmet -----
			gdfn=ukmet
			if [ -e ${gdfn} ]; then
				cat >> cmdfilemar  <<- EOF
					GDFILE  = ${gdfn}
					LINE    = 7/2/3/0
					GDATTIM = F${fcsthr}
					TITLE   = 7/-6/~ ? UKMET 00Z (DASHED)|~${metaarea} ${level} DM
					run

					EOF
			fi

			cat cmdfilemar
			gdplot2_nc < cmdfilemar
			err=$?

			if [[ $err != 0 ]]; then
				echo "FATAL ERROR in $(basename $BASH_SOURCE): gdplot2_nc failed using cmdfilemar!"
				err_chk
				exit $err
			fi
		done
	done

	# GENERATE THE PMSL LOW CENTERS
	num=" "
	metashname="LOW CNTRS"
	for fcsthr in ${fcsthrs}; do
		fcsthrsgfs=$((10#${fcsthr} + 12))
		fcsthrsgfs=$(printf %03i $fcsthrsgfs)

		fcsthrsgfs2=$((10#${fcsthr} - 6))
		fcsthrsgfs2=$(printf %03i $fcsthrsgfs2)

		grids=${memberlist}
		for fn in $(echo $grids); do
			rm -rf $fn
			if [ -r $COMIN/$COMPONENT/gempak/ge${fn}${sGrid}_${PDY}${cyc}f${fcsthr} ]; then
				ln -s $COMIN/$COMPONENT/gempak/ge${fn}${sGrid}_${PDY}${cyc}f${fcsthr} $fn
			fi
		done

		fn=gfs
		rm -rf ${fn}
		if [ -r $COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs${sGrid}_${yesterday}${gfscyc}f${fcsthrsgfs} ]; then
			ln -s $COMINsgfs/gfs.${yesterday}/${gfscyc}/gempak/gfs${sGrid}_${yesterday}${gfscyc}f${fcsthrsgfs} ${fn}
		fi

		fn=ecmwf
		rm -rf ${fn}
		if [ -r $COMINecmwf.${ecmwfdate}/gempak/ecmwf_hr_${ecmwfdate}${ecmwfcyc}f${fcsthr} ]; then
			ln -s $COMINecmwf.${ecmwfdate}/gempak/ecmwf_hr_${ecmwfdate}${ecmwfcyc}f${fcsthr} ${fn}
		fi

		fn=ukmet
		rm -rf ${fn}
		if [ -r $COMINukmet.${PDY}/gempak/ukmet_hr_${PDY}${cyc}f${fcsthr} ]; then
			ln -s $COMINukmet.${PDY}/gempak/ukmet_hr_${PDY}${cyc}f${fcsthr} ${fn}
		fi

		export pgm=gdplot2_nc;. prep_step; startmsg

		cat > cmdfilemar_low  <<- EOF
			DEVICE  = ${device}
			PANEL   = 0
			TEXT    = s/22/1/1/hw
			CONTUR  = 2
			MAP = 1
			CLEAR   = yes
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

			HLSYM   = l/22/3/hw
			CLRBAR  = 0
			WIND    = 0
			REFVEC  =

			EOF

		WrottenZERO=0
		grids=${memberlist}
		line_count=2
		color_number=9
		for gridl in ${grids}; do
			# ----- gridl -----
			gdfn=${gridl}

			if [ ${gdfn} == c00 ]; then
				color_number=2
				sline_count="-1"
				sCNTL="(CNTL)"
			else
				color_number=$(echo $gdfn | cut -c2-)
				line_count=$color_number
			   
				sline_count="+${line_count}"
				sCNTL=""

				#let line_count=$line_count+1
			fi

			if [ -e ${gdfn} ]; then
				cat >> cmdfilemar_low  <<- EOF
					GDFILE  = ${gdfn}
					HILO    = ${color_number}/L${num}/900-1016/5/50/y
					TITLE   = ${color_number}/${sline_count}/~ ? ${gdfn} ${sCNTL} |~${metaarea} ${metashname}
					GDATTIM = F${fcsthr}
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

		# ----- gfs -----
		gdfn=gfs
		if [ -e ${gdfn} ]; then
			cat >> cmdfilemar_low  <<- EOF
				GDFILE  = ${gdfn}
				HILO    = 3/L${num}/900-1016/5/50/y
				TITLE   = 3/-3/~ ? ${gdfn} 12Z YEST|~${metaarea} ${metashname}
				GDATTIM = F${fcsthrsgfs}
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

		# ----- ecmwf -----
		gdfn=ecmwf
		if [ -e ${gdfn} ]; then
			cat >> cmdfilemar_low  <<- EOF
				GDFILE  = ${gdfn}
				HILO    = 31/L${num}/900-1016/5/50/y
				TITLE   = 31/-5/~ ? ${gdfn} 12Z YEST|~${metaarea} ${metashname}
				GDATTIM = F${fcsthrsgfs}
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

		# ----- ukmet -----
		gdfn=ukmet
		if [ -e ${gdfn} ]; then
			cat >> cmdfilemar_low  <<- EOF
				GDFILE  = ${gdfn}
				HILO    = 26/L${num}/900-1016/5/50/y
				TITLE   = 26/-6/~ ? ${gdfn} 00Z|~${metaarea} ${metashname}
				GDATTIM = F${fcsthr}
				run

				EOF

		fi

		cat cmdfilemar_low
		gdplot2_nc < cmdfilemar_low
		err=$?

		if [[ $err != 0 ]]; then
			echo "FATAL ERROR in $(basename $BASH_SOURCE): gdplot2_nc failed using cmdfilemar_low!"
			err_chk
			exit $err
		fi
	done

	#####################################################
	# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
	# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
	# FOR THIS CASE HERE.
	#####################################################

	ls -l ${metaname}
	err=$?

	if [[ $err != 0 ]]; then
		echo "FATAL ERROR in $(basename $BASH_SOURCE): metafile ${metaname} not created!"
		err_chk
		exit $err
	fi

	if [ $SENDCOM = "YES" ] ; then
		mv ${metaname} ${COMOUT}/$COMPONENT/gempak/meta/
		if [ $SENDDBN = "YES" ] ; then
			$DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/$COMPONENT/gempak/meta/${metaname}
		fi
	fi
done

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit 0

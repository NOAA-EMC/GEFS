#!/bin/sh
#
# Metafile Script : gefs_meta_qpf
#
# Log :
# J. Carr/HPC    12/12/2003    Moved over from IBM.
# Xianwu Xue/EMC 04/06/2020    Modified for GEFS v12
# Xianwu Xue/EMC 04/07/2020    Shorten the 12 & 24 HR PCPN FOR QPF 
#                               by assigning start hour=48, due to 
#                               500 frames for meta file
#
# Set Up Local Variables
#
echo "$(date -u) begin $(basename $BASH_SOURCE)"

set -xa
if [[ ${STRICT:-NO} == "YES" ]]; then
	# Turn on strict bash error checking
	set -eu
fi

echo "memberlist=$memberlist"

# datatypes.tbl uses COMIN, so have to update it locally
COMIN="$COMIN/$COMPONENT/gempak"

sGrid=${sGrid} #:-"_0p50"}

ddate=$(echo $PDY | cut -c3-8)
ddatem1=$(echo $PDYm1 | cut -c3-8)
if [ ${cyc} = "00" ];then
   cycm12=12
else
   cycm12=00
fi

if [ ${cyc} = "00" ]; then
	grids="gfs $memberlist ecmwf"
elif [ ${cyc} = "12" ]; then
	grids="gfs $memberlist"
fi

####
mkdir $DATA/gefs_meta_qpf_nam
cd $DATA/gefs_meta_qpf_nam

name2="500 & PMSL"
metaname="gefs${sGrid}_${PDY}_${cyc}_meta_nam"
device="nc | $metaname"
fcsthrs="036 048 060 072 084 096 108 120 132 144 156 168 180 192 204" # shorten making starthour from 024 to 036 due to 500 frame limit

for fcsthr in ${fcsthrs}; do
	for fn in $(echo $memberlist); do
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

	if [ ${cyc} = "00" ]; then
		fn=ecmwf
		rm -rf ${fn}
		if [ -r $COMINecmwf.${PDYm1}/gempak/ecmwf_hr_${PDYm1}${cycm12}f${fcsthr} ]; then
			ln -s $COMINecmwf.${PDYm1}/gempak/ecmwf_hr_${PDYm1}${cycm12}f${fcsthr} ${fn}
		fi
	fi

	for grid in ${grids}; do
		gdfn=$grid
		name="${grid} ${name2}"
		title="1/-1/~ ? ${name}|~${name}"

		if [ -e ${gdfn} ]; then

			cat > cmdfile_meta_nam <<- EOF 
				DEVICE  = ${device}
				PANEL   = 0
				TEXT    = 1/22/1/1/hw
				MAP     = 11 !0
				CLEAR   = yes
				GAREA   = 2;-139;27;-22
				PROJ    = STR/90;-105;0
				LATLON  = 11/10/1/1/20;20!0
				GLEVEL  = 0              !500
				GVCORD  = none           !pres
				SKIP    = 0
				SCALE   = 0              !-1
				GDPFUN  = sm5s(pmsl)     !sm9s(hght)
				TYPE    = c              !c
				CONTUR  = 2
				CINT    = 8              !6
				LINE    = 19/1/3/1       !2/2/3/1
				FINT    = 0
				FLINE   = 0
				HILO    = 19/H#;L#/1020-1070;900-1012 !0
				HLSYM   = 1;1//22;22/2;2/hw           !0
				CLRBAR  = 0

				GDFILE  = ${gdfn}
				LINE    = 19/1/3/1       !2/2/3/1
				TITLE   = ${title}
				GDATTIM = F${fcsthr}
				run

				EOF

			cat cmdfile_meta_nam

			gdplot2_nc < cmdfile_meta_nam
			err=$?

			if [[ $err != 0 ]]; then
				echo "FATAL ERROR in $(basename $BASH_SOURCE): gdplot2_nc failed using cmdfile_meta_nam!"
				err_chk
				exit $err
			fi

		fi
	done
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
	mv ${metaname} ${COMOUT}/$COMPONENT/gempak/meta/ #$metaname
	if [ $SENDDBN = "YES" ] ; then
		$DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/$COMPONENT/gempak/meta/$metaname
	fi
fi

####
if [ ${cyc} = "00" ]; then
	grids="gfs $memberlist" # 1) ecmwf can not generate qpf; 2) 500 frame limit
elif [ ${cyc} = "12" ]; then
	grids="gfs $memberlist"
fi

for area in us sam us12 us24; do
	# GENERATE 48 HR PCPN TOTALS FOR THE MEDIUM RANGE FORECASTER.
	if [ ${area} = "us" ]; then
		mkdir $DATA/gefs_meta_qpf_us
		cd $DATA/gefs_meta_qpf_us
		cp $FIXgempak/datatype${sGrid}.tbl datatype.tbl

		metaname="gefs${sGrid}_${PDY}_${cyc}_meta_qpf_medr"
		device="nc | $metaname"

		if [ ${cyc} = "00" ]; then
			gdattim="f132"
			#fcsthrs="132"
		else
			gdattim="f120"
			#fcsthrs="120"
		fi

		garea="us"
		proj=" "
		glevel="0"
		gvcord="none"
		gdpfun="p48i"
		type="f"
		contur="1"
		cint="0"
		line="0"
		fint=".1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9"
		fline="0;22-30;14-20;5"
		hilo="31;0/x#2/.03-20/50;50//y"
		hlsym="1//22/2/hw"
		scale="0"
		clrbar="1"
		name2="48-HR PCPN"
		fcmdfile=cmdfile_meta_qpf_medr

	# GENERATE 24 HR PCPN TOTALS FOR QPF.
	elif [ ${area} = "us24" ]; then
		mkdir $DATA/gefs_meta_qpf_us24
		cd $DATA/gefs_meta_qpf_us24
		cp $FIXgempak/datatype${sGrid}.tbl datatype.tbl

		metaname="gefs${sGrid}_${PDY}_${cyc}_meta_qpf_us24"
		device="nc | $metaname"

		gdattim="f48-f216-12" # Change the start hour from 24 to 48 due to the 500 frames limit for meta file
		#fcsthrs="024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216"
		garea="us"
		proj=" "
		glevel="0"
		gvcord="none"
		gdpfun="p24i"
		type="f"
		contur="1"
		cint="0"
		line="0"
		fint=".1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9"
		fline="0;22-30;14-20;5"
		hilo="31;0/x#2/.03-20/50;50//y"
		hlsym="1//22/2/hw"
		scale="0"
		clrbar="1"
		name2="24-HR PCPN"
		fcmdfile=cmdfile_meta_qpf_us24

	# GENERATE 12 HR PCPN TOTALS FOR QPF.
	elif [ ${area} = "us12" ]; then
		mkdir $DATA/gefs_meta_qpf_us12
		cd $DATA/gefs_meta_qpf_us12
		cp $FIXgempak/datatype${sGrid}.tbl datatype.tbl

		metaname="gefs${sGrid}_${PDY}_${cyc}_meta_qpf_us12"
		device="nc | $metaname"

		gdattim="f48-f216-12" # Change the start hour from 12 to 48 due to the 500 frames limit for meta file
		#fcsthrs="012 024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216"
		garea="us"
		proj=" "
		glevel="0"
		gvcord="none"
		gdpfun="p12i"
		type="f"
		contur="1"
		cint="0"
		line="0"
		fint=".1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9"
		fline="0;22-30;14-20;5"
		hilo="31;0/x#2/.03-20/50;50//y"
		hlsym="1//22/2/hw"
		scale="0"
		clrbar="1"
		name2="12-HR PCPN"
		fcmdfile=cmdfile_meta_qpf_us12

	# GENERATE 24 HR PCPN TOTALS FOR SAM QPF.
	elif [ ${area} = "sam" ]; then
		mkdir $DATA/gefs_meta_qpf_sam
		cd $DATA/gefs_meta_qpf_sam
		cp $FIXgempak/datatype${sGrid}.tbl datatype.tbl

		metaname="gefs${sGrid}_${PDY}_${cyc}_meta_samqpf"
		device="nc | $metaname"

		if [ ${cyc} = "00" ]; then
			gdattim="f24-f144-12"
			#fcsthrs="024 036 048 060 072 084 096 108 120 132 144"
		else
			gdattim="f24-f144-12"
			#fcsthrs="024 036 048 060 072 084 096 108 120 132 144"
		fi
		garea="-28.2;-140.5;14.1;-32.6"
		proj="str/-85;-70;0"
		glevel="0"
		gvcord="none"
		gdpfun="p24m"
		type="f"
		contur="1"
		cint="0"
		line="0"
		fint="1;5;10;15;20;25;30;35;40;45;50;55;60;65;70;75;80;85"
		fline="0;21-30;14-20;5"
		hilo="31;0/x#/10-500/50;50//y"
		hlsym="1"
		scale="0"
		clrbar="1"
		name2="24-HR PCPN"
		fcmdfile=cmdfile_meta_samqpf
	fi
	
	ln -s $COMIN/ge*${sGrid}_${PDY}${cyc}f* ./
	ln -s $COMINsgfs/gfs.${PDY}/${cyc}/gempak/gfs${sGrid}_${PDY}${cyc}f* ./
	#ln -s $COMINecmwf.${PDYm1}/gempak/ecmwf_hr_${PDYm1}${cycm12}f* ./
	#ln -s $COMINecmwf.${PDY}/gempak/ecmwf_hr_${PDY}${cycm12}f* ./
	for grid in ${grids}; do
		grid=$(echo $grid | tr [a-z] [A-Z])
		name="${grid} ${name2}"
		title="1/0/~ ? ${name}|~${name}"
		#grid=$(echo $grid | tr [a-z] [A-Z])
		if [ ${grid} = "GFS" ]; then
			GDFILE="F-GFS | ${ddate}/${cyc}00"
			COMINtmp=$COMIN
			# #export COMIN=$COMINsgfs/gfs.${PDY}/${cyc}/gempak
			export COMIN=./
		elif [ ${grid} = "ECMWF" ]; then
			if [ $cyc = "12" ]; then
				COMINtmp=$COMIN
				# #export COMIN=$COMINecmwf.${PDY}/gempak
				export COMIN=./
				GDFILE="F-ECMWF | ${ddate}/${cycm12}00"
			else
				COMINtmp=$COMIN
				# #export COMIN=$COMINecmwf.${PDYm1}/gempak
				export COMIN=./
				GDFILE="F-ECMWF | ${ddatem1}/${cycm12}00"
			fi
			if [ ${area} = "us" ]; then
				gdattim="f144"
			fi
		else
			GDFILE="F-GEFS$grid | ${ddate}/${cyc}00"
			COMINtmp=$COMIN
			export COMIN=./
		fi

		cat > $fcmdfile  <<- EOF 
			DEVICE  = ${device}
			PANEL   = 0
			TEXT    = 1/22/1/1/hw
			MAP     = 11!0
			CLEAR   = yes
			GAREA   = ${garea}
			PROJ    = ${proj}
			LATLON  = 11/10/1/1/20;20!0

			GLEVEL  = ${glevel}
			GVCORD  = ${gvcord}
			SKIP    = 0
			SCALE   = ${scale}
			GDPFUN  = ${gdpfun}
			TYPE    = ${type}
			CONTUR  = ${contur}
			CINT    = ${cint}
			LINE    = ${line}
			FINT    = ${fint}
			FLINE   = ${fline}
			HILO    = ${hilo}
			HLSYM   = ${hlsym}
			CLRBAR  = ${clrbar}
			WIND    = 0
			REFVEC  =

			GDFILE  = ${GDFILE}
			GDATTIM = ${gdattim}
			LINE    = ${line}
			TITLE   = ${title}
			run

			EOF

		cat $fcmdfile

		gdplot2_nc < $fcmdfile
		err=$?

		if [[ $err != 0 ]]; then
			echo "FATAL ERROR in $(basename $BASH_SOURCE): gdplot2_nc failed using $fcmdfile!"
			err_chk
			exit $err
		fi
		
		# export COMIN=$COMINtmp
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
		mv ${metaname} ${COMOUT}/$COMPONENT/gempak/meta/$metaname
		if [ $SENDDBN = "YES" ] ; then
			$DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/$COMPONENT/gempak/meta/$metaname
		fi
	fi

done

echo "$(date -u) end $(basename $BASH_SOURCE)"

exit 0

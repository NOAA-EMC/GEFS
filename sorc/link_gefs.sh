#!/bin/ksh
set -ex

#--make symbolic links for EMC installation and hardcopies for NCO delivery

while getopts e:m: option
do
    case "${option}"
    in
        e) RUN_ENVIR=${OPTARG};;
        m) machine=${OPTARG};;

    esac

done

RUN_ENVIR=${RUN_ENVIR:-emc}
machine=${machine:-dell}

echo $RUN_ENVIR
echo $machine

LINK="ln -fs"
[[ $RUN_ENVIR = nco ]] && LINK="cp -rp"

pwd=$(pwd -P)

#------------------------------
#--model fix fields
#------------------------------
if [ $machine == "cray" ]; then
    FIX_DIR="/gpfs/hps3/emc/ensemble/noscrub/emc.enspara/common/git/fv3gefs/fix_20200927"
    FIX_DIR_FV3="/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "dell" ]; then
    FIX_DIR="/gpfs/dell2/emc/verification/noscrub/emc.enspara/common/git/fv3gefs/fix_20200927"
    FIX_DIR_FV3="/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "hera" ]; then
    FIX_DIR="/scratch2/NCEPDEV/ensemble/noscrub/common/git/fv3gefs/fix_20200927"
    FIX_DIR_FV3="/scratch1/NCEPDEV/global/glopara/fix"
fi

# Delete Fix folder and relink/recopy it
cd ${pwd}/../fix
for dir in fix_gefs fix_wave fix_emission; do
    if [[ -d $dir ]]; then
        echo "Fix folder exists, deleting it..."
        rm -rf $dir
    fi
    $LINK $FIX_DIR/$dir $dir
done
cd ${pwd}

if [[ -d global-workflow.fd ]] ; then
    cd ${pwd}/../fix

    for gw_dir in fix_am fix_fv3_gmted2010/C384 fix_chem; do
        if [[ -d $gw_dir ]]; then rm -Rf $gw_dir; fi
        mkdir -p $(dirname $gw_dir)
        $LINK $FIX_DIR_FV3/$gw_dir $gw_dir
    done

    # product
    sFolder=product
    if [[ -d $sFolder ]]; then
        rm -rf $sFolder
    fi
    $LINK ${pwd}/../sorc/global-workflow.fd/fix/${sFolder} ${sFolder}  


    cd ${pwd}
fi

# global-workflow
#cd $pwd
#if [[ -d global-workflow.fd ]] ; then
#    if [[ ! -L global-workflow.fd ]] ; then
#        echo "not link"
#        cd global-workflow.fd/sorc
#        #./link_fv3gfs.sh $RUN_ENVIR $machine
#        cd ../../
#    fi
#fi

# copy/link exec files
cd $pwd
if [[ -d global-workflow.fd ]] ; then
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/exec/nemsio_read ../exec/
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/exec/nemsio_get ../exec/
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/exec/global_chgres ../exec/

    sPath=../sorc/global-workflow.fd/sorc/fv3gfs.fd/WW3/model/exe
    for sFile in ${sPath}/ww3_*
    do
        echo $sFile
    done
    $LINK ${sPath}/ww3_* ../exec/

    sPath=../sorc/global-workflow.fd/sorc/fv3gfs.fd/NEMS/exe
    $LINK ${sPath}/global_fv3gfs.* ../exec/

    sPath=../sorc/global-workflow.fd/sorc/gfs_post.fd/exec
    $LINK ${sPath}/ncep_post ../exec/gfs_ncep_post

    sPath=../sorc/global-workflow.fd/sorc/gsi.fd/exec
    $LINK ${sPath}/getsigensmeanp_smooth.* ../exec/
    $LINK ${sPath}/getsfcensmeanp.* ../exec/

    sPath=../sorc/global-workflow.fd/exec
    $LINK ${sPath}/gfs_bufr ../exec/
    $LINK ${sPath}/tocsbufr ../exec/

    # chem_prep_emissions
    sPath=../sorc/global-workflow.fd/sorc/gsd_prep_chem.fd/workflow/emc-global/exec
    $LINK ${sPath}/prep_chem_sources_RADM_FV3_SIMPLE.exe ../exec/

fi

# Copy/Link parm files
cd $pwd
if [[ -d global-workflow.fd ]] ; then
    if [[ -d ../parm/parm_fv3diag ]]; then
        rm -rf ../parm/parm_fv3diag
    fi
    $LINK ../sorc/global-workflow.fd/parm/parm_fv3diag ../parm/

    if [[ -d ../parm/post ]]; then
        rm -rf ../parm/post
    fi
    $LINK ../sorc/global-workflow.fd/sorc/gfs_post.fd/parm ../parm/post

    if [[ -d ../parm/product ]]; then
        rm -rf ../parm/product
    fi
    $LINK ../sorc/global-workflow.fd/parm/product ../parm/
fi

# Copy/Link ush files
cd $pwd
if [[ -d global-workflow.fd ]] ; then
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/ush/global_chgres_driver.sh ../ush/
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/ush/global_chgres.sh ../ush/

    $LINK ../sorc/global-workflow.fd/sorc/gfs_post.fd/ush/gfs_nceppost.sh ../ush/

fi

# For Forecast
cd $pwd
if [[ -d global-workflow.fd ]] ; then
    sFile=exglobal_fcst_nemsfv3gfs.sh
    if [[ -e ../scripts/$sFile ]]; then
        if [[ -L ../scripts/$sFile ]]; then
            rm ../scripts/$sFile
            $LINK ../sorc/global-workflow.fd/scripts/$sFile ../scripts/
        fi
    else
        $LINK ../sorc/global-workflow.fd/scripts/$sFile ../scripts/
    fi
fi

# For wave
echo $pwd
cd $pwd
if [[ -d global-workflow.fd ]]; then
    lScripts="exwave_init.sh exwave_nawips.sh exwave_post_sbs.sh exwave_prep.sh exwave_stat.sh"
    for sFile in $lScripts; do
        $LINK ../sorc/global-workflow.fd/scripts/$sFile ../scripts/
    done

    lUsh="wave_ens_bull.sh wave_ens_stat.sh wave_grib2_sbs.sh wave_grid_interp_sbs.sh wave_grid_moddef.sh wave_outp_spec.sh wave_prnc_cur.sh wave_prnc_ice.sh wave_tar.sh"
    for sFile in $lUsh; do
        $LINK ../sorc/global-workflow.fd/ush/$sFile ../ush/
    done

    if [[ -e ../env ]]; then
        if [[ -L ../env ]]; then
            rm ../env
            $LINK ${pwd}/global-workflow.fd/env ../
        fi
    else
        $LINK ${pwd}/global-workflow.fd/env ../
    fi
fi

# for CHEM
if [[ -d global-workflow.fd ]]; then
    # for chem_prep_emissions
    $LINK ../sorc/global-workflow.fd/sorc/gsd_prep_chem.fd/workflow/emc-global/scripts/exglobal_prep_chem.sh ../scripts/
    $LINK ../sorc/global-workflow.fd/sorc/gsd_prep_chem.fd/workflow/emc-global/parm/prep_chem_sources.inp.IN ../parm/

    # for init_aerosol
    $LINK ../sorc/global-workflow.fd/ush/merge_fv3_chem_tile.py ../ush/
    $LINK ../sorc/global-workflow.fd/exec/chgres_recenter.exe ../exec/
    $LINK ../sorc/global-workflow.fd/sorc/gsi.fd/exec/calc_increment_ens_gsdchem.x ../exec/
fi


exit 0

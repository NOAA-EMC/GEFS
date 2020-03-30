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
    FIX_DIR="/gpfs/hps3/emc/ensemble/noscrub/emc.enspara/common/git/fv3gefs/fix_sst721_short"
    FIX_DIR_FV3="/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "dell" ]; then
    FIX_DIR="/gpfs/dell2/emc/verification/noscrub/emc.enspara/common/git/fv3gefs/fix_sst721_short"
    FIX_DIR_FV3="/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix"
elif [ $machine = "hera" ]; then
    FIX_DIR="/scratch2/NCEPDEV/ensemble/noscrub/common/git/fv3gefs/fix_sst721_short"
    FIX_DIR_FV3="/scratch1/NCEPDEV/global/glopara/fix"
fi

# Delete Fix folder and relink/recopy it
cd ${pwd}/../fix
if [[ -d fix_gefs ]]; then
    echo "Fix folder exists, deleting it..."
    rm -rf fix_gefs
fi
$LINK $FIX_DIR fix_gefs
cd ${pwd}

cd ${pwd}/../fix
# fix_wave
sFolder=fix_wave
if [[ -d $sFolder ]]; then
    rm -rf $sFolder
fi
$LINK ${pwd}/../${sFolder} ${sFolder}
cd ${pwd}


if [[ -d global-workflow.fd ]] ; then
    cd ${pwd}/../fix
    # fix_am
    sFolder=fix_am
    if [[ -d $sFolder ]]; then
         rm -rf $sFolder
    fi
    $LINK $FIX_DIR_FV3/$sFolder $sFolder

    # fix_fv3_gmted2010/C384
    sFolder=fix_fv3_gmted2010/C384
    if [[ -d $sFolder ]]; then
        rm -rf $sFolder
    fi
    sFolder2=fix_fv3_gmted2010
    if [[ ! -d $sFolder2 ]]; then
         mkdir $sFolder2
    fi
    $LINK $FIX_DIR_FV3/$sFolder $sFolder

    # product
    sFolder=product
    if [[ -d $sFolder ]]; then
        rm -rf $sFolder
    fi
    $LINK ${pwd}/../sorc/global-workflow.fd/fix/${sFolder} ${sFolder}

    # chem_prep_emissions
    sFolder=fix_chem
    if [[ -d $sFolder ]]; then
        rm -rf $sFolder
    fi
    $LINK ${FIX_DIR_FV3}/${sFolder} ${sFolder}

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
if [[ -d global-workflow.fd ]] ; then
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/ush/global_chgres_driver.sh ../ush/
    $LINK ../sorc/global-workflow.fd/sorc/ufs_utils.fd/ush/global_chgres.sh ../ush/

    $LINK ../sorc/global-workflow.fd/sorc/gfs_post.fd/ush/gfs_nceppost.sh ../ush/

fi

# For Forecast
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

# for CHEM
if [[ -d global-workflow.fd ]]; then
    # for chem_prep_emissions
    $LINK ../sorc/global-workflow.fd/sorc/gsd_prep_chem.fd/workflow/emc-global/scripts/exglobal_prep_chem.bash ../scripts/
    $LINK ../sorc/global-workflow.fd/sorc/gsd_prep_chem.fd/workflow/emc-global/parm/prep_chem_sources.inp.IN ../parm/

    # for init_aerosol
    $LINK ../sorc/global-workflow.fd/ush/merge_fv3_chem_tile.py ../ush/
    $LINK ../sorc/global-workflow.fd/exec/chgres_recenter.exe ../exec/
    $LINK ../sorc/global-workflow.fd/sorc/gsi.fd/exec/calc_increment_ens_gsdchem.x ../exec/
fi


exit 0

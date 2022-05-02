def create_crontab(dicBase, OnlyForTest=False, cronint=5):
    '''
        Create crontab to execute rocotorun every cronint (5) minutes
    '''
    ##########################################################
    # Generate crotab file based on different computers.
    #
    # Inputs (via environment variables):
    #         dicBase         : The base configuration from user config file
    #         OnlyForTest     : For test the rocoto development
    #         cronint         : How much minutes you want to execute rocotorun
    #
    # Outputs:
    #         create a crontab file
    #
    # Error Codes:
    #         -501 : rocotorun is not found.
    #         -502 : WHERE_AM_I is not in the list
    #
    ##########################################################

    # No point creating a crontab if rocotorun is not available.
    import sys
    from distutils.spawn import find_executable

    if OnlyForTest:
        rocotoruncmd = "/opt/modules/3.2.10.3/init/sh"
    else:
        rocotoruncmd = find_executable('rocotorun')
        if rocotoruncmd is None:
            print('Failed to find rocotorun, crontab will not be created')
            sys.exit(-501)
            return

    sRocotoPath = dicBase['GEFS_ROCOTO'.upper()]
    sCrontab_FileName = dicBase['crontab'.upper()]
    sXML_FileName = dicBase['XML'.upper()]
    sDB_FileName = dicBase['DB'.upper()]

    # print(sRocotoPath + "/" + sCrontab_FileName)
    crontab_string = '# This is a basic crontab file to use with given settings ' \
                     'that will execute rocotorun every {0} minutes\n'.format(cronint)
    crontab_usage = '# Usage: crontab ' + dicBase['crontab'.upper()] + '\n'
    crontab_usage += '#        crontab -l   lists current crontab\n'
    crontab_usage += '#        crontab -r   removes current crontab\n'
    crontab_time = '*/{0} * * * * '.format(cronint)

    system = dicBase["WHERE_AM_I"]

    rocotorun_args = rocotoruncmd + ' -d ' + sRocotoPath + '/' + sDB_FileName \
                     + ' -w ' + sRocotoPath + '/' + sXML_FileName
    if system == 'wins' or system == 'hera':
        crontab_string += crontab_usage
        crontab_string += crontab_time + rocotorun_args
    elif system == "cray":
        crontab_string += crontab_usage
        crontab_string += crontab_time
        crontab_string += ' (. /opt/modules/3.2.10.3/init/sh; ' \
                          'module use /usrx/local/emc_rocoto/modulefiles ; ' \
                          'module load xt-lsfhpc; module load rocoto;'
        crontab_string += rocotorun_args
        crontab_string += ') > /dev/null 2>&1'
    elif system in ["wcoss_dell_p3", "wcoss_dell_p35"]:
        # crontab_string += crontab_usage
        import os
        if not os.path.exists(sRocotoPath + "/logs"):
            os.mkdir(sRocotoPath + "/logs")
        sBashFile = sRocotoPath + "/logs/crontab.sh"
        sLogFile = sRocotoPath + "/logs/crontab.log"

        bash_file = open(sBashFile, 'w')
        bash_file.write("#!/bin/ksh --login\n")
        bash_file.write("set -x\n")

        bash_file.write("\n")
        bash_file.write(". $MODULESHOME/init/bash                          2>/dev/null\n")
        bash_file.write("module load lsf/10.1                              2>/dev/null\n")
        bash_file.write("module use /usrx/local/dev/emc_rocoto/modulefiles 2>/dev/null\n")
        bash_file.write("module load ruby/2.5.1 rocoto/complete            2>/dev/null\n")
        bash_file.write("\n")
        bash_file.write(rocotorun_args + "\n")
        bash_file.write("\n")
        bash_file.write("date\n")
        bash_file.close()
        os.chmod(sBashFile, 0o755)
        crontab_string = ""
        crontab_string += crontab_time
        # crontab_string += ' (. /usrx/local/prod/lmod/lmod/init/sh; ' \
        #                  'module use /gpfs/dell3/usrx/local/dev/emc_rocoto/modulefiles; ' \
        #                  'module load lsf/10.1; module load ruby/2.5.1 rocoto/complete;'
        # crontab_string += rocotorun_args
        # crontab_string += ') 1>>{0} 2>&1'.format(sRocotoPath + "/logs/crontab.log")
        crontab_string += sBashFile
        crontab_string += ' 1>>{0} 2>&1'.format(sRocotoPath + "/logs/crontab.log")
    elif system == 'wcoss2':
        crontab_string += crontab_usage
        crontab_string += crontab_time + rocotorun_args
    else:
        print("CRITICAL ERROR: auto-crontab file generation for %s still needs to be implemented" % system)
        sys.exit(-502)

    crontab_string += "\n"
    crontab_file = open(sRocotoPath + "/" + sCrontab_FileName, 'w')
    crontab_file.write(crontab_string)
    crontab_file.close()

    return

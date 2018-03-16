def create_crontab(dicBase, g_OnlyForTest=False, cronint=5):
    '''
        Create crontab to execute rocotorun every cronint (5) minutes
    '''

    # No point creating a crontab if rocotorun is not available.
    from distutils.spawn import find_executable

    if g_OnlyForTest:
        rocotoruncmd = "/opt/modules/3.2.10.3/init/sh"
    else:
        rocotoruncmd = find_executable('rocotorun')
        if rocotoruncmd is None:
            print('Failed to find rocotorun, crontab will not be created')
            return

    sRocotoPath = dicBase['GEFS_ROCOTO'.upper()]
    sCrontab_FileName = dicBase['crontab'.upper()]
    sXML_FileName = dicBase['XML'.upper()]
    sDB_FileName = dicBase['DB'.upper()]
    # print(sXML_FileName)
    # print(sDB_FileName)
    # exit()

    # print(sRocotoPath + "/" + sCrontab_FileName)
    crontab_string = '# This is a basic crontab file to use with given settings that will execute rocotorun every {0} minutes\n'.format(
        cronint)
    crontab_usage = '# Usage: crontab ' + dicBase['crontab'.upper()] + '\n'
    crontab_usage += '#        crontab -l   lists current crontab\n'
    crontab_usage += '#        crontab -r   removes current crontab\n'
    crontab_time = '*/{0} * * * * '.format(cronint)

    system = dicBase["WHERE_AM_I"]

    rocotorun_args = rocotoruncmd + ' -d ' + sRocotoPath + '/' + sDB_FileName \
                     + ' -w ' + sRocotoPath + '/' + sXML_FileName
    if system == 'theia' or system =='wins':
        crontab_string += crontab_usage
        crontab_string += crontab_time + rocotorun_args
    elif system == 'gyre' or system == 'tide':

        head = system[0];
        hosts = ''
        for host in ('10a1', '10a2', '14a1', '14a2'):
            hosts += head + host + ' '
        crontab_string += '# When on ' + system + ' you can only run cron on the hosts: ' + hosts + '\n'
        crontab_string += crontab_usage
        crontab_string += crontab_time + '(. /usrx/local/Modules/default/init/sh; module load lsf; module use /usrx/local/emc_rocoto/modulefiles ; module load rocoto ;' + rocotorun_args + ') > /dev/null 2>&1\n'
    elif system == 'luna' or system == 'surge':
        crontab_string += crontab_usage
        crontab_string += crontab_time + '(. /opt/modules/3.2.10.3/init/sh; module use /usrx/local/emc_rocoto/modulefiles ; module load xt-lsfhpc; module load rocoto;' + rocotorun_args + ') > /dev/null 2>&1\n'
    elif system == "cray":
        crontab_string += crontab_usage
        crontab_string += crontab_time
        crontab_string += ' (. /opt/modules/3.2.10.3/init/sh; module use /usrx/local/emc_rocoto/modulefiles ; module load xt-lsfhpc; module load rocoto;'
        crontab_string += rocotorun_args
        crontab_string += ') > /dev/null 2>&1'


    else:
        print("CRITICAL ERROR: auto-crontab file generation for %s still needs to be implemented" % system)
        import sys
        sys.exit(0)

    crontab_string += "\n"
    crontab_file = open(sRocotoPath + "/" + sCrontab_FileName, 'w')
    crontab_file.write(crontab_string)
    crontab_file.close()

    return


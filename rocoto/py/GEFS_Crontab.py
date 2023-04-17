#!/usr/bin/env python3

import sys
import os
from distutils.spawn import find_executable

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

    sXML_File = os.path.join(sRocotoPath, sXML_FileName)
    sDB_File = os.path.join(sRocotoPath, sDB_FileName)
    rocotorun_args = f"{rocotoruncmd} -d f{sDB_File} -w f{sXML_File}"
    if system in ['wcoss2', 'hera', 'wins']:
        crontab_string += crontab_usage
        crontab_string += crontab_time + rocotorun_args
    else:
        print(f"CRITICAL ERROR: auto-crontab file generation for {system} still needs to be implemented")
        sys.exit(-502)

    crontab_string += "\n"
    sFile = os.path.join(sRocotoPath, sCrontab_FileName)
    crontab_file = open(sFile, 'w')
    crontab_file.write(crontab_string)
    crontab_file.close()

    return

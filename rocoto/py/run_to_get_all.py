#!/usr/bin/env python

## new rocoto workflow
## history:
##   03/19/2018 first released version by Xianwu Xue
##   03/22/2018 Revised to use the default config file based on WHERE_AM_ID by Xianwu Xue

g_OnlyForTest = False
g_Rocoto_ForTest = ""

import GEFS_UserConfig as gefs_config
import GEFS_XML as gefs_xml
import GEFS_XML_For_Tasks as gefs_xml_for_tasks
import GEFS_Parm as gefs_parm
#import GEFS_Bin as gefs_bin
import GEFS_Crontab as gefs_crontab

def main():
    import os, sys
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    print("--Starting to generate all files you need!")

    print("--Getting user config file!")
    sConfig, sRocoto_WS = gefs_config.get_config_file(OnlyForTest=g_OnlyForTest)

    g_Rocoto_ForTest = sRocoto_WS

    print("--Reading user config file...")

    dicBase = gefs_config.read_config(sConfig)

    print("--Checking the must parameters for the config file")
    sMust_Items = ['SDATE', 'EDATE']
    for sMust_Item in sMust_Items:
        if sMust_Item not in dicBase:
            print("You need assign value of {0}".format(sMust_Item))
            exit(-1)

    # Get the default value
    print("--Getting default values from default user config file!")
    gefs_config.get_and_merge_default_config(dicBase)

    # Only for test
    if g_OnlyForTest:
        dicBase["GEFS_ROCOTO"] = g_Rocoto_ForTest
        dicBase["WORKDIR"] = g_Rocoto_ForTest

    # Automatically turns on coupled forecast if wave prep is run
    #   unless it has already been defined
    if not 'cplwav' in dicBase:
        if dicBase['RUN_WAVE_PREP'].upper()[0] == "Y" or gefs_xml_for_tasks.DoesTaskExist(dicBase, "wave_prep"):
            dicBase['cplwav'] = ".true."
        else:
            dicBase['cplwav'] = ".false."

    print("--Assign default values for the config file")
    gefs_xml.assign_default_for_xml_def(dicBase, sRocoto_WS=sRocoto_WS)

    print("--Create folders for the output...")
    gefs_config.create_folders(dicBase)

    print("--Generating XML file ...")
    gefs_xml.create_xml(dicBase)
    print("--Generated XML file!")

    sVarName = "GenParm".upper()
    if sVarName in dicBase:
        sValue = dicBase[sVarName]
        if sValue == 'YES' or sValue[0] == 'Y':
            # check gets_dev_parm items in configure file
            print("--Generating files for parm...")
            gefs_parm.create_parm(sConfig, dicBase)
            print("--Generated files for parm!")

    print("--Generating crontab file...")
    gefs_crontab.create_crontab(dicBase, cronint=5, OnlyForTest=g_OnlyForTest)
    print("--Generated crotab file!")


if __name__ == '__main__':
    import sys

    main()

    print("--Done to generate all files!")
    sys.exit(0)

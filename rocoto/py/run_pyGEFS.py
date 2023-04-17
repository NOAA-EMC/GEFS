#!/usr/bin/env python

## new rocoto workflow
## history:
##   03/19/2018 first released version by Xianwu Xue
##   03/22/2018 Revised to use the default config file based on WHERE_AM_ID by Xianwu Xue

import sys
import argparse
import GEFS_UserConfig as gefs_config
import GEFS_XML as gefs_xml
import GEFS_XML_For_Tasks as gefs_xml_for_tasks
import GEFS_Parm as gefs_parm
import GEFS_Crontab as gefs_crontab

def main():
    parser = argparse.ArgumentParser(description='pyGEFS: Python-Based Workflow Management of NCEP Global Ensemble Forecast System!')
    parser.add_argument("-r", "--Rocoto", default="yes", type=str, help="Generate rocoto xml related files! [yes|no]")
    parser.add_argument("-o", "--Operation", default="no", type=str, help="Generate operation workflow related files! [rocoto|devecf|ecflow|no]")
    parser.add_argument("-f", "--ConfigFile", default="user_full.conf", type=str, help="User configure file name!")

    args = parser.parse_args()
    print(args)

    print("=========================================")

    print("--Starting to generate all files you need!")

    # Getting database from User Configuration File
    sConfig, sRocoto_WS, dicBase = gefs_config.get_dicBase_from_Config(sConfigFile=args.ConfigFile)
    
    # Checking MUST parameters
    print("--Checking the must parameters for the config file")
    sMust_Items = ['SDATE', 'EDATE']
    for sMust_Item in sMust_Items:
        if sMust_Item not in dicBase:
            print("You need assign value of {0}".format(sMust_Item))
            exit(-1)

    ifhmaxh = int(dicBase["fhmaxh".upper()])
    ifhmax = int(dicBase["fhmax".upper()])
    if ifhmaxh > ifhmax:
        print("FATAL Error: fhmax ({0}) must be larger than fhmaxh ({1})!".format(ifhmax, ifhmaxh))
        exit(-1)

    # Automatically turns on coupled forecast if wave prep is run
    #   unless it has already been defined
    if not 'cplwav' in dicBase:
        if dicBase['RUN_WAVE_PREP'].upper()[0] == "Y" or gefs_xml_for_tasks.DoesTaskExist(dicBase, "wave_prep"):
            dicBase['cplwav'] = ".true."
        else:
            dicBase['cplwav'] = ".false."

    if args.Rocoto.lower() == "yes":
        print("--Assign default values for the config file")
        gefs_xml.assign_default_for_xml_def(dicBase, sRocoto_WS=sRocoto_WS)

        print("--Create folders for the output...")
        gefs_config.create_folders(dicBase)

        print("--Generating XML file ...")
        gefs_xml.create_xml(dicBase)
        #print("--Generated XML file!")

        # For gefs_dev.parm
        gefs_parm.create_parm2(sConfig, dicBase)

        print("--Generating crontab file...")
        gefs_crontab.create_crontab(dicBase, cronint=5)

    

if __name__ == '__main__':
    main()

    print("--Done to generate all files!")
    sys.exit(0)

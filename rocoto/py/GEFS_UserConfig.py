#!/usr/bin/env python3

import os
import sys
import datetime
from collections import OrderedDict

# =======================================================
def get_and_merge_default_config(dicBase):
    # To get the WHERE_AM_I from dicBase or identify it using default methode
    get_WHERE_AM_I(dicBase)
    WHERE_AM_I = dicBase["WHERE_AM_I"]
    print("----*You are working on machine: {0}".format(WHERE_AM_I))
    sDefaultConfig_File =os.path.join(sys.path[0], f"user_{WHERE_AM_I}.conf")

    if os.path.exists(sDefaultConfig_File):
        print("----Default User Configure file was found! Reading ...")
        dicBase_Default = read_config(sDefaultConfig_File)

        print("----Merging ...")
        for sDic in dicBase_Default:
            if sDic not in dicBase:
                dicBase[sDic] = dicBase_Default[sDic]

# =======================================================
def get_config_file2(sConfigFile="user_full.conf"):
    sRocoto_WS = os.getcwd()
    sConfig = os.path.join(sRocoto_WS, sConfigFile)
    if not os.path.exists(sConfig):
        sRocoto_WS = os.path.join(os.getcwd(), "..")
        sConfig = os.path.join(sRocoto_WS, sConfigFile)
        if not os.path.exists(sConfig):
            print("Please check whether you have config file in your rocoto path!")
            sys.exit(-5)

    sRocoto_WS = os.path.abspath(sRocoto_WS)
    return sConfig, sRocoto_WS

# =======================================================
def read_config(sConfig):
    # read config file
    dicBase = OrderedDict()
    iTaskName_Num = 0
    with open(sConfig, "r") as f:
        for sLine in f:
            # print(sLine)
            sLine = sLine.strip()

            if len(sLine) != 0:
                if str(sLine).startswith("#"):
                    # print("This is the comment: {0}".format(sLine))
                    continue
                else:
                    # print(sLine)
                    a, b = sLine.split("=", 1)
                    b = b.split(" #", 1)[0]

                    a = str(a).strip()
                    b = str(b).strip()

                    if b.startswith('"'):
                        b = b.replace('"', "", 1)
                    if b.endswith('"'):
                        b = b[:-1]

                    if b.startswith("'"):
                        b = b.replace(",", "", 1)
                    if b.endswith(","):
                        b = b[:-1]

                    b = str(b).strip()

                    # To get the number of tasks from the config file
                    if a == "taskname":
                        iTaskName_Num += 1
                        sTaskName = "taskname_{0}".format(iTaskName_Num)
                        dicBase[sTaskName.upper()] = b
                    else:
                        dicBase[a.upper()] = b
                    # print(a)

    dicBase['taskname_num'.upper()] = iTaskName_Num

    return dicBase


# =======================================================
def create_folders(dicBase):
    EXPID = dicBase['EXPID']
    WORKDIR = str(dicBase['WORKDIR']).replace("&EXPID;", EXPID)
    WHERE_AM_I = dicBase["WHERE_AM_I".upper()]

    if not os.path.exists(WORKDIR):
        os.makedirs(WORKDIR)

    sPath = os.path.join(WORKDIR, "tmp")
    if not os.path.exists(sPath):
        os.makedirs(sPath)

    sPath = os.path.join(WORKDIR, "dev", "output")
    if not os.path.exists(sPath):
        os.makedirs(sPath)

    date1 = datetime.datetime.strptime(dicBase['SDATE'][0:8], "%Y%m%d")
    date2 = datetime.datetime.strptime(dicBase['EDATE'][0:8], "%Y%m%d")
    day = datetime.timedelta(days=1)

    while date1 <= date2:
        sPath1 = os.path.join(sPath, date1.strftime('%Y%m%d'))
        if not os.path.exists(sPath1):
            #print(sPath1)
            os.makedirs(sPath1)
        date1 = date1 + day

# =======================================================
def get_WHERE_AM_I(dicBase):
    sVarName = 'WHERE_AM_I'
    if sVarName not in dicBase:
        if os.path.exists('/scratch1/NCEPDEV'):
            dicBase[sVarName] = 'hera'
        elif os.path.exists('/apps/prod'):
            dicBase[sVarName] = 'wcoss2'
        else:
            print('workflow is currently only supported on: %s' % ' '.join('other'))
            raise NotImplementedError('Cannot auto-detect platform, ABORT!')
            
            
# =======================================================
def get_dicBase_from_Config(sConfigFile="user_full.conf"):

    print("--Getting database from User Configuration File: {0}".format(sConfigFile))
    
    # Read User Configuration File
    print("----Getting user config file!")
    sConfig, sRocoto_WS = get_config_file2(sConfigFile=sConfigFile)
    
    print("----Reading user config file...")
    dicBase = read_config(sConfig)
    
    # Get the default value
    print("----Getting default values from default user config file!")
    get_and_merge_default_config(dicBase)
    
    return sConfig, sRocoto_WS, dicBase
    

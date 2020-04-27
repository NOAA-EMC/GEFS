# =======================================================
def get_and_merge_default_config(dicBase):
    import os, sys
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    # To get the WHERE_AM_I from dicBase or identify it using default methode
    get_WHERE_AM_I(dicBase)
    WHERE_AM_I = dicBase["WHERE_AM_I"]
    print("----*You are working on machine: {0}".format(WHERE_AM_I))
    sDefaultConfig_File = sys.path[0] + sSep + "user_{0}.conf".format(WHERE_AM_I)

    if os.path.exists(sDefaultConfig_File):
        print("----Default User Configure file was found! Reading ...")
        dicBase_Default = read_config(sDefaultConfig_File)

        print("----Merging ...")
        for sDic in dicBase_Default:
            if sDic not in dicBase:
                dicBase[sDic] = dicBase_Default[sDic]


# =======================================================
def get_config_file(OnlyForTest=False):
    import os, sys

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    sRocoto_WS = os.getcwd()
    sConfig = ""  # "user_conf"
    if OnlyForTest:

        sRocoto_WS = os.getcwd() + sSep + '..'

        # sConfig = "user.conf"
        sConfig = sRocoto_WS + sSep + "user_full.conf"

        if not os.path.exists(sConfig):
            sRocoto_WS = os.getcwd()
            sConfig = sRocoto_WS + sSep + "user_full.conf"
    else:

        if len(sys.argv) == 2:
            sConfig = sys.argv[1]
        else:
            sConfig = "user.conf"

        if not os.path.exists(sConfig):
            sConfig = ".." + sSep + "user.conf"

            if not os.path.exists(sConfig):
                sConfig = "user_full.conf"

                if not os.path.exists(sConfig):
                    sConfig = ".." + sSep + "user_full.conf"

                    if not os.path.exists(sConfig):
                        print("Please check whether you have config file in your rocoto path!")
                        sys.exit(-5)
                    else:
                        sRocoto_WS = os.getcwd() + sSep + ".."

    sRocoto_WS = os.path.abspath(sRocoto_WS)
    return sConfig, sRocoto_WS


# =======================================================
def get_config_file2(sConfigFile="user_full.conf"):
    import os, sys

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    sRocoto_WS = os.getcwd()
    sConfig = sRocoto_WS + sSep + sConfigFile
    if not os.path.exists(sConfig):
        sRocoto_WS = os.getcwd() + sSep + ".."
        sConfig = sRocoto_WS + sSep + sConfigFile
        if not os.path.exists(sConfig):
            print("Please check whether you have config file in your rocoto path!")
            sys.exit(-5)

    sRocoto_WS = os.path.abspath(sRocoto_WS)
    return sConfig, sRocoto_WS


# =======================================================
def read_config(sConfig):
    # read config file
    from collections import OrderedDict
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
    import os, sys

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    EXPID = dicBase['EXPID']
    WORKDIR = str(dicBase['WORKDIR']).replace("&EXPID;", EXPID)

    if not os.path.exists(WORKDIR):
        os.makedirs(WORKDIR)

    sPath = WORKDIR + sSep + 'tmpnwprd'
    if not os.path.exists(sPath):
        os.makedirs(sPath)

    sPath = WORKDIR + '{0}com{0}output{0}dev{0}'.format(sSep)
    if not os.path.exists(sPath):
        os.makedirs(sPath)

    import datetime
    date1 = datetime.datetime.strptime(dicBase['SDATE'][0:8], "%Y%m%d")
    date2 = datetime.datetime.strptime(dicBase['EDATE'][0:8], "%Y%m%d")
    day = datetime.timedelta(days=1)

    while date1 <= date2:
        sPath1 = sPath + date1.strftime('%Y%m%d')
        if not os.path.exists(sPath1):
            os.makedirs(sPath1)
        date1 = date1 + day


# =======================================================
def get_WHERE_AM_I(dicBase):
    sVarName = 'WHERE_AM_I'
    import os
    
    #sCPath = os.getcwd()
    #if sCPath.startswith("/gpfs/dell6"):
    #    dicBase[sVarName] = 'wcoss_dell_p35'
    
    if sVarName not in dicBase:
        sCPath = os.getcwd()
        if sCPath.startswith("/gpfs/dell6"):
            dicBase[sVarName] = 'wcoss_dell_p35'
            return

        if os.path.exists('/scratch1/NCEPDEV'):
            dicBase[sVarName] = 'hera'
        elif os.path.exists('/gpfs') and os.path.exists('/etc/SuSE-release'):
            dicBase[sVarName] = 'cray'
        elif os.path.lexists('/usrx') and os.path.realpath('/usrx').startswith('/gpfs/dell'):
            dicBase[sVarName] = 'wcoss_dell_p3'
        elif os.path.exists('c:'):
            dicBase[sVarName] = 'wins'
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
    

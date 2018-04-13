#=======================================================
def get_and_merge_default_config(dicBase):
    import os, sys
    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    # To get the WHERE_AM_I from dicBase or identify it using default methode
    get_WHERE_AM_I(dicBase)
    WHERE_AM_I = dicBase["WHERE_AM_I"]
    sDefaultConfig_File = sys.path[0] + sSep + "user_new_{0}.conf".format(WHERE_AM_I)

    if os.path.exists(sDefaultConfig_File):
        print("---Default User Configure file was found! Reading ...")
        dicBase_Default = read_config(sDefaultConfig_File)

        print("---Merging ...")
        for sDic in dicBase_Default:
            if sDic not in dicBase:
                dicBase[sDic] = dicBase_Default[sDic]

#=======================================================
def get_config_file(OnlyForTest = False):
    import os, sys

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    sRocoto_WS = os.getcwd()
    sConfig = "" #"user_new.conf"
    if OnlyForTest:

        sRocoto_WS = os.getcwd() + sSep + '..'

        # sConfig = "user_new.conf"
        sConfig = sRocoto_WS + sSep + "user_new_full.conf"

        if not os.path.exists(sConfig):
            sRocoto_WS = os.getcwd()
            sConfig = sRocoto_WS + sSep + "user_new_full.conf"
    else:

        if len(sys.argv) == 2:
            sConfig = sys.argv[1]
        else:
            sConfig = "user_new.conf"

        if not os.path.exists(sConfig):
            sConfig = ".." + sSep + "user_new.conf"

            if not os.path.exists(sConfig):
                sConfig = "user_new_full.conf"

                if not os.path.exists(sConfig):
                    sConfig = ".." + sSep + "user_new_full.conf"

                    if not os.path.exists(sConfig):
                        print("Please check whether you have config file in your rocoto path!")
                        sys.exit(-5)
                    else:
                        sRocoto_WS = os.getcwd() + sSep + ".."

    sRocoto_WS = os.path.abspath(sRocoto_WS)
    return sConfig, sRocoto_WS

#=======================================================
def read_config(sConfig):
    # read config file
    from collections import OrderedDict
    dicBase = OrderedDict()
    iTaskName_Num = 0
    with open(sConfig, "r")as f:
        for sLine in f:
            # print(sLine)
            sLine = sLine.strip()

            if len(sLine) != 0:
                if str(sLine).startswith("#"):
                    # print("This is the comment: {0}".format(sLine))
                    continue
                else:
                    # print(sLine)
                    a, b = sLine.split("=",1)

                    a = str(a).strip()
                    b = str(b).strip()

                    if b.startswith('"'):
                        b = b.replace('"', "",1)
                    if b.endswith('"'):
                        b = b[:-1]

                    if b.startswith("'"):
                        b = b.replace(",", "",1)
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

#=======================================================
def create_folders(dicBase):
    import os, sys

    sSep = "/"
    if sys.platform == 'win32':
        sSep = r'\\'

    EXPID = dicBase['EXPID']
    WORKDIR = str(dicBase['WORKDIR']).replace("&EXPID;",EXPID)

    if not os.path.exists(WORKDIR):
        os.makedirs(WORKDIR)

    sWS_Out = WORKDIR # + sSep + EXPID
    if not os.path.exists(sWS_Out):
        os.mkdir(sWS_Out)

    sPath = sWS_Out + sSep + 'tmpnwprd'

    if not os.path.exists(sPath):
        os.mkdir(sPath)

    import datetime
    #
    dd = WORKDIR + sSep + 'tmpnwprd'
    if not os.path.exists(dd):
        os.mkdirs(dd)
    # cmd = "mkdir -p " + dd + "/tmpnwprd"
    # os.system(cmd)
    dd = WORKDIR + '{0}com{0}output{0}dev{0}'.format(sSep)
    if not os.path.exists(dd):
        os.makedirs(dd)

    pdy = dicBase['SDATE'][0:8]
    year = dicBase['SDATE'][0:4]
    month = dicBase['SDATE'][4:6]
    day = dicBase['SDATE'][6:8]

    date1 = datetime.datetime.strptime(dicBase['SDATE'][0:8], "%Y%m%d")
    date2 = datetime.datetime.strptime(dicBase['EDATE'][0:8], "%Y%m%d")
    day = datetime.timedelta(days=1)

    while date1 <= date2:
        d = date1.strftime('%Y%m%d')
        d1 = dd + d
        # cmd = "mkdir -p " + d1
        # os.system(cmd)
        if not os.path.exists(d1):
            os.makedirs(d1)
        date1 = date1 + day

#=======================================================
def get_WHERE_AM_I(dicBase):
    sVarName = 'WHERE_AM_I'
    import os
    if sVarName not in dicBase:
        if os.path.exists('/scratch4'):
            dicBase[sVarName] = 'theia'
        elif os.path.exists('/gpfs') and os.path.exists('/etc/SuSE-release'):
            dicBase[sVarName] = 'cray'
        #     machine = 'WCOSS_C'
        elif os.path.exists('c:'):
            dicBase[sVarName] = 'wins'
        else:
            print('workflow is currently only supported on: %s' % ' '.join('other'))
            raise NotImplementedError('Cannot auto-detect platform, ABORT!')


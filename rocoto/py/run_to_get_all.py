#!/usr/bin/env python



# import string

g_OnlyForTest = False

g_Rocoto_ForTest = ""



import  GEFS_Crontab as gefs_crontab

import GEFS_Parm as gefs_parm



from GEFS_XML import *



def main():



    print("--Starting to generate all files you need!")



    sConfig, sRocoto_WS = get_config_file(OnlyForTest=g_OnlyForTest)

    g_Rocoto_ForTest = sRocoto_WS





    # Below statements will be fix

    dicBase = {}



    # read config file

    get_config(sConfig, dicBase)



    # check the must parameters for the config file

    sMust_Items = ['SDATE', 'EDATE', 'First'.upper(), 'Last'.upper(), 'EXPID', 'HPS']

    for sMust_Item in sMust_Items:

        if sMust_Item not in dicBase:

            print("You need assign value of {0}".format(sMust_Item))

            exit(0)



    # Assign default values for the config'ed file

    assign_default_for_xml_def(dicBase,sRocoto_WS=sRocoto_WS)



    if g_OnlyForTest:

        dicBase["GEFS_ROCOTO"] = g_Rocoto_ForTest



    create_folders(dicBase)



    config_tasknames(dicBase)



    # if havexml:

    #     if not ask('ALERT! %s: XML file exists.  Overwrite (y/n)?' % (outxml,)):

    #         logger.error('%s: file exists, user does not want to overwrite.'

    #                      % (outxml,))

    #         sys.exit(1)

    #     else:

    #         logger.warning('%s: overwriting pre-existing XML file.' % (outxml,))

    #



    create_xml(dicBase)



    sVarName = "GenParm".upper()

    if sVarName in dicBase:

        sValue = dicBase[sVarName]

        if sValue == 'YES' or sValue[0] == 'Y':

            # check gets_dev_parm items in configure file

            gefs_parm.assign_default_for_gets_dev_parm(dicBase)

            gefs_parm.create_gets_dev_parm(dicBase)



    # #---

    # sVarName = "GenBinSH".upper()

    # if sVarName in dicBase:

    #     sValue = dicBase[sVarName]

    #     if sValue == 'YES' or sValue[0] == 'Y':

    #         # check gets_dev_parm items in configure file

    #         print("It is not done!")





    gefs_crontab.create_crontab(dicBase, cronint=5, g_OnlyForTest=g_OnlyForTest)



    # dd = os.environ['WORKDIR']

    # dd += "/" + os.environ['EXPID']

    # cmd = "mkdir -p " + dd + "/tmpnwprd"

    # os.system(cmd)

    # dd += '/com/output/dev/'



def create_folders(dicBase):

    import os, sys



    sSep = "/"

    if sys.platform == 'win32':

        sSep = r'\\'



    WORKDIR = dicBase['WORKDIR']



    if not os.path.exists(WORKDIR):

        os.mkdir(WORKDIR)



    EXPID = dicBase['EXPID']



    sWS_Out = WORKDIR + sSep + EXPID

    if not os.path.exists(sWS_Out):

        os.mkdir(sWS_Out)



    sPath = sWS_Out + sSep + 'tmpnwprd'



    if not os.path.exists(sPath):

        os.mkdir(sPath)



    import datetime

    #

    dd = WORKDIR

    dd += sSep + dicBase['EXPID'] + sSep + 'tmpnwprd'

    if not os.path.exists(dd):

        os.mkdir(dd)

    # cmd = "mkdir -p " + dd + "/tmpnwprd"

    # os.system(cmd)

    dd += '{0}com{0}output{0}dev{0}'.format(sSep)

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



def get_config_file(OnlyForTest = False):

    import os, sys



    sSep = "/"

    if sys.platform == 'win32':

        sSep = r'\\'



    sRocoto_WS = os.getcwd()

    sConfig = "user_new_full_ForTheia.conf"

    # sConfig = "user_new.conf"

    if OnlyForTest:



        sRocoto_WS = os.getcwd() + sSep + '..'



        # sConfig = "user_new.conf"

        sConfig = sRocoto_WS + sSep + "user_new_full_ForTheia.conf"



        if not os.path.exists(sConfig):

            sRocoto_WS = os.getcwd()

            sConfig = sRocoto_WS + sSep + "user_new_full_ForTheia.conf"





    else:



        if len(sys.argv) == 2:

            sConfig = sys.argv[1]

        else:

            sConfig = "user_new.conf"



        if not os.path.exists(sConfig):

            sConfig = "user_new_full.conf"



            if not os.path.exists(sConfig):

                sConfig = ".." + sSep + "user_new_full.conf"



                if not os.path.exists(sConfig):

                    print("Please check whether you have config file in your rocoto path!")

                    sys.exit(5)

                else:

                    sRocoto_WS = os.getcwd() + sSep + ".."



    sRocoto_WS = os.path.abspath(sRocoto_WS)

    return sConfig, sRocoto_WS





def ask(question):

    sys.stdout.write(question)

    itry=0

    itrytoohard=100

    go=True

    while go:

        itry+=1

        x=sys.stdin.readline()

        if x.lower()=='y\n':

            return True

        elif x.lower()=='n\n':

            return False

        elif itry>=itrytoohard:

            sys.stderr.write('Giving up after %d failed responses.'%itry)

            sys.exit(2)

        else:

            sys.stdout.write('Please answer y or n.')





def get_config(sConfig, dicBase):

    # read config file



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

                    a, b = sLine.split("=")



                    a = str(a).strip()

                    b = str(b).strip()



                    if b.startswith('"') or b.endswith('"'):

                        b = b.replace('"', "")

                    b = str(b).strip()



                    # print(len(ab))

                    # print(a, b)

                    # To get the number of tasks from the config file

                    if a == "taskname":

                        iTaskName_Num += 1

                        sTaskName = "taskname_{0}".format(iTaskName_Num)

                        dicBase[sTaskName.upper()] = b

                    else:

                        dicBase[a.upper()] = b

                    # print(a)

    dicBase['taskname_num'.upper()] = iTaskName_Num











if __name__ == '__main__':

    import sys



    main()



    print("--Done to generate all files!")

    sys.exit(0)


#!/usr/bin/env python

sInputCron = "cron_rocoto"

def Get_Cron_Line(sInputCron):
    sCron = ""
    with open(sInputCron, "r") as f:
        for sLine in f:
            sLine = sLine.strip()
            if len(sLine) != 0:
                if str(sLine).startswith("#"):
                    continue
                else:
                    sCron = str(sLine)


    return sCron

sCronLine = Get_Cron_Line(sInputCron)

#print(sCronLine)
            
import os
#print(os.environ['HOME'])
sHomeDIR = os.environ['HOME']

sMyCrontab = sHomeDIR + "/cron/mycrontab"

def Add_Cron_To_myCrontab(sMyCrontab, sCronLine):
    sLines = ""

    sFile = open(sMyCrontab, "r")
    for sLine in sFile:
        #print(sLine)
        #import sys
        #sys.stdout.write(sLine)

        if sLine.strip() == sCronLine:
            #print("found")
            sFile.close
            return 0

        sLines += sLine
    
    sFile.close

    sLines += sCronLine

    sFile = open(sMyCrontab, "w")
    sFile.write(sLines)
    print("writed to mycrontab!")
    sFile.flush
    sFile.close

    #print(data)
    return 0
    
ss = Add_Cron_To_myCrontab(sMyCrontab, sCronLine)
#print(ss)

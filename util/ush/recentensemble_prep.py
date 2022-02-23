#!/usr/bin/env python
# program to write the ensemble of IC files 
# calculate ensemble mean from 6 tile file and recentered to control
# written by Xiaqiong Zhou IMSG and Xianwu Xue at EMC/NCEP/NOAA on 11/15/2018
# Inputs: npert, ntiles, filename, sInWS, sOutWS
##
## Example iTile = 1...nTile
# Npert=2 # 1
# NTile=6 # 2
# sFileName='gfs_data.tile' # 3
# sInWS='/gpfs/dell2/emc/retros/noscrub/Xiaqiong.Zhou/my_util/recent_tile/00' # 4
# sOutWS="/gpfs/dell2/emc/retros/noscrub/Xianwu.Xue/ForKate/recent_tile/00"   # 5
# iTile=5 #6
#recenensemble.py $Npert $NTile $sFileName $sInWS $sOutWS [$iTile]
##
import sys
import datetime as dt
from functools import partial

print = partial(print, flush=True)

def main():
    print(dt.datetime.now())
 
    sVars = []
    sVars.append('ps')
    sVars.append('w')
    sVars.append('zh')
    sVars.append('t')
    sVars.append('sphum')
    sVars.append('o3mr')
    sVars.append('liq_wat')
    sVars.append('rainwat')
    sVars.append('ice_wat')
    sVars.append('snowwat')
    sVars.append('graupel')
    sVars.append('u_w')
    sVars.append('v_w')
    sVars.append('u_s')
    sVars.append('v_s')

    try: 
        import netCDF4
    except:
        print("FATAL ERROR: Failed to import netCDF4 in recenterensemble.py; exiting!")
        sys.exit(-5)
        
    
    if len(sys.argv) == 6:
        print(sys.argv)
        # sys.stdout.flush()
        Npert = int(sys.argv[1])
        NTiles = int(sys.argv[2])
        sFileName = sys.argv[3]
        sInWS = sys.argv[4]
        sOutWS = sys.argv[5]

        print("Npert = {0}".format(Npert))
        print("NTiles = {0}".format(NTiles))
        print("sFileName = {0}".format(sFileName))
        print("Input Workspace: {0}".format(sInWS))
        print("Output Workspace: {0}".format(sOutWS))

        print("Cheking variables ...")

        RemoveUnusedVars(sInWS,sFileName,sVars)

        DoAllTiles(Npert, NTiles, sFileName, sInWS, sOutWS, sVars)
        
        
    elif len(sys.argv) == 7:
        print(sys.argv)
        
        Npert = int(sys.argv[1])
        NTiles = int(sys.argv[2])
        sFileName = sys.argv[3]
        sInWS = sys.argv[4]
        sOutWS = sys.argv[5]
        iTile = int(sys.argv[6])

        print("\nWorking on the Tile {1} - Npert = {0}".format(Npert, iTile))
        print("Working on the Tile {1} - NTiles = {0}".format(NTiles, iTile))
        print("Working on the Tile {1} - sFileName = {0}".format(sFileName, iTile))
        print("Working on the Tile {1} - Input Workspace: {0}".format(sInWS, iTile))
        print("Working on the Tile {1} - Output Workspace: {0}".format(sOutWS, iTile))
        print("Working on the Tile {1} - iTile: {0}\n".format(iTile, iTile))
        
        print("Working on the Tile {0} - Cheking variables ...".format(iTile))
        
        RemoveUnusedVars(sInWS,sFileName,sVars,iTile)
 
        getMems_mean(iTile, Npert, sInWS, sOutWS, sFileName, sVars)

    else:
        print('You should run "recentensemble.py Npert Ntiles filename sInWS sOutWS [iTile]"')
        exit(-1)


def RemoveUnusedVars(sInWS,sFileName,sVars,iTile=-1):
    sControlFile = sInWS + "/c00/{0}{1}.nc".format(sFileName, 1)
    import os
    if os.path.exists(sControlFile):
        import netCDF4
        dset = netCDF4.Dataset(sControlFile)

        sVarsDel = []
        for sVar in sVars:
            # print(sVar)

            if sVar not in dset.variables.keys():
                sVarsDel.append(sVar)
                print("Var: {0} will be removed".format(sVar))

        for sVar in sVarsDel:
            sVars.remove(sVar)
            if iTile != -1:
                print("Working on the Tile {0} - Removed '{1}' because it does not exist in the Variable list!".format(iTile, sVar))
            else:
                print("Removed '{0}' because it does not exist in the Variable list!".format(sVar))

        #print(sVars)
    else:
        print("The control file ({0}) does not exist!, therefore, you don't need to remove vars!".format(sControlFile))

def DoAllTiles(Npert, NTiles, sFileName, sInWS, sOutWS, sVars):

    for iTile in range(NTiles):
        print("Dealing with Tile: {0}".format(iTile + 1))

        getMems_mean(iTile+1, Npert, sInWS, sOutWS, sFileName, sVars)

def getMems_mean(iTile, Npert, sInWS, sOutWS, sFileName, sVars):
    from netCDF4 import Dataset
    import os #sys,
    import shutil
    from contextlib import suppress

    wmeans = [None] * len(sVars)

    print("Working on the Tile {0}  Calculating Ensemble Mean ...".format(iTile))
    
    print(dt.datetime.now())

    for iPert in range(Npert):
        sPath = sOutWS + "/p{0:02}".format(iPert + 1)
        with suppress(FileExistsError):
            os.mkdir(sPath)

        sInFile = sInWS + "/p{0:02}/{1}{2}.nc".format(iPert + 1, sFileName, iTile)
        sOutFile = sOutWS + "/p{0:02}/{1}{2}.nc".format(iPert + 1, sFileName, iTile)

        shutil.copyfile(sInFile, sOutFile)
        print("Copying file from {0} to {1}".format(sInFile, sOutFile))

        nc_fid = Dataset(sInFile, 'r')
        # print(nc_fid.variables.keys())
        for k in range(len(sVars)):
            sVar = sVars[k]
            wmeans[k] = calValue(nc_fid, sVar, wmeans[k], iPert, Npert=Npert)

        nc_fid.close

    # ----
    print("Working on the Tile {0} -  Writing bias to netCDF files ...".format(iTile))
    print(dt.datetime.now())

    sOutFile = sOutWS + "/{0}{1}.nc".format(sFileName, iTile)
    print("Working on the Tile {0} - ".format(iTile) + sOutFile)

    nc_fid = Dataset(sOutFile, 'a')
    for k in range(len(sVars)):
        sVar = sVars[k]

        nc_fid[sVar][:] = wmeans[k][:]

    nc_fid.close

    print(dt.datetime.now())

    return


def calValue(ensmem, sVar,  mem_mean, memno, Npert=20):
    VarValue = ensmem[sVar][:]
    if memno == 0:
        mem_mean = VarValue / float(Npert)
    else:
        mem_mean += VarValue / float(Npert)
    return mem_mean


if __name__ == '__main__':

    print(f"Starting {__file__}")
    main()
    print(f"{__file__} completed successfully")

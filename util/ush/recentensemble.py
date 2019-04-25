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

def main():
    
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
    
    if len(sys.argv) == 6:
        print(sys.argv)
        sys.stdout.flush()
        Npert = int(sys.argv[1])
        NTiles = int(sys.argv[2])
        sFileName = sys.argv[3]
        sInWS = sys.argv[4]
        sOutWS = sys.argv[5]

        print("Npert = {0}".format(Npert))
        sys.stdout.flush()
        print("NTiles = {0}".format(NTiles))
        sys.stdout.flush()
        print("sFileName = {0}".format(sFileName))
        sys.stdout.flush()
        print("Input Workspace: {0}".format(sInWS))
        sys.stdout.flush()
        print("Output Workspace: {0}".format(sOutWS))
        sys.stdout.flush()

        print("Cheking variables ...")
        sys.stdout.flush()

        RemoveUnusedVars(sInWS,sFileName,sVars)

        DoAllTiles(Npert, NTiles, sFileName, sInWS, sOutWS, sVars)
        
        
    elif len(sys.argv) == 7:
        print(sys.argv)
        sys.stdout.flush()
        
        Npert = int(sys.argv[1])
        NTiles = int(sys.argv[2])
        sFileName = sys.argv[3]
        sInWS = sys.argv[4]
        sOutWS = sys.argv[5]
        iTile = int(sys.argv[6])

        print("\nWorking on the Tile {1} - Npert = {0}".format(Npert, iTile))
        sys.stdout.flush()
        print("Working on the Tile {1} - NTiles = {0}".format(NTiles, iTile))
        sys.stdout.flush()
        print("Working on the Tile {1} - sFileName = {0}".format(sFileName, iTile))
        sys.stdout.flush()
        print("Working on the Tile {1} - Input Workspace: {0}".format(sInWS, iTile))
        sys.stdout.flush()
        print("Working on the Tile {1} - Output Workspace: {0}".format(sOutWS, iTile))
        sys.stdout.flush()
        print("Working on the Tile {1} - iTile: {0}\n".format(iTile, iTile))
        sys.stdout.flush()
        
        print("Working on the Tile {0} - Cheking variables ...".format(iTile))
        sys.stdout.flush()
        
        RemoveUnusedVars(sInWS,sFileName,sVars,iTile)
 
        getMems_mean(iTile, Npert, sInWS, sOutWS, sFileName, sVars)
    else:
        print('You should run "recentensemble.py Npert Ntiles filename sInWS sOutWS [iTile]"')
        exit(-1)


def RemoveUnusedVars(sInWS,sFileName,sVars,iTile=-1):
    sControlFile = sInWS + "/c00/{0}{1}.nc".format(sFileName, 1)
    import netCDF4
    dset = netCDF4.Dataset(sControlFile)
    sys.stdout.flush()

    sVarsDel = []
    for sVar in sVars:
        print(sVar)
        sys.stdout.flush()

        if sVar not in dset.variables.keys():
            sVarsDel.append(sVar)
            print("Var: {0} will be removed".format(sVar))
            sys.stdout.flush()

    for sVar in sVarsDel:
        #print(sVar)
        sVars.remove(sVar)
        if iTile != -1:
            print("Working on the Tile {0} - Removed '{1}' because it does not exist in the Variable list!".format(iTile, sVar))
            sys.stdout.flush()
        else:
            print("Removed '{0}' because it does not exist in the Variable list!".format(sVar))
            sys.stdout.flush()

    print(sVars)
    sys.stdout.flush()

def DoAllTiles(Npert, NTiles, sFileName, sInWS, sOutWS, sVars):
    
    for iTile in range(NTiles):
        print("Dealing with Tile: {0}".format(iTile + 1))
        sys.stdout.flush()
        
        getMems_mean(iTile+1, Npert, sInWS, sOutWS, sFileName, sVars)


def getMems_mean(iTile, Npert, sInWS, sOutWS, sFileName, sVars):
    from netCDF4 import Dataset
    import numpy as np
    import sys, os
    from netCDF4 import num2date, date2num, date2index
    from subprocess import call
    wmeans = [None] * len(sVars)
    w_c = [None] * len(sVars)

    print("Working on the Tile {0}  Calculating Ensemble Mean ...".format(iTile))
    sys.stdout.flush()
    
    for iPert in range(Npert):
        print("Working on the Tile {0} for the Pert {1}".format(iTile, iPert))
        sys.stdout.flush()
        
        sPath = sOutWS + "/p{0:02}".format(iPert + 1)
        if not os.path.exists(sPath):
            os.mkdir(sPath)

        sInFile = sInWS + "/p{0:02}/{1}{2}.nc".format(iPert + 1, sFileName, iTile)
        sOutFile = sOutWS + "/p{0:02}/{1}{2}.nc".format(iPert + 1, sFileName, iTile)
        call(["cp", sInFile, sOutFile])

        nc_fid = Dataset(sInFile, 'r')
        print(nc_fid.variables.keys())
        sys.stdout.flush()
        for k in range(len(sVars)):
            sVar = sVars[k]
            print("  working on vaiable: {0} on iTile {1}".format(sVar, iTile))
            sys.stdout.flush()
            wmeans[k] = calValue(nc_fid, sVar, wmeans[k], iPert, Npert=Npert)

        nc_fid.close

    #print("  Reading Control Member Data ...")
    sCFile = sInWS + "/c00/{1}{2}.nc".format(iPert + 1, sFileName, iTile)
    #print(sCFile)
    nc_fid = Dataset(sCFile, 'r')
    for k in range(len(sVars)):
        sVar = sVars[k]
        w_c[k] = nc_fid[sVar][:]

    nc_fid.close

    print("Working on the Tile {0} -  Writing to netCDF files ...".format(iTile))
    sys.stdout.flush()
    
    for iPert in range(Npert):
        #print(iTile, " - ", iPert)
        #sys.stdout.flush()
        
        sOutFile = sOutWS + "/p{0:02}/{1}{2}.nc".format(iPert + 1, sFileName, iTile)
        print("Working on the Tile {0} - ".format(iTile) + sOutFile)
        sys.stdout.flush()
        
        nc_fid = Dataset(sOutFile, 'a')
        #print("----")
        #print(nc_fid.variables.keys())
        for k in range(len(sVars)):
            sVar = sVars[k]
            #print(sVar)
            # print(np.nanmean(ensmem[sVar][:]))
            nc_fid[sVar][:] = nc_fid[sVar][:] - wmeans[k][:] + w_c[k][:]
            # print(np.nanmean(ensmem[sVar][:]))
            #print(nc_fid[sVar][:].shape)
            #print(wmeans[k][:].shape)
            #print(w_c[k][:].shape)

        nc_fid.close

    return


def calValue(ensmem, sVar,  mem_mean, memno, Npert=20):
    VarValue = ensmem[sVar][:]
    if memno == 0:
        mem_mean = VarValue / float(Npert)
    else:
        mem_mean += VarValue / float(Npert)
    return mem_mean


if __name__ == '__main__':
    import sys

    main()

    print("--Done!")
    sys.stdout.flush()
    


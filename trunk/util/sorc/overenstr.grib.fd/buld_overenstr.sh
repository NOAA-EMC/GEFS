
set -x

make clean

module purge
module list
#Loading intel suite
#module load ics/15.0.1

# Load NCEP libs modules 
#module load EnvVars/1.0.0
#module load w3nco/v2.0.6
#module load bacio/v2.0.1

echo $W3NCO_LIB4
echo $BACIO_LIB4

make -f makefile




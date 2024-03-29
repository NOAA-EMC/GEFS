#! /usr/bin/env python3


"""
Copies specified directories of GEFS output from temporary location to another location.

Inputs (via environment variables):
    WORKDIR              : The base GEFS output directory (usually in ptmp)
    KEEP_DIR             : The base directory to store GEFS output (usually in noscrub)
    DIRS_TO_KEEP_WAVE    : A comma-separated list of directories to retain
    PDY                  : Initialization date in YYYYMMDD form
    cyc                  : Initialization hour in HH form

    GEFS output must be located in the WORKDIR com directory as such:
        <WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/wave/<directory>

Outputs:
    For each directory in DIRS_TO_KEEP_WAVE, that directory will be copied to the KEEP_DIR in
        the following location:
            <KEEP_DIR>/gefs.<PDY>/<cyc>/wave/<directory>

Error Codes:
    -100 : Required environment variable not defined
    -102 : KEEP_DIR already exists and couldn't be deleted

"""

import os
import shutil
from datetime import datetime
from datetime import timedelta
from functools import partial

#output_pattern = "{work_dir}/com/gefs/dev/gefs.%Y%m%d/%H/wave"
output_pattern = "{work_dir}/dev/com/gefs/v12.3/gefs.%Y%m%d/%H/wave"
destination_pattern = "{keep_dir}/gefs.%Y%m%d/%H/wave"

clobber = True

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)

# Read in environment variables and make sure they exist
work_dir = os.environ.get("WORKDIR")
if( work_dir is None ):
    print("FATAL: Environment variable WORKDIR not set")
    quit(-100)

keep_dir = os.environ.get("KEEP_DIR")
if( keep_dir is None ):
    print("FATAL: Environment variable KEEP_DIR not set")
    quit(-100)

dirs_to_keep_string = os.environ.get("DIRS_TO_KEEP_WAVE")
if( dirs_to_keep_string is None ):
    print("FATAL: Environment variable DIRS_TO_KEEP_WAVE not set")
    quit(-100)
# Convert to array using commas and removing whitespace
dirs_to_keep = [x.strip() for x in dirs_to_keep_string.split(',')]

pdy = os.environ.get("PDY")
if( pdy is None ):
    print("FATAL: Environment variable PDY not set")
    quit(-100)

cycle = os.environ.get("cyc")
if( cycle is None ):
    print("FATAL: Environment variable cyc not set")
    quit(-100)

interval = int(os.environ.get("gefs_cych"))
if( interval is None ):
    print("FATAL: Environment variable gefs_cych not set")
    quit(-100)

time = datetime.strptime("{pdy}{cycle}".format(pdy=pdy, cycle=cycle), "%Y%m%d%H")
time_next = time + timedelta(hours=interval)

print("Starting GEFS keep_data with the following settings:")
print("Source directory      : {work_dir}".format(work_dir=work_dir))
print("Destination Directory : {keep_dir}".format(keep_dir=keep_dir))
print("Directories to Keep   : {dirs_to_keep}".format(dirs_to_keep=str(dirs_to_keep)))
print(time.strftime("Date/Cycle                    : %Y%m%d_%H"))
print(time_next.strftime("Next Date/Cycle                    : %Y%m%d_%H"))

# Output directories
for directory in dirs_to_keep:
    if directory == "restart":
        output_path = time_next.strftime( output_pattern.format( work_dir=work_dir ) )
        destination_path = time_next.strftime( destination_pattern.format( keep_dir=keep_dir ) )
    else:
        output_path = time.strftime( output_pattern.format( work_dir=work_dir ) )
        destination_path = time.strftime( destination_pattern.format( keep_dir=keep_dir ) )

    os.makedirs(destination_path, exist_ok=True)

    output_dir = "{output_path}/{directory}".format(output_path=output_path, directory=directory)
    destination_dir = time.strftime( "{destination_path}/{directory}".format( destination_path=destination_path, directory=directory ) )

    if os.path.exists(output_dir):
        if ( os.path.exists(destination_dir) ):
            if(clobber):
                shutil.rmtree(destination_dir + "/")
            else:
                print("FATAL: Destination diretory {destination_dir} already exists and clobber is False".format(destination_path=destination_path))
                quit(-102)

        print("Copying files for {directory}".format(directory=directory))
        print("    From {output_dir} to {destination_dir}".format(output_dir=output_dir, destination_dir=destination_dir))
        shutil.copytree(output_dir, destination_dir)
    else:
        print("WARNING: {output_dir} does not exist, skipping".format(output_dir=output_dir))

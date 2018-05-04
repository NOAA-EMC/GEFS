#! /usr/bin/env python

import os, sys, re, logging, collections, StringIO, getopt
from os.path import realpath, normpath, dirname

logging.basicConfig()

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

def usage(message=None,logger=None):
    """!Dumps a usage message and exits with status 2.
    @param message An extra message to send to stderr after the usage message
    @param logger Ignored."""
    print>>sys.stderr, '''
Usage: run_gefs.py [options]

Workflow options:
  -w workflow-file.xml -- use this as the output XML file to send
        into rocotorun (rocotorun's -w option)
  -d workflow-db.db -- use this as the SQLite3 database file for
        Rocoto (rocotorun's -d option)
    '''
    if message is not None:
        print>>sys.stderr,str(message).rstrip()+'\n'
    sys.exit(2)

def create_crontab_file(crontab_filename, outxml):
    cmd = batchexe('which') ['rocotorun']
    try:
        which_rocoto = runstr(cmd).strip()
    except Exception:
        print "\nCRITICAL ERROR: No crontab file was created because rocotorun is not in your path"
        sys.exit(0)

    system = produtil.cluster.name()

    crontab_string  = '# This is a basic crontab file to use with given settings that will execute rocotorun every 5 minutes\n'
    crontab_usage   = '# Usage: crontab '+crontab_filename+'\n'
    crontab_usage  += '#        crontab -l   lists current crontab\n'
    crontab_usage  += '#        crontab -r   removes current crontab\n'
    crontab_time    = '*/5 * * * * '
    pwd =  os.getcwd()
    outxml_abspath =  os.path.abspath(outxml)
    rocotorun_args = which_rocoto + ' -d ' + pwd +'/'+database_file + ' -w ' + outxml_abspath
    if system == 'theia':
        crontab_string += crontab_usage
        crontab_string += crontab_time + rocotorun_args
    elif system == 'gyre' or system == 'tide':
        head = system[0] ; hosts = ''
        for host in ('10a1','10a2','14a1','14a2'):
            hosts += head+host+' '
        crontab_string += '# When on '+system+' you can only run cron on the hosts: '+hosts+'\n'
        crontab_string += crontab_usage
        crontab_string += crontab_time + '(. /usrx/local/Modules/default/init/sh; module load lsf; module use /usrx/local/emc_rocoto/modulefiles ; module load rocoto ;' + rocotorun_args + ') > /dev/null 2>&1\n'
    elif system == 'luna' or system == 'surge':
        crontab_string += crontab_usage
        crontab_string += crontab_time + '(. /opt/modules/3.2.10.3/init/sh; module use /usrx/local/emc_rocoto/modulefiles ; module load xt-lsfhpc; module load rocoto;' + rocotorun_args + ') > /dev/null 2>&1\n'
    else:
        print "CRITICAL ERROR: auto-crontab file generation for %s still needs to be implemented"%system
        sys.exit(0)

    crontab_file = open( crontab_filename,'w')
    crontab_file.write( crontab_string )
    crontab_file.close()

def load_produtil_pythonpath():

    try:
        import produtil.cluster
        return True
    except ImportError:
        pass

    PRODUTIL = collections.defaultdict(list)
    PRODUTIL['theia'] = '/scratch4/NCEPDEV/global/save/glopara/svn/nceplibs/produtil/trunk/ush'
    PRODUTIL['luna']  = '/gpfs/hps3/emc/global/noscrub/emc.glopara/svn/nceplibs/produtil/trunk/ush'
    PRODUTIL['tide']  = '/global/save/emc.glopara/svn/nceplibs/produtil/trunk/ush'
    try_clusters = ('theia','luna','tide')

    for cluster in try_clusters:
        sys.path.append(PRODUTIL[cluster])
        try:
            import produtil.cluster
            return True
        except ImportError:
            pass
    return False

########################################################################
# Load and set up the produtil package.
if not load_produtil_pythonpath():
        curses.endwin()
        print '\n\nCRITICAL ERROR: The produtil package could not be loaded from your system'
        sys.exit(-1)

import produtil.setup, produtil.atparse, produtil.run, produtil.prog, \
    produtil.fileop, produtil.batchsystem, produtil.cluster
from produtil.run import run, batchexe, runstr
from produtil.fileop import remove_file, isnonempty

########################################################################
# Global variables and constants

logger=logging.getLogger('run_gefs')

outxml           = ''
outdb            = ''
dateargs         = list()
iarg             = 0
firstarg         = 0

########################################################################
# Parse the options and arguments.

short_opts = "d:w:"
long_opts  = ["database=",
              "workflow="
             ]

try:
   opts, args = getopt.getopt(sys.argv[1:], short_opts, long_opts)
except getopt.GetoptError as err:
   print str(err)
   usage('SCRIPT IS ABORTING DUE TO UNRECOGNIZED ARGUMENT')

database_file='gefs.db'

for k, v in opts:
   if   k in ('-d', '--database'):
      outdb = v
   elif k in ('-w', '--workflow'):
      outxml = v
   else:
      assert False, "UNHANDLED OPTION"

# Make sure the workflow isn't the database
if outxml[-3:]=='.db':
    usage('When using the -d option, the Rocoto XML filename must '
          'not end with ".db".')
# Make sure the database isn't the workflow
if outdb[-4:]=='.xml':
    usage('When using the -d option, the database filename must '
          'not end with ".xml".')

#outxml=None
#outdb=None
#args=sys.argv[1:]
#dateargs=list()
#firstarg=0
#iarg=0
#while iarg<len(args):
#   arg=args[iarg]
#   if arg=='-w':
#      if iarg+1>=len(args): usage('-w requires an argument')
#      logger.info('-w %s -- use this workflow XML file'%(repr(args[iarg+1]),))
#      outxml=args[iarg+1]
#      iarg+=1
#   iarg+=1

########################################################################
# Create the list of variables to send to the ATParser

# Read user.conf file and set to environment variables
with open("user.conf","r")as f:
   for line in f:
      a, b = line.strip().split("=")
      os.environ[a]=b

VARS=dict(os.environ)

#VARS.update(RUN_INIT='YES')

fgatstr=3
fgatend=9
fgatinv=3
fhrlist=' '.join([ '%d'%(i+6) for i in xrange(fgatstr,fgatend+1,fgatinv) ])
VARS.update(INIT_FHR=fhrlist)
#VARS.update(WHERE_AM_I='wcoss')

########################################################################
# Order the ATParser to create the XML file.

rocotoxml=StringIO.StringIO()
parser=produtil.atparse.ATParser(rocotoxml,varhash=VARS,logger=logger)
parser.parse_file('gefs.xml.in')

if not outxml: outxml='gefs.xml'
if not outdb: outdb='gefs.db'
havexml=isnonempty(outxml)

if havexml:
   if not ask('ALERT! %s: XML file exists.  Overwrite (y/n)?'%(outxml,)):
       logger.error('%s: file exists, user does not want to overwrite.'
                     %(outxml,))
       sys.exit(1)
   else:
       logger.warning('%s: overwriting pre-existing XML file.'%(outxml,))

with open(outxml,'wt') as outf:
    outf.write(rocotoxml.getvalue())

########################################################################
# Create working directories
import datetime

dd=os.environ['WORKDIR']
dd+="/"+os.environ['EXPID']
cmd="mkdir -p "+dd+"/tmpnwprd"
os.system(cmd)
dd+='/com/output/dev/'

pdy=os.environ['SDATE'][0:8]
year=os.environ['SDATE'][0:4]
month=os.environ['SDATE'][4:6]
day=os.environ['SDATE'][6:8]
date1=datetime.datetime.strptime(os.environ['SDATE'][0:8],"%Y%m%d")
date2=datetime.datetime.strptime(os.environ['EDATE'][0:8],"%Y%m%d")
day=datetime.timedelta(days=1)

while date1 <= date2:
    d=date1.strftime('%Y%m%d')
    d1=dd+d
    cmd="mkdir -p "+d1
    os.system(cmd)
    date1 = date1 + day

########################################################################
# Generate crontab file
crontab_filename='cron_rocoto'
create_crontab_file(crontab_filename, outxml)

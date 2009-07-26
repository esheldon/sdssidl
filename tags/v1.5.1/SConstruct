# vim: set filetype=python :

import os
import sys
from sys import stdout,stderr


# Subdirectories containing SConscript files.  These are always processed.
# We also add postgres and the gsl based sdsspix if available
subdirs=['IDLStruct','gauleg','total_int','fileio',
         'htm','atlas','sphpoly_masks']

# Configurations will be saved here so command line options don't
# have to be sent more than once
#config_file = '.scons.conf'

# set up the environment
initial_env = Environment()
initial_env['subdirs'] = subdirs

# first check for a saved conf file
#opts = Variables(config_file)
#opts=Options()
opts=Variables()

opts.Add(BoolVariable('IMPORT_PATHS',
    'Import PATH, C_INCLUDE_PATH/CPATH and LIBRARY_PATH/LD_LIBRARY_PATH environment variables', True))

opts.Update(initial_env)
#opts.Save(config_file,initial_env)
Help(opts.GenerateHelpText(initial_env))




def CheckIDL_DIR():
    if not os.environ.has_key('IDL_DIR'):
        stdout.write('IDL_DIR not found\n')
        Exit(45)
    idl_dir = os.getenv('IDL_DIR')
    if idl_dir == None:
        stdout.write('IDL_DIR is empty\n')
        Exit(45)
    return idl_dir


def CheckIDL_C(context):
    """
    We have to do a custom check because idl_export.h actually requires
    stdio.h
    """

    source_file="""
/* this needed to use idl_export.h */
#include <stdio.h>
#include "idl_export.h"

int main() 
{
    int i=0;
    return 0;
}
"""
    
    context.Message("Checking for idl_export.h...")
    if context.TryCompile(source_file, '.c'):
        result=True
    else:
        result=False

    context.Result(result)
    return result

def LibAndHeaderChecks(config):
    # make sure we can find the header
    if not config.CheckIDL_C():
        stdout.write('  Fatal Error\n')
        Exit(45)
    
    # See if we can build against postgres header/libraries
    #if not config.CheckLibWithHeader('pq','libpq-fe.h',language='C'):
    if not config.CheckHeader('libpq-fe.h',language='C'):
        stdout.write('postgres library/header not found. Not building pgsql\n')
        stdout.write('you might try augmenting your '+\
                     'C_INCLUDE_PATH/LD_LIBRARY_PATH\n')
    else:
        env['subdirs'] += ['pgsql']

    # See if we can build against postgres header/libraries
    #if not config.CheckLibWithHeader('pq','libpq-fe.h',language='C'):
    if not config.CheckHeader('gsl/gsl_vector.h',language='C'):
        stdout.write('gsl headers not found. Not building sdsspix\n')
        stdout.write('you might try augmenting your '+\
                     'C_INCLUDE_PATH/LD_LIBRARY_PATH\n')
    else:
        pass
        env['subdirs'] += ['sdsspixIDL']



def AddPath(pathlist, newpath):
    """
    Add path(s) to a list of paths.  Check the path exists and that it is
    not already in the list
    """
    if type(newpath) == list:
        for l in newpath:
            AddPath(pathlist, l)
    else:
        # to deal with expansions and possible end / which 
        # messes up uniqueness test
        p = os.path.abspath(newpath) 
        if os.path.exists(p):
            if pathlist.count(p) == 0:
                pathlist.append(p)


def ImportPaths(env):
    # now import user's paths
    cpp_paths = []
    lib_paths = []

    if env['IMPORT_PATHS'] and os.environ.has_key('C_INCLUDE_PATH'):
        paths=os.environ['C_INCLUDE_PATH']
        paths=paths.split(os.pathsep)
        AddPath(cpp_paths, paths)
    if env['IMPORT_PATHS'] and os.environ.has_key('CPATH'):
        paths=os.environ['CPATH']
        paths=paths.split(os.pathsep)
        AddPath(cpp_paths, paths)


    if env['IMPORT_PATHS'] and os.environ.has_key('LIBRARY_PATH'):
        paths=os.environ['LIBRARY_PATH']
        paths=paths.split(os.pathsep)
        AddPath(lib_paths, paths)

    if env['IMPORT_PATHS'] and os.environ.has_key('LD_LIBRARY_PATH'):
        paths=os.environ['LD_LIBRARY_PATH']
        paths=paths.split(os.pathsep)
        AddPath(lib_paths, paths)

    env.Prepend(LIBPATH= lib_paths)
    env.Prepend(CPPPATH= cpp_paths)
    env.Prepend(CPATH= cpp_paths)
    #print 'c_paths=',cpp_paths
    #print 'lib_paths=',lib_paths


def GetIDLArch():
    import subprocess
    idl_dir = CheckIDL_DIR()
    bin_dir = os.path.join(idl_dir, 'bin')
    executable=os.path.join(bin_dir, 'idl')
    command = "echo 'print,!version.arch' | "+executable

    pobj=subprocess.Popen(command,
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE,
                          shell=True)
                          

    stdout_ret, stderr_ret = pobj.communicate()
    arch = stdout_ret.strip()
    return arch

def AddExtraPaths(env):
    # make sure IDL_DIR is set.  Will exit if not.
    env['idl_dir'] = CheckIDL_DIR()

    # here CPP is c pre-processor I think
    idl_external_dir=os.path.join(env['idl_dir'], 'external')
    idl_include_dir=os.path.join(idl_external_dir, 'include')
    env.Prepend(CPPPATH=[idl_include_dir])
    env.Prepend(CPATH=[idl_include_dir])

    # Import some paths from the user's environment
    ImportPaths(env)


def SetFlags(env):
    # put endian check here too?
    env['SDSS_CFLAGS'] = ["-DLINKAGE",
                          "-DCHECK_LEAKS",
                          "-DSTAND_ALONE",
                          "-DSDSS_LITTLE_ENDIAN"]
    env['CFLAGS'] += ['-O2']
    env['CCFLAGS'] += ['-O2']
    if os.uname()[0] == 'Darwin':
        # Use lists: Kludge to fix Darwin problem with putting quotes around 
        # strings when there are spaces

        arch = GetIDLArch()
        if arch == 'x86_64':
            bits_flags='-m64'
        else:
            bits_flags='-m32'
        env['CFLAGS'] += [bits_flags]
        env['CCFLAGS'] += [bits_flags]

        elements=['-undefined','dynamic_lookup']
        elements+=['-single_module']
        elements+=[bits_flags]
        env.Append(SHLINKFLAGS=elements)

        # IDL always looks for .so files
        env['SHLIBSUFFIX']='.so'





def DoConfig(env):

    env.Append(CFLAGS=["-Wall"])

    AddExtraPaths(env)
    SetFlags(env)

    config = env.Configure(custom_tests={'CheckIDL_C':CheckIDL_C})
    LibAndHeaderChecks(config)

    env = config.Finish()

    # turn off stupid warnings 
    SetOption('warn', ['no-duplicate-environment'] + GetOption('warn'))


if not GetOption('help'):

    env = initial_env
    DoConfig(env)

    # subdirectores to process.  We process src by default
    src_dir='src'
    script_files=[]
    #env['subdirs']=['total_int']
    #env['subdirs']=['sdsspixIDL']
    for d in env['subdirs']:
        script_dir = os.path.join(src_dir, d)
        script_file = os.path.join(script_dir, 'SConscript')
        script_files.append(script_file)

    SConscript(script_files, exports='env')



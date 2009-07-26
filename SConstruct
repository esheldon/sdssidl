# vim: set filetype=python et ts=4 sw=4:
# to do:  
#   Add more support for other compilers
#   will support g++ and icpc at least
# 
# Always run scons from the root directory of wl

import os
import sys
from sys import stdout,stderr

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



initial_env = Environment()

env = initial_env



# make sure idl is found
idl_dir=os.getenv('IDL_DIR')
if idl_dir == '':
    stdout.write('IDL_DIR not found\n')
    Exit(1)

# here CPP is c pre-processor I think
# putting things there will also make the programs depend upon them,
# so maybe I shouldn't put idlstruct there?
idl_include_dir=os.path.join(idl_dir, 'external/include')
env.Prepend(CPPPATH=[idl_include_dir])
env.Prepend(CPATH=[idl_include_dir])

env.Append(CCFLAGS=["-Wall"])
env['ARFLAGS'] = '-rvcs'

SetOption('warn', ['no-duplicate-environment'] + GetOption('warn'))

# put endian check here too?
env['SDSS_CFLAGS'] = ["-DLINKAGE","-DCHECK_LEAKS","-DSTAND_ALONE","-DSDSS_LITTLE_ENDIAN"]

if os.uname()[0] == 'Darwin':
    # Use lists: Kludge to fix Darwin problem with putting quotes around 
    # strings when there are spaces

    # need to do explicit check for bits
    bits_flags='-m64'

    elements=['-undefined','dynamic_lookup']
    elements+=['-single_module']
    elements+=[bits_flags]
    env.Append(SHLINKFLAGS=elements)

    # IDL always looks for .so files
    env['SHLIBSUFFIX']='.so'
    env['CCFLAGS'] = ['-Wall','-dynamic','-g','-m64']




config = env.Configure(custom_tests={'CheckIDL_C':CheckIDL_C})
if not config.CheckIDL_C():
    stdout.write('  Fatal Error\n')
    Exit(45)

subdirs=['IDLStruct','gauleg','total_int','fileio','htm']
if not config.CheckLibWithHeader('pq','libpq-fe.h',language='C'):
    stdout.write('postgres library/header not found (This is OK)\n')
else:
    subdirs += 'pgsql'

# haven't implemented config yet
env = config.Finish()


# subdirectores to process.  We process src by default
src_dir='src'
script_files=[]
for d in subdirs:
    script_dir = os.path.join(src_dir, d)
    script_file = os.path.join(script_dir, 'SConscript')
    script_files.append(script_file)

SConscript(script_files, exports='env')



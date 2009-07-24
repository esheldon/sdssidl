# vim: set filetype=python et ts=4 sw=4:
# to do:  
#   Add more support for other compilers
#   will support g++ and icpc at least
# 
# Always run scons from the root directory of wl

import os
import sys
from sys import stdout,stderr


initial_env = Environment()

#opts.Update(initial_env)
#opts.Save(config_file,initial_env)
#Help(opts.GenerateHelpText(initial_env))

env = initial_env

config = env.Configure()


# make sure idl is found
idl_dir=os.getenv('IDL_DIR')
if idl_dir == '':
    stdout.write('IDL_DIR not found\n')
    Exit(1)

idl_include_dir=os.path.join(idl_dir, 'external/include')
env.Prepend(CPPPATH=[idl_include_dir])
env.Prepend(CPATH=idl_include_dir)
env.Prepend(CPPPATH=["#src/IDLStruct"])

env.Append(CCFLAGS=["-Wall"])

subdirs=['gauleg','total_int','fileio']
if not config.CheckLibWithHeader('pq','libpq-fe.h',language='C'):
    stdout.write('postgres library/header not found (This is OK)\n')
else:
    subdirs += 'pgsql'

env = config.Finish()


# subdirectores to process.  We process src by default
src_dir='src'
script_files=[]
for d in subdirs:
    script_dir = os.path.join(src_dir, d)
    script_file = os.path.join(script_dir, 'SConscript')
    script_files.append(script_file)

SConscript(script_files, exports='env')



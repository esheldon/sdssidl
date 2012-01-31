#!/bin/sh
# 
# Copy this to another location, and modify the config file path to point
# at your local config file (see ../config for an example). This file should
# be sourced by the user.

# The location of the sdssidl installation. replace this with the path to 
# your local installation
export SDSSIDL_DIR=some_path/sdssidl

export SDSSIDL_PRO_DIR=${SDSSIDL_DIR}/pro
export SDSSIDL_DLM_DIR=${SDSSIDL_DIR}/src/DLM

# Location of the config file. Replace this with the path to you local copy
export SDSSIDL_CONFIG=${SDSSIDL_DIR}/config/sdssidl_setup_default.config

# Modify the IDL_PATH.  Place ours in the FRONT

if [ ${IDL_PATH:+1} ] # returns 1 (true) if it exists
then
    export IDL_PATH=+${SDSSIDL_PRO_DIR}:${IDL_PATH}
else
    export IDL_PATH=+${SDSSIDL_PRO_DIR}:"<IDL_DEFAULT>"
fi

# The DLM Path is probably not set.  Always put ours at the END

if [ ${IDL_DLM_PATH:+1} ] # returns 1 (true) if it exists
then
    export IDL_DLM_PATH=${IDL_DLM_PATH}:${SDSSIDL_DLM_DIR}
else
    export IDL_DLM_PATH="<IDL_DEFAULT>":${SDSSIDL_DLM_DIR}
fi

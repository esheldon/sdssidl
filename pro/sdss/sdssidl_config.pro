;+
; NAME:
;  SDSSIDL_CONFIG
;
; PURPOSE:
;  Return the value of an sdssidl configuration variable
;
; CATEGORY:
;  SDSS Specific routine
;
; CALLING SEQUENCE:
;  value = sdssidl_config(varName, /struct, exists=exists)
;
; INPUTS:
;  varName: The name of the SDSSIDL config. variable to get.
;
; KEYWORD PARAMETERS:
;  /struct: Return the structure containing all the configuration variables. In
;           this case the user need not send any arguments.
;
; OUTPUTS:
;  By default, either the value of the configuration varaible or -1 if it does
;    not exist. 
;  If /struct, then the configuration structure, or -1 if it cannot be read. 
;
; OPTIONAL OUTPUTS:
;  exists=exists:  1 if the variable exists or 0 if not
;
; COMMON BLOCKS:
;  sdssidl_config_block, configStruct, tags
;
;
; SIDE EFFECTS:
;  If the config file has not been loaded, then sdssidl_load_config is run and
;  the common block above is modified.
;
; EXAMPLE:
;  ;; Read the sdss data_dir config variable
;  data_dir = sdssidl_config('data_dir')
;  
;  ;; List the available configuration variables
;  help,sdssidl_config(/struct),/str
;
;  ;; Check for a variable and if it is defined, continue
;  lss_dir = sdssidl_config('lss_dir', exists=lss_dir_exists)
;  if lss_dir_exists then begin 
;      file = lss_dir + '......'
;
;
; MODIFICATION HISTORY:
;  Created: Nov-2004, Erin Sheldon UChicago
;
;-
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;                                       


function sdssidl_config, varname, struct=struct, exists=exists

  common sdssidl_config_block, configstruct, tags

  if (n_params() eq 0) and (not keyword_set(struct)) then begin 
      print,'-Syntax: val=sdssidl_config(varName, /struct, exists=exists)'
      print,'Use doc_library,"sdssidl_config" for help'
      exists = 0
      return,-1
  endif 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Make sure the configuration is loaded
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if n_elements(configstruct) eq 0 then begin 
      sdssidl_load_config
      if n_elements(configstruct) eq 0 then begin 
          message,'Could not load config file',/inf
          exists = 0
          return,-1
      endif 
  endif 

  IF keyword_set(struct) THEN BEGIN 
      exists = 1
      return,configStruct
  endif 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check for the input config var
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  checkName = strupcase(varName[0])

  w=where(tags eq checkname, nw)
  if nw eq 0 then begin 
      exists = 0
      return,-1
  endif else begin 
      exists = 1
      return,configstruct.(w[0])
  endelse 

end 

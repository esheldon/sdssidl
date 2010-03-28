;+
; NAME:
;  SDSSIDL_LOAD_CONFIG
;
;
; PURPOSE: 
;  Load the configuration file.  The $SDSSIDL_CONFIG variable msut be set by
;  the user.  The configuration variables are loaded into a structure, stored
;  in a common block.  In addition, the hostname and the ra/dec search runs are
;  loaded.
;
;
; CATEGORY:
;  SDSS specific routine.
;
;
; CALLING SEQUENCE:
;  sdssidl_load_config
;
; COMMON BLOCKS:
;  sdssidl_config_block
;
;
; SIDE EFFECTS:
;  configStruct and tags, from the common block, are set
;
;
; RESTRICTIONS:
;  The $SDSSIDL_CONFIG environment variable must be set by the user.
;
; MODIFICATION HISTORY:
;  Created: Erin Sheldon UofChicago
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


PRO sdssidl_load_config

  COMMON sdssidl_config_block, configStruct, tags

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read the config file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  config_file = getenv("SDSSIDL_CONFIG")

  IF config_file[0] EQ '' THEN BEGIN 
      message, "$SDSSIDL_CONFIG undefined. Returning", /inf
      return
  ENDIF ELSE BEGIN 
      print,'Loading config file: ',config_file
      parse_config, config_file, keywords, values
  ENDELSE 
  lkey = strlowcase(keywords)  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create the config structure
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  nkey = n_elements(keywords)
  
  FOR i=0L, nkey-1 DO BEGIN 
       
      IF i EQ 0 THEN BEGIN 
          configStruct = $
            create_struct(keywords[i], values[i])
      ENDIF ELSE BEGIN 
          configStruct = $
            create_struct(configStruct, keywords[i], values[i])
      ENDELSE 
      
  ENDFOR 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; If spec_dir not specified, look for subdirectory of data_dir
  ;; called "spectro"
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  w=where(lkey EQ 'spec_dir', nw)
  IF nw EQ 0 THEN BEGIN 
      w2=where(lkey EQ 'data_dir',nw2)
      IF nw2 NE 0 THEN BEGIN 
          dir = values[w2[0]]+'/spectro/'
          IF file_test(dir) THEN BEGIN 
              configStruct = $
                create_struct(configStruct, 'spec_dir', dir)
          ENDIF 
      ENDIF 
  ENDIF 

  ;; Host name, just the front
  hostname = getenv('HOSTNAME')
  hostname = str_sep(hostname[0], '.')
  hostname=hostname[0]
  
  configStruct = $
    create_struct(configStruct, 'HOSTNAME', hostname)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read in the search runs if the directory is defined
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  w=where(lkey EQ 'radec_search_dir', nw)

  IF nw NE 0 THEN BEGIN 

      dir = values[w[0]]
      files=findfile(dir)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; get runs from file names
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
      IF files[0] NE '' THEN BEGIN 
          nf = n_elements(files)
          search_runs=replicate(-1,nf)
          FOR i=0L, nf-1 DO BEGIN 
              tmp=str_sep(files[i], '-')
              IF n_elements(tmp) EQ 4 THEN search_runs[i] = long( tmp[2] )
          ENDFOR 
          wgood=where(search_runs NE -1, ngood)
          IF ngood NE 0 THEN BEGIN 
              search_runs = search_runs[wgood]
              search_runs = search_runs[rem_dup(search_runs)]
              
              configStruct = $
                create_struct(configStruct, 'SEARCH_RUNS', search_runs)
          ENDIF
      ENDIF
  ENDIF 

  tags = tag_names(configStruct)

END 

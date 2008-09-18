;+
; NAME:
;  READ_SPDIAG1D
;
;
; PURPOSE:
;  read in the spDiag file for a given plateid (mjd)
;
;
; CATEGORY:
;  SDSS specific routine.
;
;
; CALLING SEQUENCE:
;  read_spdiag1d, plateID, spdiag, mjd=mjd, spec_vers=spec_vers, /silent
;
;
; INPUTS:
;  plateID: plate number in integer form.
;
;
; OPTIONAL INPUTS:
;  mjd: modified julian date
;  spec_vers: version of the spectra.  Default is to use SPEC_VERS from the
;             configuraton file.
;
; KEYWORD PARAMETERS:
;  /silent: do not print out informative messages.
;
;
; OUTPUTS:
;  spDiag:  the contents of the spDiag file.
;
;
; RESTRICTIONS:
;  The user needs yanny_read.pro and the spectra data on disk.  SPEC_VERS
;  and SPEC_DIR must be defined in the config file
;
;
; PROCEDURE:
;  Use yanny_read to read the contents of the spDiag file.
;
;
; EXAMPLE:
;  read_spDiag1D, 1026, spDiag
;
;
; MODIFICATION HISTORY:
;  Creation: ??-??-2003.  Erin Sheldon UofChicago
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


PRO read_spdiag1d, plateID, spdiag, mjd=mjd, spec_vers=spec_vers, silent=silent

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: read_spdiag1d, plateID, spdiag, mjd=mjd, spec_vers=spec_vers, silent=silent'
      return
  ENDIF 

  delvarx, spdiag

  pstr = strn(plateID,length=4,padch='0')


  IF n_elements(spec_vers) EQ 0 THEN BEGIN 
      spec_vers = sdssidl_config('SPEC_VERS', exists=v_exists)
      IF NOT v_exists THEN BEGIN 
          message,'SDSSIDL Config variable SPEC_VERS must be defined'
      ENDIF 
  ENDIF 

  sdss_spec_dir = sdssidl_config('SPEC_DIR', exists=d_exists)
  IF NOT d_exists THEN BEGIN 
      message,'SDSSIDL Config variable SPEC_DIR must be defined'
  ENDIF 

  dir=SDSS_SPEC_DIR+spec_vers+'/'+pstr+'/1d/'

  IF n_elements(mjd) EQ 0 THEN BEGIN  
      files = findfile(dir + 'spDiag1d-?????-'+pstr+'.par',$
                       count=count)

      IF count EQ 0 THEN BEGIN 
          IF NOT keyword_set(silent) THEN BEGIN
              print,'plate: '+pstr+' fiber: '+fstr+' does not exist'
          ENDIF 
          delvarx,spdiag
          return
      ENDIF 
      
      ;; get the latest
      file = files[count-1]
      
      tmp = str_sep(file, '-')
      mjdstr = tmp[1]
      mjd = long(mjdstr)

  ENDIF ELSE BEGIN 
      mjdstr = strn(mjd, length=5, padch='0')
      file = dir + 'spDiag1d-'+mjdstr+'-'+pstr+'.par'
  ENDELSE 

  yanny_read, file, pdata
  spdiag = *pdata
  ptr_free, pdata

END 

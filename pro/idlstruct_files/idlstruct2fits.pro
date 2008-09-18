;+
; NAME:
;  idlstruct2fits
;
; PURPOSE:
;  Convert idlstruct files back and forth from fits files.
;
; CATEGORY:
;  File I/O
;
; CALLING SEQUENCE:
;  idlstruct2fits, infiles, outfiles, /reverse, status=
;
; INPUTS:
;  infiles: idlstruct file name(s).
;  outfiles: fits file name(s).
;
; KEYWORD PARAMETERS:
;  /reverse:  The infiles are actually a list of fits filenames
;     and outfiles are a set of idlstruct file names.
;
; OPTIONAL OUTPUTS:
;  status: 0 for success.
;
; MODIFICATION HISTORY:
;  Created: 2005, ErinSheldon, UChicago
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

PRO idlstruct2fits_tofits, idlstruct_files, fits_files, status=status

  nst = n_elements(fits_files)
  FOR i=0L, nst-1 DO BEGIN 

      IF idlstruct_files[i] EQ fits_files[i] THEN BEGIN 
          message,'fits output filename is same as input idlstruct name!'
      ENDIF 

      IF nst GT 1 THEN BEGIN 
          print
          print,'----------------------------------------------------'
      ENDIF 
      print
      print,'Reading idlstruct file: ',idlstruct_files[i]
      t = read_idlstruct(idlstruct_files[i], status=status)
      
      IF status EQ 0 THEN BEGIN 
          print
          print,'Writing to fits file: ',fits_files[i]
          mwrfits, t, fits_files[i], /create
      ENDIF ELSE BEGIN 
          print,'Unable to read idlstruct file'
      ENDELSE 

  ENDFOR 

END 

PRO idlstruct2fits_toidlstruct, fits_files, idlstruct_files, status=status

  nst = n_elements(fits_files)
  FOR i=0L, nst-1 DO BEGIN 

      IF idlstruct_files[i] EQ fits_files[i] THEN BEGIN 
          message,'idlstruct output filename is same as input fits name!'
      ENDIF 

      IF nst GT 1 THEN BEGIN 
          print
          print,'----------------------------------------------------'
      ENDIF 
      print
      print,'Reading fits file: ',fits_files[i]
      t = mrdfits(fits_files[i], 1, status=status)
      
      IF status EQ 0 THEN BEGIN 
          print
          print,'Writing to idlstruct file: ',idlstruct_files[i]
          write_idlstruct, t, idlstruct_files[i]
      ENDIF ELSE BEGIN 
          print,'Unable to read fits file'
      ENDELSE 

  ENDFOR 

END 



PRO idlstruct2fits, infiles, outfiles, reverse=reverse, status=status

  status = 1
  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: idlstruct2fits, idlstruct_files, fits_files, status='
      print,'      -- OR --'
      print,'-Syntax: idlstruct2fits, fits_files, idlstruct_files, /reverse, status='
      print
      print,' Use /reverse to convert fits to idlstruct'
      return
  ENDIF 

  ninput = n_elements(infiles)
  noutput = n_elements(outfiles)

  IF ninput NE noutput THEN BEGIN 
      message,'Input file list must be same length output file list'
  ENDIF 

  IF NOT keyword_set(reverse) THEN BEGIN 
      idlstruct_files = infiles
      fits_files = outfiles
      idlstruct2fits_tofits, idlstruct_files, fits_files, status=status
  ENDIF ELSE BEGIN 
      fits_files = infiles
      idlstruct_files = outfiles
      idlstruct2fits_toidlstruct, fits_files, idlstruct_files, status=status
  ENDELSE 

END 


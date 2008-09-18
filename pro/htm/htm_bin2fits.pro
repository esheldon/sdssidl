;+
; NAME:
;  htm_bin2fits
;
; PURPOSE:
;  Convert a binary output file created by htm_match into a FITS file.
;
; CALLING SEQUENCE:
;  htm_bin2fits, file [, outfile, /remove]
;
; INPUTS:
;  file: The path to a binary file.
;
; OPTIONAL INPUTS:
;  outfile: The name of the output fits file.  If not entered,
;      the file is set to file+'.fits'
;
; KEYWORD PARAMETERS:
;  /remove: Remove the old binary file after converting.
;
; MODIFICATION HISTORY:
;  Created: 2006-08-15 Erin Sheldon, NYU
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

PRO htm_bin2fits, file, outfile, remove=remove

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: htm_bin2fits, frontfile, [outfile, /remove]'
      return
  ENDIF 

  IF n_elements(outfile) EQ 0 THEN outfile = file + '.fits'
  print
  print,'Old file file: ', file
  print,'Fits file: ',outfile

  ;; read the data
  struct = htm_readbin(file, status=status)
  IF status NE 0 THEN return

  mwrfits2, struct, outfile, /create, /destroy

  ;; remove bin file if requested
  IF keyword_set(remove) THEN BEGIN 
      print,'Removing old file: ',file
      spawn, 'rm '+file
  ENDIF 

  return
END 

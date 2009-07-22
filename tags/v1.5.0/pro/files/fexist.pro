
;+
;
; NAME:
;    FEXIST
;       
; PURPOSE:
;    Check if a file exists. This now uses built-in function file_test()
;    for versions of IDL >= 5.4  For efficiency the user should use that
;    function if possible.  Otherwise a test using the openr function is 
;    used. In that case directories cannot be checked.
;
; CALLING SEQUENCE:
;    file_exists=fexist(file, _extra=_extra)
;
; INPUTS: 
;    file: A full path filename.
;
; OPTIONAL INPUTS:
;    _extra: For IDL >= 5.4 these are the options to the file_test() function
;
; KEYWORD PARAMETERS:
;    NONE
;       
; OUTPUTS: 
;    1 or 0: 1 means the file exists, 0 means the file does not exist.
;
; OPTIONAL OUTPUTS:
;    NONE
;
; EXAMPLE:
;    outfile='test.dat'
;    ;; don't overwrite existing file
;    if fexist(file) then outfile = 'some other name'
;    openw, lun, file, /get_lun
;    ....etc.
;
; CALLED ROUTINES:
;    FILE_TEST (IDL >= 5.4)
;    OPENR (IDL < 5.4 )
;    FREE_LUN (IDL < 5.4 )
;
;	
;
; REVISION HISTORY:
;    07-DEC-2000 Creation. Erin Scott Sheldon
;    01-MAR-2003 Added use of FILE_TEST function                                      
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


function fexist, file, _extra=_extra

  if n_params() eq 0 then begin 
     message,'ERROR: no input file.  -Syntax: check=fexist(file, _extra=_extra)'
  endif 

  if float(!version.release) ge 5.4 then begin 
      return, file_test(file, _extra=_extra)
  endif else begin 
      error=0
      openr, lun, file, /get_lun,error=error
      if error ne 0 then begin
          return,0 
      endif else begin
          free_lun,lun
          return,1
      endelse 
  endelse 

end 


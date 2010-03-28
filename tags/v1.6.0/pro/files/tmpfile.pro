;+
; NAME:
;    TMPFILE
;
; PURPOSE:
;    Create a temporary, non-existent filename based on random numbers.
;
; CALLING SEQUENCE:
;	s = tmpfile(prefix=, suffix=, tmpdir=)
;
; OPTIONAL INPUTS:
;   prefix: A prefix to add to the file name.
;   suffix: A suffix for the file name.
;   tmpdir: A directory to prepend to the file name.
;
; Example:
;	IDL> print,tmpfile(prefix='temp-', suffix='.tmp', tmpdir='/tmp')
;   /tmp/temp-1187981506047-217287.tmp
;       
; Revision History:
;   Creator: Dave Johnston
;   Name was not unique enough.  Sometimes when two programs called this
;   one within one second, they could get the same file name. 
;   Cleaned up the code some.
;       2005-01-28, Erin Sheldon UofChicago
;   Added tmpdir
;       2007-08-24, Erin Sheldon, NYU
;
;-
;  Copyright (C) 2006  Dave Johnston
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of version 2 of the GNU General Public License as 
;    published by the Free Software Foundation.
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

function tmpfile, tmpdir=tmpdir, prefix=prefix, suffix=suffix, help=help

  if keyword_set(help) then begin
      doc_library, 'tmpfile'
      return, 0
  endif 

  if n_elements(prefix) eq 0 then prefix = 'tmpfile-'
  if n_elements(suffix) eq 0 then suffix = ''
  if n_elements(tmpdir) eq 0 then tmpdir = ''
  
  repeat begin
      
      numstr =  $
        strtrim(string(ulong64(1000*systime(1))), 2) + '-' + $
        strtrim(string(long(1000000*randomu(seed))), 2)
      
      file = path_join(tmpdir, prefix + numstr + suffix)
      
  endrep until not file_test(file)
  
  return, file
  
end 

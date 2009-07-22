;+
; NAME:
;  MKSTR
;
; PURPOSE:
;  Make a string of a give length.  Default is an empty string, but a value can be sent. Can optionally make string arrays.
;
; CATEGORY:
;  String manipulation.
;
; CALLING SEQUENCE:
;  str = mkstr(len [, d1, ..., d8, val=)
;
; INPUTS:
;  len: The length of of the string.
;
; OPTIONAL INPUTS:
;  d1...d8:  The size of up to 8 dimensions (IDL's limit).
;
; KEYWORD PARAMETERS:
;  val=:  The value of each string element.  By default it is a space ' '.
;
; OUTPUTS:
;  The string.
;
;
; MODIFICATION HISTORY:
;  Created 24-May-2005.  Author Erin Sheldon UofChicago
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


function mkstr, len, d1, d2, d3, d4, d5, d6, d7, d8, val=val

  np = n_params()
  if np lt 1 then begin 
      print,'-Syntax: str = mkstr(len [, d1, ..., d8, val=])'
      return,''
  endif 

  if n_elements(val) eq 0 then val=' '
  str = strjoin( replicate(val, len) )

  if np gt 1 then begin 
      case np of
          2: str = replicate(str, d1)
          3: str = replicate(str, d1, d2)
          4: str = replicate(str, d1, d2, d3)
          5: str = replicate(str, d1, d2, d3, d4)
          6: str = replicate(str, d1, d2, d3, d4, d5)
          7: str = replicate(str, d1, d2, d3, d4, d5, d6)
          8: str = replicate(str, d1, d2, d3, d4, d5, d6, d7)
          9: str = replicate(str, d1, d2, d3, d4, d5, d6, d7, d8)
          ELSE: message,'Arrays are allowed 1 - 8 dimensions'
      endcase 
  endif 

  return,str

end 

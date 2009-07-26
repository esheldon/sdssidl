;+
; NAME:
;  methods
;
; PURPOSE:
;  Display a list of methods for the input IDL class.
;
; CALLING SEQUENCE:
;  methods, classname, grepfor=
;
; INPUTS:
;  classname: The class name in string form.
;
; OPTIONAL INPUTS:
;  grepfor: A string to grep for in the list of methods.
;    Only lines that match the patter will be displayed.
;
; EXAMPLE:
;  IDL> methods, 'postgres'
;
;
; MODIFICATION HISTORY:
;   Created: 2006, Erin Sheldon, NYU
;
;-
;
;
;
;  Copyright (C) 2006  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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

pro methods, class_name, grepfor=grepfor

  if n_elements(class_name) eq 0 then begin 
      print,'-Syntax: methods, class_name, grepfor='
      print,'class_name and grepfor should be strings'
      return
  endif 

  which, class_name[0]+'__define', files=files, /silent

  if n_elements(files) eq 0 then begin 
      print,'Class file for: ',class_name[0],' not found'
      return
  endif 

  if not keyword_set(grepfor) then begin 
      ;; grep for methods ::

      ;; functions 
      regexp = 'FUNCTION.*'+class_name[0]+'::'
      spawn,['grep','-i',regexp,files[0]],/noshell
      regexp = 'PRO.*'+class_name[0]+'::'
      spawn,['grep','-i',regexp,files[0]],/noshell

      ;; also list the structure so we can see what gets inherited
      print
      print,'Inherited classes: '
      spawn,['grep','-i','INHERITS',files[0]],/noshell

  endif else begin 
      grep_string = class_name[0]+':: '+files[0]
      if n_elements(grepfor) ne 0 then begin 
          grep_string = grep_string + ' | grep -i '+grepfor[0]
      endif 
      spawn,'grep -i '+grep_string
  endelse 

end 

;+
; NAME:
;  doc_method
;
; PURPOSE:
;  Display the documentation for a method of an IDL class.  This requires that
;  the method has documention surrounded by 
;         ;docstart::classname::methodname
;         ;docend::classname::methodname
;
; CATEGORY:
;  IDL Classes,objects
;
; CALLING SEQUENCE:
;  doc_method, methodname, /class
;
; INPUTS:
;  methodname: Format:  classname::methodname
;
; KEYWORD PARAMETERS:
;  /class:  Display the doc for the class. This is equivalent to 
;         doc_library, 'classname__define'
;
; RESTRICTIONS:
;  The method doc must be formatted properly with text surrounded by
;         ;docstart::classname::methodname
;         ;docend::classname::methodname
;
; EXAMPLE:
;  doc_method, 'postgres::query
;
; MODIFICATION HISTORY:
;	Created: 2006, Erin Sheldon, NYU
;	2009-01-22: Use file_which to get the file path instead of which. E.S.
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

pro _doc_method_syntax
  print,'-Syntax: doc_method, method, /class'
  print,' method should be of the form class::method unless /class when the full doc is printed'
end 
pro doc_method, method, class=class

  if n_elements(method) eq 0 then begin 
      _doc_method_syntax
      return
  endif 

  msp = strsplit(method[0], '::', /extract)

  if keyword_set(class) then begin 
      class_file = msp[0]+'__define'
      doc_library,class_file
      return
  endif 


  if n_elements(msp) ne 2 then begin 
      _doc_method_syntax
      return
  endif 

  class_name = msp[0]
  method_name = msp[1]

  file=file_which(class_name+'__define.pro')
  if file eq '' then begin 
      message,'Class '+string(class_name)+' not found',/inf
	  return
  endif 

  ;; Now get everything between these
  ;; docstart::class_name::method_name 
  ;; docend::class_name::method_name 

  docstart = 'docstart::'+class_name+'::'+method_name
  docend   = 'docend::'+class_name+'::'+method_name

  openr, lun, file[0], /get_lun

  before_doc = 1
  in_doc = 0
  while ~ eof(lun) do begin 
      line = ''
      readf, lun, line

      if before_doc then begin 
          if stregex(line, docstart, /bool) then begin 
              before_doc = 0
              in_doc = 1
          endif 
      endif else begin 
          if stregex(line, docend, /bool) then begin 
              break
          endif else begin 
              print,line
          endelse 
      endelse 

  endwhile 

  free_lun, lun

end 

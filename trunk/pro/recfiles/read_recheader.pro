;+
; NAME:
;	read_recheader
;
;
; PURPOSE:
;	Read the header from a ".rec" records file. The result is a
;	structure containing the NROWS and STRUCTDEF tags.  .rec files are
;	usually created using the sfile package in esutil.
;
; CALLING SEQUENCE:
;	hstruct = read_recheader(filename/unit)
;
; INPUTS:
;	filename or unit: Name of the file to be read, or the logical unit of an
;		open file.
;
; OUTPUTS:
;  A structure containing the NROWS, STRUCTDEF tags.  The structdef is
;		itself a structure representing each rown in the file.
;
; EXAMPLE:
;	file = 'test.rec'
;	hstruct = read_recheader(file)
;
;
; MODIFICATION HISTORY:
;  Created  2010-02-24 Erin Sheldon, BNL
;
;-
;
;
;
;  Copyright (C) 2010  Erin Sheldon, BNL.  erin dot sheldon at gmail dot com
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

function rrh_expand_tilde_gdl_kludge, fname

	; expand_tilde fails in gdl, do a kludge
	dir=file_dirname(fname)
	newdir = expand_tilde(dir)
	return, path_join(newdir, file_basename(fname))
end

function rrh_openfile, filename, error=error, tname=tname

  tname = size(filename,/tname)

  if (tname eq 'INT' or tname eq 'UINT' or $
      tname eq 'LONG' or tname eq 'ULONG' or $
      tname eq 'LONG64' or tname eq 'ULONG64' or $
      tname eq 'BYTE') then begin 

      lun = filename
      error = 0
      return, lun

  endif else if tname EQ 'STRING' then begin 

	  fname = rrh_expand_tilde_gdl_kludge(filename)
      openr, lun, fname, /get_lun, error=error

      if error ne 0 then begin 
          print,'Error opening file '+filename+': '+!error_state.sys_msg
          return,-1
      endif else begin 
          return,lun
      endelse 

  endif else begin
 
      print,'Datatype of filename/unit '+tname+' is incorrect'
      error = -2000
      return,-1

  endelse 

end 



function rrh_get_dtype, dictstring
	; find the '_DTYPE' element in the dict string

	p=strpos(dictstring,'_DTYPE')
	if p eq -1 then message,'Expected _DTYPE in header dict string'

	; get passed the '_DTYPE' including closing quote
	rest = strmid(dictstring, p+7)

	; now skip the ':'
	p = strpos(rest, ':')
	if p eq -1 then message,"Expected '_DTYPE': in header dict string" 

	rest = strmid(rest, p+1)

	; now the dtype is either a string or is between the [ ]
	; but we will only allow structs so look for [ ]

	p1=strpos(rest,'[')
	p2=strpos(rest,']')

	if p1 eq -1 or p2 eq -1 then begin
		message,"exptected dtype of the form [('fname','..etc]"
	endif

	rest = strmid(rest,p1,p2)
	return, rest
end

FUNCTION read_recheader, filename, status=status
  
	status = 1
	if n_params() lt 1 then begin 
		print,'-Syntax: hdrStruct = read_idlheader(filename/unit, status=)'
		return,-1
	endif 

	;; Open the file or just set lun if filename is file unit
	lun = rrh_openfile(filename, error=error, tname=tname)

	if error ne 0 then return, -1

	hdrStruct = {_nrows:0ULL, $
				 _delim: 'none',$
				 _version:0.0}


	; read until we get a non-blank, which better be the NROWS = or SIZE =
	; line!

	line = ''
	readf, lun, line
	line=strtrim(line,2)

	while line eq '' do begin
		readf, lun, line
		line=strtrim(line,2)
	endwhile



	; ok, first non-empty line.  Look for the NROWS = or SIZE =
	tmp = strsplit(line, /extract)
	n=strupcase(tmp[0])
	if n ne 'NROWS' and n ne 'SIZE' or n_elements(tmp) ne 3 then begin
		message,'Exptected "NROWS = num" or "SIZE = num" at top'
	endif
	if tmp[1] ne '=' then begin
		message,'Exptected "NROWS = num" or "SIZE = num" at top'
	endif

	hdrstruct._nrows = ulong64(tmp[2])



	; now the rest must be a python dictionary {....}

	readf, lun, line
	line=strtrim(line,2)

	dict = ''
	while strupcase(line) ne 'END' do begin 
		dict += line

		readf, lun, line
		line=strtrim(line,2)
	endwhile 


	;; read the next line which should be empty
	readf, lun, line
	line=strtrim(line,2)
	if line ne '' then message,'Expected empty line after END header marker'

	if tname EQ 'STRING' then free_lun, lun

	p1 = strpos(dict,'{')
	p2 = strpos(dict,'}',/reverse_search)

	if (p1 eq -1) or (p2 eq -1) then begin
		message,'Expected open-close { } in header'
	endif

	dtype = rrh_get_dtype(dict)

	structdef = dtype2struct(dtype, endian=endian)

	hdrStruct = create_struct(hdrStruct, $
		                      '_dict', dict, $
							  '_dtype', dtype, $
							  '_endian', endian, $
							  '_structdef', structdef)

	status = 0
	return,hdrStruct

END 

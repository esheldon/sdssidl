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
;	filename: Name of the file to be read
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

function read_recheader, filename, noshell=noshell, status=status
	
	status = 1
	if n_params() lt 1 then begin 
		print,'-Syntax: hdrStruct = '+$
			'read_recheader(filename, /noshell, status=)'
		on_error,2
		message,'halting'
	endif 


	sdssidl_dir=getenv('SDSSIDL_DIR')
	if sdssidl_dir eq '' then message,'SDSSIDL_DIR is not set'

	pyfile=filepath(root=sdssidl_dir,sub=['python'],'rec2idl.py')

	if not keyword_set(noshell) then begin
		command='python '+pyfile+' '+filename
		spawn, command, res, err
		if err ne '' then message,'Command failed: "'+command+'"'
	endif else begin
		command=['python',pyfile,filename]
		spawn, command, res, err, /noshell
		if err ne '' then message,'Command failed: "'+command+'"'
	endelse


	res = strjoin(res,' ')

	; this version has only the strings in it
	st=eval(res)

	; try to evaluate the arguments.  We treat _dtype specially, calling
	; dtype2struct, and we place it in the _structdef argument.  Arguments
	; that do not eval are placed in the struct as strings.

	tn=tag_names(st)
	ntags = n_elements(tn)

	for i=0L, ntags-1 do begin

		if tn[i] eq '_DTYPE' then begin
			dtype = st.(i)
			val=dtype2struct(dtype, endian=endian)
			name = '_structdef'
		endif else begin

			name = tn[i]
			stringvar = st.(i)
			command = 'val = '+stringvar
			if not execute(command) then begin
				print,'could not eval: '+stringvar
				val = stringvar
			endif

		endelse

		if n_elements(struct) eq 0 then begin
			struct=create_struct(name, val)
		endif else begin
			struct=create_struct(struct,name,val)
		endelse
	endfor

	; add in endian and dtype
	struct = create_struct($
		struct, $
		'_dtype', dtype, $
		'_endian', endian)

	return, struct
end

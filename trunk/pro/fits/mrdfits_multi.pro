;+
; NAME:
;  MRDFITS_MULTI
;
; PURPOSE:
;  Read binary tables from multiple FITS files using mrdfits. Return
;  a single structure containing all data.
;
; CALLING SEQUENCE:
;  struct = mrdfits_multi(files, $
;                         extension=, $
;                         /diff, $
;                         columns=, $
;                         /unsigned=, $
;                         /compress, $
;                         count=, $
;                         /silent, $
;                         error_action=, $
;                         status=)
;
;
; INPUTS:
;  files: A set of fits file names.
;
; OPTIONAL INPUTS:
;  extension=: the extension to read from each file. Default is 1.
;  columns: The columns to read.
;
; KEYWORD PARAMETERS:
;  /diff:  This informs that the structures from each file may be different. A
;         slower but safer method is used that defines the output structure
;         based upon the first file and just copies the matching tags
;         afterward.
;  /unsigned,/compress, /silent, error_action=, status=: See mrdfits
;
; OUTPUTS:
;  Combined structure.
;
; OPTIONAL OUTPUTS:
;  count: the total count of rows read.
;
; MODIFICATION HISTORY:
;  Author: Erin Sheldon, NYU
;  Documented: 2006-July-21.  E.S.
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

function mrdfits_multi, infiles, $
                        extension=extension, $
                        diff=diff, $
                        count=count, $
                        columns=columns, $
                        unsigned=unsigned, $
                        compress=compress, $
                        silent=silent, $
                        error_action = error_action, $
                        status=status

	count=0
	if n_params() lt 1 then begin 
		print,'-Syntax: st = mrdfits_multi(files, extension=, /diff, extension=, columns=, /unsigned, /compress, count=, error_action=, status=, /silent)'
		print
		print,'Must all be the same structure unless /diff is set. If /diff then'
		print,' the structure is taken from the *first* file and matching tags are copied in'
		return,-1
	endif 

	delvarx,struct
	status=1

	if n_elements(extension) eq 0 then extension=1
	extstr='['+string(extension,f='(i0)')+']'

	;; See which files actually exist
	nfiles = n_elements(infiles)
	for i=0l, nfiles-1 do begin
		if fexist(infiles[i]) then begin 
			add_arrval, infiles[i], files
		endif else begin 
			print,'File "'+infiles[i]+'" does not exist. Skipping'
		endelse 
	endfor 

	nfiles = n_elements(files)
	if nfiles eq 0 then begin  
		message,'None of the files were found',/inf
		return,-1
	endif 

	;; just one file?
	if nfiles eq 1 then begin 
		struct = mrdfits(files, extension, $
			columns=columns, $
			unsigned=unsigned, $
			compress=compress, $
			silent=silent, $
			error_action = error_action, $
			status=status)
		if n_tags(struct) ne 0 then begin
			count=n_elements(struct)
		endif
		return,struct
	end 

	;; first go through, read the headers and find how many objects
	;; are in each.  Then allocate a big struct and copy it in.

	if not keyword_set(silent) then begin
		print
		print,'Reading headers'
	endif 
	ntotal = 0LL
	numlist = lonarr(nfiles)
	for i=0l, nfiles-1 do begin 

		; we already know the files exists from above
		hdr = headfits(files[i], ext=extension)
		; make sure we were able to read the header.
		; it should be a string array
		if size(hdr,/type) eq 7 then begin
			numlist[i] = sxpar(hdr,'naxis2')
			ntotal = ntotal + numlist[i]
		endif else begin
			message,'Could not read header',/inf
			return,-1
		endelse

	endfor 

	if not keyword_set(silent) then begin
		print
		print,'Total number of rows: ',ntotal,f='(a,i0)'
	endif 

	if not keyword_set(diff) then begin 

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; No indication structs are different
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		beg =0L
		for i=0l, nfiles-1 do begin 

			if numlist[i] ne 0 then begin 
                if not keyword_set(silent) then begin
                    print,'Reading File: ',files[i],extstr,form='(a,a,a)'
                endif
				t = mrdfits(files[i], extension, $
					columns=columns, $
					unsigned=unsigned, $
					compress=compress, $
					silent=silent, $
					error_action = error_action, $
					status=status)

				if status ne 0 then begin 
					message,'Error reading file: ',files[i],/inf
					message,'Returning -1 and nonzero status',/inf
					return,-1
				endif 
				if n_tags(t) eq 0 then begin
					message,'Result is not a structure',/inf
					message,'Returning -1 and nonzero status',/inf
					return,-1
				endif

				if i eq 0 then begin 
					struct=replicate(t[0], ntotal)
					struct[beg:beg+numlist[i]-1] = t
					beg = beg+numlist[i]
				endif else begin 
					struct[beg:beg+numlist[i]-1] = t
					beg = beg+numlist[i]
				endelse 


				t = 0

			endif else begin  
				print,'File is empty: ',files[i],form='(a,a)'
			endelse 
		endfor 
	endif else begin 

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; User has indicated that the structures may be 
		;; different.  Use slower but safer method.
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		beg =0L
		for i=0l, nfiles-1 do begin 

			if numlist[i] ne 0 then begin 
                if not keyword_set(silent) then begin
                    print,'Reading File: ',files[i],form='(a,a)'
                endif

				t = mrdfits(files[i], extension, $
					columns=columns, $
					unsigned=unsigned, $
					compress=compress, $
					silent=silent, $
					error_action = error_action, $
					status=status)

				if status ne 0 then begin 
					message,'Error reading file: ',files[i],/inf
					message,'Returning -1 and nonzero status',/inf
					return,-1
				endif 
				if n_tags(t) eq 0 then begin
					message,'Result is not a structure',/inf
					message,'Returning -1 and nonzero status',/inf
					return,-1
				endif


				if i eq 0 then begin 
					struct_tags = tag_names(t[0])
					struct_ind  = lindgen(n_elements(struct_tags))

					;; "zero" the struct for those tags that
					;; will not be copied.
					struct = t[0]
					zero_struct,struct

					;; simple copy on first one
					struct=replicate(struct, ntotal)
					struct[beg:beg+numlist[i]-1] = t
					beg = beg+numlist[i]
				endif else begin 

					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;; must match up the tags for possibly different struct
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					newtags = tag_names(t[0])
					match, struct_tags, newtags, mstr, mnew

					if mstr[0] ne -1 then begin 
						nmatch = n_elements(mnew)
						;; loop over tags
						for j=0l, nmatch-1 do begin 
							si = mstr[j]
							sn = mnew[j]
							struct[beg:beg+numlist[i]-1].(si) = t.(sn)
						endfor 
					endif else begin 
						message,'No compatible tags from file: '+files[i],/inf
						message,'Structure will be zero for rows [',beg,', ',numlist[i]-1,']', $
							form='(a,i0,a,i0,a)'
					endelse 

					beg = beg+numlist[i]
				endelse 

				t = 0

			endif else begin  
				print,'File is empty: ',files[i],form='(a,a)'
			endelse 
		endfor 
	endelse 

	count = ntotal
	status=0
	return,struct

end 

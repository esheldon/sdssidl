;+
;  NAME:
;    sdss_objname
;  PURPOSE:
;    Generate a standard sdss filename chunk from SDSS id info.
;  CALLING SEQUENCE:
;    name = sdss_objname(run,rerun,camcol,field,id, prefix=, suffix=)
;	OR a structure with id info
;    name = sdss_objname(structure)
;  EXAMPLE:
;    IDL> print,sdss_objname(756,137,2,125,221, prefix='myobject-')
;    myobject-000756-137-2-0125-00221
;
;  CREATED:
;   Erin Sheldon, UofMichigan, ????
;	Added suffix keyword, cleaned up a little.  Can use either prefix= or 
;		the old front= keyword. 2009-03-19. E.S. BNL
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

function sdss_objname, run, rerun, camcol, field, id, front=front, prefix=prefix, suffix=suffix

	; if input is a structure
	if n_elements(run) ne 0 then begin
		if size(run,/tname) eq 'STRUCT' then begin
			return, sdss_objname($
				run.run, run.rerun, run.camcol, run.field, run.id, $
				front=front, prefix=prefix, suffix=suffix)
		endif
	endif

    if n_params() lt 5 then begin
        on_error, 2
        print,'-Syntax: name = sdss_objname(run, rerun, camcol, field, id, front=, suffix=)'
        print
        message,'Halting'
    endif

    rstr = run2string(run)
    rrstr = ntostr(rerun)
    cstr = ntostr(camcol)
    fstr = field2string(field)
    idstr = id2string(id)

    object_name = rstr+'-'+rrstr+'-'+cstr+'-'+fstr+'-'+idstr

	if n_elements(prefix) ne 0 then begin
		object_name = prefix + object_name
	endif else if n_elements(front) ne 0 then begin
		; old front keyword
		object_name = front + object_name
	endif
	if n_elements(suffix) ne 0 then object_name = object_name + suffix
  
    return, object_name

end 

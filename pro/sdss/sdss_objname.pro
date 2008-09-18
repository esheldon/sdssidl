;+
;  NAME:
;    sdss_objname
;  PURPOSE:
;    Generate a standard sdss filename chunk from SDSS id info.
;  CALLING SEQUENCE:
;    name = sdss_objname(run,rerun,camcol,field,id,front=)
;  EXAMPLE:
;    IDL> print,sdss_objname(756,137,2,125,221, front='myobject-')
;    myobject-000756-137-2-0125-00221
;
;  CREATED:
;    Erin Sheldon, UofMichigan, ????
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

function sdss_objname, run, rerun, camcol, field, id, front=front_in

    if n_params() lt 5 then begin
        on_error, 2
        print,'-Syntax: name = sdss_objname(run,rerun,camcol,field,id,front=)'
        print
        message,'Halting'
    endif

    if n_elements(front_in) eq 0 then front = '' else front=front_in+'-'
    rstr = run2string(run)
    rrstr = ntostr(rerun)
    cstr = ntostr(camcol)
    fstr = field2string(field)
    idstr = id2string(id)

    object_name = front+rstr+'-'+rrstr+'-'+cstr+'-'+fstr+'-'+idstr
  
    return, object_name

end 


;+
;
; NAME:
;   rowcol2munu 
;       
; PURPOSE:
;    convert from row-column to great circle coordinates (mu,nu)
;
; CALLING SEQUENCE:
;    rowcol2munu, trans, field, row, col, mu, nu, ri=ri
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    row,col: the row,column to be converted to mu,nu
;
; OPTIONAL INPUTS:
;    ri: the r-i color of the objects. 
;
; OUTPUTS: 
;    mu,nu: SDSS great circle coords.
;
; OPTIONAL OUTPUTS:
;    status: 1 for success, 0 for failure
;
; REVISION HISTORY:
;    14-NOV-2000 Creation.  Erin Scott Sheldon UofMich
;       
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



pro rowcol2munu, trans, field, row, col, mu, nu, ri=ri, status=status

    if n_params() lt 6 then begin 
        on_error, 2
        print,'-Syntax: rowcol2munu, trans, field, row, col, mu, nu, ri=, status='
        print
        message,'Halting'
    endif 

    trans=obj_new('sdss_transform')
    trans->rowcol2munu, trans, field, row, col, mu, nu, ri=ri, status=status
    obj_destroy, trans

end

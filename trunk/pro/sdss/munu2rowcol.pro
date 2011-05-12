
;+
;
; NAME:
;   munu2rowcol 
;       
; PURPOSE:
;    convert from great circle coordinates to row/col, ignoring higher
;    order terms in the transformation. Good to .1 pixels or so
;
; CALLING SEQUENCE:
;    munu2rowcol, trans, field, mu, nu, row, col, ri=, /nonlinear, status=
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    mu,nu: great circle coordinates
;
; Optional Inputs:
;    ri=: the r-i color used in color term of transform
;    /nonlinear:  use a non-linear transform.
;
; OUTPUTS: 
;    row,col: row/col in the bandpass of the trans structure
;
; OPTIONAL OUTPUTS:
;    status: 1 for success, 0 for failure
;
; REVISION HISTORY:
;    15-AUG-2002 Creation.  Erin Scott Sheldon UofMich
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



pro munu2rowcol, trans, field, mu_in, nu_in, row, col, ri=ri, nonlinear=nonlinear, status=status

	if n_params() lt 6 then begin 
		on_error, 2
		print,'-Syntax: munu2rowcol, trans, field, mu, nu, row, col, ri=, /nonlinear, status='
		print
		message,'Halting'
	endif 

    trans=obj_new('sdss_transform')
    trans->munu2rowcol, trans, field, mu_in, nu_in, row, col, ri=ri, nonlinear=nonlinear, status=status
    obj_destroy, trans

end 


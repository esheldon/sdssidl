
;+
;
; NAME:
;   radec2rowcol
;       
; PURPOSE:
;    convert from equatorial coordinates to row/col, ignoring higher
;    order terms in the transformation. Good to .1 pixels or so. For this
;    crude program, you need to read in the transformation file and know
;    the sdss field to do the transformation.  
;
; CALLING SEQUENCE:
;    radec2rowcol, trans, node, inc, field, ra, dec, row, col, status=
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          sdss_read() to get the astrans file.
;    field: the field
;    ra,dec: equatorial coordinates
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
; PROCEDURE: 
;    We know the ra/dec are in field 562 of run 756
;    run=756
;    rerun=44
;    camcol=3
;    field = 562
;    band=2
;    trans=sdss_read('astrans',run,camcol,band=band,node=node,inc=inc)
;    radec2rowcol, trans, node, inc, field, ra, dec, row, col
;	
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

pro radec2rowcol, trans, node, inc, field, ra, dec, row, col, ri=ri, nonlinear=nonlinear, status=status

    sdssidl_setup
    !sdss->eq2rowcol, trans, node, inc, field, ra, dec, row, col, ri=ri, nonlinear=nonlinear, status=status

end 

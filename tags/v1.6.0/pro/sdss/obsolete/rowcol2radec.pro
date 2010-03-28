;+
;
; NAME:
;   ROWCOL2RADEC
;       
; PURPOSE:
;    convert from row/col to equatorial coords, ignoring higher
;    order terms in the transformation. For this
;    crude program, you need to read in the transformation file and know
;    the sdss field to do the transformation.  
;
; CALLING SEQUENCE:
;    rowcol2radec, trans, node, inc, field, row, col, ra, dec, status=
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    row,col: sdss row/col coordinates for a given bandpass
;
; OUTPUTS: 
;    ra,dec: equatorial coordinates
;
; OPTIONAL OUTPUTS:
;    status: 1 for success, 0 for failure
;
; PROCEDURE: 
;    The row,col are from 562 of run 756, the r-band
;    run=756
;    rerun=44
;    camcol=3
;    field = 562
;    bandpass=2
;    trans=sdss_read('astrans',run,camcol,bandpass=bandpass,node=node,inc=inc)
;    owcol2radec, trans, node, inc, field, row, col, ra, dec
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


pro rowcol2radec, trans, node, inc, field, row, col, ra, dec, status=status

    sdssidl_setup
    !sdss->rowcol2eq, trans, node, inc, field, row, col, ra, dec, status=status

end 

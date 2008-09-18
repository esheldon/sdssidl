;+
;
; NAME:
;    read_astrans
;       
; PURPOSE:
;    Read from an asTrans astrometry transformation file for 
;    given run, camcol, and bandpass. Can be used to 
;    transform between (row,col), or CCD coordinates, and (mu,nu),
;    or great circle coords.
;
; CALLING SEQUENCE:
;    st = sf->astrans_read(run, camcol, bandpass, rerun=, file=, node=, inc=)
;    
; INPUTS: 
;    run, camcol: run and camcol numbers.
;    bandpass: The number of the bandpass.  u,g,r,i,z->0,1,2,3,4
;
; OPTIONAL INPUTS:
;    rerun: SDSS rerun.
;    file: the astrans file to read.
;
; KEYWORD PARAMETERS:
;    /silent: No messages printed.
;       
; OUTPUTS: 
;    trans: The astrometry structure.
;
; OPTIONAL OUTPUTS:
;    node: The node position of this stripe. 
;    inc: Inclination of this stripe. 
;       node and inc required by rowcol2munu.pro, and gc2eq.pro or
;       gc2survey.pro
;
; PROCEDURE: 
;    asTrans files conain info for each camcol/field/bandpass for a given
;    column.  The different camcol/bandpasses are in different 
;    extensions.  See http://www-sdss.fnal.gov:8000/dm/flatFiles/asTrans.html
;    for details.
;	
;
; REVISION HISTORY:
;    Created: 23-OCT-2000 Erin Scott Sheldon
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



function read_astrans, run, camcol, bandpass, node=node, inc=inc, rerun=rerun, silent=silent, indir=indir, dir=dir, file=file

    sdssidl_setup
    return, !sdss->read('astrans', run, camcol, bandpass=bandpass, node=node, inc=inc, rerun=rerun, silent=silent, indir=indir, dir=dir)

END 

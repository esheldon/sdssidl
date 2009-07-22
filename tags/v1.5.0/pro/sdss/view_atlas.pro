;+
;
; NAME:
;       VIEW_ATLAS
; PURPOSE:
;	Display the atlas images for an object.  Images are read using the
;       dynamically linked DLM rdAtlas (type rdAtlas at the prompt for 
;       syntax).
;
; CALLING SEQUENCE:
;        view_atlas, objStruct -OR- run, rerun, camcol, field, id, 
;                    clr=, 
;                    dir=, 
;                    index=, 
;                    imu=, img=, imr=, imi=, imz=, 
;                    imtot=, 
;                    col0=, row0=, dcol=, drow=, ncol=, nrow=,
;                    status=, 
;                    /info, 
;                    maguse=, 
;                    /hideradec, 
;                    /silent,
;                    _extra=_extra
;
; INPUTS: The user can either input the 
;                  run,rerun,camcol,field,id: Unique SDSS identifier
;         OR
;                  objStruct: a photo structure containig the id information.
; 
; OPTIONAL INPUTS: 
;         clr: indices of colors to use.  Can be an array. Default is all
;              bands. Follows photo convention: 0,1,2,3,4 -> u,g,r,i,z
;
;         dir: directory of atlas images. THIS IS NOW OPTIONAL, since this
;              information is stored in a system variable.
;         index: Index for when input structure is an array.  Defaults to zero.
;         _extra=extra: extra keywords for plotting.
;
; KEYWORD PARAMETERS:
;         /info: add information to the plot, such as magnitudes, colors,
;                ra/dec, and position angle when the object is a double.
;	  /hideradec: will not include ra dec in titles 
;         /silent: will run silently unless bad images or missing
;                 images are found
;
; OPTIONAL OUTPUTS: 
;         imu=, img=, imr=, imi=, imz=:  The images for individual
;                 colors.  
;         imtot=:  An composite image containing all requested bands.  
;         row0=, col0=:  objc_rowc, objc_colc position of bottom left corner
;                        of atlas image.  
;         dcol=, drow=: positional difference between each band and the r-band
;         nrow=, ncol=: number of columns, rows in each image.
;         status= The exit status.  0 is good, anything else is bad
;
; RESTRICTIONS:
;    The C-function rdAtlas must be compiled, and the DLM directory must be in
;    the user's $IDL_DLM_PATH.  Also, the directory configuration variable
;        DATA_DIR
;    must be set and the runs must be visible from there with the usual
;    directory structure.  If not, then the directory can be sent with the
;    dir=dir keyword.
; 
; EXAMPLES:
;   Look at the atlas images in each bandpass for first 10 objects in frame 35
;   of run 109
;
; IDL> run=745 & rerun=20 & camcol=3 & field = 123 & id = 27
; IDL> view_atlas, run, rerun, camcol, field, id
;
;   Use an input struct. Save the r-band image.
;
; IDL> view_atlas, struct[27], clr=2, imr=red_image
; 
;
; REVISION HISTORY:
;       Author:
;       16-Nov-2004: Erin Scott Sheldon UChicago  
;                        Based on obsolete get_atlas.pro
;       Renamed to view_spec1d.  30-Nov-2006, ES NYU
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

PRO view_atlas, p1, p2, p3, p4, p5, rerun=rerunin, $
                clr=clr, $
                dir=dir, $
                index=index, $
                $
                imu=imu, $
                img=img, $
                imr=imr, $
                imi=imi, $
                imz=imz, $
                imtot=imtot, $
                $
                col0=col0, row0=row0, $
                dcol=dcol, drow=drow, $
                ncol=ncol, nrow=nrow, $
                $
                status=status, $
                $
                info=info, $
                maguse=maguse, $
                hideradec=hideradec, $
                silent=silent,$
                _extra=_extra

   
    sdssidl_setup

    ;; Deal with old syntax
    if n_params() eq 5 then begin
        ros=p1 & rerun=p2 & camcol=p3 & field=p4 & id=p5
    endif else if n_params() eq 4 then begin
        ros=p1 & camcol=p2 & field=p3 & id=p4
        if n_elements(rerunin) ne 0 then rerun=rerunin
    endif else begin
        if n_elements(p1) ne 0 then ros = p1        
    endelse

    comm = '!sdss->atlas_view, ros, camcol, field, id, rerun=rerun, '
    comm = comm + 'clr=clr, '
    comm = comm + 'dir=dir, '
    comm = comm + 'index=index, '

    if arg_present(imu) then comm = comm + 'imu=imu, '
    if arg_present(img) then comm = comm + 'img=img, '
    if arg_present(imr) then comm = comm + 'imr=imr, '
    if arg_present(imi) then comm = comm + 'imi=imi, '
    if arg_present(imz) then comm = comm + 'imz=imz, '
    if arg_present(imtot) then comm = comm + 'imtot=imtot, '
                
                
    comm = comm + 'col0=col0, row0=row0, '
    comm = comm + 'dcol=dcol, drow=drow, '
    comm = comm + 'ncol=ncol, nrow=nrow, '
                
    if arg_present(atlas_struct) then comm=comm + 'atlas_struct=atlas_struct, '
    comm = comm + 'maguse=maguse, hideradec=hideradec, '
    comm = comm + 'status=status, '
    comm = comm + '_extra=_extra, '            
    comm = comm + 'silent=silent'

    if not execute(comm) then begin
        on_error, 2
        message,'Could not run !sdss->atlas_read'
    endif
    return

END 

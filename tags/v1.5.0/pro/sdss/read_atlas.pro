;+
; NAME:
;  READ_ATLAS
;
;
; PURPOSE: 
;  Read atlas images specified by SDSS run,rerun,camcol,field,id,
;  or from a structure containing that info.  This is a wrapper for the rdAtlas
;  procedure which reads from input file name and id (type rdAtlas at a prompt
;  to see the syntax).  That program is more efficient but you must feed it
;  the atlas image file name.
;
;
; CATEGORY:
;  SDSS specific routine
;
;
; CALLING SEQUENCE:
;    read_atlas, objStruct -OR- run, camcol, field, id, rerun=,
;                image=, 
;                atlas_struct=,
;                imu=, img=, imr=, imi=, imz=, 
;                clr=, 
;                index=, 
;                dir=, 
;                col0=, row0=, dcol=, drow=, ncol=, nrow=,
;                atlas_struct=, 
;                status=, 
;                /silent
;
;
;
; INPUTS: The user can either input the 
;                  run,rerun,camcol,field,id: Unique SDSS identifier
;         OR
;                  objStruct: a photo structure containig the id information.
;
;
; OPTIONAL KEYWORD INPUTS:
;         clr: The bandpass to read. Will be returned in the image keyword.
;              must be a scalar.
;         dir: directory of atlas images. THIS IS OPTIONAL, since this
;              information can be generated from the id info and the config
;              variable DATA_DIR
;         index: Index for when input structure is an array.  Defaults to zero,
;              the first element in struct.
;
;
; OUTPUT KEYWORDS:
;         image: 
;            An array containing the image(s).  If clr= is sent, just that
;            bandpass is returned through this keyword. Otherwise, If one of
;            the imr=imr type arguments is not sent and atlas_struct is not
;            present, then all images are copied into this argument as a
;            [5,nx,ny] array.
;
;         atlas_struct: 
;            A structure with all images and statistics.
;
;         imu=, img=, imr=, imi=, imz=:  
;            The images for individual bandpasses. Just set these equal to a 
;            named variable to return the image you need.
;
;         row0=, col0=:  objc_rowc, objc_colc position of bottom left corner
;                        of atlas cutout in the original image.
;         dcol=, drow=: positional difference between each band and the r-band
;         nrow=, ncol=: number of columns, rows in each image.
;         status= The exit status.  0 is good, anything else is bad
;
; KEYWORD PARAMETERS:
;         /silent: will run silently unless bad images or missing
;                 images are found
;
; RESTRICTIONS:
;    The C-function rdAtlas must be compiled, and the DLM directory must be in
;    the user's $IDL_DLM_PATH.  Also, the directory configuration variable
;        DATA_DIR
;    must be set and the runs must be visible from there with the usual
;    directory structure.  If not, then the directory can be sent with the
;    dir=dir keyword.
;
; EXAMPLE:
;  - Read an r-band atlas image into memory
;    IDL> read_atlas, run, rerun, camcol, field, id, imr=imr
;  - Read using a structure; get all images in atlas_struct
;    IDL> read_atlas, objStruct, atlas_struct=as
;
;
; MODIFICATION HISTORY:
;   Created: 17-Nov-2004 from old get_atlas.  Erin Sheldon, UChicago.
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

pro read_atlas, p1, p2, p3, p4, p5, rerun=rerunin, $
                dir=dir, $
                index=index, $
                $
                imu=imu, $
                img=img, $
                imr=imr, $
                imi=imi, $
                imz=imz, $
                $
                clr=clr, $
                image=image, $
                $
                col0=col0, row0=row0, $
                dcol=dcol, drow=drow, $
                ncol=ncol, nrow=nrow, $
                $
                atlas_struct=atlas_struct, $
                status=status, $
                $
                silent=silent

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

    comm = '!sdss->atlas_read, ros, camcol, field, id, rerun=rerun, '
    comm = comm + 'dir=dir, '
    comm = comm + 'index=index, '

    if arg_present(imu) then comm = comm + 'imu=imu, '
    if arg_present(img) then comm = comm + 'img=img, '
    if arg_present(imr) then comm = comm + 'imr=imr, '
    if arg_present(imi) then comm = comm + 'imi=imi, '
    if arg_present(imz) then comm = comm + 'imz=imz, '
                
    comm = comm + 'clr=clr, '
    if arg_present(image) then comm = comm + 'image=image, '
                
    comm = comm + 'col0=col0, row0=row0, '
    comm = comm + 'dcol=dcol, drow=drow, '
    comm = comm + 'ncol=ncol, nrow=nrow, '
                
    if arg_present(atlas_struct) then comm=comm + 'atlas_struct=atlas_struct, '
    comm = comm + 'status=status, '
                
    comm = comm + 'silent=silent'

    if not execute(comm) then message,'Could not run !sdss->atlas_read'
    return
END 

;+
; NAME:
;  sdss_recframe
;
; PURPOSE:
;  Reconstruct an SDSS frame from atlas images.
;
; CALLING SEQUENCE:
;  sdss_recframe, run, camcol, field, rerun=, imu=,img=,imr=,imi=,imz=,
;    idlist=, struct=, status= 
;
; INPUTS:
;  run,camcol,field,rerun=: SDSS field id.
;
; OPTIONAL INPUTS:
;  idlist:  The list of objects to use for reconstruction by SDSS id. If not
;     sent, this list is chosen as all objects in the field with no children.
;
; OUTPUTS:
;  imu,img,imr,imi,imz=: The optional images returned.  If the keyword arg
;     is not present, then that image is not retrieved from disk.  This can
;     save considerable time.
;
; RESTRICTIONS:
;  The sdss atlas images and fpobjc files must be on disk and 
;  in the directories listed in the sdssidl config file.
; 
; EXAMPLE:
;  sdss_recframe, 756, 3, 125, imr=imr
;
; MODIFICATION HISTORY:
;  Created sometime in 2004.  Documented 2006. Erin Sheldon, NYU
;
;-
function sdss_recframe_getidlist, run, camcol, field, rerun=rerun, $
    struct=struct, silent=silent

    common sdss_recframe_common, ftype
    if n_elements(ftype) eq 0 then ftype='fpobjc'

    ;; Try to read in the default filetype
    struct = sdss_read(ftype,run, camcol, $
                       rerun=rerun, field=field, $
                       /nomodrow, $
                       verbose=0)
    if n_tags(struct) eq 0 then begin
        if not keyword_set(silent) then begin
            name=sdss_file(ftype,run, camcol, rerun=rerun, field=field)
            message,string(f='("Did not find fpobjc file: ",a)',name),/inf
        endif
        return,-1
    endif
    
    w = where(struct.nchild EQ 0, nw)
    if nw eq 0 then begin 
        if not keyword_set(silent) then message,'No children found',/inf
        return, -1
    endif 

    return, struct[w].id
    
end 
pro sdss_recframe, run, camcol, field, rerun=rerun, $
                   imu=imu, img=img, imr=imr, imi=imi, imz=imz, $
                   idlist=idlist, $
                   struct=struct, $
                   status=status
               
    status=1
    if n_params() lt 3 then begin 
        print,'-Syntax: sdss_recframe, run, camcol, field, rerun=, '
        print,'                        imu=, img=, imr=, imi=, imz=, '
        print,'                        idlist=, struct=, status='
        return
    endif 

    sdssidl_setup,/silent

    atlas_file = sdss_file('fpatlas', run, camcol, rerun=rerun, fields=field)

    if not fexist(atlas_file) then begin 
        if not keyword_set(silent) then begin
            message,'Atlas file not found: '+atlas_file,/inf
        endif
        return
    endif 

    ; idlist not sent, try to get one from catalogs
    if n_elements(idlist) eq 0 then begin 
        idlist = sdss_recframe_getidlist(run,camcol,field,rerun=rerun, $
                                         struct=struct)
        if idlist[0] eq -1 then return
    endif 

    command = 'recframe, atlas_file, idlist'
    if arg_present(imu) then command = command + ', imu=imu'
    if arg_present(img) then command = command + ', img=img'
    if arg_present(imr) then command = command + ', imr=imr'
    if arg_present(imi) then command = command + ', imi=imi'
    if arg_present(imz) then command = command + ', imz=imz'

    command = command + ', status=status'
    if not execute(command) then begin 
        message,'Error executing recframe',/inf
        return
    endif 

end 

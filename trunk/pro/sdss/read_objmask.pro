;+
; NAME:
;  read_objmask()
;
; PURPOSE:
;  Read in object mask data for objects in an SDSS atlas file.
;
; CATEGORY:
;  SDSS routine.
;
; CALLING SEQUENCE:
;  st = read_objmask(run, camcol, field, rerun=, idlist=, status=)
;
; INPUTS:
;  run,camcol,field: SDSS field info.
;
; OPTIONAL INPUTS:
;  rerun: The sdss rerun. If not sent it is taken as the latest available.
;  idlist: List of sdss ids.  If not sent all are retrieved.
;
; OUTPUTS:
;  Structure with mask info for each object.
;    run, rerun, camcol, field, id, 
;    row0[5], col0[5], rowmax[5], colmax[5], drow[5], dcol[5]
;
; OPTIONAL OUTPUTS:
;  status: 0 for success.
;
; RESTRICTIONS:
;  Must have DLM rdObjmask compiled and linked in.
;
; MODIFICATION HISTORY:
;  Created: 2007-02-26, Erin Sheldon, NYU
;
;-


function read_objmask, run, camcol, field ,$ 
    rerun=rerun, $
    idlist=idlist, $
    status=status

    sdssidl_setup

    sf = obj_new('sdss_files')
    struct = sf->objmask_read(run, camcol, field, $
        rerun=rerun, idlist=idlist, status=status)

    obj_destroy, sf
    return, struct
END 

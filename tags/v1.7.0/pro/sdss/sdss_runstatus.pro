;+
; NAME:
;    sdss_runstatus()
;
; PURPOSE:
;    Return the runstatus information structure.  Load the data if needed.
;    The runstatus structure contains information about each run on disk,
;    such as stripe, strip, rerun, and whether certain files are found.
;
; CATEGORY:
;    SDSS utility
;
; CALLING SEQUENCE:
;    rs = sdss_runstatus(exists=, /reload)
;
; KEYWORD PARAMETERS:
;    /reload:  Reload the runstatus file.
;
; OUTPUTS:
;    The runstatus structure.
;
; OPTIONAL OUTPUTS:
;    exists=:  1 if the data was loaded, 0 if not.
;    /silent:  Print nothing.
;
; COMMON BLOCKS:
;    runstatus_block, runstatus
; EXAMPLE:
;    rs = sdss_runstatus()
;    runs = rs.run
;    reruns = rs.reruns
;
; MODIFICATION HISTORY:
;    Some time in 2004.  Erin Sheldon, NYU
;
;-
;  Copyright (C) 2006  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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


function sdss_runstatus_load, silent=silent
    common runstatus_block, runstatus

    runstatus_file = sdssidl_config('run_status_file', exists=rexists)

    if not rexists then begin 
        if not keyword_set(silent) then begin 
            message,'runstatus file is undefined in config.  not loading',/inf
        endif 
        return,0
    endif 

    if not fexist(runstatus_file) then begin 
        if not keyword_set(silent) then begin 
            message,'runstatus file '+ntostr(runstatus_file)+' does not exist',/inf
            message,'Use sdssidl_create_runstatus.pro to create',/inf
        endif 
        return,0
    endif 

    if not keyword_set(silent) then begin
        print,'Loading runstatus file: ',runstatus_file
    endif
    struct = mrdfits(runstatus_file, 1,status=status, /silent)
    if status ne 0 then begin 
        if not keyword_set(silent) then begin 
            message,'Failed to load runstatus file: '+ntostr(runstatus_file),/inf
        endif 
        return,0
    endif 

    ; only copy if we found it.
    runstatus = temporary(struct)

    ;; for backward compatability, but should not be used internally
    defsysv, '!run_status', exist=rexist
    if not rexist then begin 
        defsysv, '!run_status', runstatus
    endif

    return,1

end 

function sdss_runstatus, reload=reload, exists=exists, silent=silent
    common runstatus_block, runstatus

    if n_elements(runstatus) eq 0 or keyword_set(reload) then begin 
        
        if not sdss_runstatus_load(silent=silent) then begin
            exists=0
            return, -1
        endif 
    endif     
    exists=1
    return, runstatus

end 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    sdss_runstatus_print 
;       
; PURPOSE:
;    Decode and print the status for the input run/rerun
;
; CALLING SEQUENCE:
;    print_runstatus, run, rerun
;
; INPUTS: 
;    run: photo run
;    rerun: photo rerun
;
; OUTPUTS: 
;    print to screen
;
; REVISION HISTORY:
;    
;       Creation:
;          ??-??-2000: Erin Scott Sheldon UofChicago
;          18-Nov-2002: Added bad2 flag checking: tsField files
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro sdss_runstatus_print, run, rerun=rerun 

    if n_elements(run) eq 0 then begin 
        on_error, 2
        print,'-Syntax: print_runstatus, run, rerun'
        print
        message,'Halting'
    endif 

    runstatus = sdss_runstatus(exists=exists)
    if not exists then begin
        message,'runstatus info not found',/inf
        return
    endif
  
    w=where( runstatus.run EQ run,nw)
  
    IF nw EQ 0 THEN BEGIN 
        print,"Don't know anything about run "+ntostr(run)
        return
    ENDIF 

    if n_elements(rerun) eq 0 then begin
        ; This can be multiple reruns
        rerun = runstatus[w].rerun
    endif else begin
        wr = where(runstatus[w].rerun eq rerun[0], nwr)
        if nwr eq 0 then begin
            message,"Don't know anyting about rerun: "+ntostr(rerun[0]),/inf
            return
        endif
        w = w[wr]
    endelse

    ; Get a flag structure for runstatus
    rfs = sdss_flag_struct('runstatus')

    flags = tag_names(rfs)
    nflags = n_elements(flags)

    nrerun = n_elements(rerun)
    for irerun=0L, nrerun-1 do begin
        print
        print,'Run: '+ntostr(run)+'  Rerun: '+ntostr(rerun[irerun])
        print,'----------------------------------------------------------'

        for i=0L, nflags-1 do begin
            if (runstatus[w[irerun]].flags and rfs.(i)) ne 0 then begin
                status='Yes' 
            endif else begin
                status='No'
            endelse 
            print,flags[i],status,format='(A-25,A-15)'
        endfor
    endfor    
    return

end 

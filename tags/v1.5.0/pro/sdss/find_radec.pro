;+
;
; NAME:
;       FIND_RADEC
;       
; PURPOSE:
;	Find the run, camcol and field that contain the input ra and dec.
;       More than one run may be returned if ra,dec is in overlap region
;       between the runs.
; 
; CALLING SEQUENCE:
;       find_radec, ra, dec [, run, camcol, field, 
;                   runstr=runstr, colstr=colstr, fstr=fstr, 
;                   /first, /silent, /astrans]
;                 
; INPUTS: 
;       ra, dec: The positions in degrees.
;
; KEYWORD PARAMETERS:
;       /first: just find the first run that matches
;       silent=: silent=1: don't print run, camcol, field
;                silent>1: don't print anything.
;       /astrans: call find_radec_astrans to find run. Slower but more accurate
;       
; OUTPUTS: 
;       run, camcol, field
;
; OPTIONAL OUTPUTS:
;       runstr=runstr: run string with standard padded zeros
;       colstr=colstr: column string
;       fstr=fstr: field string with standard padded zeros.
;       
; PROCEDURE: 

;       Must have The configuration variables RADEC_SEARCH_DIR, SEARCH_RUNS,
;       and the RUN_STATUS info must be defined. The directory contains the
;       files created by FIELD_RANGE.PRO, which finds the lambda-eta range of
;       each field.  SEARCH_RUNS is all the runs which have been run through
;       FIELD_RANGE.  These files contain structures for each field with the
;       following tags: field, cetamin, cetamax, clambdamin, clambdamax. Assumes
;       that fields are approximately square in lambda-eta.  RUN_STATUS has
;       all the runs we know about on the current machine.
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofMich 10/15/99 
;       Converged to survey coords. 9-OCT-2000 E.S.S.
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

pro find_radec, ra, dec, mrun, mcamcol, mfield, $
                runstr=runstr, colstr=colstr, fstr=fstr, $
                silent=silent, first=first, astrans=astrans



    if n_params() eq 0 then begin 
        print,'-Syntax: find_radec, ra, dec, run, camcol, field, runstr=runstr, colstr=colstr, fstr=fstr, /silent, /astrans'
        print,''
        print,'Use doc_library,"find_radec"  for more help.'  
        return
    endif 

    ;; use astrans method?

    if keyword_set(astrans) then begin 
        find_radec_astrans, ra, dec, mrun, mcamcol, mfield, $
            runstr=runstr, colstr=colstr, fstr=fstr, $
            silent=silent, first=first
        return
    endif 

    IF n_elements(silent) EQ 0 THEN silent = 0

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; These will be system dependent variables.
    ;; The search directory and the searchable runs.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    delvarx, mrun, mcamcol, mfield, runstr, colstr, fstr

    runstatus = sdss_runstatus(exists=rs_exists)
    if not rs_exists then begin 
        message,'RUN_STATUS structure not found'
    endif 

    radec_search_dir = sdssidl_config('radec_search_dir', exists=ra_exists)
    if not ra_exists then begin 
        message,'SDSSIDL Config variable RADEC_SEARCH_DIR must be defined'
    endif 

    search_runs = sdssidl_config('search_runs', exists=sr_exists)
    if not sr_exists then begin 
        message,'SDSSIDL Config variable SEARCH_RUNS must be defined'
    endif 

    ;; convert to survey coords
    eq2csurvey, ra, dec, clambda, ceta

    ;; see which stripe this is. May give one of the surrounding 
    ;; stripes if near edge in outer column, so
    ;; search this stripe and surrounding stripes (if possible)

    stripe = ceta2stripenum(ceta, /closest)

    if stripe ge 75 then begin
        stripe = [75, 76, 82, 86]
    endif else begin
        stripe = [stripe-1, stripe, stripe+1]
    endelse
    n_search_stripe = n_elements(stripe)

    ;; runs with astrans (for which we could have gotten field range)
    wst1 = sdss_flag_select(runstatus.flags, 'runstatus', {astrans_exist:'y'})
    if wst1[0] eq -1 then begin 
        message,'No good runs found!!'
    endif 

    for i=0l, n_search_stripe-1 do begin 
        wst = where( runstatus[wst1].stripe EQ stripe[i], nmatch)
        if nmatch ne 0 then add_arrval, runstatus[wst1[wst]].run, msearch_runs

    endfor 

    nfound = n_elements(msearch_runs)

    if nfound eq 0 then begin 
        if silent le 1 then print,'No good stripes found for this ra and dec'
        mrun=-1
        mcamcol=-1
        mfield=-1
        return
    endif 

    ;; now make sure we have processed the runs in search_runs
    match, search_runs, msearch_runs, mproc, msearch, /sort
    if mproc[0] eq -1 then begin 
        if silent le 1 then print,'None of the runs in the stripes have been processed by field_range.pro'
        mrun=-1
        mcamcol=-1
        mfield=-1
        return
    endif 
    search_runs = search_runs[mproc]

    ;;  Search
    nrun = n_elements(search_runs)
    colmin=1
    colmax=6

    for irun = 0l, nrun-1 do begin 
        trunstr = run2string(search_runs[irun])
        for camcol = colmin, colmax do begin 
            tcolstr = ntostr(camcol)

            file = $
                'clamceta-range-' + trunstr + '-' + tcolstr + '.fit'
            file=path_join(radec_search_dir, file)

            t=mrdfits(file,1,/silent)

            nw=0
            w=where(ceta le t.cetamax $
                and ceta ge t.cetamin $
                and clambda le t.clambdamax $
                and clambda ge t.clambdamin, nw)
            if nw ne 0 then begin 

                tfstr = field2string(t[w[0]].field)
                add_arrval, search_runs[irun], mrun
                add_arrval, t[w[0]].field, mfield
                add_arrval, camcol, mcamcol
                add_arrval, trunstr, runstr
                add_arrval, tcolstr, colstr
                add_arrval, tfstr, fstr

                if not keyword_set(silent) then begin 

                    print,'Run: ',trunstr,' Camcol: ',tcolstr,' Field: ',tfstr
                endif 
                if keyword_set(first) then return

            endif 
        endfor 
    endfor           

    if n_elements(mrun) eq 0 then begin
        if silent le 1 then print,'Coordinates Not Found'
        mrun=-1
        mcamcol=-1
        mfield=-1
    endif 
    return 
END 

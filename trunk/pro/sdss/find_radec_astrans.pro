
;+
;
; THIS IS DEPRECATED:  Please use the sdss_astr2fields program
; from the photoop IDL package.
;
; NAME:
;       FIND_RADEC_ASTRANS
;       
; PURPOSE:
;	Find the run, camcol and field that contain the input ra and dec.
;       More than one run may be returned if ra,dec is in overlap region
;       between the runs. Uses astrans file; more accurate than find_radec
;       without /astrans keyword, but slower
; 
; CALLING SEQUENCE:
;       find_radec_astrans, ra, dec [, run, camcol, field, 
;                   runstr=runstr, colstr=colstr, fstr=fstr, 
;                   /first, /silent]
;                 
; INPUTS: 
;       ra, dec: The positions in degrees.
;
; KEYWORD PARAMETERS:
;       /first: just find the first run that matches
;       /silent: silent=1: don't print run, camcol, field
;                silent>1: don't print anything.
;       
; OUTPUTS: 
;       run, camcol, field
;
; OPTIONAL OUTPUTS:
;       runstr=runstr: run string with standard padded zeros
;       colstr=colstr: column string
;       fstr=fstr: field string with standard padded zeros.
;       
;
; CALLED ROUTINES:
; 
;       EQ2SURVEY
;       ETA2STRIPENUM
;       (SDSSIDL_SETUP)
;       sdss_read() 
;
;
; PROCEDURE: 
;
; REVISION HISTORY:
;	Created: 15-AUG-2002 Erin Sheldon UofMich
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

PRO find_radec_astrans, ra, dec, mrun, mcamcol, mfield, $
                        runstr=runstr, colstr=colstr, fstr=fstr, $
                        silent=silent, first=first



  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: find_radec_astrans, ra, dec, run, camcol, field, runstr=runstr, colstr=colstr, fstr=fstr, /first, /silent'
     print,''
     print,'Use doc_library,"find_radec"  for more help.'  
     return
  ENDIF 

  IF n_elements(silent) EQ 0 THEN silent = 0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; These will be system dependent variables.
  ;; The search directory and the searchable runs.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  delvarx, mrun, mcamcol, mfield, runstr, colstr, fstr

  sdssidl_setup, /silent
  run_status = sdss_runstatus(exists=rs_exists)
  IF NOT rs_exists THEN BEGIN 
      message,'RUN_STATUS information not retrieved'
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; convert to survey coords
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  eq2csurvey, ra, dec, clambda, ceta

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; see which stripe this is. May give one of the surrounding 
  ;; stripes if near edge in outer column, so
  ;; search this stripe and surrounding stripes (if possible)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  stripe = ceta2stripenum(ceta, /closest)

  if stripe ge 75 then begin
      stripe = [75, 76, 82, 86]
  endif else begin
      stripe = [stripe-1, stripe, stripe+1]
  endelse
  n_search_stripe = n_elements(stripe)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; See which stripes we know about
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  wst1 = sdss_flag_select(run_status.flags, 'runstatus', {astrans_exist:'y'})

  IF wst1[0] EQ -1 THEN BEGIN 
      message,'No runs with asTrans found!!'
  ENDIF 

  for i=0l, n_search_stripe-1 do begin 

      ;; any runs in this stripe?
      wst = where( run_status[wst1].stripe eq stripe[i], nmatch)
      if nmatch ne 0 then begin
          ind = wst1[wst]

          struns = run_status[ind].run
          streruns = run_status[ind].rerun

          ;; get unique runs
          rmd = rem_dup(struns)
          nuniq = n_elements(rmd)

          ;; find max rerun and use it
          for j=0l, nuniq-1 do begin 
              w=where( run_status[ind].run eq struns[rmd[j]] )
              w = ind[w]
              maxrerun = max( long(run_status[w].rerun) )
              trun = struns[rmd[j]]
              add_arrval, trun, search_runs
              add_arrval, maxrerun, search_reruns
          endfor 

      endif 

  endfor 

  nfound = n_elements(search_runs)
  IF nfound EQ 0 THEN BEGIN 
      IF silent LE 1 THEN print,'No good stripes found for this ra and dec'
      mrun=-1
      mcamcol=-1
      mfield=-1
      return
  ENDIF 

  nfound = n_elements(search_runs)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Search
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  nrun = n_elements(search_runs)
  colmin=1
  colmax=6

  mincol = 0.0
  maxcol = 2048.

    FOR irun = 0L, nrun-1 DO BEGIN 
        trunstr = run2string(search_runs[irun])
        FOR camcol = colmin, colmax DO BEGIN 
            tcolstr = ntostr(camcol)

            ;; search in r-band
            clr = 2
            trans=sdss_read('astrans',search_runs[irun], camcol, $
                bandpass=clr, rerun=search_reruns[irun],$
                node=node, inc=inc, /silent, status=status)

            if status eq 0 then begin
          
                nfields = n_elements(trans)
                fieldmin = trans[0].field
                fieldmax = trans[nfields-1].field

                FOR fi=0L, nfields-1 DO BEGIN 
                    field = trans[fi].field
                    eq2rowcol, trans, node, inc, field, ra, dec, row, col

                    minrow = (field GT fieldmin)*64 ;don't include overlap region except
                    maxrow = 1489 - (field LT fieldmax)*64 ;first and last frames

                    ;; is it in this field?
                    IF ( (row LT maxrow) AND (row GT minrow) AND $
                        (col LT maxcol) AND (col GT mincol) ) THEN BEGIN 

                        tfstr = field2string(field)
                        add_arrval, search_runs[irun], mrun
                        add_arrval, field, mfield
                        add_arrval, camcol, mcamcol
                        add_arrval, trunstr, runstr
                        add_arrval, tcolstr, colstr
                        add_arrval, tfstr, fstr
                  
                        IF NOT keyword_set(silent) THEN BEGIN 
                      
                            print,'Run: ',trunstr,$
                                ' Camcol: ',tcolstr,$
                                ' Field: ',tfstr
                        ENDIF 
                        IF keyword_set(first) THEN return

                    ENDIF 

                ENDFOR ;; fields
            endif ;; read of file was successful

        ENDFOR ;; camcols
    ENDFOR ;; runs

    IF n_elements(mrun) EQ 0 THEN BEGIN
        IF silent LE 1 THEN print,'Coordinates Not Found'
        mrun=-1
        mcamcol=-1
        mfield=-1
    ENDIF 
    return 
END 

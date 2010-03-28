
;+
;
; NAME:
;    sdss_runstatus_create
;       
; PURPOSE:
;    Find files and other info for the runs in the SDSS DATA_DIR and
;    SHAPECORR_DIR
;
; CALLING SEQUENCE:
;    sdss_runstatus_create, file=, /photoop
;
; OPTIONAL INPUTS:
;    file: The output file.
;    /photoop: Indicates we are working with photoop outputs instead
;       of fermilab.
;
; OUTPUTS: 
;    Outputs RUN_STATUS_FILE, a fits file containing info about
;    known runs.
;
;
; REVISION HISTORY:
;    14-NOV-2000 Erin Scott Sheldon UofMich
;    30-Nov-2006: Renamed
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

function sdss_runstatus_create_bycolfiles
    return,$
        ['tsobj','tsfield','fpatlas','fpobjc','fpbin','fpfieldstat',$
         'fpm','psfield','psbb','adatc']

end

pro sdss_runstatus_create_getreruns, dir, runs, reruns, corr=corr

  delvarx, runs, reruns
  print
  print,'Finding Run Directories in: '+dir

  if not file_test(dir) then begin 
      print,'Directory '+dir+' does not exist'
      return
  endif 
  cd,dir

  if keyword_set(corr) then begin 
      runDirs = file_search('corr[0-9]*', count=ndirs)
  endif else begin 
      runDirs = file_search('[0-9]*', count=ndirs)
  endelse 

  if ndirs eq 0 then begin
      print,'Nothing Found!'
      return
  endif 
  
  ;; in case we are looking in corrected dir, remove leading "corr"
  for i=0l, ndirs-1 do begin

      runDir = runDirs[i]
      if keyword_set(corr) then begin 
          runDir = str_sep(runDir,'corr')
          runDir = runDir[1] 
      endif

      if strnumber(rundir) then begin
          add_arrval, long(runDir), truns
          add_arrval, runDirs[i],   tdir
      endif 
  endfor 

  nrun = n_elements(truns)

  print,'Found '+ntostr(nrun)+' Runs'
  ;; find reruns

  print,'Finding Rerun Directories'
  for i=0l, nrun-1 do begin 

      tmprun = truns[i]
      runstr = ntostr(tmprun)

      tmpdir = dir+'/'+tdir[i]

      if file_test(tmpdir) then begin 

          cd,tmpdir
          rerunDirs = file_search('[0-9]*', count=nrerun)
          
          if nrerun ne 0 then begin 
              for j=0l, nrerun-1 do begin 
                  if strnumber(rerundirs[j]) then begin
                      add_arrval, tmprun, runs
                      add_arrval, fix(rerunDirs[j]), reruns
                  endif 
              endfor 
          endif ;; loop over reruns

      endif  ;; does the directory (or link) actually exist?

  endfor ;; loop over run directories
 

  nrerun = n_elements(reruns)
  print,'Found '+ntostr(nrerun)+' reruns'

end 

pro sdss_runstatus_create, photoop=photoop, rerun=rerun, file=file

    on_error, 2

    sdssidl_setup

    data_dir = sdssidl_config('DATA_DIR',exists=d_exists)
    if not d_exists then begin 
        message,'DATA_DIR must be defined'
    endif 

    ;; Note: shapecorr_dir is not necessary
    shapecorr_dir = sdssidl_config('SHAPECORR_DIR')
    c_exists = fexist(shapecorr_dir)
    if not c_exists then begin 
        shapecorr_dir = ''
        message,'SHAPECORR_DIR not found. Continuing',/inf
    endif 

    if n_elements(file) eq 0 then begin
        run_status_file = sdssidl_config('run_status_file',exists=rsfile_exists)
        if not rsfile_exists then begin 
            message,'RUN_STATUS_FILE must be defined'
        endif 
    endif else begin
        run_status_file = file
    endelse

    print,'Will write to file: ',run_status_file


    photoz_dir = sdssidl_config('photoz_dir')
    ph_exists = fexist(photoz_dir)
    if not ph_exists then photoz_dir = ''
 
    if keyword_set(photoop) then begin
        if n_elements(rerun) eq 0 then begin
           message,'You must send a rerun for /photoop' 
        endif
        runlist = sdss_runlist(rerun=rerun)
        runs=runlist.run & reruns = replicate(rerun, n_elements(runlist))
    endif else begin
        sdss_runstatus_create_getreruns, data_dir, runs, reruns
    endelse

    print

    ; if shapecorr dir defined, get these runs as well and remove duplicates

    if c_exists then begin 

        sdss_runstatus_create_getreruns, shapecorr_dir, cruns, creruns,/corr

        ten = ulong64(10)
        super = ulong64(runs)*ten^6 + ulong64(reruns)
        csuper = ulong64(cruns)*ten^6 + ulong64(creruns)
        match, super, csuper, mt, mtc, /sort
      
        if mtc[0] ne -1 then begin 
            nmtc = n_elements(mtc)
            if nmtc ne n_elements(creruns) then begin 
                remove, mtc, cruns, creruns, csuper
                runs = [runs, cruns]
                reruns = [reruns, creruns]
              
                super = [super, csuper]
            endif 
        endif 
    endif 

    nrun = n_elements(runs)
    s={stripe: -1, strip: "?", run: -1L, rerun: -1, flags: 0L}
    run_status = replicate(s, nrun)
    if keyword_set(photoop) then begin
        run_status.stripe = runlist.stripe
        run_status.strip = runlist.strip
        run_status.run = runlist.run
        run_status.rerun = rerun
    endif else begin
        run_status.run = runs
        run_status.rerun = reruns
    endelse
    ; check if directories are OK

    print,' run  rerun'
    colprint, run_status.run, run_status.rerun


    print,'Checking for files'
    print

    w=where(run_status.rerun GE 0, ngood)

    bycol= sdss_runstatus_create_bycolfiles()
    nbycol = n_elements(bycol)

    for i=0l, ngood-1 do begin 
        ind=w[i]

        run = run_status[ind].run
        rerun = run_status[ind].rerun

        rstr = ntostr(run)
        rstr2 = run2string(run)
        rrstr = ntostr(rerun)

        print,"Run: "+rstr+" Rerun: "+rrstr

        ; initialize flags

        flags  = 0LL

        ; check for asTrans file
        transFile = sdss_file('astrans', run, rerun=rerun)

        if fexist(transfile) then begin
            flags = flags+sdss_flag('runstatus','astrans_exist')
        endif

        ; check other file types

        for ift=0L, nbycol-1 do begin
            ftype=bycol[ift]
    
            camcount = 0
            for camcol=1,6 do begin 

                if ftype eq 'fpm' or ftype eq 'fpbin' or ftype eq 'psbb' then begin
                    bands = [0,1,2,3,4]
                endif else begin
                    bands = 2 ; doesn't matter
                endelse
                nband=n_elements(bands)
                bcount = 0
                for ib=0L, nband-1 do begin
                    flist=sdss_filelist(ftype,run,camcol,rerun=rerun,$
                                    band=bands[ib], status=fstatus)
                    if fstatus eq 0 then begin
                        bcount=bcount+1

                        ; For fermilab stuff we need to do this ourselves.
                        if run_status[ind].stripe eq -1 then begin
                            if ftype eq 'tsobj' then begin
                                hdr=headfits(flist[0])
                                run_status[ind].stripe = sxpar(hdr,'stripe')
                                run_status[ind].strip = sxpar(hdr,'strip')
                            endif
                        endif 
                    endif
                endfor
                if bcount eq nband then begin
                    camcount = camcount + 1
                endif 

            endfor    

            if camcount eq 6 then begin
                flags = flags + sdss_flag('runstatus',ftype+'_exist')
            endif
        endfor

        ; photozs
        pzcount=0
        for camcol=1,6 do begin
            cstr = ntostr(camcol)
            photoz_file = $
                concat_dir(photoz_dir,$
                       'tsObj_ascii_'+rstr+'_'+rrstr+'_'+cstr+'.dat')
            if fexist(photoz_file) then begin
                pzcount = pzcount+1
            endif
        endfor
        if pzcount eq 6 then begin
            flags = flags + sdss_flag('runstatus','photoz_exist')
        endif

        run_status[ind].flags  = flags

  endfor 

  print
  print,'run_status_file: ',run_status_file
  print
  mwrfits, run_status, run_status_file, /create
  
  return
end 

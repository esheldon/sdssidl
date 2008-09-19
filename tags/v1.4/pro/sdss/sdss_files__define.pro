;+
; NAME:
;  SDSS_FILES__DEFINE
;
;
; PURPOSE:
;  A class definition file containing methods for dealing with sdss files.  
;  This is a much more compact way to store these programs and avoids 
;  namespace collisions.
;
; SOME METHODS Defined in this class file:
;
;  INITIALIZE A NEW OBJECT:
;    IDL> sf = obj_new('sdss_files')
;
;  RUN_EXISTS
;    IDL> if sf->run_exists(run, reruns=, info=) then ...
;    can return array of available reruns and info structs
;
;  RERUN_EXISTS
;    IDL> if sf->rerun_exists(run, rerun) then ....
;
;  RERUN
;    IDL> reruns = sf->rerun(runs, /min)
;    By default it finds latest rerun for the input run.  Set /min to find
;    earliest. Can send an array of runs.
;
;  STRIPE
;    IDL> stripes = sf->stripe(runs)
;
;  STRIP
;    IDL> strips = sf->strip(runs)
;
;  RUNS
;    IDL> runs = sf->runs()
;    Get a list of all unique runs
;  RUNS_RERUNS
;    IDL> sf->runs_reruns, runs, reruns
;    Get a list of runs and reruns.  Runs may be duplicated here.
;
;  RUNDIR
;    IDL> rundir = sf->rundir(run, rerun=, /min, /corrected, exists=)
;    Get the directory for an sdss run. See rerun for /min.
;
;  FILEDIR
;    IDL> dir = sf->filedir(subdir, run, rerun=, camcol=, /corrected, exists=])
;    Get the directory for a subdir of a run.  Valid subdirs:
;         astrom, calibChunks, corr, nfcalib, objcs, photo, zoom, rezoom,
;         combined.  If camcol is sent, and there are camcol subdirectories for
;         this type, then that is appended to the directory.
;
;  FILE2FIELD
;    IDL> fields = sf->file2field(files)
;    Extract the field numbers from a set of sdss files.  Assumes last 0000
;    represent the field, as in tsObj-000756-3-44-0803.fit
;
;  FILELIST
;    IDL> files = sf->filelist(filetype, run, camcol, rerun=, bandpass=,
;                              fields=, dir=, exists=)
;    Get all the files of a certain type.  Valid file types:
;         tsObj, tsField, fpObjc, fpAtlas, fpM, fpBin, psField, adatc
;
;  FILE
;    IDL> name = sf->file(type, run, [camcol, fields=, rerun=, bandpass=, dir=,
;                         /nodir, /stuffdb])
;    Supported file types:  asTrans, tsObj, tsField,
;                           fpObjc, fpAtlas, fpM, fpBIN, fpFieldStat,
;                           psField, psBB, adatc
;    All types except asTrans require that the camcol is entered.
;    For fpM, fpBIN, and psBB you must enter the bandpass as an integer or
;      string. 
;    If fields is not entered, a value of '*' is used.  Useful for making file
;      patterns.  Fields can be an array.
;    Examples:
;        IDL> fname = sf->name('tsObj', 756, 2, field=125, /nodir)
;        IDL> file = sf->name('asTrans', 756)
;
;  READ
;    This is still basic.  Will add more functionality such as is in
;    read_tsobj.
;    IDL> struct = sf->read(type, run, [camcol, fields=, rerun=, bandpass=, 
;                           taglist=, wstring=, /pointers, status=)
;
;
;  STRIPE2STRING
;    IDL> result = sf->stripe2string(stripes)
;  RUN2STRING
;    IDL> result = sf->run2string(runs)
;  FIELD2STRING
;    IDL> result = sf->field2string(fields)
;  ID2STRING
;    IDL> result = sf->id2string(ids)
;
;  METHODS
;    IDL> methods,'sdss_files'
;    IDL> doc_method,'sdss_files::read'
;
;  HELP
;    Print the documentation for this class
;
;
; COMMON BLOCKS:
;   sdss_files_filelist_block, filetype_old, run_old, rerun_sent, rerun_old, $
;       camcol_sent, camcol_old, files_old
;
; RESTRICTIONS:
;  Needs to read the run_status file for most operations. 
;
; MODIFICATION HISTORY:
;   Created: 12-Apr-2005, Erin Sheldon, UofChicago.  Consolidated from
;            existing procedures.
;
;-
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


function sdss_files::init
    return,1
end 

;; run doc_library

pro sdss_files::help
  doc_library,'sdss_files__define'
end 

function sdss_files::run_exists, run, reruns=reruns, info=info

  if n_params() lt 1 then begin 
      print,'-syntax: if obj->run_exists(run, reruns=, info=) then ....'
      return,-1
  endif 
  
  reruns=-1

  run_status = sdss_runstatus(exists=exists)
  if not exists then return, 0

  ;; check if run exists on disk
  w=where(run_status.run eq run[0], nw)
  if nw eq 0 then return,0
  

  reruns = run_status[w].rerun
  if nw eq 1 then reruns=reruns[0]

  if arg_present(info) then info=run_status[w]

  return,1

end 

function sdss_files::rerun_exists, run, rerun

  if n_params() lt 1 then begin 
      print,'-syntax: if obj->rerun_exists(run, rerun) then ....'
      return,-1
  endif 

  exists=0

  if self->run_exists(run, reruns=reruns) then begin 
      w=where(reruns eq rerun, nw)
      if nw ne 0 then return,1 else return,0
  endif else begin 
      return, 0
  endelse 

end 


function sdss_files::rerun, runs, min=min, exists=exists

  if n_params() lt 1 then begin 
      print,'-syntax: reruns = obj->rerun(runs, /min, exists=)'
      print,' run can be an array'
      print,' /min to get min rather than max rerun'
      return,-1
  endif 

  exists=0
  nrun = n_elements(runs)
  if nrun eq 1 then begin 
      if not self->run_exists(runs, reruns=treruns) then begin 
          return,-1
      endif

      exists=1
      if keyword_set(min) then return,min(treruns) $
      else return, max(treruns)

  endif else begin 

      ;; for large arrays, sdss_run_exists is too slow, we will do something a
      ;; little more clever

      reruns=replicate(-1, nrun)

      ;; histogram them runs, there can't be more than a few hundred
      h=histogram(runs, rev=runrev)
      nh = n_elements(h)

      ;; loop and only check the unique runs
      for i=0l, nh-1 do begin 

          ;; any runs in this bin?
          if runrev[i] ne runrev[i+1] then begin 
          
              ;; get the indices for these runs
              wr = runrev[ runrev[i]:runrev[i+1] -1 ]
              
              trun = runs[wr[0]]
              
              if not self->run_exists(trun, rerun=treruns) then begin 
;                  message,$
;                    'run '+strtrim(string(trun),2)+$
;                    ' is unknown: not in run status file',/inf
              endif else begin 
                  if keyword_set(min) then reruns[wr] = min(treruns) $
                  else reruns[wr] = max(treruns)
              endelse 

          endif 

      endfor 
  endelse 

  exists=1
  return,reruns


end 

function sdss_files::stripe, runs

  if n_params() lt 1 then begin 
      print,'-Syntax: stripes = obj->stripe(runs)'
      print,' run can be an array'
      return,-1
  endif 

  nrun = n_elements(runs)
  if nrun eq 1 then stripes=-1 else stripes=replicate(-1, nrun)

  for i=0l, nrun-1 do begin 

      if not self->run_exists(runs[i], info=info) then begin 
      endif else begin 
          stripes[i] = info[0].stripe
      endelse 
      
  endfor 
  return,stripes

end 
function sdss_files::strip, runs

  if n_params() lt 1 then begin 
      print,'-Syntax: strips = obj->strip(runs)'
      print,' run can be an array'
      return,-1
  endif 

  nrun = n_elements(runs)
  if nrun eq 1 then strips='' else strips=replicate('', nrun)

  for i=0l, nrun-1 do begin 

      if not self->run_exists(runs[i], info=info) then begin 
;          message,$
;            'run '+strtrim(string(runs[i]),2)+$
;            ' is unknown: not in run status file',/inf
      endif else begin 
          strips[i] = info[0].strip
      endelse 
      
  endfor 
  return,strips

end 

function sdss_files::runs
    run_status = sdss_runstatus(exists=exists)
    if not exists then return, -1
    runs = run_status[rem_dup(run_status.run)].run
    return,runs
end 

pro sdss_files::runs_reruns, runs, reruns
    runs=-1
    reruns=-1
    run_status = sdss_runstatus(exists=exists)
    if not exists then return
    
    runs = run_status.run
    reruns = run_status.rerun
end 


function sdss_files::rundir, run, rerun=rerun, min=min, corrected=corrected, exists=exists

    badval=""

    if n_params() lt 1 then begin 
        on_error, 2
        print,'-Syntax: rundir = obj->rundir(run, rerun=, /corrected, exists=)'
        print
        message,'Halting'
    ENDIF 

    ;; only one at a time
    userun = run[0]
    rstr = strtrim(string(userun), 2)

    if n_elements(rerun) ne 0 then begin 
        usererun = rerun[0]
        rrstr = strtrim(string(usererun), 2)
    endif else begin 
        ;; Do we know this run?
        rexist = self->run_exists(run, reruns=all_reruns)
        if not rexist then begin
            message,'Need run_status if rerun not sent'
            exists=0
            return,badval
        endif else begin
            maxrerun = max(all_reruns, min=minrerun)

            ;; get the rerun, either max or min
            if keyword_set(min) then usererun=minrerun $
            else usererun = maxrerun

            ;; return the rerun we are using
            rerun = usererun

            rrstr = strtrim(string(usererun), 2)
        endelse
    endelse 

    ;; get basic data directory
    if not keyword_set(corrected) then begin 
        data_dir = sdssidl_config('data_dir', exists=dexists)
        if not dexists then begin 
            message,'DATA_DIR is not configured',/inf
            exists=0
            return,badval
        endif 

        rundir = concat_dir(data_dir, strtrim(string(userun),2))
    endif else begin 
        data_dir = sdssidl_config('shapecorr_dir', exists=dexists)
        if not dexists then begin 
            message,'SHAPECORR_DIR is not configured',/inf
            exists=0
            return,badval
        ENDIF 

        rundir = concat_dir(data_dir, 'corr'+strtrim(string(userun),2))

    ENDELSE 

    ;; Now get the rerun directory
    rundir = concat_dir(rundir, rrstr+'/')

    if arg_present(exists) then begin
        exists=fexist(rundir)
    endif
 
    return,rundir

END 


function sdss_files::filedir, subdir, run, rerun=rerun, camcol=camcol, corrected=corrected, exists=exists, silent=silent

  badval=""

  if n_params() lt 2 then begin 
      on_error, 2
      print,'-Syntax: filedir=obj->filedir(subdir, run, rerun=, camcol=, /corrected, exists=)'
      print
      message,'Halting'
  endif 

  rundir = self->rundir(run,rerun=rerun,corrected=corrected)

  if n_elements(camcol) ne 0 then cstr = strtrim(string(camcol[0]),2)

  case strlowcase(subdir[0]) of
      ;; SDSS outputs
      'astrom': filedir = concat_dir(rundir, 'astrom')
      'calibchunks': begin 
          filedir = concat_dir(rundir, 'calibChunks')
          if n_elements(camcol) ne 0 then filedir=concat_dir(filedir,cstr)
      end 
      'corr': begin 
          filedir=concat_dir(rundir,'corr')
          if n_elements(camcol) ne 0 then filedir=concat_dir(filedir, cstr)
      end 
      'nfcalib': filedir = concat_dir(rundir, 'nfcalib')
      'objcs': begin 
          filedir=concat_dir(rundir, 'objcs')
          if n_elements(camcol) ne 0 then filedir=concat_dir(filedir, cstr)
      end 
      'photo': filedir=concat_dir(rundir,'photo')
      'zoom': begin 
          filedir = concat_dir(rundir, 'Zoom')
          if n_elements(camcol) ne 0 then filedir=concat_dir(filedir,cstr)
      end 
      'rezoom': begin 
          filedir=concat_dir(rundir, 'reZoom')
          if n_elements(camcol) ne 0 then filedir=concat_dir(filedir,cstr)
      end 

      ;; Corrected outputs
      'combined': filedir = concat_dir(rundir, 'combined')

      else: begin 
          if not keyword_set(silent) then begin 
              message,'Unknown subdirectory: '+subdir[0],/inf
              message,'Assigning to main directory', /inf
          endif 
          filedir = concat_dir(rundir, subdir)
      end 
  endcase 

  ;; For back-compatability
  filedir = filedir + '/'

  if arg_present(exists) then begin
      exists = fexist(filedir)
  endif

  return,filedir

end 

function sdss_files::file2field, files

  if n_elements(files) eq 0 then begin 
      print,'-Syntax: fields = obj->file2field(files)'
      return,-1
  endif 

  ;; assumes that the last 0000.fit represents the
  ;; field number

  n=n_elements(files)
  if n eq 1 then fields=0 else fields = intarr(n)
  for i=0l, n-1 do begin 

      if files[i] eq '' then begin 
          fields[i] = -1
      endif else begin 
          a = strsplit(files[i], '-', /extract)
          nn = n_elements(a)
          last = a[nn-1]
          
          fn = long( strsplit(last, "\.fit*", /ext, /regex) )
          fields[i] = fn
      endelse 
  endfor 

  return,fields

end 

function sdss_files::stripe2string, stripe

  if n_params() eq 0 then begin
      on_error, 2
      print,'-Syntax: result = obj->stripe2string(stripe)'
      message,'Halting'
  endif

  return, strtrim(string(stripe,format='(i20.2)'),2)
end 

function sdss_files::run2string, run

  if N_params() eq 0 then begin
      on_error, 2
      print,'-Syntax: result = obj->run2string(run)'
      message,'Halting'
  endif

  return, strtrim(string(run,format='(I20.6)'),2)
end 

function sdss_files::field2string, field

  if N_params() eq 0 then begin
      on_error, 2
      print,'-Syntax: result = obj->field2string(field)'
      message,'Halting'
  endif

  return, strtrim(string(field,format='(I20.4)'),2)

end 

function sdss_files::id2string, id

  if N_params() eq 0 then begin
      on_error, 2
      print,'-Syntax: result = obj->id2string(id)'
      message,'Halting'
  endif

  return, strtrim(string(id,format='(I20.5)'),2)
end 

;docstart::sdss_files::file
; NAME:
;  sdss_file()
;
; PURPOSE:
;  Return an SDSS file name for the input filetype, and id information.
;
; CATEGORY:
;  SDSS specific
;
; CALLING SEQUENCE:
;   file=sf->file(type, run, camcol, rerun=, bandpass=, fields=, 
;                  dir=, /nodir, /stuffdb, exists=)
;  
;
; INPUTS:
;   type: file type.  For a list of types do
;     print,!sdss->supported_filetypes()
;   run: The sdss run.
;   camcol: The camcol.  This is optional for some file types.
;
; OPTIONAL INPUTS:
;   rerun: SDSS rerun.  If not sent, the run_status structure is searched
;       for the run and the latest rerun is returned.
;   bandpass: The bandpass in numerical of string form where
;       u,g,r,i,z -> 0,1,2,3,4
;   fields: The fields to read. Defaults to a pattern with '*' for the 
;       field number.
;   indir: The directory to use for the file.
;   /nodir: Do not prepend the directory.
;   /stuffdb:  Filenames for db stuffing.
;
; OUTPUTS:
;   The file name.
;
; OPTIONAL OUTPUTS:
;   dir: The directory.
;   exists: 1 for yes, 0 for no
;
; RESTRICTIONS:
;   If rerun is not sent, the run must be defined in the run status structure.
;
; MODIFICATION HISTORY:
;   Created Erin Sheldon, UChicago 
;
;docend::sdss_files::file

function sdss_files::file, type_in, run, camcol, fields=fields, rerun=rerun, bandpass=bandpass, indir=indir, dir=dir, nodir=nodir, stuffdb=stuffdb, exists=exists

  ntype=n_elements(type_in)
  nrun=n_elements(run)
  if ntype eq 0 or nrun eq 0 then begin 
      on_error, 2
      print,'-Syntax: file = obj->file(type, run, [camcol, rerun=, bandpass=, fields=, indir=, dir=, /nodir, /stuffdb, exists=])'
      print,'optional bandpass can be index or color string (e.g. 2 or "r")'
      print,'fields can be an array'
      print
      message,'Halting'
  endif 

  type = strlowcase(type_in[0])

  runstr = self->run2string(run[0])
  if n_elements(camcol) eq 0 then begin 
      if type ne 'astrans' then begin 
          message,'You must enter camcol to generate a '+type_in+' file',/inf
          exists=0
          return,''
      endif 
  endif else begin 
      camcolstr = ntostr(long(camcol[0]))
  endelse 

  if n_elements(rerun) eq 0 then begin 
      rerun = self->rerun(run, exist=rexist)
      if not rexist then begin
          exists=0
          return,''
      endif
  endif 
  rerunstr = ntostr(long(rerun))

  if n_elements(fields) eq 0 then begin 
      fieldstr = '*'
  endif else begin 
      fieldstr = self->field2string(fields)
  endelse 

  if n_elements(bandpass) ne 0 then begin 
      bt = size(bandpass,/tname)
      if bt eq 'STRING' then begin 
          bpstr = bandpass[0]
      endif else begin 
          colors = ['u','g','r','i','z']
          bpstr = colors[long(bandpass[0])]
      endelse 
  endif else begin 
      ;; match up to those that require bandpass to be entered
      match, type, ['fpm','fpbin','psbb'], mtype, mbptype
      if mtype[0] ne -1 then begin 
          message,$
            'You must enter bandpass= to get generate a '+type_in[0]+' file',$
            /inf
          exists=0
          return,''
      endif 
  endelse 

  if not keyword_set(nodir) then dodir = 1 else dodir = 0

  case type of 
      'astrans': begin 
          file = 'asTrans-'+runstr+'.fit'
          if dodir then begin 
              dir = self->filedir('astrom',run,rerun=rerun)
          endif 
      end 
      'tsobj': begin 
          file = $
            'tsObj-'+runstr+'-'+camcolstr+'-'+rerunstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('calibChunks',run,rerun=rerun,camcol=camcol)
          endif 
      end 
      'tsfield': begin 
          file = $
            'tsField-'+runstr+'-'+camcolstr+'-'+rerunstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('calibChunks',run,rerun=rerun,camcol=camcol)
          endif 
      end 
      'fpobjc': begin 
          file = $
            'fpObjc-'+runstr+'-'+camcolstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('objcs',run,rerun=rerun,camcol=camcol)
          endif 
      end 
      'fpatlas': begin 
          file = $
            'fpAtlas-'+runstr+'-'+camcolstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('objcs',run,rerun=rerun,camcol=camcol)
          endif 
      end 
      'fpm': begin           
          file = $
            'fpM-'+runstr+'-'+bpstr+camcolstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('objcs',run,rerun=rerun,camcol=camcol)
          endif 
      end 
      'fpbin': begin 
          file = $
            'fpBIN-'+runstr+'-'+bpstr+camcolstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('objcs',run,rerun=rerun,camcol=camcol)
          endif 
      end
      'fpfieldstat': begin 
          file = $
            'fpFieldStat-'+runstr+'-'+camcolstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('objcs',run,rerun=rerun,camcol=camcol)
          endif 
      end 
      'psfield': begin 
          file = $
            'psField-'+runstr+'-'+camcolstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('objcs',run,rerun=rerun,camcol=camcol)
          endif 
      end 
      'psbb': begin 
          file = $
            'psBB-'+runstr+'-'+bpstr+camcolstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('objcs',run,rerun=rerun,camcol=camcol)
          endif 
      end

      ;; Corrected
      'adatc': begin 
          file = $
            'adatc-'+runstr+'-'+camcolstr+'-'+rerunstr+'-'+fieldstr+'.fit'
          if dodir then begin 
              dir = self->filedir('calibChunks',run,rerun=rerun,camcol=camcol,$
                                  /corrected)
          endif 
      end 

      else: begin 
          message,'Unsupported file type: '+type,/inf
          exists=0
          return,''
      end 
  endcase 

  if n_elements(indir) ne 0 then begin
      dir = indir
  endif
  if dodir then file = concat_dir(dir, file)

  if arg_present(exists) then exists=fexist(file)

  return,file

end 



function sdss_files::_file_search, file_pattern, count=count

  idlversion = float(!version.release)
  if idlversion lt 5.4 then begin 
      files = findfile(file_pattern, count=count)
  endif  else begin 
      files = file_search(file_pattern, count=count)
  endelse 
  return,files

end 

;; See if the same args as last time were sent. If so, return the
;; old file list
function sdss_files::_filelist_sameargs, filetype, run, camcol, rerun=rerun

  common sdss_files_filelist_block, $
    filetype_old, $
    run_old, $
    camcol_old, $
    rerun_sent, rerun_old, $
    allfiles_old, allfnums

  if n_elements(filetype_old) eq 0 then begin 
      filetype_old = 'NONE'
      return,0
  endif 

  if n_elements(run_old) eq 0 then runold = -1l


  if n_elements(rerun_sent) eq 0 then rerun_sent = 0
  if n_elements(rerun_old) eq 0 then rerunold = -1l

  if n_elements(camcol_old) eq 0 then camcolold = -1l

  notmatched = 0

  if filetype[0] ne filetype_old then return,0
  if run[0] ne run_old then return,0
  if camcol[0] ne camcol_old then return,0

  ;; these were optional, so might not need to check them
  if rerun_sent then begin 
      if n_elements(rerun) ne 0 then begin 
          if rerun[0] ne rerun_old then return,0
      endif 
  endif 

  ;; if we get here, they match the old arguments
  ;; now set the old arguments to the new arguments
  filetype_old = filetype[0]
  run_old = run[0]
  camcol_old = camcol[0]

  if n_elements(rerun) ne 0 then begin 
      rerun_old = rerun[0]
      rerun_sent = 1
  endif 

  return,1

end 


function sdss_files::extract_fields, filelist, fnums, fieldsin, goodfnums=goodfnums, status=status

  status = 1
  fields = fieldsin[rem_dup(fieldsin)]
  match, fields, fnums, mf, mfn
  
  if mf[0] eq -1 then begin 
      message,'None of the requested fields were found',/inf
      return,''
  endif 
  
  if n_elements(mf) eq 1 then begin 
      mf = mf[0]
      mfn = mfn[0]
  endif 

  goodfnums = fields[mf]
  status = 0
  return,filelist[mfn]

end 


function sdss_files::supported_filetypes
  return,['astrans',$
          'tsobj','tsfield',$
          'fpobjc','fpatlas','fpm','fpbin','fpfieldstat','psfield','psbb',$
          'adatc']
end 
function sdss_files::filetype_supported, filetype
  match, strlowcase(filetype), self->supported_filetypes(), mf, mt
  if mt[0] eq -1 then return,0 else return,1
end 



;docstart::sdss_files::filelist
; NAME:
;   filelist()
;
; PURPOSE:
;   Return a list of files for the input filetype and SDSS id info.
;
; CATEGORY:
;   SDSS specifie.
;
; CALLING SEQUENCE:
;   sdss_filelist(filetype, run, camcol, rerun=, bandpass=, fields=, 
;                 fnums=, dir=, /silent, status=)
;
; INPUTS:
;   filetype: file type.  For a list of types do
;     print,!sdss->supported_filetypes()
;   run: The sdss run.
;   camcol: The camcol.  This is optional for some file types.
;
; OPTIONAL INPUTS:
;   rerun: SDSS rerun.  If not sent, the run_status structure is searched
;       for the run and the latest rerun is returned.
;   bandpass: The bandpass in numerical of string form where
;       u,g,r,i,z -> 0,1,2,3,4
;   fields: The fields to read. Defaults to a pattern with '*' for the 
;       field number.
;   /silent:
;
; OUTPUTS:
;   The file name.
;
; OPTIONAL OUTPUTS:
;   dir: The directory.
;   exists: 1 for yes, 0 for no
;
; RESTRICTIONS:
;   If rerun is not sent, the run must be defined in the run status structure.
;
; MODIFICATION HISTORY:
;   Created Erin Sheldon, UChicago 
;
;docend::sdss_files::filelist

function sdss_files::filelist, filetype_in, run, camcol, rerun=rerun, bandpass=bandpass, fields=fields, fnums=fnums, indir=indir, dir=dir, silent=silent, status=status

    common sdss_files_filelist_block, $
        filetype_old, $
        run_old, $
        camcol_old, $
        rerun_sent, rerun_old, $
        allfiles_old, allfnums

    status = 1
    badval=""

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: filedir=obj->filelist(filetype, run, camcol, '+$
            'rerun=, bandpass=, fields=, fnums=, dir=, /silent, status=)'
        print
        message,'Halting'
    endif 

    filetype = strlowcase(filetype_in[0])

    if not self->_filelist_sameargs(filetype, run, camcol, rerun=rerun) then begin 

        ;; see what file type is requested
        case strlowcase(filetype[0]) of
            ;; astrom
            'astrans': file_pattern = $
                self->file('astrans', run, rerun=rerun, indir=indir, dir=dir)
            ;; calibChunks directory
            'tsobj': file_pattern = $
                self->file('tsObj', run, camcol, rerun=rerun, indir=indir,  dir=dir)
            'tsfield': file_pattern = $
                self->file('tsField', run, camcol, rerun=rerun, indir=indir, dir=dir)
          
            ;; objcs directory
            'fpobjc': file_pattern = $
                self->file('fpobjc', run, camcol, rerun=rerun, indir=indir, dir=dir)
            'fpatlas': file_pattern = $
                self->file('fpAtlas', run, camcol, rerun=rerun, indir=indir, dir=dir)
            'fpm': file_pattern = $
                self->file('fpM', run, camcol, rerun=rerun,bandpass=bandpass, indir=indir, dir=dir)
            'fpbin': file_pattern = $
                self->file('fpBIN', run, camcol,rerun=rerun,bandpass=bandpass, indir=indir, dir=dir)
            'fpfieldstat': file_pattern = $
                self->file('fpFieldStat', run, camcol,rerun=rerun, indir=indir, dir=dir)
            'psfield': file_pattern = $
                self->file('psField', run, camcol, rerun=rerun, indir=indir, dir=dir)
            'psbb': file_pattern = $
                self->file('psBB', run, camcol,rerun=rerun,bandpass=bandpass, indir=indir, dir=dir)

          
            ;; Corrected files
            'adatc': file_pattern = $
                self->file('adatc', run, camcol, rerun=rerun, indir=indir, dir=dir)
          
            else: begin 
                message,'Unknown file type: '+filetype,/inf
                return,badval
            end 
        endcase 

        if n_elements(dir) eq 0 then return, badval
        if not fexist(dir) then return,badval
        allfiles = self->_file_search(file_pattern, count=count)
        if n_elements(allfiles) eq 1 then allfiles = allfiles[0]

        allfiles_old = allfiles
        if count eq 0 then begin 
            return,badval
        endif 

        allfnums = self->file2field(allfiles)

    endif else begin 
        allfiles = allfiles_old
    endelse 

    if n_elements(fields) ne 0 then begin 

        if size(fields[0],/tname) eq 'STRING' then begin
            if fields[0] eq '*' then begin
                status=0
                fnums=allfnums
                return, allfiles 
            endif else begin
                print,'Cannot interpret fields keyword: ',fields
                return,badval
            endelse
        endif

        files = self->extract_fields(allfiles, allfnums, fields, $
                                    goodfnums=fnums, $
                                    status=extstatus)
        if extstatus ne 0 then begin 
            return,badval
        endif 
        status = 0
        return,files
    endif else begin 
        status = 0
        fnums = allfnums
        return,allfiles
    endelse 


end 















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Read fits tables.  Hacked mrdfits main procedure for our speed 
; purposes
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



function sdss_files::rdtable, file, extension, header,  silent = silent, deja_vu=deja_vu, status=status


  common sdss_files_mrdfits_block, rsize, tab, nfld, typarr,        $ 
    fnames, fvalues, vcls, vtpes, scales, offsets, scaling, $
    columns, no_tdim

  status = 1

  use_colnum=0
  on_error, 2
  
  
  if n_params() LT 2 then begin
      message,'mrdfits(file, extension [, header, /silent, /deja_vu, status=]'
      return, 0
  endif
  
  if n_params() eq 1 then extension = 0

  if extension lt 1 then begin 
      message,'Only binary table extensions supported',/inf
      return,0
  endif 
  
  
; Open the file and position to the appropriate extension then read
; the header.
  
  unit = fxposit(file, extension, compress=compress, /readonly)
  
  if unit lt 0 then begin
      message, 'File access error'+strn(junk),/inf
      return, 0
  endif
  
  if eof(unit) then begin
      message,'Extension past EOF',/inf
	free_lun, unit
      return, 0
  endif
  
  mrd_hread, unit, header, hstatus, silent=silent
  
  num_axis2=fxpar(header,'NAXIS2')
  arange=[0,num_axis2-1]
  
  if num_axis2 eq 0 then begin
      message,"FILE IS EMPTY",/inf
      return,0
  endif
  
  
  if hstatus lt 0 then begin
      message, 'Unable to read header for extension',/inf
      return, 0
  endif
  
  xten = strtrim( fxpar(header,'XTENSION'),2)
  if xten ne 'BINTABLE' and xten ne 'A3DTABLE' then begin 
      message,'Only binary tables are supported',/inf
      return,0
  endif 
  type = 2
  
  scaling = keyword_set(fscale) or keyword_set(dscale)
  
      
  ;; *** Binary tables.
      
  if not keyword_set(deja_vu) then begin 
      
      self->mrdfits::mrd_table, header, structyp, use_colnum, $
        arange, rsize, table, nrows, nfld, typarr,        $ 
        fnames, fvalues, vcls, vtpes, scales, offsets, scaling, wstatus, $
        rows=rows, $
        silent=silent, columns=columns, $
        no_tdim=no_tdim, alias=alias, unsigned=unsigned
      
      tab=table(0)
      
  endif else begin              ;use old table. saves much time.
      wstatus=0
      table=replicate(tab,num_axis2)
  endelse
  
  size = nfld*(arange[1] - arange[0] + 1)
  if wstatus ge 0  and size gt 0 then begin 
      
      ;;*** read data.
      self->mrdfits::mrd_read_table, $
        unit, arange, rsize, structyp, num_axis2, $
        nfld, typarr, table, rows=rows

      if wstatus ge 0 and n_elements(vcls) gt 0 then begin  
          
          ;;*** get variable length columns
          self->mrdfits::mrd_read_heap, unit, header, arange, fnames, fvalues, $
            vcls, vtpes, table, structyp, scaling, scales, offsets, wstatus, $
            silent=silent, $
            columns=columns, rows = rows, pointer_var=pointer_var, fixed_var=fixed_var
      endif 
  endif 
      
    
  if  wstatus ge 0  and  scaling  and  size gt 0  then begin
      w = where(scales ne 1.d0  or  offsets ne 0.0d0)
      
                                ;*** apply scalings.
      if w[0] ne -1 then self->mrdfits::mrd_scale, type, scales, offsets, table, header,$
        fnames, fvalues, 1+arange[1]-arange[0], $
        dscale=dscale, structyp=structyp, silent=silent
  endif
  
  free_lun, unit

  if wstatus ge 0 then begin
      status=0
      return, table 
  endif else begin
      return, 0
  endelse
  
end







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Read SDSS fits files
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function sdss_files::get_all_structdef, files, status=status

  status=1
  
  lnew = self->rdtable(files[0], 1, /silent, status=status)
  
  if status ne 0 then begin 
      ;; do ones best to find a non-empty file to make the structure
      message,files[0]+'  IS AN EMPTY FILE',/inf
      numbfiles=n_elements(files)
      findex=1
      while ( (numbfiles gt 1) and $
              (findex le numbfiles-1) and $
              (status ne 0) )                do begin 
          message,'TRYING NEXT ONE',/inf
          lnew = self->rdtable(files[findex], 1, /silent, status=status)

          findex=findex+1	
      endwhile 
      if status ne 0 then begin 
          message,"NONE OF THESE FILES ARE NON-EMPTY FILES",/inf
          return,-1
      endif 
  endif 

  status=0
  return,lnew[0]

end 

;; Create the output structure

function sdss_files::make_structdef, all_structdef, tagids

  if n_params() lt 2 then begin 
      message,'learn how to use your own program!!'
  endif 

  delvarx, str
  tags = (tag_names(all_structdef))[tagids]

  for i=0l, n_elements(tags)-1 do begin 
      if n_elements(str) eq 0 then begin 
          str = create_struct( tags[i], all_structdef.(tagids[i]) )
      endif else begin 
          str = create_struct( str, tags[i], all_structdef.(tagids[i]) )
      endelse 
  endfor 

  return, str

end 


;; can't use match program because there may be duplicates and we want to 
;; remove them without the resulting taglist being sorted out of the 
;; user's requested order.

function sdss_files::get_structdef, filetype, filelist, $
            taglist=taglist_in, tagids=tagids,  $
            ex_struct=ex_struct, $
            copy=copy, deja_vu=deja_vu, $
            verbose=verbose, status=status

    common sdss_files_read_block, filetype_old, all_structdef
  
    status = 1

    if n_elements(verbose) eq 0 then verbose=1

    ;; Only need to get all_structdef if filetype has changed
    if filetype ne filetype_old then begin
        ;; Get struct information for these files
        all_structdef = self->get_all_structdef(filelist, status=sstatus)
        if sstatus ne 0 then return, -1
        filetype_old = filetype
        deja_vu=1
    endif

    ntags = n_elements(taglist_in) 
    nextra = n_elements(ex_struct)
  
    ;; if no tags sent and no ex_struct, we just return all_structdef
    if ntags eq 0 and nextra eq 0 then begin
        status=0
        copy=0
        structdef = all_structdef
        return, structdef
    endif else begin
        copy=1
        if ntags eq 0 then begin
            structdef = all_structdef
            tagids = lindgen(n_tags(structdef))
        endif else begin
            alltags = tag_names(all_structdef)
            taglist = strupcase(taglist_in)

            ntags = n_elements(taglist)
            nmatch = 0L
            for i=0L, ntags-1 do begin 

                ;; any matches in alltags?
                wtag = where(alltags eq taglist[i], nwtag)

                if nwtag ne 0 then begin 
                    wtag=wtag[0]
                    ;; make sure not a duplicate
                    if nmatch ne 0 then begin 
                        wdup = where(tagids eq wtag, ndup)
                    endif else begin
                        ndup = 0
                    endelse 
            
                    ;; copy in if not duplicate
                    if ndup eq 0 then begin 
                        add_arrval, wtag, tagids
                        nmatch = nmatch + 1
                    endif 
                endif else if verbose gt 0 then begin 
                    ;; no match found
                    ;message,'tag "'+taglist[i]+'" was not found',/inf
                endif 
        
            endfor 

            if nmatch ne 0 then begin 
                structdef = self->make_structdef(all_structdef, tagids)
                status = 0
            endif else begin
                if verbose gt 0 then begin
                    print,'No requested tags found' 
                endif
                return, -1  
            endelse
        endelse

        if nextra ne 0 then begin
            ; we want to catch this error
            comm='structdef2 = create_struct(structdef, ex_struct)'
            if not execute(comm) then begin
                if verbose gt 0 then begin
                    print,'Failed to add ex_struct. Ignoring'
                endif
            endif else begin
                structdef = temporary(structdef2)
            endelse
        endif

    endelse

    status=0
    return, structdef
end 


function sdss_files::copytags, struct, keep, structdef, tagids

  outstruct = replicate(structdef, n_elements(keep))

  ntagids = n_elements(tagids)
  for i=0l, ntagids-1 do begin 
      outstruct.(i) = struct[keep].(tagids[i])
  endfor 
  return,outstruct

end 

;; for tsobj files we remove extra rows
function sdss_files::modrow, filetype
  if filetype eq 'tsobj' or filetype eq 'fpobjc' then return, 1 else return,0
end 



function sdss_files::apply_where_string, lnew, wstring, nkeep
  command = 'keep = '+wstring
  if not execute(command) then begin 
      message,'where string is invalid',/inf
      return,-1
  endif 
  if keep[0] eq -1 then nkeep = 0 else nkeep = n_elements(keep)
  return,keep
end 


function sdss_files::_extension, filetype
  case filetype of
      'tsobj': return,1
      'tsfield': return,1
      'adatc': return,1
      'fpobjc': return,1
      ;; Do not support other extensions here because
      ;; they have variable length arrays
      'psfield': return,6
      else: return,0
  endcase 
end 

pro sdss_files::_set_silent_verbose, silent, verbose
    ns = n_elements(silent)
    nv = n_elements(verbose)

    vdefault = 1
    if ns ne 0 and nv ne 0 then begin
        ; verbose takes precedence
        if verbose eq 0 then silent = 1 else silent = 0
    endif else if ns eq 0 and nv eq 0 then begin
        verbose=1
    endif else if ns ne 0 then begin
        if keyword_set(silent) then verbose=0 else verbose = 1
    endif else if nv ne 0 then begin
        if verbose eq 0 then silent = 1
    endif
end

; What a pain in the ass just because fpObjc don't have the id info
pro sdss_files::add_rrcf, filetype, taglist=taglist, ex_struct=ex_struct, $
    addrun=addrun, addrerun=addrerun, addcamcol=addcamcol, addfield=addfield


    if filetype eq 'fpobjc' then begin

        addrun=1 & addrerun=1 & addcamcol=1 & addfield=1

        ; First, did the user request them in the tags?
        ; If tags sent but not requested, then don't add
        if n_elements(taglist) ne 0 then begin
            tl = strlowcase(taglist)

            w=where(taglist eq 'run',nw)
            if nw eq 0 then addrun=0
            w=where(taglist eq 'rerun',nw)
            if nw eq 0 then addrerun=0
            w=where(taglist eq 'camcol',nw)
            if nw eq 0 then addcamcol=0
            w=where(taglist eq 'field',nw)
            if nw eq 0 then addfield=0
        endif

        ; Now, either add to ex_struct or create ex_struct
        if n_elements(ex_struct) ne 0 then begin
            etags = tag_names(ex_struct)
            if addrun then begin
                w=where(etags eq 'RUN',nw)
                if nw eq 0 then begin
                    ex_struct=create_struct(ex_struct, 'run', 0L)
                endif
            endif
            if addrerun then begin
                w=where(etags eq 'RERUN',nw)
                if nw eq 0 then begin
                    ex_struct=create_struct(ex_struct, 'rerun', 0L)
                endif
            endif
            if addcamcol then begin
                w=where(etags eq 'CAMCOL',nw)
                if nw eq 0 then begin
                    ex_struct=create_struct(ex_struct, 'camcol', 0L)
                endif
            endif
            if addfield then begin
                w=where(etags eq 'FIELD',nw)
                if nw eq 0 then begin
                    ex_struct=create_struct(ex_struct, 'field', 0L)
                endif
            endif

        endif else begin

            if addrun then add_arrval, 'run', tags2add
            if addrerun then add_arrval, 'rerun', tags2add
            if addcamcol then add_arrval, 'camcol', tags2add
            if addfield then add_arrval, 'field', tags2add

            nadd = n_elements(tags2add)
            if nadd gt 0 then begin
                vals2add = replicate('0L', nadd)
                ex_struct = mrd_struct(tags2add, vals2add, 1)
            endif
        endelse
    endif else begin
        addrun=0 & addrerun=0 & addcamcol=0 & addfield=0
    endelse  
end

;docstart::sdss_files::read
; NAME:
;  read()
;
; PURPOSE:
;  Generic SDSS file reader. Reads asTrans, tsObj, tsField, fpObjc, psField...
;  Atlas files have a special reader sf->atlas_read (procedural read_atlas)
;  psField files are normally read the same way as tsObj, etc for extension 6 
;  (the default).  For the other extensions you can send an extension and 
;  read a single field only. 
;
; CATEGORY:
;  SDSS routine.
;
; CALLING SEQUENCE:
;   st = sf->read(type, run, camcol, rerun=, bandpass=, fields=, /all,
;                 taglist=, wstring=, ex_struct=, extension=, dir=, 
;                 /pointers, verbose=, status=)
;
;   if type is 'astrans' the extra keywords node= and inc= exist.
;
; INPUTS:
;   type: The file type. Currently supported types are:
;            astrans, tsobj, tsfield, fpobjc, psfield  
;         Note: for psField, Multiple files can only be read for 
;               extension=6 (the default)
;
;         Atlas files have a special reader sf->atlas read, or for the
;         procedural interface read_atlas.
;   run, camcol: SDSS id info.
;
; OPTIONAL INPUTS:
;   rerun: Rerun number.  If not sent, latest is used.
;   bandpass:  For files that require a bandpass.
;   fields: Field number(s).  If not sent, the first is read.
;   /all: read all fields.
;   taglist: List of tags to read. Default is all.
;   wstring: A string that can be sent to the where function to select
;     objects.  Should refer the structure as "lnew"
;   ex_struct: A structure that will be added to the output structure
;     definition.
;   extension: FITS extension to read.
;   indir: Directory from which to read.  Overrides default directory.
;   dir: Directory used for the read.  
;
;   /pointers:  Return an array of pointers rather than an array of structures.
;      This saves a factor of two in memory since no copy must be made.
;   verbose=: Verbosity level.  0 for silent, 1 for some info, 2 for lots.
;             default 1
;   /silent: Same as verbose=0.  Verbose keyword takes precedence.
;
; OUTPUTS:
;   An array structure or array of pointers if /pointers is sent.
;
; OPTIONAL OUTPUTS:
;   status: 0 for success, 1 for failure
;
; EXAMPLES:
;   run=756
;   camcol=3
;   fields=[35,88]
;   st = sdss_read('tsobj', run, camcol, fields=fields)
;
;   kl=sdss_read('psfield', run, camcol, field=field, extension=3)
;   psf=sdss_psfrec(kl, row, col)
;
;
; MODIFICATION HISTORY:
;   Conglomeration of various read tools.  Erin Sheldon, NYU
;
;docend::sdss_files::read


function sdss_files::read, filetype_in, run, camcol, rerun=rerun, bandpass=bandpass, fields=fields, all=all, extension=extension, indir=indir, dir=dir, taglist=taglist, ex_struct=ex_struct, wstring=wstring, nomodrow=nomodrow, node=node, inc=inc, pointers=pointers, verbose=verbose, silent=silent, status=status

    common sdss_files_read_block, filetype_old, all_structdef
  
    status = 1

    np = n_elements(filetype_in) + n_elements(run) + n_elements(camcol)
    if np lt 3 then begin 
        on_error, 2
        print,'-Syntax: sdss->read(filetype, run, camcol, rerun=, bandpass=, extension=, fields=, /all, indir=, dir=, taglist=, wstring=, /nomodow, ex_struct=, /pointers, /silent, verbose=, /silent, status='
        print,'Supported types: '
        print,'  astrans, tsobj, tsfield, fpobjc, psfield'
        print
        print,'For filetype=asTrans, the extra keywords node=,inc= exist'
        print,'For atlas images see isf->atlas_read() or read_atlas()'
        print
        message,'Halting'
    endif 

    self->_set_silent_verbose, silent, verbose


    filetype = strlowcase(filetype_in)
    if n_elements(filetype_old) eq 0 then filetype_old = 'NONE'

    if not self->filetype_supported(filetype) then begin 
        if verbose gt 0 then begin
            message,'Unsupported file type: '+ntostr(filetype),/inf
        endif
        return,-1
    endif 

    ; astrans are special
    if filetype eq 'astrans' then begin
        return, self->astrans_read(run, camcol, bandpass, rerun=rerun, $
            indir=indir, dir=dir, node=node, inc=inc, $
            silent=silent, status=status)
    endif

    ; psfield has two read modes:
    ;  multiple fields with extension 6
    ;  single fields with any extension.  This is because the structures
    ;     are not fixed for extensions 1-5 from field to field so they
    ;  cannot be put into an array of structures.

    if filetype eq 'psfield' then begin
        if n_elements(extension) ne 0 then begin
            if extension[0] ne 6 then begin
                if n_elements(fields) eq 0 then begin
                    print,'For psfield and ext ne 6 you must specify a field'
                    return,-1
                endif
                return,self->psfield_read(run, camcol, fields, rerun=rerun, $
                                          extension=extension, status=status)
            endif
        endif
    endif

    ntags = n_elements(taglist)
    nextra = n_elements(ex_struct)

    ;; for tsobj files we want to cut rows
    if keyword_set(nomodrow) then modrow=0 else modrow = self->modrow(filetype)
    extension = self->_extension(filetype)

    ;; get the file list.  this is smart in that it only will retrieve
    ;; the file list from disk if the arguments have changed.
    filelist = self->filelist(filetype, run, camcol, $
        rerun=rerun, bandpass=bandpass, $
        fields=fields, fnums=fnums, indir=indir, dir=dir, $
        status = fstatus)

    if fstatus ne 0 then begin 
        if verbose gt 0 then message,'Files not found',/inf
        return,-1
    endif 

    ; if no info sent about field selection, just grab the first one
    if n_elements(fields) eq 0 and not keyword_set(all) then begin
        filelist=filelist[0]
    endif

    ;; if the user wants all tags and no ex_struct then things are 
    ;; simplified.  otherwise we will need to check struct
    ;; definitions

    self->add_rrcf, filetype, taglist=taglist, ex_struct=ex_struct, $
        addrun=addrun, addrerun=addrerun, addcamcol=addcamcol, addfield=addfield

    structdef = self->get_structdef(filetype, filelist, $
        taglist=taglist, tagids=tagids, $
        ex_struct=ex_struct, $
        copy=copy, deja_vu=deja_vu, verbose=verbose, status=sstatus)
    if sstatus ne 0 then begin
        return, -1
    endif

    nfiles = n_elements(filelist)

    ;; data will be copied into this array
    ptrlist=ptrarr(nfiles)

    if verbose gt 0 then begin
        print,'Reading '+ntostr(nfiles)+' fields from Run: '+$
            ntostr(run)+' camcol: '+ntostr(camcol)
    endif
    for i=0l, nfiles-1 do begin 
        file = filelist[i]
        if verbose gt 1 then print,'Reading file: ',file
        lnew = self->rdtable(file, extension, hdr, /silent, $
                             deja_vu=deja_vu, status=status)
        if status eq 0 then begin 

            nrows = n_elements(lnew)

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; user defined cuts via the where string
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            if n_elements(wstring) ne 0 then begin 
                keep = self->apply_where_string(lnew, wstring, nkeep)
            endif else begin 
                keep = lindgen(nrows) & nkeep = nrows
            endelse 

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; special tsobj cut
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            if modrow then begin 
                keepnew = where( lnew[keep].objc_rowc gt 64 and $
                                 lnew[keep].objc_rowc lt 1425, nkeep)
                if nkeep ne 0 then keep = keep[keepnew]
            endif 

            if nkeep ne 0 then begin 
                
                ;; if user requested certain tags we must do a copy
                if copy then begin 
                    copystruct = self->copytags(lnew, keep, structdef, tagids)
                    lnew = 0
                endif else begin 
                    copystruct = lnew[keep]
                    lnew = 0
                endelse 

                if addrun then copystruct.run = run
                if addrerun then copystruct.rerun = rerun
                if addcamcol then copystruct.camcol = camcol
                if addfield then copystruct.field = fnums[i] 

                ptrlist[i] = ptr_new(copystruct, /no_copy)
            endif 
            deja_vu = 1
        endif
    endfor 
  
    if keyword_set(pointers) then begin 
        output = reform_ptrlist(ptrlist)
    endif else begin 
        output = combine_ptrlist(ptrlist)
    endelse 

    if size(output,/tname) eq 'STRUCT' then begin
        status = 0
    endif
    
    return,output

end 





;docstart::sdss_files::psfield_read
; NAME:
;  psfield_read()
;
; PURPOSE:
;  This is a special psfield reader used for psf reconstruction.  This reads
;  extensions 1-6 from the file and places them in a pointer array, unless
;  extension is sent in which case the corresponding structure is returned.  
;  extensions 1-5 correspond to the psf reconstruction information for the
;  u,g,r,i,z bands respectively.  The result from extensions 1-5 can be sent 
;  to the psf reconstruction code sdss_psfrec().  A single field is all that 
;  can be read with this function.  See sdss_read for reading multiple fields 
;  for extension 6. 
;
; CATEGORY:
;  SDSS routine.
;
; CALLING SEQUENCE:
;  psp = sf->psfield_read(run, camcol, field, rerun=, extension=, status=)
;
; INPUTS:
;  run, camcol, field: sdss id info.
;
; OPTIONAL INPUTS:
;  rerun: Optional rerun number.
;  extension: the fits extension to read.  Defaults to all placed in a 
;   pointer array.
;
; OUTPUTS:
;  pointer array with extensions 1-6, or the requested extension.
;
; OPTIONAL OUTPUTS:
;  status: 0 for success 1 for failure.
;   
; MODIFICATION HISTORY:
;  Some time in 2002.  Dave Johnston, Erin Sheldon NYU.
;
;docend::sdss_files::psfield_read

function sdss_files::psfield_read, run, camcol, field, $
                   extension=extension, rerun=rerun, status=status

  status = 1
  if n_params() lt 3 then begin 
      print,$
        '-Syntax: ps = sf->psfield_read(run, camcol, field, rerun=, extension=, status=]'
      print,' Default is all extensions in a pointer array'
      print
      message,'Halting'
  endif 
  
  file = self->filelist('psField', run, camcol, rerun=rerun, fields=field[0])

  if n_elements(extension) ne 0 then begin
      return, mrdfits(file, extension, status=status)
  endif
  
  psp = ptrarr(6)
  for ext=1,6 do begin 
      tmp = mrdfits(file, ext, status=rstatus,/silent)
      if rstatus ne 0 then begin 
          message,'Could not read extension '+strn(ext),/inf
          ptr_free, psp
          return,-1
      endif 
      
      psp[ext-1] = ptr_new( tmp, /no_copy )
  endfor 
  status = 0
  return,psp

end 

;docstart::sdss_files::astrans_read
;
; NAME:
;    astrans_read
;       
; PURPOSE:
;    Read from an asTrans astrometry transformation file for 
;    given run, camcol, and bandpass. Can be used to 
;    transform between (row,col), or CCD coordinates, and (mu,nu),
;    or great circle coords.
;
; CALLING SEQUENCE:
;    st = sf->astrans_read(run, camcol, bandpass, rerun=, 
;                          indir=, dir=, file=, node=, inc=)
;    
; INPUTS: 
;    run, camcol: run and camcol numbers.
;    bandpass: The number of the bandpass.  u,g,r,i,z->0,1,2,3,4
;
; OPTIONAL INPUTS:
;    rerun: SDSS rerun.
;    indir: Directory from which to read.  Overrides default directory.
;    dir: Directory used for the read.  
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
;   status: 0 for success, 1 for failure
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
;docstart::sdss_files::astrans_read


function sdss_files::astrans_read, run, camcol, clr, rerun=rerun, node=node, inc=inc, silent=silent, indir=indir, dir=dir, file=file, status=status

  nr=n_elements(run)
  nc=n_elements(camcol)
  nclr=n_elements(clr)
  IF (nr+nc+nclr) lt 3 THEN BEGIN 
      on_error, 2
      print,'-Syntax: trans=sf->astrans_read(run, camcol, bandpass, rerun=, indir=, dir=, file=, node=, inc=, /silent, status=]'
      print,''
      print,'Use doc_method,"sdss_files::astrans_read"  for more help.'  
      print
      message,'Halting'
  ENDIF 

  status = 1
  colors = ['u','g','r','i','z']
  delvarx,trans

  if n_elements(file) eq 0 then begin
      file = self->filelist('astrans', run, rerun=rerun, $
          indir=indir, dir=dir, status=flstatus)
  endif   
  if flstatus ne 0 then begin
      if not keyword_set(silent) then message,'file not found',/inf
      return, -1
  endif
  hdr = headfits(file)

  camcols = sxpar(hdr,'camcols')
  filters = sxpar(hdr,'filters')
  node = sxpar(hdr,'node')
  inc = sxpar(hdr,'incl')

  camarray = fix( str_sep(camcols, ' ') )
  filtarray = str_sep(filters, ' ')

  ncams = n_elements(camarray)
  nfils = n_elements(filtarray)
  
  wcam = where( camarray EQ camcol, nc)
  IF nc EQ 0 THEN BEGIN
      if not keyword_set(silent) then print,'Camcol ',camcol,' not processed'
      return,-1
  ENDIF 
  if size(clr,/tname) eq 'STRING' then begin
      wclr = where(filtarray eq strlowcase(clr), nclr)
  endif else begin
      wclr = where(filtarray EQ colors[clr], nclr)
  endelse
  IF nclr EQ 0 THEN BEGIN 
      if not keyword_set(silent) then print,'Filter ',colors[clr],' not processed'
      return, -1
  ENDIF 

  ext = wcam[0]*nfils + (wclr[0] + 1)
  trans = mrdfits(file, ext, ehdr,/silent)

  ccam=sxpar(ehdr, 'camcol',count=count)
  IF count EQ 0 THEN BEGIN
      if not keyword_set(silent) then print,'No camcol in header'
      return, -1
  ENDIF 
  cfilt=sxpar(ehdr, 'filter',count=count)
  IF count EQ 0 THEN BEGIN
      if not keyword_set(silent) then print,'No filter in header'
      return, -1
  ENDIF 

  status=0
  return, trans
END 





function sdss_files::datasweep_dir, run, camcol, rerun=rerun
  basedir=sdssidl_config('calibobj_dir')
  return, basedir
end 
pro sdss_files::_datasweep_read_filelist, reload=reload
  common datasweep_block, files
  if n_elements(file) eq 0 or keyword_set(reload) then begin 
      dir = self->datasweep_dir(run,camcol,rerun=rerun)
      pattern = 'calibObj-??????-?-*.fit*'
      pattern = concat_dir(dir, pattern)

      files = file_search(pattern)
  endif 
end 
function sdss_files::datasweep_filelist
  common datasweep_block, files
  self->_datasweep_read_filelist
  return, files
end 

function sdss_files::datasweep_file, run, camcol, type_in, rerun=rerun, $
                   pattern=pattern, $
                   nodir=nodir, status=status, nocompress=nocompress

  if n_params() lt 3 then begin 
      on_error, 2
      print,'-Syntax: sf->datasweep_file(run,camcol,type,/nocompress,/pattern,/nodir,status=)'
  endif 
  common datasweep_block, files

  type = strlowcase(type_in)
  if type ne 'gal' and type ne 'star' then message,'Type must be "gal" or "star"'
  
  file = 'calibObj-'+self->run2string(run)+'-'+ntostr(camcol)+$
    '-'+type+'.fit'

  if not keyword_set(pattern) then file = file +'s' else file='*'+file+'*'
  if not keyword_set(nocompress) then file=file+'.gz'

  if not keyword_set(nodir) and not keyword_set(pattern) then begin 
      dir = self->datasweep_dir(run,camcol,rerun=rerun)
      file=concat_dir(dir,file)
  endif 

  return, file

end 

function sdss_files::datasweep_matchfile, run, camcol, type, rerun=rerun
  common datasweep_block, files
  self->_datasweep_read_filelist

  pattern=self->datasweep_file(run,camcol,type,rerun=rerun,/pattern)
  w=where( strmatch(files,pattern), nw)
  if nw eq 0 then return,'' else return,files[w[0]]
end 


function sdss_files::datasweep_read, run, camcol, type, rerun=rerun, $
    silent=silent, verbose=verbose, $
    status=status

  if n_params() lt 3 then begin 
      on_error, 2
      print,'-Syntax: sf->datasweep_read(run,camcol,type,verbose=, /silent, status=)'
  endif 
  status = 1
  common datasweep_block, files

  status=1

  if keyword_set(silent) then verbose=0
  if n_elements(verbose) eq 0 then verbose = 1

  file = self->datasweep_matchfile(run,camcol,type,rerun=rerun)
  if file eq '' then return,-1

  if verbose gt 0 then print,'Reading file: ',file
  st = mrdfits(file, 1, silent=silent, status=status)
  return, st

end 

;docstart::sdss_files::atlas_read
; NAME:
;  obj->atlas_read
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
;    obj->atlas_read, objStruct -OR- run, rerun, camcol, field, id, 
;                clr=, 
;                image=, 
;                imu=, img=, imr=, imi=, imz=, 
;                atlas_struct=,
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
;         image=:  If clr= is sent then the image is returned through this
;            keyword.
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
;    IDL> atlas_read, run, rerun, camcol, field, id, imr=imr
;  - Read using a structure; get all images in atlas_struct
;    IDL> atlas_read, objStruct, atlas_struct=as
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
;docend::sdss_files::atlas_read


pro sdss_files::atlas_read_syntax

  print,'Syntax - '
  print,'  sf->atlas_read, objStruct -OR- run, camcol, field, id, rerun='
  print,'              clr='
  print,'              image=, '
  print,'              imu=, img=, imr=, imi=, imz=, '
  print,'              atlas_struct=, '
  print,'              index=, '
  print,'              dir=, '
  print,'              col0=, row0=, dcol=, drow=, ncol=, nrow=,'
  print,'              status=, '
  print,'              /silent,'
  
  print,''
  print,'Use doc_method,"sdss_files::atlas_read"  for more help'
end 

function sdss_files::atlas_read_checktags, struct, run, rerun, camcol, field, id

    tags = tag_names(struct)
  
    wrun    = where(tags EQ 'RUN', nrun)
    wrerun  = where(tags EQ 'RERUN', nrerun)
    wcamcol = where(tags EQ 'CAMCOL', ncamcol)
    wfield  = where(tags EQ 'FIELD', nfield)
    wid     = where(tags EQ 'ID', nid)

    IF nrun    EQ 0 THEN print,'atlas_read: Structure must have RUN tag'
    IF nrerun  EQ 0 THEN print,'atlas_read: Structure must have RERUN tag'
    IF ncamcol EQ 0 THEN print,'atlas_read: Structure must have CAMCOL tag'
    IF nfield  EQ 0 THEN print,'atlas_read: Structure must have FIELD tag'
    IF nid     EQ 0 THEN print,'atlas_read: Structure must have ID tag'
 
    testSum = nrun + nrerun + ncamcol + nfield + nid
    IF testSum NE 5 THEN idExist = 0 ELSE idExist = 1
    if not idExist then return, idExist

    run = Struct[0].run
    rerun = Struct[0].rerun
    camcol = Struct[0].camcol
    field = Struct[0].field
    id = Struct[0].id

    return,idExist

end 


pro sdss_files::atlas_read, run_OR_struct, camcol, field, id, rerun=rerun, $
                clr=clr, $
                image=image, $
                $
                dir=dir, $
                index=index_in, $
                $
                imu=imu, $
                img=img, $
                imr=imr, $
                imi=imi, $
                imz=imz, $
                atlas_struct=atlas_struct, $
                $
                col0=col0, row0=row0, $
                dcol=dcol, drow=drow, $
                ncol=ncol, nrow=nrow, $
                $
                status=status, $
                $
                silent=silent

  On_error,2
  status = 1

  sdssidl_setup, /silent

  proNames  = routine_info(/system)
  wrd = where(proNames EQ 'RDATLAS', nrd)
  IF nrd EQ 0 THEN BEGIN 
      message,'The rdAtlas DLM was not found'
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check arguments
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ninput = n_elements(run_or_struct)
    nind = n_elements(index_in)
    if nind ne 0 then begin
        index=index_in[0]
        if index ge ninput then begin
            message,'index is out of bounds', /inf
            return
        endif
    endif else begin
        index=0
    endelse

  runType = size(run_OR_struct, /tname) 

  IF runType EQ 'STRUCT' THEN BEGIN 

      nObjStruct = n_elements(run_OR_struct)

      objStruct = run_OR_struct[index]
 
      IF NOT self->atlas_read_checktags(objStruct, $
                         run, rerun, camcol, field, id) THEN return

      ;; If struct is sent, we will try to display extra info
      info=1
  ENDIF ELSE BEGIN 
      np = $
          n_elements(run_or_struct) + $
          n_elements(camcol) + $
          n_elements(field) + $
          n_elements(id)
      IF np LT 4 then BEGIN
          self->atlas_read_syntax
          return
      ENDIF ELSE BEGIN 
          run = run_or_struct
          if n_elements(rerun) eq 0 then begin
              rerun=self->rerun(run)
              if rerun[0] eq -1 then begin
                  print,'Run: '+ntostr(run)+' not found'
                  return
              endif
          endif
      ENDELSE 

  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check for output arguments
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  apu = arg_present(imu)
  apg = arg_present(img)
  apr = arg_present(imr)
  api = arg_present(imi)
  apz = arg_present(imz)
  
  imargsp = apu+apg+apr+api+apz

  apimage = arg_present(image)
  
  apatlas_struct = arg_present(atlas_struct)

  IF imargsp EQ 0 AND apimage EQ 0 AND apatlas_struct EQ 0 THEN BEGIN 
      print,'Please request a returned image in some form'
      self->atlas_read_syntax
      print
      return
  ENDIF 

  fname = self->file('fpatlas', run, camcol, rerun=rerun, fields=field)
  if not fexist(fname) then begin
      if not keyword_set(silent) then print,'Atlas file not found: '+fname
      return
  endif
  IF NOT keyword_set(silent) THEN BEGIN
      print,'atlas_read: Reading atlas image: ',fname
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Using an execution string allows us to take advantage 
  ;; of the speedup of rdAtlas when only certain images are requested.
  ;; There is a 50% gain when only one image is requested
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  command = $
    'rdAtlas, fname, id, ' + $
    ' row0=row0, col0=col0, '+$
    ' drow=drow, dcol=dcol, '+$
    ' ncol=ncol, nrow=nrow, '+$
    ' status=status'


  IF n_elements(clr) NE 0 THEN BEGIN 
      CASE clr[0] OF 
          0: cimname='imu'
          1: cimname='img'
          2: cimname='imr'
          3: cimname='imi'
          4: cimname='imz'
         ELSE: message,'clr must be in [0,4]'
      ENDCASE 
      command = command + ', '+cimname+'='+cimname
  ENDIF ELSE BEGIN 

      ;; For atlas_struct, we always get all images unless clr=clr was sent
      ;; or one of the color arguments was sent.


      IF arg_present(atlas_struct) AND imargsp EQ 0 THEN BEGIN 
          command = command + ',imu=imu,img=img,imr=imr,imi=imi,imz=imz'
      ENDIF ELSE IF imargsp NE 0 THEN BEGIN  
          IF apu THEN command = command + ', imu=imu'
          IF apg THEN command = command + ', img=img'
          IF apr THEN command = command + ', imr=imr'
          IF api THEN command = command + ', imi=imi'
          IF apz THEN command = command + ', imz=imz'
      ENDIF ELSE IF arg_present(image) THEN BEGIN 
          command = command + ',imu=imu,img=img,imr=imr,imi=imi,imz=imz'
      ENDIF ELSE BEGIN 
          
      ENDELSE 

  ENDELSE 


  IF NOT execute(command) THEN BEGIN 
      print,'Error executing rdAtlas'
      return
  ENDIF 

  IF status NE 0 THEN return

  IF arg_present(atlas_struct) THEN BEGIN 

      mkAtlasCommand = $
        'atlas_struct = create_struct("run", run,'+$
        '"rerun", rerun, '  +$
        '"camcol", camcol, '+$
        '"field", field, '  +$
        '"id", id, '        +$
        '"col0", col0, '    +$
        '"row0", row0, '    +$
        '"ncol", ncol, '    +$
        '"nrow", nrow, '    +$
        '"dcol", dcol, '    +$
        '"drow", drow, '    +$
        '"status", status'

      IF n_elements(imu) NE 0 THEN BEGIN 
          mkAtlasCommand = $
            mkAtlasCommand + ', "imu", imu'
      ENDIF 
      IF n_elements(img) NE 0 THEN BEGIN 
          mkAtlasCommand = $
            mkAtlasCommand + ', "img", img'
      ENDIF 
      IF n_elements(imr) NE 0 THEN BEGIN 
          mkAtlasCommand = $
            mkAtlasCommand + ', "imr", imr'
      ENDIF 
      IF n_elements(imi) NE 0 THEN BEGIN 
          mkAtlasCommand = $
            mkAtlasCommand + ', "imi", imi'
      ENDIF 
      IF n_elements(imz) NE 0 THEN BEGIN 
          mkAtlasCommand = $
            mkAtlasCommand + ', "imz", imz'
      ENDIF 

      mkAtlasCommand = mkAtlasCommand + ')'

      IF NOT execute(mkAtlasCommand) THEN BEGIN 
          print,'Could not make atlas structure'
          return
      ENDIF 

  ENDIF ELSE BEGIN 
      ; Was a particular bandpass requested?
      IF n_elements(clr) NE 0 THEN BEGIN 
          CASE clr[0] OF 
              0: image = temporary(imu)
              1: image = temporary(img)
              2: image = temporary(imr)
              3: image = temporary(imi)
              4: image = temporary(imz)
              ELSE: message,'clr must be in [0,4]'
          ENDCASE 
      ENDIF ELSE BEGIN 
          ;; copy images into this argument
          IF NOT imargsp AND apimage THEN BEGIN 
              sz = size(imu,/dim)
              image = lonarr(5, sz[0], sz[1])
              
              image[0,*,*] = temporary(imu)
              image[1,*,*] = temporary(img)
              image[2,*,*] = temporary(imr)
              image[3,*,*] = temporary(imi)
              image[4,*,*] = temporary(imz)
          ENDIF 
      ENDELSE 
  ENDELSE 


return
end




;docstart::sdss_files::atlas_view
;
; NAME:
;       sf->atlas_view
; PURPOSE:
;	Display the atlas images for an object.  Images are read using the
;       dynamically linked DLM rdAtlas (type rdAtlas at the prompt for 
;       syntax).
;
; CALLING SEQUENCE:
;        sf->atlas_view, objStruct -OR- run, rerun, camcol, field, id, 
;                    clr=, 
;                    imtot=, 
;                    imu=, img=, imr=, imi=, imz=, 
;                    dir=, 
;                    index=, 
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
; IDL> sf=obj_new('sdss_files')
; IDL> run=745 & rerun=20 & camcol=3 & field = 123 & id = 27
; IDL> sf->atlas_view, run, rerun, camcol, field, id
;
;   Use an input struct. Save the r-band image.
;
; IDL> sdssidl_setup
; IDL> !sdss->atlas_view, struct[27], clr=2, imr=red_image
; 
;
; REVISION HISTORY:
;       Author:
;       16-Nov-2004: Erin Scott Sheldon UChicago  
;                        Based on obsolete get_atlas.pro
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
;docend::sdss_files::atlas_view


;
; NAME: 
;    atlas_view_copy
;
; PURPOSE: 
;    Copy into big image

PRO sdss_files::atlas_view_copy, beg, ncol, nrow, im, imtot        

  n_dim = size(im, /n_dim)
  sz = size(im, /dim)

  IF (n_dim NE 2) THEN BEGIN 
      imtot[beg:beg+ncol-1, *] = lonarr(ncol, nrow)
  ENDIF ELSE IF (sz[0] NE ncol) OR (sz[1] NE nrow) THEN BEGIN 
      imtot[beg:beg+ncol-1, *] = lonarr(ncol, nrow)
  ENDIF ELSE BEGIN 
      imtot[beg:beg+ncol-1, *] = im
  ENDELSE 

  return
END 

;
; NAME: 
;    atlas_view_combine
;
; PURPOSE: 
;    Combine into one big image


PRO sdss_files::atlas_view_combine, colors, ncol, nrow, imtot, $
                        imu=imu, $
                        img=img, $
                        imr=imr, $
                        imi=imi, $
                        imz=imz

  ncolor = n_elements(colors)
  
  ;; Side-by-Side
  total_size_x = ncol*ncolor
  total_size_y = nrow

  imtot = lonarr(total_size_x, total_size_y)

  beg = 0L
  FOR i=0L, ncolor-1 DO BEGIN 

      CASE colors[i] OF
          0: self->atlas_view_copy, beg, ncol, nrow, imu, imtot
          1: self->atlas_view_copy, beg, ncol, nrow, img, imtot
          2: self->atlas_view_copy, beg, ncol, nrow, imr, imtot
          3: self->atlas_view_copy, beg, ncol, nrow, imi, imtot
          4: self->atlas_view_copy, beg, ncol, nrow, imz, imtot
          ELSE: 
      ENDCASE 
      beg = beg+ncol

  ENDFOR 


END 

;
; NAME: 
;    atlas_view_checktags
;
; PURPOSE:
;    Check for required tags in struct

PRO sdss_files::atlas_view_checktags, struct, $
                          idexist, nchild_exist, $
                          radec_exist, $
                          silent=silent

  tags = tag_names(struct)
  
  ;; check for required tags
  match, tags, 'RUN', mtmp, mrun
  match, tags, 'RERUN', mtmp, mrerun
  match, tags, 'CAMCOL', mtmp, mcamcol
  match, tags, 'FIELD', mtmp, mfield
  match, tags, 'ID', mtmp, mid

  IF mrun[0] EQ -1 THEN print,'atlas_view: Structure must have RUN tag'
  IF mrerun[0] EQ -1 THEN print,'atlas_view: Structure must have RERUN tag'
  IF mcamcol[0] EQ -1 THEN print,'atlas_view: Structure must have CAMCOL tag'
  IF mfield[0] EQ -1 THEN print,'atlas_view: Structure must have FIELD tag'
  IF mid[0] EQ -1 THEN print,'atlas_view: Structure must have ID tag'
  IF ( (mrun[0] EQ -1) OR $
       (mrerun[0] EQ -1) OR $
       (mcamcol[0] EQ -1) OR $
       (mfield[0] EQ -1) OR $
       (mid[0] EQ -1) ) THEN BEGIN 

      idexist = 0
      return
  ENDIF ELSE idexist = 1

  wt = where(tags EQ 'NCHILD', nwt)
  IF nwt NE 0 THEN nchild_exist = 1 ELSE nchild_exist=0
  
  ;; check for ra/dec
  wra = where(tags EQ 'RA')
  wdec = where(tags EQ 'DEC')

  IF (wra[0] EQ -1 OR wdec[0] EQ -1) THEN BEGIN
      IF NOT keyword_set(silent) THEN BEGIN 
          print,'atlas_view: No RA/DEC found. Not displaying position'
      ENDIF 
      radec_exist = 0
  ENDIF ELSE radec_exist=1

END 

; NAME:
;    atlas_view_compute_sep
; PURPOSE:
;  compute the seperation in arc seconds and position angle
;  between an objects two children if it has two children
;  position angle is an angle between 0 degrees and 180 degrees
;  NOT 0 to 360 
;  so it is not really the position angle of ONE with
;  respect to the OTHER (ie. it is order independent)
;  0 degrees is horizontal and 90 degrees vertical 
;  cat is the sdss photo structure and index is the index of the parent
;  assumes two children are indices index+1 and index+2
;
; CALLING SEQUENCE:
;  get_atlas_compute_sep,cat,index,separation,position_angle
;
;Revision History:
;	David Johnston 5/20/99

pro sdss_files::atlas_view_compute_sep,cat,index,sep,pa


  if n_params() LT 2 then begin
      print,'-syntax atlas_view_compute_sep,cat,index,sep,pa'
      return
  endif

  if cat(index).nchild ne 2 then begin
      print,'GET_ATLAS_COMPUTE_SEP : object must have two children'
      print,'to compute seperation and pa'
      sep = -1 & pa=-1
      return
  endif

  c1=cat(index+1).objc_colc
  c2=cat(index+2).objc_colc
  r1=cat(index+1).objc_rowc
  r2=cat(index+2).objc_rowc
  dc=c1-c2
  dr=r1-r2

  sep=.4*sqrt(dc^2+dr^2)
;.4 arcsec /pixel for sdss only

  if sep lt .04 then begin
                                ;sep less than .1 pixels so define pa to be 0 by default
                                ;since it is not really meaningful
      pa=0.0
  endif else begin
      pa=atan(dr,dc)
  endelse

  pa=pa*180.0/3.14159
  if pa lt 0 then pa=180+pa

  return
end

;
; NAME: 
;    atlas_view_posangle
;
; PURPOSE: 
;    Measure the position angle when deblended into two objects

PRO sdss_files::atlas_view_posangle, objStruct, index, sep, pa, silent=silent

    ntot = n_elements(objStruct)
    if index[0] ge ntot then begin
        message,'Index is out of bounds',/inf
        return
    endif
    obj_id = objStruct[index].id

  ;; check nchild 
  IF tag_exist(objStruct[0], 'nchild') THEN BEGIN 

      IF (objStruct[index].nchild EQ 2) THEN BEGIN 
          ;; are there are two more objects after this?
          IF ( index LE ((ntot-1)-2) ) THEN BEGIN 
              ;; are they the children?
              IF( (objStruct[index+1].id EQ (obj_id+1)) AND $
                  (objStruct[index+2].id EQ (obj_id+2)) ) THEN BEGIN 
                  self->atlas_view_compute_sep, objStruct, index, sep, pa
              ENDIF ELSE BEGIN 
                  IF NOT keyword_set(silent) THEN print,$
                    'GET_ATLAS: Cannot compute sep/pos angle: children not in objStructure'
              ENDELSE 
          ENDIF ELSE BEGIN
              IF NOT keyword_set(silent) THEN print,$
                'GET_ATLAS: Cannot compute sep/pos angle: children not in structure'
          ENDELSE 
      ENDIF 
  ENDIF 

END 

;
; NAME: 
;    atlas_view_subtitle
;
; PURPOSE: 
;    Create the subtitle with colors

function sdss_files::atlas_view_subtitle, mag, colors, domag=domag

  subtitle = ''
  tsubtitle = ''
  IF keyword_set(domag) THEN BEGIN 
      magstr=['','','','','']
      cname = ['u','g','r','i','z']
      
      nclr = n_elements(colors)
      FOR j=0,nclr-1 DO BEGIN 
          jj = colors[j]
          magstr[jj]=strmid( strtrim(string(mag[jj]),2), 0, 5)
      ENDFOR 
      
;      FOR j=0,4 DO BEGIN 
;          jj = colors[j]
;          IF (magstr[jj] NE '') THEN BEGIN 
;              IF (subtitle NE '') THEN subtitle=subtitle+'  '
;              subtitle=subtitle + cname[jj]+'='+magstr[jj]
;          ENDIF 
;      ENDFOR 
      FOR j=0L,nclr-1 DO BEGIN 
          jj = colors[j]
          IF (magstr[jj] NE '') THEN BEGIN 
              IF (subtitle NE '') THEN subtitle=subtitle+'  '
              subtitle=subtitle + cname[jj]+'='+magstr[jj]
          ENDIF 
      ENDFOR       

      FOR j=0,3 DO BEGIN 
          IF (magstr[j] NE '' AND magstr[j+1] NE '') THEN BEGIN 
              diff=strmid(strtrim(string(mag[j] - mag[j+1]),2), 0, 5)
              tsubtitle=tsubtitle+'  '+cname[j]+'-'+cname[j+1]+'='+diff
          ENDIF 
      ENDFOR 
      subtitle=subtitle + '  ' + tsubtitle
      
  ENDIF 

  return,subtitle

END 

;
; NAME: 
;    atlas_view_titles
;
; PURPOSE: 
;    Create the comples titles

pro sdss_files::atlas_view_titles, objStruct, colors, imtot, $
                               title, subtitle, $
                               maguse=maguse, $
                               hideradec=hideradec, $
                               sep=sep, pa=pa, $
                               silent=silent, $
                               _extra=_extra

  tags = tag_names(objStruct)
  wmag = sdss_maguse(objStruct, maguse=maguse, silent=silent)

  IF (wmag NE -1) AND (NOT keyword_set(silent)) THEN BEGIN  
      print,'atlas_view: Using '+tags[wmag]
  ENDIF 
  IF wmag EQ -1 THEN domag=0b ELSE domag=1b

  self->atlas_view_checktags, objStruct, $
    idexist, nchild_exist, $
    radec_exist, $
    silent=silent

  IF NOT idexist THEN return
  IF NOT radec_exist THEN dora=0b ELSE dora=1b

  run = objStruct[0].run
  rerun = objStruct[0].rerun
  camcol = objStruct[0].camcol
  field = objStruct[0].field
  id = objStruct[0].id

  IF domag THEN BEGIN 
      subtitle = self->atlas_view_subtitle(objStruct[0].(wmag), colors, domag=domag)
  ENDIF 
  ;; Title can be sent through _extra
  IF n_elements(title) EQ 0 THEN BEGIN 
      title = self->run2string(run)+'-'+ntostr(rerun)+'-'+ntostr(camcol)
      title = title+ '-'+self->field2string(field)+$
        '-'+strn(id,length=5,padchar='0')      

      if (not keyword_set(hideradec)) AND dora then begin
          radecstr, objStruct.ra, objStruct.dec, ra, dec
          title=title+ '  '+ra+'  '+dec
      endif

      IF n_elements(sep) NE 0 AND n_elements(pa) NE 0 AND $
        nchild_exist THEN BEGIN
          sepst=strtrim(sep,2)  ;take only one digit after decimal place
          title=title+' sep: '+strmid(sepst,0,strpos(sepst,'.')+2)
          title=title+' pa: '+strtrim(string(fix(pa)),2)
      ENDIF

  ENDIF 


END 

;
; NAME: 
;    atlas_view_display
;
; PURPOSE: 
;    Informational labels

pro sdss_files::atlas_view_display, objStruct, colors, imtot, $
                        maguse=maguse, $
                        hideradec=hideradec, $
                        silent=silent, $
                        _extra=_extra

  self->atlas_view_titles, objStruct, colors, imtot, $
    use_title, subtitle, $
    maguse=maguse, $
    hideradec=hideradec, $
    silent=silent, $
    _extra=_extra

  tvasinh, imtot, _extra=_extra, title=use_title, subtitle=subtitle

end 

;
; NAME: 
;    atlas_view_display_basic
; PURPOSE: 
;    Very simple viewer.

pro sdss_files::atlas_view_display_basic, $
    imtot, run, rerun, camcol, field, id, $
    _extra=_extra

  ;; Title can be sent through _extra
  IF n_elements(title) EQ 0 THEN BEGIN 
      title = self->run2string(run)+'-'+ntostr(rerun)+'-'+ntostr(camcol)
      title = title+ '-'+self->field2string(field)+$
        '-'+strn(id,length=5,padchar='0')      
  endif 

  tvasinh, imtot, _extra=_extra, title=title

end 
 
pro sdss_files::atlas_view_syntax

    on_error, 2
    print,'Syntax -  '
    print,'  atlas_view, objStruct -OR- run, camcol, field, id, rerun='
    print,'              clr=, '
    print,'              dir=, '
    print,'              index=, '
    print,'              imu=, img=, imr=, imi=, imz=, '
    print,'              imtot=, '
    print,'              col0=, row0=, dcol=, drow=, ncol=, nrow=,'
    print,'              status=, '
    print,'              /info, '
    print,'              maguse=, '
    print,'              /hideradec, '
    print,'              /silent,'
    print,'              _extra=_extra'
  
    print,''
    print,'Use doc_method,"sdss_files::atlas_view"  for more help'
    print
    message,'Halting'
end 


pro sdss_files::atlas_view, run_OR_struct, camcol, field, id, rerun=rerun, $
                clr=clr, $
                dir=dir, $
                index=index_in, $
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

    status = 1

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Check arguments
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ninput = n_elements(run_or_struct)
    nind = n_elements(index_in)
    if nind ne 0 then begin
        index=index_in[0]
        if index ge ninput then begin
            message,'index is out of bounds', /inf
            return
        endif
    endif else begin
        index=0
    endelse

    runType = size(run_OR_struct, /tname) 

    if runtype eq 'STRUCT' then begin 

        nObjStruct = n_elements(run_OR_struct)

        objStruct = run_OR_struct[index]

        self->atlas_view_checktags, objStruct, $
            idexist, nchild_exist, $
            radec_exist, $
            silent=silent

        IF NOT idexist THEN return

        run = objStruct[0].run
        rerun = objStruct[0].rerun
        camcol = objStruct[0].camcol
        field = objStruct[0].field
        id = objStruct[0].id

        ;; If struct is sent, we will try to display extra info
        info=1
    endif else begin 

        np = $
          n_elements(run_or_struct) + $
          n_elements(camcol) + $
          n_elements(field) + $
          n_elements(id)
        if np lt 4 then begin
            self->atlas_view_syntax
            return
        endif else begin 
            run = run_or_struct
            if n_elements(rerun) eq 0 then begin
                rerun=self->rerun(run)
                if rerun[0] eq -1 then begin
                    print,'Run: '+ntostr(run)+' not found'
                    return
                endif
            endif
        endelse 

  endelse 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  sdssidl_setup, /silent

  ;; which colors are we getting?           
  
  ncolor = n_elements(clr)
  if ncolor eq 0 then begin 
      clr = [0,1,2,3,4]
      ncolor = 5
  endif else begin 
      maxclr = max(clr, min=minclr)
      if maxclr gt 4 or minclr lt 0 then begin 
          print,'clr must be in [0,4]'
          return
      endif 
  endelse 
  colors = clr[sort(clr)]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; construct the atlas directory and file name
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if n_elements(dir) eq 0 then begin
        fname=sdss_file('fpatlas',run,camcol,field=field,rerun=rerun)
    endif else begin
        fname=sdss_file('fpatlas',run,camcol,field=field,rerun=rerun, /nodir)
        fname=concat_dir(dir,fname)
    endelse

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read the images using the system routine
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  delvarx, $
    imu, img, imr, imi, imz, $
    row0, col0, drow, dcol

  if not keyword_set(silent) then begin 
      print,'atlas_view: Reading atlas image: '+fname
  endif 

  
  rdAtlas, fname, id, $
    imu=imu, $
    img=img, $
    imr=imr, $
    imi=imi, $
    imz=imz, $
    row0=row0, col0=col0, $
    drow=drow, dcol=dcol, $
    ncol=ncol, nrow=nrow, $
    status=status
  
  if status ne 0 then begin 
      return
  endif else begin 

      self->atlas_view_combine, colors, ncol, nrow, imtot, $
        imu=imu, $
        img=img, $
        imr=imr, $
        imi=imi, $
        imz=imz

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Does the user request additional info plotted?
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      if keyword_set(info) then begin

          IF n_elements(objStruct) EQ 0 THEN BEGIN 
              tags = ['run','rerun','camcol','field','id',$
                      'ra','dec','nchild']
              if n_elements(maguse) eq 0 then begin
                  tags = [tags, 'counts_model'] 
              endif else begin 
                  tags = [tags, maguse]
              endelse 
              
              if not keyword_set(silent) then begin 
                  print,'atlas_view: Reading tsObj file'
              endif 
              read_tsobj, [run,rerun,camcol],objStruct, start=field, verbose=0
              nObjStruct = n_elements(objStruct)
              if nobjstruct eq 0 then begin 
                  ;; couldn't read the tsObj file.  Basic display
                  self->atlas_view_display_basic, $
                    imtot, run, rerun, camcol, field, id, $
                    _extra=_extra
              endif 
          endif 

          ;; Can we get position angle info?
          if nobjstruct gt 1 then begin 
              IF runType EQ 'STRUCT' THEN BEGIN 
                  self->atlas_view_posangle, run_OR_struct, index, sep, pa, $
                    silent=silent
              endif else begin 
                  ind = where(objStruct.id EQ id, nw)
                  if nw ne 0 then begin
                    self->atlas_view_posangle, objStruct, ind, sep, pa, $
                        silent=silent
                  endif
              endelse 
          endif

          self->atlas_view_display, objStruct, colors, imtot, $
            sep=sep, pa=pa, $
            silent=silent, hideradec=hideradec, $
            _extra=_extra
      endif else begin 

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Just the basic display
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          self->atlas_view_display_basic, $
            imtot, run, rerun, camcol, field, id, $
            _extra=_extra
      endelse 

  endelse 

  return
end

;docstart::sdss_files::objmask_read
; NAME:
;  sdss_files::objmask_read()
;
; PURPOSE:
;  Read in object mask data for objects in an SDSS atlas file.
;
; CATEGORY:
;  SDSS routine.
;
; CALLING SEQUENCE:
;  st = !sdss->objmask_read(run, camcol, field, rerun=, idlist=, status=)
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
;docend::sdss_files::objmask_read

function sdss_files::objmask_read, run, camcol, field, $
    rerun=rerun, $
    idlist=idlist, $
    status=status

    nr = n_elements(run) & nc=n_elements(camcol) & nf = n_elements(field)
    if (nr+nc+nf) lt 3 then begin
        print, '-Syntax: st = read_objmask(run, camcol, field, rerun=, idlist=, status=)'
        return,-1
    endif
    if n_elements(rerun) eq 0 then begin
        rerun=self->rerun(run)
    endif

    fname = self->file('fpatlas', run, camcol, rerun=rerun, fields=field)

    ; Just in case not linked in
    struct = call_function('rdObjmask',fname, idlist=idlist, status=status)

    if size(struct,/tname) eq 'STRUCT' then begin
        ; these are actually copied by rdobjmask when the atlas image
        ; exists (and parent/id too) but we want the ones without an atlas
        ; to get this info too
        struct.run=run
        struct.rerun=rerun
        struct.camcol=camcol
        struct.field=field
    endif
        
    return, struct

end




function sdss_files::cleanup
  return,1
end 


pro sdss_files__define

  struct = {$
             sdss_files, $
             inherits mrdfits $
           }

end 

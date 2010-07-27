;+
; NAME:
; 	fetch_file_list	
;
; PURPOSE: 
;  Will return the list of tsObj files in directory between start and
;  start+nframe Can also have a front other than tsObj if so desired, but must
;  have form front-run-camcol-rerun-field.extension
;
; CALLING SEQUENCE:
;	fetch_file_list,dir, files, fnums [, 
;                    start=start, last=last, nframes=nframes,
;                    run=run, camcol=camcol, rerun=rerun,  
;                    fieldmin=fieldmin, fieldmax=fieldmax, 
;                    front=front, nchar=nchar, 
;                    allfiles=allfiles, allfnums=allfnums]
;
; INPUTS:
;	dir:  the full directory where the tsObj files live.
;         Must be of the form: "..../run/rerun/calibChunks/camcol/" or
;         fetch_file_list will fail.
;
; OPTIONAL INPUTS:
;       start: the first field to consider (default is first)
;       last: last field to read. All fields in between start and last will
;             be read
;	nframes: the total number of files to fetch (default is one)
;       front:  The front string in the files.  Default is "tsObj"
;       nchar:  The number of characters in the name.  
;               Default is 25 for tsObj files.  
;               ----THIS KEYWORD IS NOW IGNORED (UNNECESSARY)
;
; OUTPUTS:
;	files: a string array of the file names (full path)
;	fnums: a "long" array of the field numbers
;
; OPTIONAL OUTPUTS:	
;       run: the run 
;	camcol: the camera collumn
;	rerun: the rerun number
;		(presumably you had to have known these three
;		variables to get the directory 
;		but you may not have had them handy
;		so are output if you need them)
;
;       fieldmin, fieldmax: first and last field numbers
;       nfields: actual number of files found
;       allfiles: all the files for this run/rerun
;       allfnums: all field numbers
;
; EXTERNAL CALLS:
;      	none 
; METHOD:
;	uses find_file function   
; EXAMPLE
;	IDL> fetch_dir,run,camcol,rerun,dir
;	IDL> fetch_file_list,dir,files,start=13,nf=20
;	IDL> help,files   
;   	FILES           STRING    = Array[100]
;	IDL> print,files(0)
;	/usr/sdss/data02/imaging/273/1/calibChunks/2/tsObj-000273-2-1-0011.fit
;
;
; NOTES
;
; HISTORY:  written by David Johnston -University of Chicago
;       Physics Department October 9 1999
;       11/13/99  Added "front" and "nchar" inputs  Erin Scott Sheldon
;       2/20/2000 made start, nframes optional. defaults set.  E.S.S.
;         Now requires strict directory format. Gets run/camcol/rerun from
;         directory name. Checks for files with those numbers. 13-Nov-2000 E.S.S.
;       July-2002: no longer dies on certain file name format issues
;                  e.g. it can deal with rerun > 9 and variable length
;                  front without the use of nchar (now ignored). Erin Sheldon
;       08-Sep-2002: -added "last" input.  
;                    -No longer defaults to first if input range begins at a 
;                     point less than the first field.  Same with the end.
;                     Thus there may be no files found. This way the
;                     user always returns data from the requested field
;                     range or none at all.
;-

pro ffl_get_fields, start=start, last=last, nframes=nframes, fields=fields

    if n_elements(start) ne 0 then st = start else st=0
    if n_elements(last) ne 0 then begin
        nf = last - st > 0
        if nf eq 0 then return
        fields = st + lindgen(nf)
    endif else if n_elements(nframes) ne 0 then begin
        if nframes lt 1 then return
        fields = start+lindgen(nframes) 
    endif

end

pro fetch_file_list, dir, files, fnums, $
                     fields=fields, $
                     start=start_in, last=last, nframes=nframes_in,$
                     run=run, camcol=camcol, rerun=rerun,  $
                     fieldmin=fieldmin, fieldmax=fieldmax, $
                     nfields=nfields,$
                     front=front, nchar=nchar, allfiles=allfiles, $
                     allfnums=allfnums




  if n_params() LT 1 then begin
      print,'-Syntax: fetch_file_list, dir [, files, fnums, fields=fields, start=start, nframes=nframes,run=run, camcol=camcol, rerun=rerun, fieldmin=fieldmin, fieldmax=fieldmax, nfields=nfields, front=front]'
      return
  ENDIF

;    if n_elements(fields) eq 0 then begin
;        ffl_get_fields, start=start, last=last, nframes=nframes_in, $
;            fields=fields
;    endif


  ;; check for trailing backslash on dir
  len=strlen(dir)
  lastch=strmid(dir,len-1,1)
  if lastch ne '/' then dir=dir+'/'

  ;; zero fields found by default
  nfields = 0L

  IF n_elements(front) EQ 0 THEN front = 'tsObj'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check run, rerun, camcol
  ;; only works if using SDSS directory structure
  ;; get run,rerun,camcol from directory name
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  tmp = str_sep(dir, '/calibChunks/')
  IF n_elements(tmp) LT 2 THEN BEGIN 
      message,'DIRECTORY STRUCTURE IS INVALID FOR DIRECTORY '+dir,/inf
      message,'MUST BE OF THE FORM */run/rerun/calibChunks/camcol/',/inf
      return
  ENDIF 

  ;; camcol
  cstr = ( str_sep(tmp[1], '/')  )[0]
  camcol = fix(cstr)

  ;; separate calibChunks and check for proper 
  ;; directory structure

  tmp = str_sep(tmp[0],'/')
  ntmp=n_elements(tmp)
  IF ntmp LT 2 THEN BEGIN 
      message,'DIRECTORY STRUCTURE IS INVALID FOR DIRECTORY '+dir,/inf
      message,'MUST BE OF THE FORM */run/rerun/calibChunks/camcol/',/inf
      return
  ENDIF 

  ;; rerun
  rrstr=tmp[ntmp-1]
  rerun = fix(rrstr)
 
  ;; run
  rstr = tmp[ntmp-2]
  tmprun = str_sep(rstr,'corr')
  IF n_elements(tmprun) EQ 2 THEN run = long(tmprun[1]) ELSE run = long(tmprun[0])
  rstr = run2string(run)
 
  ;;all files in that directory
  files=findfile(dir)
  num=n_elements(files)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get front, run, camcol, rerun, field and extension for each file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ffront  = strarr(num)
  frun    = strarr(num)
  fcamcol = strarr(num)
  frerun  = strarr(num)
  fnums   = lonarr(num)
  fext    = strarr(num)

  FOR i=0, num-1 DO BEGIN 
      a=str_sep(files(i),'-')
      ffront[i]=a[0]
      IF (n_elements(a) EQ 5) THEN BEGIN ;must be proper format
          frun[i] = a[1]
          fcamcol[i] = a[2]
          frerun[i] = a[3]

          ;; get field/extension
          tmp = str_sep(a[4], '.')
          fnums[i] = long(tmp[0])
          IF n_elements(tmp) EQ 2 THEN fext[i] = tmp[1]
      ENDIF 
  ENDFOR 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check for files that had correct format
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  wform = where(fext NE '', num)
  IF num EQ 0 THEN BEGIN 
      print
      message,'NO FILES WITH PROPER FORMAT: front-run-camcol-rerun-field.extension',/inf
      files=''
      delvarx, fnums
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check for files with this front
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  wf=where( ffront EQ front, num)
  IF num EQ 0 THEN BEGIN
      print,' NO '+front+' FILES FOUND IN DIRECTORY: ',dir
      wf=where(ffront EQ front,nw) & print,nw
      files=''
      return 
  ENDIF 
  files   = files[wf]
  frun    = frun[wf]
  fcamcol = fcamcol[wf]
  frerun  = frerun[wf]
  fnums   = fnums[wf]
  fext    = fext[wf]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check for files with this run number
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  wr=where(frun EQ rstr,num)
  IF num EQ 0 THEN BEGIN 
      print,' NO FILES WITH RUN = ',rstr,' IN DIRECTORY ',dir
      files=''
      return
  ENDIF 
  files   = files[wr]
  fcamcol = fcamcol[wr]
  frerun  = frerun[wr]
  fnums   = fnums[wr]
  fext    = fext[wr]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check for files with this camcol number
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  wc=where(fcamcol EQ cstr,num)
  IF num EQ 0 THEN BEGIN 
      print,' NO FILES WITH CAMCOL = ',cstr,' IN DIRECTORY ',dir
      files=''
      return
  ENDIF 
  files  = files[wc]
  frerun = frerun[wc]
  fnums   = fnums[wr]
  fext    = fext[wr]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check for files with this rerun number 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  wrr=where(frerun EQ rrstr,num)
  IF num EQ 0 THEN BEGIN 
      print,' NO FILES WITH RERUN = ',rrstr,' IN DIRECTORY ',dir
      files=''
      return
  ENDIF 
  files = files[wrr]
  fnums = fnums[wrr]
  fext  = fext[wrr]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check for .fit files
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  wext=where(fext EQ 'fit',num)

  IF num EQ 0 THEN BEGIN 
      print
      message,'No .fit files!',/inf
      files = ''
      delvarx, fnums
      return
  ENDIF 

  files = files[wext]
  fnums = fnums[wext]

  fieldmin = fnums[0]
  fieldmax = fnums[num-1]
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Deal with input field ranges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  IF n_elements(start_in) NE 0 THEN start=start_in $
  ELSE start = fieldmin

  ;; don't overwrite input
  IF n_elements(nframes_in) NE 0 THEN nframes = nframes_in $
  ELSE nframes = fieldmax - start + 1

  ;; "last" takes precedence
  IF n_elements(last) NE 0 THEN BEGIN
      fin = last 
      nframes = last - start +1
  ENDIF ELSE fin = start + nframes -1

  IF start LT fieldmin THEN BEGIN
      print
      message,'Start is less than first field '+ntostr(fieldmin)+$
              '.  Beginning with field '+ntostr(fieldmin),/inf
      nframes = nframes - (fieldmin - start)
      start=fieldmin
      fin = start + nframes -1
  ENDIF 
  IF fin LT fieldmin THEN BEGIN 
      print
      message,'Requested last field '+ntostr(fin)+' is less than first field '+ntostr(fieldmin)+'. No fields read.',/inf
      files = ''
      delvarx, fnums
      return
  ENDIF 
                                ;can't get files before the first one
  IF start GT fieldmax THEN BEGIN
      print
      message,'Start is larger than last field.  No files read',/inf
      files = ''
      delvarx, fnums
      return
  ENDIF 
  IF fin GT fieldmax THEN BEGIN
      print
      message,'Cannot read past last field.  Reading to field '+$
              ntostr(fieldmax),/inf
      nframes=fieldmax-start+1
      fin = fieldmax
  ENDIF

  IF nframes LT 1 THEN BEGIN
      print
      message,'nframes must be greater than 0',/inf
      files = ''
      delvarx, fnums
      return
  ENDIF 
     
  ;; keep track of all files also
  allfiles = dir + files
  allfnums = fnums

  w=where(fnums ge start and fnums le fin,nfields)
  IF nfields EQ 0 THEN BEGIN 
      print
      message,'No fields found in range: ['+$
              ntostr(start)+', '+ntostr(fin)+']',/inf
      files = ''
      delvarx, fnums
      return
  ENDIF 
  files = files[w]
  fnums = fnums[w]
  

  
  files=dir+files

  return
end










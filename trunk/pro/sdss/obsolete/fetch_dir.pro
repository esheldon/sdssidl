pro fetch_dir, run, camcol, rerun, dir, atlasDir, file, atlasFile,$
               field=field, check=check, corrdir=corrdir,$
               corratldir=corrAtlasDir
;
;+
; NAME:
;    FETCH_DIR
;
; PURPOSE: 
;    given run camcol and rerun it will return the tsObj directory
;    (calibChunks/camcol) or atlas directory (objcs/camcol).  
;
; CALLING SEQUENCE:
;   fetch_dir, run, camcol, rerun [, dir, atldir, file, atlasFile,
;              field=field, check=check, corrdir=corrdir]
;
; INPUTS:
;    run, camcol, rerun: in integer form
; 
; OUTPUTS:
;    none
;
; OPTIONAL INPUTS:
;    field: An integer field number.  If input, file and atlasFile (tsObj and
;           fpAtlas) file names can be returned
;    check: If /check, then will make sure directory exists.  Returns '' if
;           not.
;
; OUTPUTS:
;    dir, atldir: the tsObj directory and fpAtlas directory (objcs)
;    file, atlasFile: filenames for input field number
;    corrdir: the directory holding corrected shape files. Requires
;       system variable SHAPECORR_DIR to be defined in the config file
;    corratldir:  ''
;
; NOTES 
;    1) Config variables DATA_DIR and optionally SHAPECORR_DIR must be set.
;
;    2) If /check is set and a file or directory is not found, then
;       it is set to ''
;
; EXAMPLE:
;    IDL> fetch_dir,259,5,1,200,dir
;    IDL> print,dir
;    /usr/sdss/data01/imaging/259/1/calibChunks/5
;-
;

  if n_params() LT 3 then begin
      print,'-syntax fetch_dir,run,camcol,rerun,dir,atldir,file,atlasFile,field=field,check=check, corrdir=corrdir, corratldir=corratldir'
      return
      print,'Use doc_library,"fetch_dir" for more info'
  endif

  sdss_data_dir = sdssidl_config('data_dir', exists=d_exists)
  IF NOT d_exists THEN BEGIN 
      message,'DATA_DIR config variable is undefined',/inf
      return
  ENDIF 

  runs=strcompress(run,/rem)
  reruns=strcompress(rerun,/rem)
  camcols=strcompress(camcol,/rem)
  
  dir = sdss_data_dir + runs+'/'+reruns+'/'+'calibChunks/'+camcols+'/'
  atlasDir = sdss_data_dir + runs+'/'+reruns+'/'+'objcs/'+camcols+'/'
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check the existence of the directory
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(check) THEN BEGIN

      IF NOT fexist(dir) THEN BEGIN 
          print,'tsObj Directory: ',dir,' does not exist'
          dir = ''
          file = ''
      ENDIF 

      IF NOT fexist(atlasDir) THEN BEGIN 
          print,'Atlas directory: ',atlasDir,' does not exist'
          atlasDir = ''
          atlasFile = ''
      ENDIF 

  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Is user is requesting corrdir, etc. ?
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF arg_present(corrDir) OR arg_present(corrAtlasDir) THEN BEGIN 

      sdss_shapecorr_dir = sdssidl_config('shapecorr_dir', exists=c_exists)
      IF NOT c_exists THEN BEGIN 
          message,'SHAPECORR_DIR config variable is undefined',/inf
          return
      ENDIF
 
      corrDir = $
        sdss_shapecorr_dir + $
        'corr'+runs+'/'+reruns+'/'+'calibChunks/'+camcols+'/'
      corrAtlasDir = $
        sdss_shapecorr_dir + $
        'corr'+runs+'/'+reruns+'/'+'objcs/'+camcols+'/'

      IF keyword_set(check) THEN BEGIN 
 
          IF NOT fexist(corrDir) THEN BEGIN 
              print,'corr Directory: ',corrDir,' does not exist'
              corrDir = ''
          ENDIF 

          IF NOT fexist(corrAtlasDir) THEN BEGIN 
              print,'corr Atlas Directory: ',corrAtlasDir,' does not exist'
              corrAtlasDir = ''
          ENDIF 

      ENDIF 
  ENDIF 
 
  if n_elements(field) eq 0 then return
  IF dir EQ '' OR atlasDir EQ '' THEN return

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check for a file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  file='tsObj-'
  atlasFile='fpAtlas-'
  
  f=string(1000000+run)
  f=strmid(strcompress(f,/rem),1,6)
                                ;now looks like "000259"
  g=string(10000+field)
  g=strmid(strcompress(g,/rem),1,4)
                                ;now looks like "0203"
  
  
  file=file+f
  file=file+'-'+camcols+'-'+reruns+'-'+g+'.fit'
  file=dir+file
  
  atlasFile=atlasFile+f
  atlasFile=atlasFile+'-'+camcols+'-'+g+'.fit'
  atlasFile=atlasDir+atlasFile

  if keyword_set(check) then begin
      if fexist(file) eq 0 then begin
          print
          print,'tsObj file:  '
          print,file
          print,'does not exist'
          file=''
      endif
      if fexist(atlasFile) eq 0 then begin
          print
          print,'Atlas file:  '
          print,atlasFile
          print,'does not exist'
          atlasFile=''
      endif
  endif
  
  return
end















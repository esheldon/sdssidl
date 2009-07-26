pro read_photo_col, filename, struct, $
                    start=start,all=all, nframes=nframes,$
                    maxf=maxf, $
                    taglist=taglist, phototags = phototags, $
                    struct_type=struct_type,$
                    noadd=noadd, addrow=addrow,$
                    verbose=verbose
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:  
;     READ_PHOTO_COL
;
;
; PURPOSE:
;     Reads part of a column of SDSS data into one big struct.  
;
; NOTES: 
;    objc_rowc is made continuous from the first field rather than 
;    artificially restarting at the beginning of each field.  This can be 
;    easily removed by setting /noadd.  rowc[i] are not altered.   
;
; CALLING SEQUENCE: 
;     read_photo_col, filename, struct, start=start, nframes=nframes,
;     taglist=taglist, phototags=phototags, addrow=addrow,
;     struct_type=struct_type, noadd=noadd, verbose=verbose
;
; INPUTS:  
;    filename: The full path of the photo output fits file (tsObj file)
;
; OPTIONAL INPUTS:
;
;          start:    Frame to start at relative to the first (does not start
;                    at zero.)
;          all:      /all to read in all fields.
;          nframes:  Optional parameter which tells how many frames to read
;                    from filename. NOTE: Default is 50 frames
;          maxf:     the maximum number of fields which can be read from file.
;                    This is useful if the tag 'nfields' is not in the 
;                    structure header. 
;                    Although READ_PHOTO_COL can find this number with 
;                    FITS_INFO, that is a slow process.
;          taglist:  A list of photo tags in all CAPS that the user wants
;                    in struct. The default is to call the subroutine
;                    make_default_tags.pro
;          phototags: The user may provide the list of ALLOWED photo tags
;                     in all CAPS.  Usually gotten from struct.
;          addrow:   Optional parameter which tells how many rows the user
;                    wants to add to objc_rowc (That tag must be in taglist)
;                    This may be necessary if there is more that one file
;                    for each column.
;	   struct_type: The user may specify a _different_ name for the
;			structure to be read in, if the file for example
;			does not contain default photo information
;          noadd:    usr /noadd if you don't want to add up 
;                       objc_rowc continuously.
;          verbose: Verbosity level. Default is verbose > 1 full.
;                       verbose = 0    print only error messages
;                       verbose = 1    print minimun of messages
;                       verbose > 1    print all messages.  Includes the 
;                                      field by field updates.
;
; OUTPUTS: 
;    struct:  The new photo structure.
;
; CALLED ROUTINES:  
;    MAKE_DEFAULT_TAGS
;    MAKE_TAGS
;    MAKE_STRUCT
;    TAG_TYPE
;    MRDFITS3
;
; EXAMPLES:
;
;  Read in 50 frames of data and store it in struct.  run 109, camcol 3
;
; IDL> file='tsObj-000109-3-0-0011.fit'
; IDL> read_photo_col, file, struct
;
;  Read in a 20 frames beginning with frame 33
;
; IDL> read_photo_col, file, struct, start=33, nframes=20
;
; Author:  Erin Scott Sheldon
; Date: 10/7/98
; Modified: 11/20/98  Rewrote addrow stuff.  E.S.S.
;           11/23/98  Added start keyword.   E.S.S.
;           01/12/99  Added run, camcol, field as required 
;                     parameters.  Note these are not tags but are read from 
;                     the header. E.S.S.
;	    01/19/99  Added the struct_type argument, Tim McKay
;           03/21/99  Added (i gt start) and (i lt n_ext) to the 
;                     call to where  E.S.S.
;	    6/25/99   Added two systems variables needed for 
;			when you don't give it n_frames
;				'!TEXTOUT'  '!TEXTUNIT'	  
;			Dave Johnston  (U Chicago) 
;	    6/26/99  Replaced mrdfits2 with mrdfits3. This has the 
;		     	keyword deja_vu which allows one to use common
;			blocks so that one can remember variables that
;			one has already made when the program was called
;			before.	This speeds it up considerably.
;					Dave Johnston
;
;	   6/26/99  Changed the way that read_photo_col concatonates
;			the individual structures from each field into
;			the output structure struct. Now it uses pointers
;			for each individual struct and keeps and array
;			of pointers. Then in the end it makes the big
;			output struct and copies everything in.
;			This also helps with speed.
;				Dave Johnston	
;	   6/26/99   Added default nframes=50 and prints time.
;                    Added verbose.
;          10/8/99   Added /all, maxf=maxf keywords E.S.S.
;						
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Help message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() EQ 0 THEN BEGIN 
      print,'-Syntax:  read_photo_col, filename, struct, '
      print,'             start=start, all=all, nframes=nframes,'
      print,'             maxf=maxf, '
      print,'             taglist=taglist, phototags=phototags,'
      print,'             struct_type=struct_type,'
      print,'             noadd=noadd, addrow=addrow,'
      print,'             verbose=verbose'
      print,''
      print,'Use doc_library,"read_photo_col"  for more help'
      return 
  ENDIF 

  time=dblarr(2)
  time(0)=systime(1)
  
  IF n_elements(verbose) EQ 0 THEN verbose=2
  IF keyword_set(all) THEN all=1 ELSE all=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Figure out which taglist to use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(taglist)  THEN BEGIN 
      w1 = 'Using default taglist.'
      make_default_tags,taglist
  ENDIF ELSE w1='Using input taglist.'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Find out how many frames....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(nframes) THEN nframes=50 ;the default
  n_ext=nframes 
  framemax=sxpar(headfits(filename),'nfields') 

                                ; Case where nfields is not in header
                                ; This is really slow.
  IF framemax EQ 0 THEN BEGIN
      IF n_elements(maxf) EQ 0 THEN BEGIN 
          IF verbose GT 0 THEN BEGIN 
              print,'-----------------------------------------'
              print,' Tag "nfields" undefined in header. '
              print,' Using fits_info.  This is slow.'
              print,'-----------------------------------------'
          ENDIF 
          DEFSYSV,'!TEXTOUT',1  
          DEFSYSV,'!TEXTUNIT',0
          fits_info, filename, /silent, n_ext = framemax
      ENDIF ELSE framemax = maxf
  ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If start at some place other that first frame, must take
; that into account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(start) THEN BEGIN 
      n_ext = n_ext + start-1	  
  ENDIF ELSE start=1
  n_ext=n_ext < framemax   

  IF all THEN BEGIN && start=1 && n_ext=framemax && ENDIF
  stop=strtrim(string(n_ext),2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;See if the user has input the photo tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(phototags) THEN begin
      w2='Got phototags from the file.'
      tmp = mrdfits(filename,1,hhdr,/silent)
      phototags = tag_names(tmp)
  ENDIF ELSE BEGIN 
      w2='Got phototags from the user.'
  ENDELSE 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Print out a message if verbose gt 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF verbose GT 0 THEN BEGIN 
      print,' '
      print,'*****************************************'
      print,'* Reading frames ',strtrim(string(start),2),'-',stop,' from file:'
      print,'*         '+filename
      print,'* ',w1
      print,'* ',w2
      print,'*****************************************'
      print,''
  ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;figure out which tags are good and use them to make the struct
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  make_tags,taglist,phototags,goodtags,indices,check,bad
  IF (bad eq 0) THEN BEGIN 
      return 
  ENDIF 
  make_struct,goodtags,check,subl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Now loop through and read them all
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  openr, unit, filename, /get_lun, /block, ERROR = error
  IF ERROR NE 0 THEN BEGIN 
      print,!ERR_STRING
      free_lun,unit
      return 
  ENDIF 


  mainhdr=headfits(filename)
  camcol=sxpar(mainhdr,'camcol')
  run=sxpar(mainhdr,'run')
  e=strtrim(string(n_ext),2)
  IF NOT keyword_set(noadd)  THEN noadd=0 ELSE noadd=1
  IF NOT keyword_set(struct_type) THEN struct_type = 'sdss'

	
  nframes=n_ext-start+1
  numlist=lonarr(nframes)
  ptrlist=ptrarr(nframes)       ; the array of pointers 
                                
  ntotal=0L

  FOR i=start,(n_ext),1 DO BEGIN
        
      IF (i EQ start) THEN BEGIN
          lnew = mrdfits3(unit,i,0,hdr,structyp=struct_type,$
                          /silent)
      ENDIF ELSE BEGIN
          ii=i+2
          lnew = mrdfits3(unit,i,ii,hdr,structyp=struct_type,$
                          /silent,/deja_vu)
      ENDELSE                   ; it can use the structure that it already 
                                ; made the first time around
      field = sxpar(hdr,'field')

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Due to overlap of consecutive images, throw out 
      ;; first 64 and last 64 rows unless first frame (i eq start)
      ;; or last frame (i eq n_ext)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      g=where(lnew.objc_rowc gt (i gt start)*64 and $
              lnew.objc_rowc lt (1489 - (i lt n_ext)*64) , ng)
      IF (NOT noadd) THEN $
        lnew.objc_rowc=lnew.objc_rowc+(1361L*(i-1L))

      IF verbose GT 1 AND $
        ( (i-start-1) MOD 20 EQ 0 OR i EQ n_ext OR i EQ start) THEN BEGIN 
          s=strtrim(string(i),2)
          f=strtrim(string(field),2)
          o=strtrim(string(ng),2)
          print,'hdu: ',s,'/',e,'  Field: ',f,'  Objects: ',o
      ENDIF 

      ln=replicate(subl,ng)
      FOR k=0, n_elements(goodtags)-1 DO BEGIN 
                                ; must add three due to field, camcol, and run
          ln.(k+3) = lnew(g).(indices[k])
      ENDFOR 
      ln.field = field
      ln.camcol = camcol
      ln.run = run
      
      numlist(i-start)=ng
      ntotal=ntotal+ng
      ptrlist(i-start)=ptr_new(ln)
                                ; put the pointer in the array of pointers
  ENDFOR

  IF verbose GT 0 THEN BEGIN 		
      o=strtrim(string(ntotal), 2)
      print,'Total Number of Objects: ',o
  ENDIF 

  struct=replicate(subl,ntotal)
                                ; the output structure of correct size                               
  beg=0L
  FOR i=0L,n_ext-start DO BEGIN ;now make big list from the 
      num = numlist[i]          ;array of pointers ptrlist
      struct(beg:beg+num-1)=*ptrlist(i)
      ptr_free,ptrlist(i)
      beg=beg+num               ; kill the heap variables associated with
  ENDFOR                        ; the pointer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; May need to add additional rows since there may be multiple 
; files for each column.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(addrow) THEN BEGIN 
      w = where(taglist EQ "OBJC_ROWC")
      IF (w[0] NE -1) THEN BEGIN 
          IF verbose GT 0 THEN BEGIN 
              print,'========================================='
              print,'Adding ',addrow,' rows to objc_rowc'
              struct.objc_rowc = struct.objc_rowc + addrow
          ENDIF 
      ENDIF 
  ENDIF 

  free_lun,unit

  IF verbose GT 0 THEN BEGIN 
      time(1)=systime(1)
      ptime,time(1)-time(0)
  ENDIF 
  
  return
END
	







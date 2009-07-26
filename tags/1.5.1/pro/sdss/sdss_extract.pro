pro sdss_extract, filename, ofilename, command_name, nframes=nframes, start=start, max_mag=max_mag, sig_clip=sig_clip, groupn=groupn, taglist=taglist



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;    SDSS_EXTRACT
; PURPOSE:
;
;    Applies a user defined extraction function to an SDSS column....
;  
; INPUTS:  
;    filename: name of input fits file
;    ofilename: output fits file (will overwrite existing file)
;    command_name: command to use for extraction
;		This MUST take as arguments
;			list (input)
;			index (output)
;		It should return -1 in the index if nothing passes.
;    taglist:  A list of photo tags in all CAPS that the user wants
;                    in struct
;    nframes:  Optional parameter which tells how many frames to read
;                    from filename
;    start:    Beginning fram
;    max_mag: maximum magnitude to use (default=20)
;    sig_clip: Number of sigma for clipping large radius objects (default=3)
;    addrow:   Optional parameter which tells how many rows the user
;                    wants to add to objc_rowc (That tag must be in taglist)
;    groupn: How many frames to take together
; Outputs: fits file containing stars
;
; Author:  Phil Fischer
; Date: 1/14/9
;	
; Modified: Tim McKay
; Date: 5/6/99
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Help message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if n_params() LT 3 then begin
   print,'-syntax filename, ofilename, command_name, nframes=nframes'
   print,' start=start, max_mag=max_mag, sig_clip=sigclip,addrow=addrow'
   return
endif

print,' * Reading from file: ',filename
fits_info, filename, /silent, n_ext=n_ext
print,' * Total Number of Frames: ',n_ext

if keyword_set(nframes) then begin
  if(nframes gt n_ext) then begin
    print,' * nframes set to maximum of ',n_ext
  endif else begin
    n_ext=nframes
  endelse
endif

if keyword_set(start) then begin
  print,' * Beginning with frame ',start
  n_ext = n_ext + start-1
  sta=start
endif else begin
  sta=1
endelse

if not keyword_set(groupn) then begin
	groupn = 30
endif


;Figure out how many groupn frame units there are, and what's left over....
nunits=fix(n_ext/groupn)
nextra=n_ext-nunits*groupn
print,'Total number of frames is:',n_ext,' will use:',nunits,' and:',$
	nextra,' extra files'

;Get the taglist for these files....
if not keyword_set(taglist) then begin
	make_default_tags,tlist
endif else begin
	tlist=taglist
endelse

;Start reading them in one group at a time
ntotal=0
for i=0,nunits-1,1 do begin
    read_photo_col,filename,l,start=i*groupn,nframes=groupn,taglist=tlist
    call_procedure,command_name,l,cs_index
    if (cs_index(0) ne -1) then begin
      if (i eq 0) then begin
       	mwrfits,l(cs_index),ofilename,/create
      endif else begin
	mwrfits,l(cs_index),ofilename
      endelse
      ntotal=ntotal+1
    endif
endfor

;Now read what's left over...
if (nextra gt 5) then begin
    read_photo_col,filename,l,start=(nunits-1)*groupn,nframes=nextra,$
	taglist=tlist
    call_procedure,command_name,l,cs_index
    if (cs_index(0) ne -1) then begin
      mwrfits,l(cs_index),ofilename
      ntotal=ntotal+1
    endif
endif

;Now put them all in ONE structure....
for i=0,ntotal-1,1 do begin
    s = mrdfits(ofilename,i+1,hdr,structyp='cs')
    if (i eq 0) then begin
	cs=s
    endif else begin
	cs=[cs,s]
    endelse
endfor

help,cs
mwrfits,cs,ofilename,/create

end





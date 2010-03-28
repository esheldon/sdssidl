pro extract_stars, pstruct, clr, indices, max_mag=max_mag, sig_clip=sig_clip, plot=plot, silent=silent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NAME: 
;
;    EXTRACT_STARS
;
; PURPOSE:
;     Extracts a clean set of stars suitable for PSF determination
; 
; Inputs:  pstruct: input photo structure 
;	   clr: bandpass to select on:
;          ostruct: output photo structure containing stars
;          max_mag: maximum magnitude to use (default=20)
;          indices: the indices of the stars.
;	   sig_clip: Number of sigma for clipping large radius objects (default=3)
; Outputs: Plots flags for these objects....
;
; Author:  Phil Fischer
; Date: 1/14/99
;
; Modified: Tim McKay
; Date: 4/22/99  Changed the move flag cut to the objc level
;		 Fixed indirection of the indices option
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Help message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if n_params() LT 2 then begin
   print,'-syntax extract_stars, pstruct, clr, indices, max_mag=max_mag, sig_clip=sig_clip, silent=silent'
   return
endif

IF NOT keyword_set(max_mag) THEN max_mag = 20.
IF NOT keyword_set(plot) THEN plot=0
if (not keyword_set(silent) ) then silent = 0

;extract objects classified as stars

_ss=where(pstruct.objc_type eq 6)
indices=_ss

;extract based on object1 flags in selected bandpass

make_flag_struct, fs
fs.canonical_center='N' 
fs.edge='N' 
fs.blended='N' 
fs.child='N' 
fs.peakcenter='N' 
fs.nodeblend='N' 
fs.nopetro='N'   
;fs.manypetro='N'   
fs.manyr50='N'   
fs.manyr90='N'   
fs.incomplete_profile='N'   
fs.interp='N'   
fs.notchecked='N'   
fs.subtracted='N'   
fs.nostokes='N'   
fs.badsky='N'   
;fs.petrofaint='N'   
;fs.too_large='N'   
fs.deblended_as_psf='N'
fs.deblend_pruned='N'   
fs.ellipfaint='N'   
flag_select,pstruct[indices],fs,clr,_ss

indices=indices(_ss)

;extract multiple entries using the objc flags

make_flag_struct, fs
fs.bright='N'
fs.child='N'
fs.moved='N'
flag_select,pstruct[indices],fs,clr,_ss,objc=1
make_flag_struct, fs

if (not silent) then help, indices
indices=indices(_ss)

if (not silent) then help, indices

_ss=where(pstruct[indices].petrocounts(clr) lt max_mag)
indices=indices(_ss)

sigc=3
if keyword_set(sig_clip) then begin
  sigc=sig_clip
endif

_dd=moment(pstruct[indices].petrorad(clr))
_ss=where(pstruct[indices].petrorad(clr) lt _dd(0)+sigc*sqrt(_dd(1)))

if (not silent) then help, indices
indices=indices(_ss)

if (not silent) then help, indices

IF plot THEN BEGIN 
    title='Extracted Stars'
    xtitle='petrocounts'
    ytitle='petrorad'

    plot,pstruct[indices].petrocounts(clr),$
         pstruct[indices].petrorad(clr), $
         yrange=[0,10], $
         psym=3, $
         title=title, xtitle=xtitle, ytitle=ytitle
ENDIF 
end


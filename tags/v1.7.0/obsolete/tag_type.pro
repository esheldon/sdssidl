pro tag_type,tag,t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    TAG_TYPE
;
; PURPOSE:
;   Finds the type of an input tag
;
; INPUTS: 
;   tag: an input tag
;
; OUTPUTS: 
;   t: the type of the tag
;
; Author: Erin Scott Sheldon
; Date: 10/7/98
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (tag eq 'ID' or tag eq 'PARENT' or tag eq 'NCHILD' or tag eq 'OBJC_TYPE' $
	or tag eq 'CATID' or tag eq 'OBJC_FLAGS' or tag eq 'OBJC_FLAGS2' $
	or tag eq 'STATUS' or tag eq 'PRIMTARGET' or tag eq 'SECTARGET' $
        or tag eq 'PROPERMOTIONMAT' or tag eq 'FIRSTMATCH' $
        OR tag EQ 'FIRSTID' $
	or tag eq 'ROSATMATCH' or tag eq 'ROSATFLAGS' or tag eq 'PRIORITY' $
	or tag eq 'KEYMATCH' or tag eq 'KEYTYPE') then begin
		
		t = 0
		return
endif

IF (tag EQ 'FLAGS' OR tag EQ 'FLAGS2' OR tag EQ 'TYPE' OR tag EQ 'NPROF' $
        OR tag EQ 'PRIID') THEN BEGIN 
                t = 1
                return
ENDIF 

if (tag eq 'OBJC_ROWC' or tag eq 'OBJC_ROWCERR' $
	or tag eq 'OBJC_COLC' or tag eq 'OBJC_COLCERR' $
        OR tag EQ 'ROWV' OR tag EQ 'ROWVERR' $
        OR tag EQ 'COLV' OR tag EQ 'COLVERR' $
	OR tag EQ 'PHOTOZ') then begin
	
		t = 2
		return
endif

IF (tag EQ 'DEC' OR tag EQ 'RA' OR tag EQ 'LAMBDA' OR tag EQ 'ETA' $
        OR tag EQ 'L' OR tag EQ 'B') THEN BEGIN 
                t = 3
                return
ENDIF 

if (tag eq 'PROFMEAN' or tag eq 'PROFMED' or tag eq 'PROFERR') then begin

		t = 4
		return
endif

t = 5
return
end




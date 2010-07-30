;+
; NAME:
;	shiftlon
;
; CALLING SEQUENCE:
;	1) lon_new = shiftlon(longitude, shift)
;	or
;	2) lon_new = shiftlon(longitude,/wrap)
;
; PURPOSE:
;	mode 1)
;		Shift longitude by a given amount, making sure to wrap around the 0-360 
;		boundary
;			E.g. lon = long-shift   
;                if lon < 0 then add 360 back
;	mode 2)
;		If /wrap is sent make points cover -180,180 instead of 0,360
;
;
; INPUTS:
;	longitude: in degrees.  Can be an array.
;	shift: in degrees
; KEYWORDS:
;	/wrap: If set make lon run from -180,180 with no shifting otherwise. Ignore
;		the shift argument
;
; OUTPUTS:
;	The shifted longitude
; MODIFICATION HISTORY:
;	2009-06-12: From other code, Erin Sheldon, BNL
;	2009-07-30: Added /wrap keywrod.  Erin Sheldon, BNL
;
;-
function shiftlon, lon, lon_shift, wrap=wrap
	if n_elements(lon) eq 0 or $
			(not keyword_set(wrap) and n_elements(lon_shift) eq 0) then begin
		on_error,2
		print,'Usage: new_lon = shiftlon(longitude, shift, /wrap)'
		message,'Halting'
	endif

	if keyword_set(wrap) then begin
		lon_new = lon
		w=where(lon gt 180d, nw)
		if nw ne 0 then begin
			lon_new[w] -= 360
		endif
	endif else begin
		lon_new = lon - lon_shift
		w=where(lon_new lt 0.0, nw)
		if nw ne 0 then lon_new[w] += 360
	endelse
	return, lon_new
end

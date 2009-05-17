;+
; NAME:
;   spheredist
; PURPOSE:
;   Compute great circle distances and angles between sets of points.
; CALLING SEQUENCE:
;   dis = spheredist(ra1, dec1, ra2, dec2, units=, theta=)
; INPUTS:
;   ra1,dec1: First list of positions in degrees.
;   ra2, dec2: Second list of positions in degrees.
;       The lengths of these sets of coordss must make sense for calculations 
;       such as as sin(dec1)*sin(dec2) + cos(dec1)*cos(dec2)*cos(ra1-ra2)
;       A typical use is a single ra1,dec1 and a longer list of ra2,dec2.
; KEYWORDS:
;   units=: Units of inputs and outputs.  Default 1.
;       1: Input positions in degrees.  Outputs in radians.
;       2: Input positions in degrees.  Outputs in degrees.
;       3: Input positions in radians.  Outputs in radians.
;       4: Input positions in radians.  Outputs in degrees.
; OUTPUTS:
;   distance: The distance in radians.
; OPTIONAL OUTPUTS:
;   theta=: Angle east of north from ra1,dec1 (the origin) to the ra2,dec2 point.
;           (The x-axis points toward the celestial pole; the y-axis points
;           toward increasing R.A. along a line of constant dec.)
;           Either the sets 1 and 2 must be the same length or one of the sets
;           must be length 1. 
;
; MODIFICATION HISTORY:
;   Created 2002 Erin Sheldon, UofMich. Inspired by gcirc.pro and by AEK,
;   UWashington. 
;-

function spheredist, ra1, dec1, ra2, dec2, units=units, theta=theta

    nr1=n_elements(ra1) & nd1=n_elements(dec1) 
    nr2=n_elements(ra2) & nd2=n_elements(dec2)
    npar = N_params()
    if (nr1 eq 0) or (nd1 eq 0) or (nr2 eq 0) or (nd2 eq 0) then begin
      print,'-Syntax: dis = spheredist(ra1,dec1,ra2,dec2,units=,theta=)'
      return,-1
    endif

    common spheredist_cblock, d2r, r2d
    if n_elements(d2r) eq 0 then begin
        d2r    = !DPI/180.0d0
        r2d    = 180.0d0/!DPI
    endif

    if n_elements(units) eq 0 then units = 1

    if (nr1 ne nd1) then message,'length of ra1 must length of dec1'
    if (nr2 ne nd2) then message,'length of ra2 must length of dec2'

    ; Deal with input units
    if units eq 1 or units eq 2 then begin
        ; inputs are degrees
        tdec1 = dec1*d2r
        sindec1 = sin(tdec1)
        cosdec1 = cos(tdec1)
        tdec1=0

        tdec2 = dec2*d2r
        sindec2 = sin(tdec2)
        cosdec2 = cos(tdec2)
        tdec2=0
    endif else begin
        ; inputs are radians
        sindec1 = sin(dec1)
        cosdec1 = cos(dec1)
        sindec2 = sin(dec2)
        cosdec2 = cos(dec2)
    endelse

    radiff = (ra1-ra2)*d2r
    cosradiff = cos(radiff)

    cosdis = sindec1*sindec2 + cosdec1*cosdec2*cosradiff
    ww=where(cosdis lt -1., nww)
    if nww ne 0 then cosdis[ww] = -1.
    ww=where(cosdis gt 1., nww)
    if nww ne 0 then cosdis[ww] = 1.

    dis = acos(cosdis)

    if arg_present(theta) then begin
        if (nr1 ne 1) and (nr2 ne 1) and (nr1 ne nr2) then begin 
            message,'For theta calculation one of the sets must be '+$
                'length one, or both sets must be the same length', /inf
        endif else begin 
            theta = atan( sin(radiff), $
                (sindec1*cosradiff - cosdec1*sindec2/cosdec2) ) + !dpi
            if theta eq 2.*!dpi then theta=0
            if units eq 2 or units eq 4 then theta=theta*d2r
        endelse
    endif 

    if units eq 2 or units eq 4 then dis = dis*r2d
    return, dis
end


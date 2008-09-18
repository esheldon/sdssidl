;+
; NAME:
;  errfill
;
; PURPOSE:
;  Fill in the region x, y, y+/-yerr, which is the allowed range around x,y 
;  given the errors yerr. The plot and data coordinates must have already 
;  been created as required by the polyfill procedure.
;
; CATEGORY:
;  Plotting
;
; CALLING SEQUENCE:
;  errfill, x, y, yerr, _extra=
;
; INPUTS:
;  x, y, yerr:  points and error bars. Must all be the same length.
;
; OPTIONAL INPUTS:
;  _extra: extra plotting keywords, such as color, /line_fill, pattern, etc.
;          see POLYFILL for more details.
;
; EXAMPLE:
;  plot, x, y
;  errfill, x, y, yerr, color=!red
;
; MODIFICATION HISTORY:
;  Created, 2007-01-23 Erin Sheldon, NYU
;
;-

pro errfill, x, y, yerr, _extra=_extra

    if n_params() lt 3 then begin
        on_error, 2
        print,'-Syntax: errill, x, y, yerr, _extra='
        print
        message,'Halting'
    endif
    
    n=n_elements(x)
    for i=0L,n-2 do begin

        xp = [x[i],        x[i+1],          x[i+1],          x[i]]
        yp = [y[i]-yerr[i],y[i+1]-yerr[i+1],y[i+1]+yerr[i+1],y[i]+yerr[i]]

        polyfill, xp, yp, _extra=_extra
    endfor

end

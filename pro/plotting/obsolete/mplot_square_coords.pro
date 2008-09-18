
PRO mplot_square_coords, x, y, $
                         TYPE = type, $
                         XSCALE = xscale, $
                         YSCALE = yscale, $
                         XONLY = xonly, $
                         YONLY = yonly

;+
; NAME:
;   mplot_square_coords
;
; PURPOSE:
;
;   Squish coordinates so as to produce a square plot
;
; CATEGORY:
;
;   Plotting
;
; CALLING SEQUENCE:
;
;   mplot_square_coords, x, y
; 
; INPUTS:
;
;   X, Y
;
;     Normal coordinates on the current device
;
; OPTIONAL INPUTS:
;	
; KEYWORD PARAMETERS:
;
;   TYPE
;
;     If type is 0, scales coordinates so that MPLOT will produce square
;     individual sub-plots.  If 1, scales coordinates so that the overall
;     plot is square (effect is the same if the number of rows and columns
;     is equal)
;
;   XONLY, YONLY
;
;     Do not apply the scaling to y (/xonly) or x (/yonly) coordinates
;
;   XSCALE, YSCALE
;
;     Set these to named variables if you want to recover the scaling 
;     factors for each of the coordinates
;
; OUTPUTS:
;
;   X, Y
;
;     Returns the new scaled normal coordinates
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
;   Modifies the X, Y input variables
;
; RESTRICTIONS:
;
;   Reads the system variables !p.multi, ![xy].margin, 
;   !d.[xy]_size and !d.[xy]_char_size to establish the plot parameters.
;   The squish is centered on normal coordinate (0.5, 0.5)
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;    1997oct15  jeb  jbaker@astro.berkeley.edu
;
;-

ncols = !p.multi[1]
nrows = !p.multi[2]
IF ncols LE 0 THEN ncols = 1
IF nrows LE 0 THEN nrows = 1

xmar = total(!x.margin) * !d.x_ch_size
ymar = total(!y.margin) * !d.y_ch_size

IF type EQ 1 THEN BEGIN
    aspect = (!d.y_size-ymar) / (!d.x_size-xmar)
ENDIF ELSE BEGIN
    aspect = ((!d.y_size-ymar)/float(nrows)) / ((!d.x_size-xmar)/float(ncols))
ENDELSE

xscale = 1
yscale = 1
IF aspect LE 1 AND NOT keyword_set( yonly ) THEN BEGIN 
    xscale = aspect
    x = (x-0.5)*xscale + 0.5
ENDIF

IF aspect GT 1 AND NOT keyword_set( xonly ) THEN BEGIN 
    yscale = 1./aspect
    y = (y-0.5)*yscale + 0.5
ENDIF

END

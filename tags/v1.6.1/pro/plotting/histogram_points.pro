
;+
; NAME:
;  histogram_points
;
; PURPOSE:
;  Return as set of points which when plotted show a histogram representation
;  of the input x-y points.  This is equivalent to psym=10 for normal
;  "horizontal" histograms.  This procedure also allows "vertical" histograms
;  which can be plotted against the y-axis.
;
; CATEGORY:
;  Plotting
;
; CALLING SEQUENCE:
;  histogram_points, x, y, xp, yp, type=
;
; INPUTS:
;  x, y: The x and y points.  These could be the result of a call to
;    histogram.
;
; OPTIONAL INPUTS:
;  type: The type of histogram, either 'horizontal' or 'vertical'.  Default
;    is 'horizontal'
;
; OUTPUTS:
;  xp, yp: The points for plotting.
;
; EXAMPLE:
;  ; two-dimensional histogram.  Plot the image and also project along
;  ; each axis and show 1-d histogram.
;  hist2d, x, y, hist, xbins=xbins, ybins=ybins
;  xr = [min(xbins), max(xbins)]
;  yr = [min(ybins), max(ybins)]
;  implot, hist, xrange=xr, yrange=yr
;  hx = total(hist,2)
;  hy = total(hist,1)
;  histogram_points, xbins, hx, xp, yp
;  yp = yp/max(yp)*max(ybins)/5.0
;  oplot, xp, yp
;  histogram_points, ybins, hy, xp, yp
;  xp = xp/max(xp)*max(xbins)/5.0
;  oplot, xp, yp
;
; MODIFICATION HISTORY:
;   ??-??-?? Erin Sheldon, NYU
;
;-
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of version 2 of the GNU General Public License as 
;    published by the Free Software Foundation.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;
pro histogram_points, x, y, xp, yp, type=type

    np = n_elements(y)
    nx = n_elements(x)
    if nx eq 0 or np eq 0 then begin
        on_error, 2 
        print,'-Syntax: histogram_points, x, y, xp, yp, type='
        message,'Halting'
    endif

    if np ne n_elements(x) then message,'x and y must be same size'
    if n_elements(type) eq 0 then type = 'horizontal'

    case strlowcase(type) of
        'horizontal': begin

            ii = lindgen(np-1)
            xp = (x[ii+1] + x[ii])/2.0

            xp = [1,1]#xp

            yp = [1,1]#y
        
            xp = [x[0], xp[*], x[np-1]]
            yp = yp[*]

        end
        'vertical': begin

            ; default it uses the left y axis as "x".  Can always 
            ; adjust afterward
            ii = lindgen(np-1)

            yp = (x[ii+1] + x[ii])/2.0

            yp = [1,1]#yp

            xp = [1,1]#y
        
            yp = [x[0], yp[*], x[np-1]]
            xp = xp[*]


        end
        else: message,'Unsupported histogram type: '+string(type)
    endcase

end



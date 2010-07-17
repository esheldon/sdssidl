
;+
; NAME:
;  make_rainbow()
;
; PURPOSE:
;  Return a set of N colors equally spaced along the rainbow. By default
;  the colors start at the blue end.  For X the colors range from a dark
;  violet to red. For PS they range from black to red.  The order of colors
;  may be reversed with the corresponding keyword.
;
; CALLING SEQUENCE:
;  colors = make_rainbow(number_of_colors, /reverse)
;
; INPUTS:
;  number_of_colors:  The number of colors to be returned.
;
; KEYWORD PARAMETERS:
;  /reverse: Normally the colors start from the blue end. If /reverse is
;     set then they start from the red end.
;
; OUTPUTS:
;  The colors.
;
; EXAMPLE:
;  col = make_rainbow(25)
;  for i=0L, 24 do oplot, x[i,*], y[i,*], color=col[i]
;
; MODIFICATION HISTORY:
;  Created: 2007-01-23, Erin Sheldon, NYU
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

function make_rainbow, num

    if n_elements(num) ne 1 then begin
        on_error, 2
        print,'colors = make_rainbow(number_of_colors)'
        print
        message,'Halting'
    endif

    if !d.name eq 'PS' then begin
        loadct,13
        colors = arrscl(findgen(num), 0, 255)
    endif else begin
        minh = 0.0
        maxh = 270.0

        s = replicate(1.0, num)
        v = replicate(1.0, num)
        h = arrscl(findgen(num), minh, maxh)
        hsv_to_rgb, h, s, v, R, G, B
    
        colors = R + 256L*(G+256L*B)
        colors = reverse(colors)
    endelse

    if keyword_set(reverse) then colors = reverse(colors)
    return, colors

end


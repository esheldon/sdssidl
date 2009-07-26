;+
; NAME:
;  plotposition
;
; PURPOSE:
;  Return the normalized plot position.  If !p.position is set it is
;  returned.  Otherwise a dummy plot is generated in the current 
;  device and the coordinates of that are returned.  Note, if using 
;  !p.multi the following plot must be overplotted without an erase
;  or the next (incorrect) cell will be used. Flags will have 2^0 
;  added in this case. The result is an array 
;       [ px0, py0, px1, py1 ]
;
; CATEGORY:
;  plotting
;
; CALLING SEQUENCE:
;  pos = plotposition(flags=flags)
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;  Created: 2007-01-03, Erin Sheldon, NYU
;
;-
;  Copyright (C) 2007  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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
function plotposition, pos, flags=flags

    flags = 0
    pos = !p.position
    if pos[0] ne 0 and pos[1] ne 0 and pos[2] ne 0 and pos[3] ne 0 then begin
        return, pos
    endif else begin
        flags = flags + 2^0
        plot, [0,1],[0,1],/nodata,xstyle=4,ystyle=4
        px=!x.window
        py=!y.window
        pos = [ px[0], py[0], px[1], py[1] ]
    endelse
    return, pos

end


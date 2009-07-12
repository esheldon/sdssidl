;+
;  NAME:
;   plot_box
;
;  PURPOSE:
;   Plot a box with input set of coords.
;
;  CALLING SEQUENCE:
;   plot_box, minx, maxx, miny, maxy, /polyfill, _extra=_extra
;
;  MODIFICATION HISTORY:
;   Created: 2000, Erin Sheldon, UofMichigan
;-
;
;
;
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

PRO plot_box, minx, maxx, miny, maxy, $
              polyfill=polyfill, $
			  color=color_in, $
              _extra=extra

  IF n_params() LT 4 THEN BEGIN
      print,'-Syntax:  plot_box, minx, maxx, miny, maxy, /polyfill, $'
      print,'          _extra=extra'
      return
  ENDIF 

  if size(color_in, /tname) eq 'STRING' then begin
	  ; just in case we don't have c2i() this will still
	  ; compile
	  comm = 'color = c2i(color_in)'
	  res=execute(comm)
  endif else begin
	  color = color_in
  endelse

  IF keyword_set(polyfill) THEN BEGIN 

      polyfill, $
        [minx, maxx, maxx, minx], $
        [miny, miny, maxy, maxy], line_fill=line_fill, $
        color=color, _extra=extra

  ENDIF ELSE BEGIN 

      oplot,[minx,minx],[miny,maxy], _extra=extra, color=color
      oplot,[minx,maxx],[maxy,maxy], _extra=extra, color=color
      oplot,[maxx,maxx],[maxy,miny], _extra=extra, color=color
      oplot,[maxx,minx],[miny,miny], _extra=extra, color=color

  ENDELSE 

return
END 

;+
;  NAME:
;   plot_cross
;
;  PURPOSE:
;   Plot a cross with input size and center.
;
;  CALLING SEQUENCE:
;   plot_cross, radius, x, y, hole_fraction=, _extra=
;
;  INPUTS:
;   radius: rad in data units.
;   x,y: pos in data units.
;  
;  OPTIONAL INPUTS:
;   hole_fraction:  Place a hole at the center of the cross
;     this fraction of "radius" in size. Default zero.
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
                                       
PRO plot_cross, radius, x, y, hole_fraction=hole_fraction, _extra=_extra

  IF n_params() LT 3 THEN BEGIN
      print,'-Syntax:  plot_cross, radius, x, y, hole_fraction=hole_fraction, _extra=_extra'
      return
  ENDIF 

  IF n_elements(hole_fraction) NE 1 THEN hole_fraction=0.0

  ;; top of cross
  xmax = x
  xmin = x
  ymax = y + radius
  ymin = y + radius*hole_fraction < ymax

  oplot, [xmin,xmax],[ymin,ymax],_extra=_extra

  ;; bottom of cross
  xmax = x
  xmin = x
  ymin = y - radius
  ymax = y - radius*hole_fraction > ymin

  oplot, [xmin,xmax],[ymin,ymax],_extra=_extra

  ;; left side of cross
  xmin = x - radius
  xmax = x - radius*hole_fraction > xmin
  ymin = y
  ymax = y

  oplot, [xmin,xmax],[ymin,ymax],_extra=_extra

  ;; right side of cross
  xmax = x + radius
  xmin = x + radius*hole_fraction < xmax
  ymin = y
  ymax = y

  oplot, [xmin,xmax],[ymin,ymax],_extra=_extra

return
END 

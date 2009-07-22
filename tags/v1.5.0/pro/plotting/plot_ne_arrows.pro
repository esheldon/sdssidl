;+
; NAME:
;  plot_ne_arrows
;
; PURPOSE:
;  Plot a north and east arrow, with tails back to
;  back, in the upper right corner of the current plot device.
;
; CALLING SEQUENCE:
;  plot_ne_arrows, angle, fracsize=, offset_frac=, order=
;
; INPUTS:
;  angle: Angle on the device in radians.
;
; OPTIONAL INPUTS:
;  fracsize: Size of arrows relative to plot region. Default 0.1
;  offset_frac: Offset from side of plot region relative to
;    size of arrows.
;  order: Is image flipped?
;
; MODIFICATION HISTORY:
;  Created: 2003, Erin Sheldon, UChicago
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

PRO plot_ne_arrows, angle, fracsize=fracsize, offset_frac=offset_frac, $
                    order=order

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: plot_ne_arrows, angle, fracsize=fracsize, offset_frac=offset_frac, order=order'
      return
  ENDIF 

  IF n_elements(fracsize) EQ 0 THEN fracsize = 0.1
  IF n_elements(offset_frac) EQ 0 THEN offset_frac = 0.5
  
  xsize = abs(!x.crange[1]-!x.crange[0])
  ysize = abs(!y.crange[1]-!y.crange[0])

  arrow_size = min([xsize, ysize])*fracsize

  origin = fltarr(2)
  origin[0] = (!x.crange[1] - arrow_size*(1.+offset_frac))
  IF keyword_set(order) THEN BEGIN 
      origin[1] = (!y.crange[1] - arrow_size)
  ENDIF ELSE BEGIN 
      origin[1] = (!y.crange[1] - arrow_size*(1.+offset_frac))
  ENDELSE 

  ;; unrotated
  x = [ 0.0, 0.0, 1.0]*arrow_size
  y = [ 1.0, 0.0, 0.0]*arrow_size

  ;; rotate
  xp =  cos(angle)*x + sin(angle)*y
  yp = -sin(angle)*x + cos(angle)*y

  IF keyword_set(order) THEN BEGIN 
      yp = -yp
  ENDIF 

  ;; translate
  xp = xp+origin[0]
  yp = yp+origin[1]

  oplot, xp, yp
  
  IF keyword_set(order) THEN BEGIN 
      xyouts, xp[0]-arrow_size/3.0, yp[0]-arrow_size/3.0, 'E'
      xyouts, xp[2], yp[2]+arrow_size/8.0, 'N'
  ENDIF ELSE BEGIN 
      xyouts, xp[0]-arrow_size/3., yp[0], 'E'
      xyouts, xp[2], yp[2]-arrow_size/3., 'N'
  ENDELSE 

END 

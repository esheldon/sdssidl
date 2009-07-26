PRO plot_ellipse, radius, xc, yc, $
               axis_ratio=axis_ratio, $
               pos_angle=pos_angle, $
               device_coords=device_coords, $
               NPOINTS = npoints, _extra=_extra
;+
; NAME:
;	PLOT_ELLIPSE
;
; PURPOSE:
;	Draw an ellipse on the current graphics device.
;
; CALLING SEQUENCE:
;  plot_ellipse, radius, x, y, axis_ratio=, pos_angle=, /device,
;                npoints=, _extra=
; 
; REVISION HISTORY:
;   Converted tvellipse to be more powerful and intuitive.
;      Erin Sheldon UChicago 18-Oct-2004
;
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
  On_error,2                    ;Return to caller

  if N_params() lt 3 then begin
      print,'Syntax - tvellipse, radius, xc, yc, $'
      print,'                    axis_ratio=axis_ratio, $'
      print,'                    pos_angle=pos_angle, $'
      print,'                    /device_coords, $'
      print,'                    NPOINTS = npoints, _extra=_extra'
      return
  endif

  IF n_elements(axis_ratio) EQ 0 THEN axis_ratio = 1.0
  IF n_elements(pos_angle) EQ 0 THEN pos_angle = 0.0
  IF n_elements(NPOINTS) EQ 0 THEN npoints = 120 ;Number of points to connect

  rmax = radius
  rmin = radius*axis_ratio

  phi = 2*!pi*(findgen(npoints)/(npoints-1)) ;Will connect 120 points
  

  IF NOT keyword_set(device_coords) THEN BEGIN  
      zero = convert_coord(0,0,/data,/to_dev) 

      coord = convert_coord(rmax,0,/data,/to_dev) - zero
      x =  coord[0]*cos(phi)

      coord = convert_coord(rmin,0,/data,/to_dev) - zero
      y =  coord[0]*sin(phi)

  ENDIF ELSE BEGIN 
      x =  rmax*cos(phi)        ;Parameterized equation of ellipse
      y =  rmin*sin(phi)
  ENDELSE 

  ang = pos_angle/!RADEG          ;Position angle in radians

  cosang = cos(ang)
  sinang = sin(ang)

  IF NOT keyword_set(device_coords) THEN BEGIN 
      coord = convert_coord( xc, yc, /DATA, /TO_DEVICE)
      xcen = coord[0,*]   &    ycen = coord[1,*]
      ;; Rotate to desired position angle
      xprime = round(xcen[0] + x*cosang - y*sinang) 
      yprime = round(ycen[0] + x*sinang + y*cosang)
  ENDIF ELSE BEGIN
      ;; Rotate to desired position angle
      xprime = round(xc + x*cosang - y*sinang) 
      yprime = round(yc + x*sinang + y*cosang)
  ENDELSE 

  plots, xprime, yprime, /DEVICE, _extra=_extra

  return
END 

;+
; NAME:
;  IMPLOT_SETUP
;
; PURPOSE:
;  Given an image, generate position vectors and xrange/yrange vectors which
;  can be sent to plot
;
; CATEGORY:
;  plotting
;
; CALLING SEQUENCE:
;  implot_setup, image, xsize, ysize, px, py, xrange, yrange, /center
;
; INPUTS:
;  image: image, dimenstions of which are used to generate display info
;
;
; KEYWORD PARAMETERS:
;  /center: center the image
;
;
; OUTPUTS:
;  xsize,ysize: size of the image in device units
;  px,py: position vectors in device units
;  xrange,yrange: can be sent to xrange/yrange keywords in plot
;
; EXAMPLE:
;  ;; in X-windows
;  implot_setup, image, xsize, ysize, px, py, xrng, yrng, /center
;  tv, congrid(image, xsize, ysize),px[0],py[0]
;  pos = [px[0], py[0], px[1], py[1]]
;  
;   plot, [0,0], [0,0], xstyle=5, ystyle=5, $
;     title=title, $
;     xrange=xrng, yrange=yrng, position=pos,$
;     /noerase, /device, /nodata
;
;
; MODIFICATION HISTORY:
;   2005 Erin Sheldon, NYU
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


PRO implot_setup, image, xsize, ysize, px, py, xrange, yrange, center=center

  IF n_params() LT 1 THEN BEGIN  
      print,'-Syntax: implot_setup, image, xsize, ysize, px, py, xrange, yrange, center=center'
      return
  ENDIF 

  ;; Here is how I use this:

  sz = size(image, /dimensions)

  nx = sz[0]
  ny = sz[1]

  aspect = float(nx)/ny

  plot, [0,1],[0,1],/nodata,xstyle=4,ystyle=4
  px=!x.window*!d.x_vsize
  py=!y.window*!d.y_vsize
  xsize=px[1]-px[0]
  ysize=py[1]-py[0]
  
  IF xsize GT ysize*aspect THEN xsize=ysize*aspect ELSE ysize=xsize/aspect 
  px[1]=px[0]+xsize
  py[1]=py[0]+ysize

  nxm=nx-1
  nym=ny-1

  IF n_elements(xrange) EQ 0 THEN BEGIN
      xrange=[ -0.5, nxm+0.5]
  ENDIF ELSE BEGIN
      xrange=[xrange(0), xrange(n_elements(xrange)-1)]
  ENDELSE 

  IF n_elements(yrange) EQ 0 THEN BEGIN
      yrange = [-0.5,nym+0.5]
  ENDIF ELSE BEGIN
      yrange = [yrange(0), yrange(n_elements(yrange)-1)]
  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; center up the display
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(center) THEN dcenter, xsize, ysize, px, py, /silent

END 

;+
; NAME:
;  TVASINH
;
;
; PURPOSE:
;  Display an input image using an asinh stretch (Lupton, et al...)
;
;
; CATEGORY:
;  Plotting routine.
;
;
; CALLING SEQUENCE:
;   tvasinh, image, $
;            scaled_image=scaled_image, $
;            order=, $
;            nonlinearity=, alpha=, $
;            sky=, high=, $
;            /zoom, $
;            /noframe, /nolabels, $
;            xtitle=, ytitle=, title=, subtitle=, $
;            /invbw, $
;            max_color=, $
;            _extra=_extra
;
;
; INPUTS:
;  image: two-dimensional image.
;
;
; OPTIONAL INPUTS:
;  order=:  Image order, flips across y-axis.
;  nonlinearity: Nonlinearity parameter in Lupton's asinh stretch.
;  alpha: alpha parameter in Lupton's asinh stretch.
;  sky=: The sky value, saves time.
;  high: The high value to use, rather than the max in the image.
;
;  xtitle=, ytitle=, title=, subtitle=: Plotting keywords
; 
;  max_color=: Maximum color index available.
;
;
; KEYWORD PARAMETERS:
;  /zoom: Allow the user to interactively zoom into the image.
;  /noframe: Don't draw a frame around the plot.
;  /nolabels: Don't draw labels on the plot.
;  /invbw: Invert the greyscale.
;
; OPTIONAL OUTPUTS:
;  scaled_image=: The user can return the asinh scaled image through this
;                 keyword. 
;
; MODIFICATION HISTORY:
;   Created: Erin Sheldon, UChicago, some time in 2003
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


PRO tvasinh_scaleimage, image, alpha, nonlinearity, image_out

  image_out = asinh( alpha*nonlinearity*image )/nonlinearity
  image_out = bytscl( temporary(image_out) )

END 

PRO tvasinh_setupplot, nx, ny, aspect, xsize, ysize, px, py, xrng, yrng, $
                       xrange=xrange, yrange=yrange

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up plot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      xrng=[ -0.5, nxm+0.5]
  ENDIF ELSE BEGIN
      xrng=[xrange(0), xrange(n_elements(xrange)-1)]
  ENDELSE 

  IF n_elements(yrange) EQ 0 THEN BEGIN
      yrng = [-0.5,nym+0.5]
  ENDIF ELSE BEGIN
      yrng = [yrange(0), yrange(n_elements(yrange)-1)]
  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; center up the display
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  dcenter, xsize, ysize, px, py, /silent

  ;; PS device?
  IF !d.name EQ 'PS' THEN BEGIN 
      ;; this is white on black, make sure we use small
      ;; thickness
      !p.thick=1
      !x.thick=1
      !y.thick=1
      !p.charsize=1
      !p.charthick=1
  ENDIF 


END 

PRO tvasinh, image, scaled_image=scaled_image, $
             order=order, $
             nonlinearity=nonlinearity, alpha=alpha, $
             sky=sky, high=high, $
             zoom=zoom, $
             noframe=noframe, nolabels=nolabels, $
             xtitle=xtitle, ytitle=ytitle, title=title, subtitle=subtitle, $
             invbw=invbw, max_color=max_color, $
             position=position, $
             _extra=_extra

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: tvasinh, image, scaled_image=scaled_image, $'
      print,'         order=order,$'
      print,'         nonlinearity=nonlinearity, alpha=alpha, $'
      print,'         sky=sky, high=high, $'
      print,'         zoom=zoom, $'
      print,'         noframe=noframe, nolabels=nolabels, $'
      print,'         xtitle=xtitle, ytitle=ytitle, title=title, $'
      print,'         subtitle=subtitle, $'
      print,'         invbw=invbw, max_color=max_color, $'
      print,'         _extra=_extra'
      return
  ENDIF 

  IF n_elements(invbw) EQ 0 THEN BEGIN 
      IF !d.name EQ 'PS' THEN invbw = 1 ELSE invbw = 0
  ENDIF 
  IF n_elements(max_color) EQ 0 THEN max_color=!d.n_colors-1

  ;; titles and stuff for axes.
  IF n_elements(title) EQ 0 THEN title = ''
  IF n_elements(xtitle) EQ 0 THEN xtitle=''
  IF n_elements(ytitle) EQ 0 THEN ytitle=''
  IF n_elements(subtitle) EQ 0 THEN subtitle=''

  ;; how to scale the image
  IF n_elements(nonlinearity) EQ 0 THEN nonlinearity = 8
  IF n_elements(alpha) EQ 0 THEN alpha = 0.02
  IF n_elements(sky) EQ 0 THEN sky=float(median(image))
  IF n_elements(high) EQ 0 THEN high = float(max(image))

  ;; see how many colors we got
  max_color=!d.n_colors-1

  IF max_color GT 10000000 THEN true_color=1 ELSE true_color=0
  IF ( (n_elements(jpegfile) NE 0) AND keyword_set(tvread) AND $
       (NOT true_color) ) THEN BEGIN 
      message,'You cannot read a jpeg from the display in 8-bit color mode',$
        /inf
      message,'Try using a gif and /tvread'
  ENDIF 
  
  IF ( ( (n_elements(giffile) NE 0) OR (n_elements(jpegfile) NE 0) ) AND $
       keyword_set(tvread) AND keyword_set(nodisplay) ) THEN BEGIN 
      message,'Cannot read from display if /nodisplay is set'
  ENDIF 

  ;; prompting
  IF NOT keyword_set(noprompt) THEN noprompt = 0

  ;; Check current device
  ;; treat X and Z the same
  IF (!d.name EQ 'X') OR (!d.name EQ 'Z') THEN doX = 1 ELSE doX = 0


;  print,'Sky = '+ntostr(sky)

;  timage = ( float(image) - sky ) > low < high
  
;  timage = (float(image) - sky) > 0 < (high-sky)

  IF keyword_set(zoom) THEN BEGIN 

      print,'Click a corner of your zoom box'
      cursor, x, y, /data, /up 
      print,'Click another corner of your zoom box'
      cursor, x1, y1, /data, /up
      xmax = max( [x, x1], min=xmin)
      ymax = max( [y, y1], min=ymin)
      
      timage = (float(image[xmin:xmax, ymin:ymax]) - sky ) > 0 < (high-sky)

      xrange = [xmin, xmax]
      yrange = [ymin, ymax]

      print,'Using xrange: ',xrange
      print,'Using yrange: ',yrange

  ENDIF ELSE BEGIN 
      timage = (float(image) - sky) > 0 < (high-sky)
  ENDELSE  
  
  sz = size(timage)
  nx = sz[1]
  ny = sz[2]

  aspect = float(nx)/ny

  tvasinh_setupplot, nx, ny, aspect, xsize, ysize, px, py, xrng, yrng, $
    xrange=xrange, yrange=yrange

  tvasinh_scaleimage, timage, alpha, nonlinearity, scaled_image
  
  IF invbw THEN scaled_image = max_color - scaled_image

  IF doX THEN BEGIN 
      
      tv, congrid(scaled_image, xsize, ysize), px[0],py[0], order=order
      pos = [px[0], py[0], px[1], py[1]]

  ENDIF ELSE BEGIN 
      pos = [px[0], py[0], px[1], py[1]]
      tv, scaled_image, px[0],py[0], xsize=xsize, ysize=ysize, /device, order=order
  ENDELSE 
  
  IF keyword_set(noframe) OR keyword_set(nolabels) THEN BEGIN 
      plot, [0,0], [0,0], xstyle=5, ystyle=5, $
        title=title,xtitle=xtitle,ytitle=ytitle, subtitle=subtitle, $
        xrange=xrng, yrange=yrng, position=pos, $
        /noerase, /device, /nodata
  ENDIF ELSE BEGIN 
      plot, [0,0], [0,0], xstyle=1, ystyle=1, $
        title=title,xtitle=xtitle,ytitle=ytitle, subtitle=subtitle, $
        xrange=xrng, yrange=yrng, position=pos,$
        /noerase, /device, /nodata
  ENDELSE 
  
  position=pos
  position[0]=position[0]/!d.x_vsize
  position[1]=position[1]/!d.y_vsize
  position[2]=position[2]/!d.x_vsize
  position[3]=position[3]/!d.y_vsize

  IF (NOT keyword_set(noframe)) AND keyword_set(nolabels) THEN BEGIN 
      axis,xaxis=1,xtickname=strarr(10)+" "
      axis,xaxis=0,xtickname=strarr(10)+" "
      axis,yaxis=1,ytickname=strarr(10)+" "
      axis,yaxis=0,ytickname=strarr(10)+" "
  ENDIF  
    
END 

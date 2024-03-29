;+
; NAME:
;  PPLOT
;
; PURPOSE:
;  A complete replacement for plot and ploterr that allows plotting of error bars in x and y as well as a user defined axis ratio.
;
; CALLING SEQUENCE:
;    pplot, x, y, xerror=, yerror=, aspect=, /center, color=, 
;           errcolor=, errstyle=, errthick=, /nohat, hatlength=, nskip=,
;           _extra=
;
;
; INPUTS:
;  x, y: x and y vectors.  The same restrictions as the builtin plot program.
;        Note if y is not sent, then plot, x is run and error bars are ignored.
;
;
; KEYWORD PARAMETERS:
;
;   xerror: Error bars on x
;   yerror: Error bars on y
;   aspect: Aspect ratio xsize/ysize.  I know many people will expect this to
;       be reversed, but this is historical from the APLOT program.
;   /center: Re-center the display after applying aspect ratio.
;   color: The color index, or a color name in string form.  The c2i.pro 
;       program is used to convert string names to color indices.  All
;       colors available by name on unix machines are valid, as described
;       in the rgb.txt file usually distributed.  For example
;           pplot, x, y, yerr=yerr
;           pplot, x, y, /overplot, color='SteelBlue'
;   _extra: Other plotting keywords.  Any keywords accepted by plot.
;  
;   These deal with the style of errors bars. This stuff stolen from the
;   Goddard ploterror program.
; 
;   errcolor: Color for error bars. Unlike ploterror, defaults to same color as
;       the points.
;   errstyle: Line style for error bars.
;   errthick: Line thickness for error bars.
;   /nohat: Don't use a hat on the error bar.
;   hatlength: Lenght of hat.  Defaults to !d.(x|y)_vsize/100.0
;   nskip:  Integer specifying the error bars to be plotted.   For example,
;       if NSKIP = 2 then every other error bar is plotted; if NSKIP=3
;       then every third error bar is plotted.   Default is to plot
;       every error bar (NSKIP = 1)
;
;   EXAMPLES:
;       pplot, x, y, aspect=1.0
;       pplot, x, y, yerror=yerr
;       pplot, x, y, xerror=xerr, yerror=yerr, errcolor='blue'
;
;
; MODIFICATION HISTORY:
;   Created: 11-May-2006, Erin Sheldon, NYU.  An amalgomation of
;       many procedures, including ploterror.pro (Goddard) and aplot.pro (ES)
;       designed to do all in one in an intuitive way.
;   2007-11-06: Added support for color strings, which are converted to
;       color indices using the c2i.pro program.  Erin Sheldon, NYU
;
;-
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of version 2 of the GNU General Public License as 
;    published by the Free Software Foundation.

;; This is a utility program.
pro _pplot_doplot, x, y, $
                   xerror=xerror, $
                   xrange=xrange, xlog=xlog, $
                   $
                   yerror=yerror, $
                   yrange=yrange, ylog=ylog, $
                   $
                   errstyle=est, $
                   errthick=eth, $
                   errcolor = ecol, $
				   color=color, $
                   nohat=hat, $
                   hatlength=hln, $
                   $
                   $
                   nskip = nskip, $
                   noclip = noclip, $
                   $
                   overplot=overplot, $
				   ANONYMOUS_ = DUMMY_, $
                  _EXTRA = _extra


  on_error, 2

  ;; Errors sent?
  nx = n_elements(x)
  ny = n_elements(y)
  nxerror = n_elements(xerror)
  nyerror = n_elements(yerror)

  ;; This is just an ordinary plot!
  if (ny eq 0) or (nxerror eq 0 and nyerror eq 0) then begin 

      
      ;; must send these keywords along 
      if ny eq 0 then begin 
          args = 'x'
      endif else begin 
          args = 'x, y'
      endelse 

      if keyword_set(overplot) then begin 
          cmd = 'oplot, '+args+', color=color, noclip=noclip, _extra=_extra'
      endif else begin 
          cmd = 'plot, '+args+$
            ', color=color, '+$
            'xlog=xlog, ylog=ylog, xrange=xrange, yrange=yrange, '+$
            'noclip=noclip, _extra=_extra'
      endelse 

      if not execute(cmd) then begin 
          message,'Could not execute plot command'
      endif 

      return
  endif 


  ;; Errors were sent so deal with them.

; Error bar keywords (except for hatlength; this one will be taken care of 
; later, when it is time to deal with the error bar hats).

  if (keyword_set(hat)) then hat = 0 else hat = 1
  if (n_elements(eth) eq 0) then eth = !p.thick
  if (n_elements(est) eq 0) then est = 0

  ;; default error color is the input color or !p.color if it isn't set
  if n_elements(color) eq 0 then color=!p.color
  if (n_elements(ecol) eq 0) then ecol = color

  if n_elements( noclip ) eq 0 then noclip = 0
  if not keyword_set(nskip) then nskip = 1

;				Other keywords.

  if not keyword_set(XLOG) then xlog = 0
  if not keyword_set(YLOG) then ylog = 0

  n = N_elements(x)

  ;if n lt 2 then $
  ;  message,'Not enough points to plot.'

; If no y-range was passed via keyword or system variable, force one large 
; enough to display all the data and the entire error bars.     
; If a reversed y-range was passed, switch ylo and yhi.

  if nyerror ne 0 then begin 
      ylo = y - yerror
      yhi = y + yerror
  
      if not keyword_set( yrange ) then yrange = !y.range
      if yrange[0] eq yrange[1] then begin
          if keyword_set( xrange ) then  begin
              good = where( (x gt min(xrange)) and (x lt max(xrange)) )
              yrange = [min(ylo[good],/nan), max(yhi[good], /nan)]
          endif else yrange = [min(ylo,/nan), max(yhi, /nan)]
      endif else if yrange[0] gt yrange[1] then begin
          ylo = y + yerror
          yhi = y - yerror
      endif

  endif else begin
      ylo = y
      yhi = y
  endelse 

;        Similarly for x-range

  if nxerror ne 0 then begin 
      xlo = x - xerror
      xhi = x + xerror

      if not keyword_set( xrange ) then xrange = !x.range
      if xrange[0] eq xrange[1] then begin 
          xrange = [min(xlo,/nan), max(xhi,/nan)]
      endif else if xrange[0] gt xrange[1] then begin
          xlo = x + xerror
          xhi = x - xerror
      endif
  endif else begin 
      xlo = x
      xhi = x
  endelse 

; Plot the positions.    Always set NSUM = 1 since we already took care of 
; smoothing with FREBIN

  if keyword_set(overplot) then begin 
      oplot, x, y, _extra = _extra, noclip = noclip, nsum= 1, color=color
  endif else begin 
      plot, x, y, device=device, $
        xrange = xrange, yrange = yrange, xlog = xlog, ylog = ylog, $
        _extra = _extra, noclip = noclip, nsum= 1, $
		color=color
  endelse 

;	Plot the error bars.   Compute the hat length in device coordinates
;       so that it remains fixed even when doing logarithmic plots.

  data_low = convert_coord(x,ylo,/TO_DEVICE)
  data_hi = convert_coord(x,yhi,/TO_DEVICE)

  x_low = convert_coord(xlo,y,/TO_DEVICE)
  x_hi = convert_coord(xhi,y,/TO_DEVICE)

  ycrange = !Y.CRANGE   &  xcrange = !X.CRANGE

  if keyword_set(ylog) then ylo = ylo > 10^ycrange[0]
  if keyword_set(xlog) then xlo = xlo > 10^xcrange[0]

; Only draw error bars for X values within XCRANGE unless /overplot
; because of log issues that may crop up. Should not have to send
; /xlog, /ylog for overplotting

  if keyword_set(overplot) then begin 
      ng = n_elements(x)
      g = lindgen(ng)
  endif else begin 
      if keyword_set(xlog) then xcrange = 10^xcrange
      g = where((x gt xcrange[0]) and (x le xcrange[1]), ng)
  endelse 

  if (Ng GT 0) and (Ng NE n) then begin  
      istart = min(g, max = iend)  
  endif else begin
      istart = 0L & iend = n-1
  endelse

  for i = istart, iend, nskip do begin     

      ;; plot y-error bars
      if nyerror ne 0 then begin 
          plots, $
            [ x[i],x[i] ], [ ylo[i],yhi[i] ], $
            linestyle=est, thick=eth,  $
            noclip = noclip, color = ecol
      endif 

      ;; plot x-error bars 
      if nxerror ne 0 then begin 
          plots, $
            [ xlo[i],xhi[i] ],[ y[i],y[i] ], $
            linestyle=est, thick=eth, $
            noclip = noclip, color = ecol
      endif 

      if (hat ne 0) then begin

          ;; plot y-error hats
          if nyerror ne 0 then begin 
              if (n_elements(hln) eq 0) then begin 
                  yhln = !d.x_vsize/100. 
              endif else begin 
                  yhln = hln
              endelse 
              ex1 = data_low[0,i] - yhln/2.
              ex2 = ex1 + yhln

              plots, $
                [ ex1,ex2 ], [ data_low[1,i],data_low[1,i] ],$
                color=ecol, $
                linestyle=est,thick=eth,/device, noclip = noclip
              plots, $
                [ ex1,ex2 ], [ data_hi[1,i],data_hi[1,i] ], $
                color = ecol,$
                linestyle=est,thick=eth,/device, noclip = noclip
          endif 


          if nxerror ne 0 then begin
              if (n_elements(hln) eq 0) then begin 
                  xhln = !d.y_vsize/100. 
              endif else begin 
                  xhln = hln
              endelse 

              ey1 = x_low[1,i] - xhln/2.
              ey2 = ey1 + xhln
              plots, $
                [ x_low[0,i],x_low[0,i] ], [ ey1,ey2 ],$
                color = ecol, $
                linestyle=est,thick=eth,/device, noclip = noclip
              plots, $
                [ x_hi[0,i],x_hi[0,i] ], [ ey1,ey2 ], $
                color = ecol, $
                linestyle=est,thick=eth,/device, noclip = noclip
          endif
      endif

  endfor

  return
end

; Get the appropriate position value for the given aspect
; ratio and centering.
function _pplot_aspect_position, aspect, center=center

  on_error, 2
  if !p.multi[1] eq 0 then !p.multi[1] = 1
  if !p.multi[2] eq 0 then !p.multi[2] = 1

  plot, [0,1], [0,1], /nodata, xstyle=4, ystyle=4
  px = !x.window*!d.x_vsize
  py = !y.window*!d.y_vsize

  xsize = px[1] - px[0]
  ysize = py[1] - py[0]

  a0 = xsize/ysize
  case 1 of
      (aspect le a0): xsize = xsize*(aspect/a0) ;shrink xsize
      (aspect gt a0): ysize = ysize*(a0/aspect) ;shrink ysize
  endcase 

  px[1] = px[0] + xsize
  py[1] = py[0] + ysize

  if keyword_set(center) then dcenter, xsize, ysize, px, py

  position = [ [ px(0), py(0)], [ px(1), py(1) ] ]

  return, position

end 

pro _pplot_convert_colors, color_in, errcolor_in, bg_in, color, errcolor, bg
    if n_elements(color_in) ne 0 then begin
        if size(color_in,/tname) eq 'STRING' then begin
            color=c2i(color_in) 
        endif else begin
            color=color_in 
        endelse
    endif
    if n_elements(errcolor_in) ne 0 then begin
        if size(errcolor_in,/tname) eq 'STRING' then begin
            errcolor=c2i(errcolor_in) 
        endif else begin
            errcolor=errcolor_in 
        endelse
    endif
    if n_elements(bg_in) ne 0 then begin
        if size(bg_in,/tname) eq 'STRING' then begin
            bg=c2i(bg_in) 
        endif else begin
            bg=bg_in 
        endelse
    endif
end

; Main procedure.
pro pplot, x, y, xerror=xerror, yerror=yerror, aspect=aspect, center=center, $
           errthick=eth, $
           color=color_in, $
           errcolor = ecol_in, $
		   background = background_in, $
           nohat=hat, $
           hatlength=hln, $
           errstyle=est, $
           $
           nskip = nskip, $
           noclip = noclip, $
           noerase=noerase, $
           device=device, $
           $
           overplot=overplot, $
           _extra=_extra

  on_error, 2

  nx = n_elements(x)
  ny = n_elements(y)

  if nx eq 0 then begin 
      print,'-Syntax: '
      print,'  pplot, x, y, xerror=, yerror=, aspect=, /center, '
      print,'         errcolor=, errstyle=, errthick=, /nohat, hatlength=, nskip=, '
      print,'         _extra='
      print
      message,'Halting'
  endif 

  ; Color input.  We support strings for colors through c2i.pro
  _pplot_convert_colors, color_in, ecol_in, background_in, color, ecol, bg

  nxerror = n_elements(xerror)
  nyerror = n_elements(nyerror)
  if (nx ne 0) and (ny ne 0) and (nx ne ny) then begin 
      message,'X and Y must be same length'
  endif 
  if nxerror ne 0 then begin 
      if nxerror ne nx then begin 
          message,'Errors must be same length as x,y vectors'
      endif 
  endif 
  if nyerror ne 0 then begin 
      if nyerror ne nx then begin 
          message,'Errors must be same length as x,y vectors'
      endif 
  endif 

  ;; Deal with the aspect ratio stuff. Note, center not used except
  ;; if aspect sent
  if keyword_set(aspect) and not keyword_set(overplot) then begin 
      position = _pplot_aspect_position(aspect, center=center)
      device=1
      noerase=1
  endif 

  
  _pplot_doplot, x, y, $
    xerror=xerror, $
    yerror=yerror, $
    $
    errthick=eth, $
    color=color, $
    errcolor = ecol, $
	background=bg, $
    nohat=hat, $
    hatlength=hln, $
    errstyle=est, $
    $
    nskip = nskip, $
    noclip = noclip, $
    position=position, $
    $
    noerase=noerase, $
    device=device, $
    $
    overplot=overplot, $
    _EXTRA = _extra, ANONYMOUS_ = DUMMY_
  

end 

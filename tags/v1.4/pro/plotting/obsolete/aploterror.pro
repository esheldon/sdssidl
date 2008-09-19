PRO aploterror, aspect, x, y, xerr, yerr, _extra=extra, center=center, background=background

;+
;
; NAME:
;    APLOTERROR
;       
; PURPOSE:
;   Wrapper for PLOTERR that forces a user defined aspect ratio. Has
;   advantages over the aspect program is that it works in all !p.multi 
;   settings.  This program is obsolete, see pplot.pro
;
; CALLING SEQUENCE:
;    aploterr, aspect, [x,] y, [xerr,] yerr, center=center, _extra=extra
;
; INPUTS: 
;    aspect: xsize/ysize
;    y: y values
;    yerr: y error values.
;
; OPTIONAL INPUTS:
;    x: optional x values
;    xerr: x error values.
;
; KEYWORD PARAMETERS:
;    /center: if set then center up the display.
;    _extra:  plotting keywords.
;       
; CALLED ROUTINES:
;    PLOTERR
; 
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    Author: Erin Scott Sheldon  UofMich 11/17/99  
;       
;                                      
;-                                       
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

  On_error,2

  np=n_params()
  IF (np LT 3) OR (np GT 5) THEN BEGIN 
     print,'-Syntax: aploterr, aspect, [x,] y, [xerr,] yerr, /center, _extra=extra'
     print,''
     print,' aspect = xsize/ysize'
     print,'Use doc_library,"aploterr"  for more help.'  
     return
  ENDIF 

  IF !p.multi[1] EQ 0 THEN !p.multi[1] = 1
  IF !p.multi[2] EQ 0 THEN !p.multi[2] = 1

  plot, [0,1], [0,1], /nodata, xstyle=4, ystyle=4, background=background
  px = !x.window*!d.x_vsize
  py = !y.window*!d.y_vsize

  xsize = px[1] - px[0]
  ysize = py[1] - py[0]

  a0 = xsize/ysize
  CASE 1 OF
      (aspect LE a0): xsize = xsize*(aspect/a0) ;shrink xsize
      (aspect GT a0): ysize = ysize*(a0/aspect) ;shrink ysize
  ENDCASE 

  px[1] = px[0] + xsize
  py[1] = py[0] + ysize

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; center up the display 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  IF keyword_set(center) THEN dcenter, xsize, ysize, px, py

  position = [ [px(0), py(0)], [px(1), py(1)] ]

  CASE np OF                    ;case where x=y, y=yerr
      3: ploterror, x, y, position=position, /device, /noerase, _extra=extra
                                ;case where x=y, y=yerr
      4: ploterror, x, y, xerr, position=position, $
        /device, /noerase,_extra=extra
      5: ploterror, x, y, xerr, yerr, position=position, $
        /device, /noerase, _extra=extra
      ELSE : print,'What!?!'
  ENDCASE 

return
END 

;+
; NAME:
;  LOGLABELS
;
; PURPOSE:
;  Generate nice labels for a log plot. 
;
; CATEGORY:
;  Plotting
;
; CALLING SEQUENCE:
;  This is used with the [xyz]tickformat keyword to plotting routines. e.g.
;  plot, x, y, /xlog, xtickf='loglabels'
;
; MODIFICATION HISTORY:
;  Written by Erin Sheldon UofChicago
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


FUNCTION loglabels, axis, index, value

  on_error,2

  ;; we won't use axis or index

  tickval = round(alog10(value))
  IF tickval EQ 0 THEN BEGIN 
      tickn = '1'
  ENDIF ELSE IF tickval EQ 1 THEN BEGIN 
      tickn = '10'
  ENDIF ELSE IF tickval EQ -1 THEN BEGIN 
      tickn = '0.1'
  ENDIF ELSE BEGIN 
      tickn = '10!U'+ntostr(tickval)+'!N'
  ENDELSE 

  return,tickn

END 



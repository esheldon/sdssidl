;+
; NAME:
;    BAYES_EVAL_POLY2D
;
;
; PURPOSE:
;  Util for bayes_prob.pro
;
; MODIFICATION HISTORY:
;    Creation:  ??-??-?? Dave Johnston, UofChicago
;
;     Current version: 1.5
;-
;
;
;
;  Copyright (C) 2006  Dave Johnston
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
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

PRO bayes_eval_poly2d,x,y,k,val

  IF n_params() EQ 0 THEN BEGIN
      print,'-syntax bayes_eval_poly2d,x,y,k,val'
      RETURN
  ENDIF

  ss=size(k)
  nx=ss(1)
  ny=ss(1)

  val = 0.0d

  FOR i=0,nx-1 DO BEGIN
      FOR j=0,ny-1 DO BEGIN
          val = val + k(j,i)*x^i*y^j
      ENDFOR
  ENDFOR


  RETURN

END











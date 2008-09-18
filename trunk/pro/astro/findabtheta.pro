

;+
;
; NAME: 
;    FINDABTHETA
;       
; PURPOSE: 
;    find the axis ratio and position angle of an object with
;          e1 and e2 as ellipticity parameters.  Currently only
;          works for unweighted or adaptively weighted moments
;          and lupton 1/r weighting
;	
;
; CALLING SEQUENCE: 
;    findabtheta, e1, e2, aratio, posangle, verbose=verbose
;      
;                 
;
; INPUTS: 
;    e1, e2: ellipticity parameters.
;
; INPUT KEYWORD PARAMETERS: 
;         /verbose: print the output e1 and e2
;         /lupton: if this is set, then use formulae for q and u
;
;       
; OUTPUTS: 
;    aratio: axis ratio
;    posangle: position angle from the x-axis.
;
; PROCEDURE: 
;	
;	
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon    UofM  5/??/99  
;-                                       
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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

pro findabtheta, e1,e2, aratio, posangle, verbose=verbose,lupton=lupton

if n_params() LT 2 then begin
	print,'-Syntax: findabtheta, e1,e2 [, aratio, posangle, lupton=lupton, verbose=verbose]'
        print,' Returns theta in radians'
        print,' Set keyword lupton for q and u'
        print,' If sqrt(e1^2 + e2^2) gt 1.0 then values are -1000.0'
	return
endif


e1=float(e1)
e2=float(e2)
n=n_elements(e1)
IF n NE 1 THEN BEGIN 
  posangle=fltarr(n)-1000.0
  aratio=fltarr(n)-1000.0
ENDIF ELSE BEGIN
  posangle = -1000.
  aratio = -1000.
ENDELSE 

w=where(e1 NE 0.0 AND e2 NE 0.0 AND sqrt(e1^2 + e2^2) LE 1.0, nw)
w1=where(e1 EQ 0.0, nw1)
w2=where(e2 EQ 0.0, nw2)


IF nw NE 0 THEN BEGIN
  posangle[w] = .5*atan(e2[w], e1[w])
  aratio[w]=sqrt( (1-e1[w]/cos(2*posangle[w]) )/(1+e1[w]/cos(2*posangle[w])) )
ENDIF 

IF nw1 NE 0 THEN BEGIN
  posangle[w1] = !pi/4.0*( (e2[w1] GT 0.0) + (-1)*(e2[w1] LT 0.0) )
  aratio[w1] = sqrt( (1-abs(e2[w1]))/(1+abs(e2[w1])) )
ENDIF
IF nw2 NE 0 THEN BEGIN
  posangle[w2] = !pi/2.0*( e1[w2] LT 0.0 )
  aratio[w2] = sqrt( (1-abs(e1[w2]))/(1+abs(e1[w2])) )
ENDIF

IF keyword_set(lupton) THEN aratio = aratio^2


IF keyword_set(verbose) THEN BEGIN
  print,'Aratio: ',strtrim(string(aratio),2)
  print,'Posangle: ',strtrim(string(posangle),2)
ENDIF

return 
END 

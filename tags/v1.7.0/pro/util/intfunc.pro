PRO intfunc, fi, xi, intfi, plot=plot, double=double

;+
;
; NAME: 
;    INTFUNC
;       
; PURPOSE: 
;    Make a cumulative integral function.  Basically just tabulate
;    the integral up to each xi
;	
;
; CALLING SEQUENCE:
;   intfunc,f, x [, intfi, plot=plot]
;
; INPUTS: 
;    f: the function
;    x: the abcissae
;
; KEYWORD PARAMETERS:
;         /plot: make a plot of the output function
;       
; OUTPUTS: 
;    the integrated function.
;
; CALLED ROUTINES: 
;          INT_TABULATED
; 
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon UofM  1/??/99
;       
;                                      
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



if n_params() LT 2 then begin
	print,'-Syntax: intfunc, f, x [, intf, plot=plot]'
	print,' Make a new function, intf, which has the integral of f to each point x'
        print,'Use doc_library,"intfunc" for more help'
	return

endif

n = n_elements(xi)

IF NOT keyword_set(double) THEN intfi = fltarr(n) ELSE intfi=dblarr(n)
intfi[0] = 0.0

for i=1, n-1 do begin

	tmpxi = xi[0:i]
	tmpfi = fi[0:i]
	intfi[i] = int_tabulated(tmpxi, tmpfi) 
endfor

if keyword_set(plot) then begin
	plot,xi,intfi,psym=1
endif

return
end


pro convert2xy,ra,dec,xout,yout,rac=rac,decc=decc

;+
; NAME:
;    CONVERT2XY
;
; CALLING SEQUENCE:
;    convert2xy,ra,dec,xout,yout,rac=rac,decc=decc
;
; PURPOSE:
;    Convert from ra and dec to x-y
;
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

if n_params() eq 0 then begin
  	print, 'syntax- convert2xy,ra,dec,xout,yout,rac=rac,decc=decc'
  	return
endif

if n_elements(rac) eq 0 then begin
	rac=(max(ra)+min(ra))/2.0
	decc=(max(dec)+min(dec))/2.0
endif

astr_struct,astr
astr.crval=[double(rac),double(decc)]
rd2xy,ra,dec,astr,xout,yout
return
end

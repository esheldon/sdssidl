
pro convert2rd,x,y,ra,dec,rac=rac,decc=decc

;+
; NAME:
;    CONVERT2RD
;
; CALLING SEQUENCE:
;    convert2rd,x,y,ra,dec,rac=rac,decc=decc
;
; PURPOSE:
;    Convert from x-y to ra and dec
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
  	print, 'syntax- convert2rd,x,y,ra,dec,rac=rac,decc=decc'
  	return
endif

astr_struct,astr
astr.crval=[double(rac),double(decc)]
xy2rd,x,y,astr,ra,dec
return
end

pro  radecstr, ra, dec, rastr, decstr

;+
;
; NAME: 
;    RADECSTR
;       
; PURPOSE: 
;    Wrapper for radec.  Returns string versions of ra and dec.
;	
;
; CALLING SEQUENCE: radecstr, ra, dec, rastr, decstr
;      
;                 
;
; INPUTS: ra, dec in degrees.
;       
; OUTPUTS: rastr, decstr.  Strings containing ra and dec.
;
; CALLED ROUTINES: radec
; 
; EXAMPLE:
;	
;	
;
; REVISION HISTORY: Erin Scott Sheldon U of Mich.  3/28/99
;	Dave Johnston	changed h m s to colons and added 
;			a little more accuracy to last digits
;			of arcseconds ,since the seconds of ra needs 3 
;			digits after the decimal to be accurate to	
;			subarcsecond level
;       Dave Johnston   made it output the negative sign on dec
;                       on the "degrees" part even if it is -0
;
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



  if N_params() LT 2 then begin
     print,'-Syntax: radecstr, ra, dec, rastr, decstr'
     print,''
     print,'Use doc_library,""  for more help.'  
     return
  endif

  chdec=0
  if dec lt 0.0 then begin
        ;temporarily change to positive
        ;this is to put the negative sign on the first "degrees"
        ;part even if it is 0
        ;like -0:34:20.5 rather than 0:-34:20.5  
        chdec=1
        dec=-1d*dec
  endif 


  radec,ra, dec, ihr, imin, xsec, ideg, imn, xsc
  
  ;; round the right significant digit
  xsec=round(xsec*1000)/1000.0
  xsc=round(xsc*100)/100.0	
  

;  rahr = strtrim(string(ihr),2)+':'
;  ramin = strtrim(string(imin), 2)+':'
;  rasec = strtrim(xsec,2)
  ;; now just find the decimal point and take 3 digits after
;  rasec = strmid(rasec, 0, strpos(rasec,'.')+4)
;  rastr = rahr+ramin+rasec

;  dechr = strtrim(string(ideg),2) + ':'
;  decmin = strtrim(string(imn), 2) + ':'
;  decsec = strtrim(string(xsc), 2)
;  decsec = strmid(decsec,0,strpos(decsec,'.')+3) 

  rahr  = strn(ihr,  len=2, padchar='0')+':'
  ramin = strn(imin, len=2, padchar='0')+':'
  rasec = strtrim(xsec,2)
  ;now just find the decimal point and take 3 digits after
  decimal_pos = strpos(rasec, '.')
  rafront = strn( strmid(rasec, 0, decimal_pos), len=2, padchar='0')
  raend = strmid(rasec, decimal_pos+1, 3)
  rasec = rafront + '.' + raend
  rastr = rahr+ramin+rasec

  dechr  = strn(ideg, len=2, padchar='0') + ':'
  decmin = strn(imn,  len=2, padchar='0') + ':'
  decsec = strtrim(string(xsc), 2)

  decimal_pos = strpos(decsec, '.')
  decfront = strn( strmid(decsec, 0, decimal_pos), len=2, padchar='0')
  decend = strmid(decsec, decimal_pos+1, 2)
  decsec = decfront + '.' + decend

  ;; Put sign on dec
  if chdec eq 1 then begin
      dechr="-"+dechr
      dec=-1d*dec
  endif ELSE BEGIN 
      dechr = '+'+dechr
  ENDELSE 

  decstr = dechr+decmin+decsec


return
end        


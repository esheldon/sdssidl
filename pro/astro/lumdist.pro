

;+
;
; NAME: 
;    LUMDIST
;       
; PURPOSE:
;    calculate luminosity distance
;    Currently only works for lambda = 0 universe.  See angdist_lambda.
;	
;
; CALLING SEQUENCE: 
;    result = lumdist(z, h=h, omegamat=omegamat, verbose=verbose, $
;                   plot=plot,oplot=oplot)
;      
; INPUTS:  
;    z: redshift
;
; OPTIONAL INPUTS: 
;    h: hubble parameter      default 1.
;    omegamat:  omega matter  default 1.
;       
; OUTPUTS: 
;   dist in Mpc.
;
; REVISION HISTORY: Erin Scott Sheldon 2/24/99
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

FUNCTION  lumdist, z, h=h, omegamat=omegamat, verbose=verbose, plot=plot,oplot=oplot

  if N_params() eq 0 then begin
	print,'Syntax: result = lumdist(z [, h=h, omegamat=omegamat, verbose=verbose])'
	print,'Returns luminosity distance in Mpc'
        print,'Use doc_library, "lumdist" for more help.'
	return, 0.
  endif
;;;;;;;;;;;;;;;;;;  check keywords  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if n_elements(h) EQ 0 then h=1.0

if n_elements(omegamat) EQ 0 then omegamat=1.0

;;;;;;;; some parameters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

c = 2.9979e5                      ;;  speed of light in km/s
q0 = omegamat/2.0                 ;;  deceleration parameter 
H0 = 100.0*h                      ;;  hubbles constant in km/s/Mpc

;;;;;;;; calculate distances  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

dlum = c*(q0*z + (q0 -1)*( sqrt(1 + 2*q0*z) - 1) )/H0/q0^2

IF keyword_set(verbose) THEN BEGIN 
  print,'-----------------------------------'
  print,'Using h = ',ntostr(h),'  omega matter = ',ntostr(omegamat)
  print,'Luminosity distance: ',ntostr(dlum),' Mpc'
  print,'-----------------------------------'
ENDIF 


IF keyword_set(plot) THEN BEGIN
  xtitle='Z'
  ytitle='Luminosity Distance (Mpc)'
  plot, z, dlum,xtitle=xtitle,ytitle=ytitle
  return, dlum
ENDIF 

IF keyword_set(oplot) THEN oplot,z,dlum

return, dlum
end
	

















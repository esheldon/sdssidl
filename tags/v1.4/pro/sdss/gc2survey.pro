;+
;
; NAME:
;    GC2SURVEY
;       
; PURPOSE:
;    convert from SDSS great circle to SDSS survey coordinates
;
; CALLING SEQUENCE:
;    gc2survey, mu, nu, node, inc, lambda, eta
;
; INPUTS: 
;    mu, nu: great circle coords.
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   lambda, eta: survye coords. 
;
;
; CALLED ROUTINES:
;   GC2EQ
;   GC2SURVEY
;
; PROCEDURE: 
;    Call gc2eq, then eq2survey
;	
;
; REVISION HISTORY:
;    26-Sep-2002  Erin Scott Sheldon
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



PRO gc2survey, mu_in, nu_in, node_in, inc_in, lambda, eta

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: gc2survey, mu, nu, node, inc, lambda, eta'
      print,' mu, nu, node, inc in degrees'
      return
  ENDIF 

  ;; just convert to ra,dec and then convert
  ;; to survey

  gc2eq, mu_in, nu_in, node_in, inc_in, ra, dec
  eq2survey, ra, dec, lambda, eta

  return
END 

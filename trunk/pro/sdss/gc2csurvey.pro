;+
;
; NAME:
;    csurvey2gc
;       
; PURPOSE:
;    convert from corrected SDSS survey coordinates to SDSS great circle
;
; CALLING SEQUENCE:
;    csurvey2eq, clambda, ceta, node, inc, mu, nu
; INPUTS: 
;   clambda, ceta: corrected survey coords. 
;   node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;    mu, nu: great circle coords.
;
; REVISION HISTORY:
;    26-Sep2002  Erin Scott Sheldon UofChicago
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


pro csurvey2gc, clambda, ceta, node, inc, mu, nu

    if n_params() lt 6 then begin 
        on_error, 2
        print,'-Syntax: csurvey2gc, clambda, ceta, node, inc, mu, nu'
        print,' all in degrees'
        message,'Halting'
    endif 

    sdssidl_setup
    !sdss->csurvey2gc, clambda, ceta, node, inc, mu, nu

end 

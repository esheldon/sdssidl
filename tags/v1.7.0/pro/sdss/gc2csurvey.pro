;+
;
; NAME:
;    gc2csurvey
;       
; PURPOSE:
;    convert from SDSS great circle to corrected SDSS survey coordinates
;
; CALLING SEQUENCE:
;    gc2csurvey, mu, nu, node, inc, clambda, ceta
;
; INPUTS: 
;    mu, nu: great circle coords.
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   clambda, ceta: survey coords. 
;
; REVISION HISTORY:
;    26-Sep2002  Erin Scott Sheldon UofChicago
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


pro gc2csurvey, mu, nu, node, inc, clambda, ceta

    if n_params() lt 6 then begin 
        on_error, 2
        print,'-Syntax: gc2csurvey, mu, nu, node, inc, clambda, ceta'
        print,' all in degrees'
        message,'Halting'
    endif 

    trans=obj_new('sdss_transform')
    trans->gc2csurvey, mu, nu, node, inc, clambda, ceta
    obj_destroy, trans

end 

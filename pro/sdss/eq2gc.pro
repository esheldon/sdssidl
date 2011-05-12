
;+
;
; NAME:
;    EQ2GC
;       
; PURPOSE:
;    convert from equatorial to great circle coordinates
;
; CALLING SEQUENCE:
;    eq2gc, ra, dec, node, inc, mu, nu
;
; INPUTS: 
;    ra, dec: equatorial
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   mu, nu: great circle coords
;
; PROCEDURE: 
;    Taken from astrotools
;
; REVISION HISTORY:
;    14-NOV-2000  Erin Scott Sheldon UofMich Taken from astrotools
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


PRO eq2gc, ra, dec, node, inc, mu, nu

    if n_params() lt 6 then begin 
        on_error, 2
        print,'-Syntax: eq2gc, ra, dec, node, inc, mu, nu'
        print,' ra, dec, node, inc in degrees'
        print
        message,'Halting'
    endif 

    trans=obj_new('sdss_transform')
    trans->eq2gc, ra, dec, node, inc, mu, nu
    obj_destroy, trans

END 


;+
;
; NAME:
;    EQ2SURVEY
;       
; PURPOSE:
;    Convert from ra, dec to lambda, eta (SDSS survey coordinates)
;    Use of these coordinates are not recommended because they do
;    not form a meaningful system.  See the documentation for eq2csurvey.
;
; CALLING SEQUENCE:
;    eq2survey, ra, dec, lambda, eta
;
; INPUTS: 
;    ra: Equatorial latitude in degrees 
;    dec: Equatorial longitude in degrees
;       
; OUTPUTS: 
;    lambda: Survey longitude in degrees
;    eta: Survey latitude in degrees
;
; REVISION HISTORY:
;    Written: 5/15/2000  Erin Scott Sheldon
;                        Taken from astrotools.
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



pro eq2survey, ra, dec, lambda, eta

    if n_params() lt 4 then begin 
        on_error, 2
        print,'-Syntax: eq2survey, ra, dec, lambda, eta'
        print,' ra, dec in degrees'
        print
        message,'Halting'
    endif 
 
    sdssidl_setup
    !sdss->eq2survey, ra, dec, lambda, eta

end


;+
;
; NAME:
;    SURVEY2EQ
;       
; PURPOSE:
;    Convert from lambda, eta (SDSS survey coordinates) to ra, dec
;    Use of these coordinates are not recommended because they do
;    not form a meaningful system.  See the documentation for eq2csurvey.
;
; CALLING SEQUENCE:
;    survey2eq, lambda, eta, ra, dec
;
; INPUTS: 
;    lambda: Survey longitude in degrees
;    eta: Survey latitude in degrees
; 
; OUTPUTS: 
;    ra: Equatorial latitude in degrees
;    dec: Equatorial longitude in degrees
;
; REVISION HISTORY:
;    Written: 5/15/2000  Erin Scott Sheldon
;                        Taken from astrotools.
	
;
; REVISION HISTORY:
;    Written: 5/15/2000  Erin Scott Sheldon
;                        Taken from astrotools.
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

pro survey2eq, lambda, eta, ra, dec

    sdssidl_setup
    !sdss->survey2eq, lambda, eta, ra, dec

end

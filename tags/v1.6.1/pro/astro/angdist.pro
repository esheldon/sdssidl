;+
;
; NAME: 
;    ANGDIST
;       
; PURPOSE: 
;    Calculate angular diameter distance between zmin and zmax in a lambda cosmology
;
; CALLING SEQUENCE: 
;    result = angdist(zmin, zmax, omega_m=, omega_l=, omega_k=, flat=, h=, npts=, Dl=)
; INPUTS:  
;   zmin, zmax: The min and max redshifts
;
; OPTIONAL INPUTS:
;   omega_m, omega_l, omega_k: The omegas. Defaults (0.27, 0.73, 0.0)
;   flat: Default is 1 (true)
;   h: Default 1.0
;   npts: # points for Ez integration, default 5 good to 1.e-7
;
; OUTPUTS: 
;    Distance in MPC/h
;
; REVISION HISTORY: 
;   Author: Erin Scott Sheldon 2/24/99
;   Converted to lambda cosmology with full integration.  Uses cosmology IDL class.
;       2007-04-28, Erin Sheldon, NYU
;                                      
;-                                     
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of version 2 of the GNU General Public License as 
;    published by the Free Software Foundation.
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


function angdist, z1, z2, $
    omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts, Dl=Dl

    on_error,2
    c=obj_new('cosmology')
    Da = c->Da(z1, z2, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, $
        npts=npts, Dl=Dl)
    obj_destroy, c
    return, Da

end


;+
; NAME:
;   aeta_lambda
;
; PURPOSE:
;   Calculate the conformal time to redshift z in a flat universe given omegam.
;   Uses 4th order polynomial approximation 
;    see Pen, U., 1999, ApJ Supplement, 20, 49.
;    For z between 0.2 and 1.0 the error is about .4% 
;    (Exact for omega_matter = 1.0)
;
; CALLING SEQUENCE:
;   aeta = aeta_lambda(z, omegamat)
; 
; MODIFICATION HISTORY
;   Author: Erin Sheldon, UChicago
;   First documented, 1-August-2005.   
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


function aeta_lambda, z, omegamat

  s3 = (1.-float(omegamat))/omegamat
  s = s3^(1./3.)
  s2 = s^2
  s4 = s^4
  a = 1./(1.+float(z))

  c0 = 1.
  c1 = -.1540
  c2 = .4304
  c3 = .19097
  c4 = .066941
  
  ex = -1./8.

  ;; returns conformal time
  return, 2.*sqrt(s3+1.)*( c0/a^4 + c1*s/a^3 + $
                           c2*s2/a^2 + c3*s3/a +c4*s4)^ex
end 

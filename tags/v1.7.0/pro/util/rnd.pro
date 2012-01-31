;+
; NAME:
;  RND
;
; PURPOSE:
;  Round a number to the nearest decimal point.  Similar to build-in round, but
;  can round to any digit.
;
;
; CATEGORY:
;  Util.
;
;
; CALLING SEQUENCE:
;  num = rnd(number [, digit])
;
;
; INPUTS:
;  number: the number to be rounded
;
; OPTIONAL INPUTS:
;  digit: The rounding digit.  Default is digit=0, or integer rounding.
;
;
; OUTPUTS:
;  number rounded to requested digit.
;
; EXAMPLE:
;  IDL> print,rnd(3.14159, 4)
;      3.14160
;
;
; MODIFICATION HISTORY:
;  First documented 1-August-2005.  Author, Erin Sheldon Uchicago
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


function rnd, val, digit

  st = size(val, /type)
  if st eq 5 then ten = 10d else ten = 10.

  if n_elements(digit) eq 0 then digit = 0
  return, round(val*ten^digit)*ten^(-digit)

end 

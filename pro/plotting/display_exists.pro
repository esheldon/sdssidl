;+
; NAME:
;  DISPLAY_EXISTS
;
;
; PURPOSE:
;  Test if there is an X display. Works on *nix systems with the sh shell.
;  Note: if no display exists, but you log into a remote machine with 
;  X-forwarding, then this program will incorrecly think the display exists.
;  This result in a crash if a window, for example, is created.
;
; CALLING SEQUENCE:
;  if display_exists() then ..... else ....
;
; RESTRICTIONS:
;  Only works on *nix systems.
;
; EXAMPLE:
;  if display_exists() then window,1
;
;
; MODIFICATION HISTORY:
;  ??-??-2003  Erin Sheldon UChicago
;  27-Oct-2004 Forced use of sh shell. Tom Downes UChicago
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


FUNCTION display_exists

   spawn,'if test -n "${DISPLAY}" ; then echo ok; fi;',ans,/SH
   IF ans[0] EQ 'ok' THEN return,1 ELSE return,0

END 

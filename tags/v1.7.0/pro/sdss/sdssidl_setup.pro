;+
; NAME:
;  SDSSIDL_SETUP (DEPRECATED)
;
; PURPOSE:
;  Set up SDSSIDL variables. The global variable !sdss is created.  This
;  is an instance of the sdss class.  This class inherits sdss_files, 
;  sdss_flags, and sdss_util classes.
;
;   This is deprecated because it is opaque and requires pre-setup by
;   the user.
;
; CATEGORY:
;  SDSS specific routine.
;
; CALLING SEQUENCE:
;  sdssidl_setup, /silent, /reload_config
;
; RESTRICTIONS:
;  To load the config file, the user must set the $SDSSIDL_CONFIG environment
;  variable.  This is automatically set if the sdssidl_setup shell script
;  is sourced.
;
; MODIFICATION HISTORY:
;  Created: Erin Sheldon, UofChicago
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


pro sdssidl_setup, silent=silent

  ;; Define !sdss as an instance of the sdss class
  defsysv, '!sdss', exist=sexists
  if not sexists then begin 
      defsysv, '!sdss', obj_new('sdss')
  endif 

end 

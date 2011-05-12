;+
; NAME:
;  SDSS__DEFINE    (An IDL Class File)
;
; PURPOSE:
;   The main SDSS class file.  Inherits sdss_util, sdss_files, sdss_flags.
;   So far the whole point of this class is to inherit the others and 
;   simplify the interface.
;
; CALLING SEQUENCE:
;   sd=obj_new('sdss')
;
;
; EXAMPLES:
;    IDL> sdss=obj_new('sdss')
;    IDL> struct = sdss->read('tsobj', 756, 2, fields=[35,112])
;
;    IDL> if sdss->run_exists(756) then ....
;    
;    IDL> fst = {galaxy_red: 'y'}
;    IDL> si = sdss->flag_select(struct.primtarget, 'primtarget', fst)
;   
; MODIFICATION HISTORY:
;  Created Mid 2005 Erin Sheldon, UChicago
;
;-
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



function sdss::init
  sfile_init = self->sdss_files::init()
  sf_init    = self->sdss_flags::init()
  ut_init    = self->sdss_util::init()
  tr_init    = self->sdss_transform::init()

  ;; some of these may fail, but we want to be able to access methods anyway
  ;; such as flags
  return,1
end 



function sdss::cleanup
  files_st = self->sdss_flags::cleanup()
  flag_st  = self->sdss_flags::cleanup()
  ut_st    = self->sdss_util::cleanup()
  tr_st    = self->sdss_transform::cleanup()
  return, (files_st + flag_st + ut_st + tr_st) eq 4
end 
pro sdss__define

  struct = {                         $
             sdss,                   $
             inherits sdss_files,    $
             inherits sdss_flags,    $
             inherits sdss_util,     $
             inherits sdss_transform $
           }

end 

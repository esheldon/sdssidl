
;+
; NAME:
;  SDSS_MAGUSE
;
;
; PURPOSE:
;  Given a structure, look for magnitude arrays and return the tag index of the
;  first one found.  The default is to look for CMODEL_COUNTS, then 
;  COUNTS_MODEL, PETROCOUNTS, FIBERCOUNTS, PSFCOUNTS. The user can tell it
;  to look for a specific tag as well using the magUse keyword.
;
;
; CATEGORY:
;  SDSS specific routine
;
;
; CALLING SEQUENCE:
;  wmag = sdss_maguse(struct, magUse=magUse, /silent)
;
;
; INPUTS:
;  struct: A structure.
;
;
; OPTIONAL INPUTS:
;  magUse: the name of the magnitude to look for.
;
;
; KEYWORD PARAMETERS:
;  /silent: no informative messages.
;
;
; OUTPUTS:
;  the tag index of the matching magnitude array.
;
;
; EXAMPLE:
;  run=756
;  rerun=40
;  camcol=3
;  field=125
;  nFrames=20
;  read_tsobj, [run,rerun,camcol], struct, start=field, nFrames=nFrames
;  wmag = sdss_maguse(struct)
;
;
; MODIFICATION HISTORY:
;   Creation: ??-??-2003  Erin Sheldon UofChicago
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


FUNCTION sdss_maguse, struct, maguse=maguse, silent=silent

  IF n_elements(struct) EQ 0 THEN BEGIN 
      print,'-Syntax: wmag = sdss_maguse(sdss_struct, maguse=maguse, /silent)'
      return,-1
  ENDIF 

  tags = tag_names(struct)

  IF n_elements(maguse) NE 0 THEN BEGIN 
      wmag = where(tags EQ strupcase(maguse), nmatch)
      IF nmatch NE 0 THEN BEGIN 
          return, wmag[0]
      ENDIF 
  ENDIF 

   wmag = where(tags EQ 'CMODEL_COUNTS',ncmod)
   IF ncmod EQ 0 THEN BEGIN 
       wmag = where(tags EQ 'COUNTS_MODEL',nmod)
       IF nmod EQ 0 THEN BEGIN 
           wmag = where(tags EQ 'PETROCOUNTS', npet)
           IF npet EQ 0 THEN BEGIN 
               wmag = where(tags EQ 'FIBERCOUNTS',nfib)
               IF nfib EQ 0 THEN BEGIN 
                   wmag = where(tags EQ 'PSFCOUNTS',npsf)
                   IF (npsf EQ 0) AND (NOT keyword_set(silent)) THEN BEGIN 
                       print,'SDSS_MAGUSE: No magnitude arrays found.'
                   ENDIF
               ENDIF
           ENDIF
       ENDIF
   ENDIF 
   
   return,wmag[0]

END 

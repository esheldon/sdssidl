;+
; NAME:
;  IDLSTRUCT_HCLEAN()
;
; PURPOSE:
;  Remove internal tags from an idlstruct header.
;    nrows, data_format, byte_order, idlstruct_version, 
;    field_names, field_types, field_descriptions, field_lengthts
;    field_input_formats
;
; CALLING SEQUENCE:
;  newhdr = idlstruct_hclean(hdr, status=)
;
; INPUTS:
;  hdr: An idlstruct header, as read by read_idlheader()
;
; OUTPUTS:
;  The cleaned header, or -1 of all tags removed.
;
; OPTIONAL OUTPUTS:
;  status: 0 for success.
;
; MODIFICATION HISTORY:
;  Created  02-Jul-2004 Erin Sheldon, UofChicago
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

FUNCTION idlstruct_hclean, hdrStruct, status=status

  status = 1
  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: new_hdr = idlstruct_hclean(hdrStruct, status=)'
      return,-1
  ENDIF 

  IF size(hdrStruct, /tname) EQ 'STRUCT' THEN BEGIN 

      rmtags = $
        ['nrows','data_format','byte_order','idlstruct_version',$
         'field_names','field_types','field_descriptions',$
         'field_lengths','field_input_formats']
      
      
      newhdr = remove_tags(hdrStruct, rmtags)
      
      status = 0
      return,newhdr
  ENDIF ELSE BEGIN 
      message,'input header must be a structure. Returning -1',/inf
      return, -1
  ENDELSE 

END 

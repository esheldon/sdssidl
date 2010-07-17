pro delvarx, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, $
             v15, v16

;+
;
; NAME:
;    DELVARX
;       
; PURPOSE:
;    Make input variables undefined. Similar to intrinsic DELVAR, but
;    works at any calling level (e.g. within procedures)
;    This is a factor of 40 times faster than the astronomy
;    library version of delvarx.
;
; CALLING SEQUENCE:
;    delvarx, v1, v2, ..., v16
;
; INPUTS: 
;    v1,v2,..... any idl variable
; 
; REVISION HISTORY:
;    Created 23-Apr-2001, Erin Scott Sheldon UofMich
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



  if n_params() eq 0 then begin 
      print,'-Syntax: delvarx, v1, v2, ....'
      print,'  Up to 16 arguments'
      print,''
      print,'Use doc_library,"delvarx"  for more help.'  
     return
  endif 

  np=n_params()

  case np of 
      1:  begin & v1=0l & tmp=temporary(v1) & end 
      2:  begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & end 
      3:  begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & end 
      4:  begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & end 
      5:  begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & end 
      6:  begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & end 
      7:  begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & end 
      8:  begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & end 
      9:  begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & v9=0l & tmp=temporary(v9) & end
      10: begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & v9=0l & tmp=temporary(v9) & v10=0l & tmp=temporary(v10) & end
      11: begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & v9=0l & tmp=temporary(v9) & v10=0l & tmp=temporary(v10) & v11=0l & tmp=temporary(v11) & end
      12: begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & v9=0l & tmp=temporary(v9) & v10=0l & tmp=temporary(v10) & v11=0l & tmp=temporary(v11) & v12=0l & tmp=temporary(v12) & end
      13: begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & v9=0l & tmp=temporary(v9) & v10=0l & tmp=temporary(v10) & v11=0l & tmp=temporary(v11) & v12=0l & tmp=temporary(v12) & v13=0l & tmp=temporary(v13) & end
      14: begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & v9=0l & tmp=temporary(v9) & v10=0l & tmp=temporary(v10) & v11=0l & tmp=temporary(v11) & v12=0l & tmp=temporary(v12) & v13=0l & tmp=temporary(v13) & v14=0l & tmp=temporary(v14) & end
      15: begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & v9=0l & tmp=temporary(v9) & v10=0l & tmp=temporary(v10) & v11=0l & tmp=temporary(v11) & v12=0l & tmp=temporary(v12) & v13=0l & tmp=temporary(v13) & v14=0l & tmp=temporary(v14) & v15=0l & tmp=temporary(v15) & end
      16: begin & v1=0l & tmp=temporary(v1) & v2=0l & tmp=temporary(v2) & v3=0l & tmp=temporary(v3) & v4=0l & tmp=temporary(v4) & v5=0l & tmp=temporary(v5) & v6=0l & tmp=temporary(v6) & v7=0l & tmp=temporary(v7) & v8=0l & tmp=temporary(v8) & v9=0l & tmp=temporary(v9) & v10=0l & tmp=temporary(v10) & v11=0l & tmp=temporary(v11) & v12=0l & tmp=temporary(v12) & v13=0l & tmp=temporary(v13) & v14=0l & tmp=temporary(v14) & v15=0l & tmp=temporary(v15) & v16=0l & tmp=temporary(v16) & end
      else: message,'Too many parameters: only 16 parameters allowed' 
  endcase 
end 

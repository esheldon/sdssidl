

;+
;
; NAME:
;    ADD_LABELS
;       
; PURPOSE:
;    Add labels to an axis.  Ticks are not added, so it is recommended
;     to only label existing ticks.
;
; CALLING SEQUENCE:
;    add_labels, xtickv=xtickv, ytickv=ytickv, xaxis=xaxis, yaxis=yaxis
;
; OPTIONAL INPUTS:
;    xtickv=xtickv: the x-values to label. See plotting keywords in idl online help.
;    ytickv=ytickv: the y-values to label. 
;    ztickv=ztickv: the y-values to label. 
;    xaxis=xaxix: which x-axis? See plotting keywords in idl online help.
;    yaxis=yaxis: which y-axis?
;    zaxis=zaxis: which z-axis?
;    
; KEYWORD PARAMETERS:
;    NONE
;       
; OUTPUTS: 
;    Plots labels on current device.
;
; OPTIONAL OUTPUTS:
;    NONE
;
; CALLED ROUTINES:
;    AXIS
; 
; PROCEDURE: 
;    run AXIS procedure, sending [XYZ]tickv and [XYZ]ticks keywords
;	
;
; REVISION HISTORY:
;    Creation:
;      7-FEB-2002, Erin Scott Sheldon  UofMIch
;       
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

PRO add_labels, xtickv=xtickv, ytickv=ytickv, ztickv=ztickv, xaxis=xaxis, yaxis=yaxis, zaxis=zaxis, _extra=_extra

  nxtick=n_elements(xtickv)
  nytick=n_elements(ytickv)
  nztick=n_elements(ztickv)

  IF (nxtick EQ 0) AND (nytick EQ 0) AND (nztick EQ 0) THEN BEGIN 
      print,'-Syntax: add_labels, xtickv=xtickv, ytickv=ytickv, ztickv=ztickv [, xaxis=xaxis, yaxis=yaxis, zaxis=zaxis]'
      print
      print,' Defaults to xaxis=0 or yaxis=0 or zaxis=0'
      print,' New ticks not created: should use to label existing ticks'
      print,'Use doc_library,"add_labels"  for more help.' 
      return
  ENDIF 

  ;; don't add ticks
  tlen = 1.e-4
  
  IF nxtick NE 0 THEN BEGIN 
      IF n_elements(xaxis) EQ 0 THEN xaxis=0
      axis, xaxis=xaxis, xtickv=xtickv, xticks=( (nxtick-1)>1 ), $
            xticklen=tlen, _extra=_extra
  ENDIF 
  IF nytick NE 0 THEN BEGIN 
      IF n_elements(yaxis) EQ 0 THEN yaxis=0
      axis, yaxis=yaxis, ytickv=ytickv, yticks=( (nytick-1)>1 ), $
            yticklen=tlen, _extra=_extra
  ENDIF 
  IF nztick NE 0 THEN BEGIN 
      IF n_elements(zaxis) EQ 0 THEN zaxis=0
      axis, zaxis=zaxis, ztickv=ztickv, zticks=( (nztick-1)>1 ), $
            zticklen=tlen, _extra=_extra
  ENDIF 


END 

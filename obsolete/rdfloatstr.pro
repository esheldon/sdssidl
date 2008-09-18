pro rdfloatstr,name,use_struct,outstruct,$
               SKIPLINE = skipline, $
               ENDSKIP = endskip, $
               NUMLINE = numline, $
               NLINES = nlines

;+
; NAME:
;      RDFLOATSTR
; PURPOSE:
;      OBSOLETE: use read_struct.pro
;      
;-

  On_error,2                           ;Return to caller
  
  if N_params() lt 2 then begin
      print,'Syntax - RDFLOATSTR, file, use_struct, outstruct, '
      print,'                    SKIPLINE=, ENDSKIP=, '
      print,'                    NLINES=, NUMLINE = ]'
      return
  endif
 
  read_struct, name, use_struct, outstruct,$
    SKIPLINE = skipline, $
    ENDSKIP = endskip, $
    NUMLINE = numline
 
  return
end

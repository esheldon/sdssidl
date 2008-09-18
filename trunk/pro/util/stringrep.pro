;+
; NAME:
;   stringrep
;
; PURPOSE:
;   Get a string representation that can be used with execute() to create a 
;   variable.  This also allows it to be used with mrd_struct() to create
;   a structure tag.  If the /structure keyword is sent then this is 
;   equivalent to size(var, /structure) but the returned structure is 
;   augmented with a new tag 'stringrep' containing the string description.
;   Currently, only 'NONE' is returned for structures, scalar complex, 
;   objects, and undefined variables.  Support for structures and scalar
;   complex may be added in the future.
;
; CALLING SEQUENCE:
;   d=stringrep(variable, /structure)
;
; INPUTS:
;   variable: an IDL variable.
;
; KEYWORD PARAMETERS:
;   /structure: Same as the result for size(var, /structure) but the returned
;       structure is augmented with the 'stringrep' tag containing the string 
;       description of the variable.
;
; OUTPUTS:
;   String description of type structure.
;
; MODIFICATION HISTORY:
;   Created: 2007-07-23.  The descriptor code is based on the old datatype()
;       program from the Goddard libraries.
;
;-

function _stringrep_scalar_postfix, sstr
    case sstr.type_name of
        'BYTE': return, 'B'
        'UINT': return, 'U'
        'LONG': return, 'L'
        'ULONG': return, 'UL'
        'LONG64': return, 'LL'
        'ULONG64': return, 'ULL'
        'DOUBLE': return, 'D'
        else: return, ''
    endcase
end
function _stringrep, var, sstr

    if sstr.type eq 0 or sstr.type eq 8 or sstr.type eq 11 then begin
        return, 'NONE'
    endif

    ; scalar: convert to string representation
    if sstr.n_dimensions eq 0 then begin
        name = sstr.type_name
        if name eq 'COMPLEX' or name eq 'DCOMPLEX' then return, 'NONE'
        if sstr.type_name eq 'POINTER' then return, 'ptr_new()'
        if sstr.type_name eq 'BYTE' then begin
            ; can't to string() on a byte to get the number. Just
            ; convert to an int
            d=strtrim(fix(var),2)
        endif else begin
            d = strtrim(var, 2)
        endelse
        pf=_stringrep_scalar_postfix(sstr)
        d = d+pf
        return, d
    endif

    ; Input is an array

    ; Front parts for array descriptions
    dnames = $
        ['UND','BYT','INT','LON','FLT','DBL','COMPLEX','STR','STC', $
        'DCOMPLEX','PTR','OBJ','UINT','ULON','LON64','ULON64']

    ; build up name with comma separated dimensions
    aa = dnames[sstr.type]+'ARR('
    for i = 0L, sstr.n_dimensions-1 do begin                      
        aa = aa + strtrim(sstr.dimensions[i],2)                 
        if i lt sstr.n_dimensions-1 then aa = aa + ','          
    endfor                                     
    aa = aa+')'                                   
    return, aa
 
end
function stringrep, var, structure=structure

    if n_elements(var) eq 0 then begin
        on_error, 2
        message,'Halting'
    endif
 
    sstr = size(var, /structure)
    desc = _stringrep(var, sstr)

    if keyword_set(structure) then begin
        sstr = create_struct(sstr, 'stringrep', desc)
        return, sstr
    endif else begin
        return, desc
    endelse

end

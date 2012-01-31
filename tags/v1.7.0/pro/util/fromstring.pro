;+
; NAME:
;	fromstring
;
; PURPOSE:
;	Convert a string to an IDL variable.
;
; CALLING SEQUENCE:
;	var=fromstring(str)
;
; INPUTS:
;	str: A string representing an IDL value.  This should be written out
;		exactly as it would be within an IDL program.
;
; OUTPUTS:
;	a variable with the converted value of the string
;
; EXAMPLES:
;	var = fromstring('2.5)
;	var = fromstring('{a:35, b:[-25d, 82.5d]}')
;
; MODIFICATION HISTORY:
;	Created:  2009-07-22 Erin Sheldon, BNL
;
;-
function fromstring, str

	if n_elements(str) eq 0 then begin
		on_error,2
		message,'Usage:  var = fromstring(str)'
	endif
	command='tmp='+str[0]
	if not execute(command) then begin
		s=string('Could not convert the string to a variable: "',str,'"',$
			form='(a,a,a)')
		message,s
	endif

	return, tmp
end

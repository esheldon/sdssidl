;+
; NAME:
;	eval
;
; PURPOSE:
;	Evaluate the contents of a string.  For example, '2' will evaluate
;	to the number 2 and "'hello'" will evaluate to the string 'hello'
;	'[3,4]' will evaluate to the array [3,4].  '{a:35}' evaluates
;	to the structure {a:35}
;
; CALLING SEQUENCE:
;	val = eval(string)
;
; INPUTS:
;	string:  A single string representation of a variable.
;
; OUTPUTS:
;	The corresponding IDL variable.
;
; EXAMPLE:
;
;	IDL> help,eval('{a:35, b:[4,5,6], c:"stuff"}'),/str
;	** Structure <9100f08>, 3 tags, length=24, data length=24, refs=1:
;	A               INT             35
;	B               INT       Array[3]
;	C               STRING    'stuff'
;
; MODIFICATION HISTORY:
;	2010-02-24, Creation, Erin Sheldon, BNL
;
;-
function eval, stringvar
	if n_elements(stringvar) ne 1 then begin
		print,'usage: val = eval(string)'
		message,'halting'
	endif
	command = 'tmp = '+stringvar

	if not execute(command) then begin
		message,'Could not evaluate expression '+stringvar
	endif

	return, tmp
end

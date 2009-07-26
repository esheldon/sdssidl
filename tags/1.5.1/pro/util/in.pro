;+
; NAME:
;	in
;
; PURPOSE:
;	return true of the given element is found in the array or scalar
;
; CALLING SEQUENCE:
;	if in(array, val) then ....
;
;   if in(array, val, index=ind, count=n) then ...
;
; INPUTS:
;	array: An array or scalar of any type.
;	val: A scalar of any type that can be converted to the type of the array.
;
; OUTPUTS:
;	true (1) if the value was found, false (0) of it was not.
;
; OPTIONAL OUTPUTS:
;	index: The result of index=where(array eq val)
;	count: The number of occurrences of the value in the array.
;
; EXAMPLE:
;	if in(array, 25) then begin
;		print,'The value',25,' was found'
;	endif
;
; MODIFICATION HISTORY:
;	2009-06-13 Created.  Motived by the 'in' operator in python.  
;		Erin Sheldon, BNL
;
;-
function in, array, val, index=index, count=count
	index=where(array eq val, count)
	return, count ne 0
end

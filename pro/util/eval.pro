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

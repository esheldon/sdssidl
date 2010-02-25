function rec2idl, recfile
	
	sdssidl_dir=getenv('SDSSIDL_DIR')
	if sdssidl_dir eq '' then message,'SDSSIDL_DIR is not set'

	pyfile=filepath(root=sdssidl_dir,sub=['python'],'rec2idl.py')

	command='python '+pyfile+' '+recfile

	spawn, command, res, err

	if err ne '' then message,'Command failed: "'+command+'"'

	res = strjoin(res,' ')

	; this version has only the strings in it
	st=eval(res)

	; try to evaluate the arguments.  We treat _dtype specially, calling
	; dtype2struct, and we place it in the _structdef argument.  Arguments
	; that do not eval are placed in the struct as strings.

	tn=tag_names(st)
	ntags = n_elements(tn)

	for i=0L, ntags-1 do begin

		if tn[i] eq '_DTYPE' then begin
			val=dtype2struct(st.(i))
			name = '_structdef'
		endif else begin

			name = tn[i]
			stringvar = st.(i)
			command = 'val = '+stringvar
			if not execute(command) then begin
				print,'could not eval: '+stringvar
				val = stringvar
			endif

		endelse

		if n_elements(struct) eq 0 then begin
			struct=create_struct(name, val)
		endif else begin
			struct=create_struct(struct,name,val)
		endelse
	endfor

	return, struct
end

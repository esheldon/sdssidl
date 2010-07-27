pro file_mkdir, path_in, noexpand_path=noexpand_path

	; requires expand_tilde from goddard library
	if keyword_set(noexpand_path) then begin
		path=path_in
	endif else begin
		path = expand_tilde(path_in)
	endelse

	case !version.os_family of
		'unix': begin
			command='mkdir -p '+path
			spawn,command,res,err,/sh
			if err ne '' then begin
				message, err
			endif
		end
		else: message,'Unsupported os_family: '+!version.os_family
	endcase

end

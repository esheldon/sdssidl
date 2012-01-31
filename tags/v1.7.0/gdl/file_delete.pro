pro file_delete, path, quiet=quiet, allow_nonexistent=allow_nonexistent
	; this is different from the idl one in that it does not take
	; all the keywords and it supports wildcards.  And it is unix
	; only right now.

	if not file_test(path) then begin
		if not keyword_set(quiet) and not keyword_set(allow_nonexistent) then begin
			message, 'Unable to delete file: '+path+': No such file or directory'
		endif
		return
	endif
	case !version.os_family of
		'unix': begin
			if file_test(path,/directory) then begin
				command='rmdir ' + path
			endif else begin
				command='rm -f ' + path
			endelse
			spawn,command,res,err,/sh
			if err ne '' then begin
				message, err
			endif
		end
		else: message,'Unsupported os_family: '+!version.os_family
	endcase
end

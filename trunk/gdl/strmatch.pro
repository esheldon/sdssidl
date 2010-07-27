function strmatch, str, pattern_input, fold_case=fold_case

	; the strmatch function is an odd beast, very nonstandard.  I'm
	; trying to fake it up with stregex
	;
	; depends on repstr from idlstro

	; treat \* specially
	psplit = strsplit(pattern_input, '\*', /extract)

	; ? -> . for regexp
	psplit = repstr(psplit, '?', '.')
	; because of strplit call above, these are bare * characters. For
	; regexp they need to be .*
	psplit = repstr(psplit, '*', '.*')

	pattern = strjoin( psplit, '\*')

	; need to escape regex stuff since strmatch does not use regexp
	pattern = repstr(pattern, '$', '\$')
	pattern = repstr(pattern, '^', '\^')
	; need more of these....

	;print,pattern

	return, stregex(str, pattern, /bool, fold_case=fold_case)
end

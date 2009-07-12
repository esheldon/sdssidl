pro which,proc_name,files=files,show=show,help=help, dlm=dlm, silent=silent
;+
; NAME:
;	WHICH
;
; PURPOSE:
;	Determine in which library/directory the procedure or function
;	specified is located in the !PATH.  This is useful for finding
;	out which library a certain procedure comes from, particularly
;	when there are duplicates.  This is similar to the unix
;	'which' command.
;
;	If no files are found, search for intrinsic procedures/functions
;	with the input name and explain if found.
;
; CALLING SEQUENCE:
;    WHICH, [ proc_name, files=, /dlm, /show, /help, /silent ]
;
; OPTIONAL INPUT:
;	proc_name - Character string giving the name of the IDL procedure or 
;		function.  Do not give an extension.   If omitted, 
;		the program will prompt for PROC_NAME.
;		/dlm: Search !DLM_PATH for dynamically loaded modules instead
;			of !PATH for .pro files
;       /show: if set, will display the first file found
;       /help: print syntax
;
; OPTIONAL OUTPUT KEYWORDS:
;	files: An array containing the filenames
;
; PROCEDURE:
;	The system variable !PATH is parsed into individual libraries or 
;	directories.   Each library or directory is then searched for the
;	procedure name.  If /dlm is set, search !DLM_PATH for .dlm files
;
; EXAMPLE:
;	Find out where the procedure CURVEFIT lives.
;
;	IDL> which, 'curvefit'
;
; RESTRICTIONS:
;	None.
;
; REVISION HISTORY:
;	29-MAY-94  Modified from getpro.pro by E. Deutsch
;	14-JUL-95  Fixed for IDL 4.0
;       03-DEC-2000 Added files and show keywords. Erin Sheldon
;       21-JUN-2004 Use FILE_WHICH procedure for IDL > 5.3  for significant
;             speed increase. Fixed intrinsic procedure searching. E.S.
;		??-??-2006 Look also for dynamically loaded modules if /dlm is set
;	27-May-2009: Due to bug in idl 7.0 file_which, moved over to using 
;		straight file_search.  This now makes the dividing line 5.5 between
;		the two methods.  Erin Sheldon, BNL
;-

  if keyword_set(help) then begin
      print,'-syntax: which, proc_name, files=files, show=show, help=help'
      return
  endif 

  ;on_error,2                    ;return to caller on error

  common which_block, funcnames, pronames

  ;; vms or unix operating system
  os = !version.os                     

  ;; can use faster searching on 5.4
  idlversion = float(!version.release)

  ;; prompt for procedure name?
  if (n_params() eq 0) then begin 	     
      proc_name = ' ' 
      read,'Enter name of procedure to look for: ',proc_name     
  endif else zparcheck, 'which', proc_name, 1, 7, 0, 'Procedure name'

  ;; Don't want file extensions

  fdecomp, proc_name, disk, dir, name      
  name = strtrim( name, 2 )  

  ;; Set up separate copy commands for VMS and Unix

  if (os eq "vms") then begin   
      sep = ',' & dirsep = '' & name = strupcase(name)
  endif else begin
      sep = ':' & dirsep = '/'
  endelse   

  ;; Get current IDL path of directories
  
  if keyword_set(dlm) then begin 
      temp = !dlm_path
      ext = '.dlm'
  endif else begin 
      temp = !path
      ext = '.pro'
  endelse                 
  if (os eq "vms") then temp = strupcase(temp)
  

  ;; Loop over each directory in !PATH until procedure name found
  delvarx, files
  
  found=0
  while (temp ne '') do begin   
      dir = gettok( temp, sep)
      
      if strmid(dir,0,1) EQ '@' then begin ;Text Library?
          if (os ne "vms") then message, $
            '!path contains a invalid VMS directory specification',/cont $
          else begin
              libname = strmid( dir,1,strlen(dir)-1 ) ;Remove the "@" symbol
;              spawn,'library/extract='+name+'/out='+name+'.pro '+$
;                libname,out,count=i

              spawn,'library/extract='+name+'/out='+name+ext+' '+$
                libname,out,count=i

              if (i eq 0) then begin ;Success?
                  message,name + ext+' extracted from ' + libname,/INF
                  return
              endif
          endelse
      endif else begin          ;Directory

          IF IDLversion LT 5.5 THEN BEGIN 
              ;; Old way
;              a = findfile(dir + dirsep + name+'.pro',COUNT=i)
              a = findfile(dir + dirsep + proc_name+ext,COUNT=i)
              if (I ge 1) then begin ;Found by FINDFILE?
;                  filename = dir+dirsep+name+'.pro'
                  filename = dir+dirsep+proc_name+ext
                  add_arrval, filename, files
                  
                  IF NOT keyword_set(silent) THEN BEGIN 
                      if (found eq 0) then print,'Using: '+filename
                      if (found eq 1) then print,'Also in: '+filename
                  ENDIF 
                  found=1
              endif
          endif else BEGIN 
              ;; New way: E.S.S.
			  filename = filepath(root=dir, proc_name+ext)
			  lfiles = file_search(dir, proc_name+ext)
			  ;if file_test(filename) then begin
			  if lfiles[0] ne '' then begin

                  if not keyword_set(silent) then begin 
                      if (found eq 0) then print,'Found in: ',dir
                      if (found eq 1) then print,'Also in: ',dir

                      ;lfiles = file_search(dir, proc_name+ext)
                      for ifl = 0L, n_elements(lfiles)-1 do begin
						  print,'   '+lfiles[ifl]
					  endfor

                  endif 
                  found=1
                  add_arrval, filename, files

              endif 
          endelse 

      endelse
  endwhile
  
  if (found eq 1) then begin
      if keyword_set(show) then spawn,'more '+files[0]
      return
  endif 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; the search failed. check for intrinsic idl procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  if n_elements(funcnames) eq 0 then begin 
      proNames  = routine_info(/system)
      funcNames = routine_info(/system,/func)
  endif 

  uname = strupcase(name)
  functest = where (funcnames eq uname, fcount)
  protest  = where (pronames  eq uname, pcount)
  
  if (fcount eq 0) and (pcount eq 0) then begin

      ;; not found
      if not keyword_set(silent) then begin 
          print, 'Procedure '+Name+' not found in a !PATH directory.'
          print, 'Check your spelling or search individual directories.'
      endif 

  endif else begin 
      files = 'INTRINSIC'

      ;; Its either a built in function or procedure
      if not keyword_set(silent) then begin 

          if fcount ne 0 then begin 
              print, 'Function ' + Name + ' is an intrinsic IDL Function.'
              print, 'No path information available.'
          endif else begin 
              print, 'Procedure ' + Name + ' is an intrinsic IDL procedure.'
              print, 'No path information available.'
          endelse 
      endif 

  endelse 

  return

end 
  

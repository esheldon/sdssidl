
;+
; NAME:  
;    read_tsobj
;
; PURPOSE:
;    Reads in fields from a column of SDSS data into one big structure or
;    array of pointers to object structures. 
;
;    Notes:
;      Try not to kill read_tsObj while running; it uses local pointers which
;         are not freed until the end of execution.  
; 
; CALLING SEQUENCE: 
;     read_tsobj,  directory/[run,rerun,camcol], struct, 
;               start=, last=, nframes=, 
;               /all, 
;               taglist=, 
;               tsobjstr=, 
;               /corrected, 
;               /pointers,
;               /addrowc,
;               /noadd, 
;               /nomodrow, 
;               front=, 
;               verbose=verbose, 
;               ex_struct=ex_struct,
;               wstring=wstring,
;               /add_seeing, /add_sky, 
;               status=status
;
; 
; INPUTS:  
;    directory/[run,rerun,camcol]: Either the full directory where the photo
;         output fits files (tsObj files) live or an int or long array
;         containing [run,rerun,camcol] in that order.
;
; OUTPUTS: 
;    struct: The structure containing the requested tags, or an array of 
;            pointers to objects structures.
;
; OPTIONAL INPUTS:
;    -SPECIFYING THE FIELD RANGE: (default is to read first field)
;          start:     Field to start with.  Default is first field.
;          last:      Last field to read. Takes precedence over "nframes".
;          nframes:   How many frames to read, beginning with "start"
;          /all:      Read all fields. Takes precedence over "start",
;                     "nframes", and "last"
;
;   -TAGS TO READ: (Default is all tags)
;          taglist:   A list of photo tags that the user wants
;                     in struct. The default is to read all tags.
;          /default_tags: run the program make_default_tags.pro to get taglist.
;
;   -RETURN VALUE
;          /pointers: read_tsobj returns an array of pointers, each pointer
;                     referencing an object structure, rather than the default
;                     array of object structures.  Saves factor of 2 max memory
;                     since a copy is not needed.
;
;   -OTHER
;
;          tsobjstr=: Info structure to save time on consecutive calls.  If not
;                     entered, this structure will be set and returned through
;                     the keyword.  This can then be fed into program to save
;                     time.  Contains: (1) good tags from first call (2)
;                     indices of those tags (3) the tsObj structure used. Saves
;                     time because don't have to read in a field to establish
;                     structure definition.  (4) the file list. Avoides using
;                     fetch_file_list which is a slow program.
;
;          ex_struct=: This is for adding extra tags onto the output structure.
;                      These will be left blank so that you can fill them in
;                      later. The exception is "SEEING" which will be 
;                      conveniently filled in from the header information if 
;                      /add_seeing is set.   If tsobjstr is created by
;                      read_tsobj then these  extra tags will be included in
;                      the tsobjstr struct ; for use next time around 
;                      Example:  
;                        IDL> ex={seeing:fltarr(5),probgal:0.0,redshift:0.0}
;                        IDL> read_tsObj, dir, struct, start=13, nframes=100,$
;                             ex_struct=ex
;	
;	   wstring=:  This is a string that you can give to read_tsobj that 
;		      contains the "where" function being appied to "lnew"
;		      (which is the name of the tsobj structure at each 
;		      iteration over fields inside the program).  It is
;		      executed to make a cut before it concatanates into the
;		      big output structure. This can be used to cut down on
;		      memory and execution time if one wants to make a severe
;		      cut on the data. It can be used on any tag in the tsObj
;		      file, not just those tags which will be returned.
;		      Example: (for getting only bright objects)
;			IDL>  wstr="where(lnew.counts_model[2] lt 20.0)"
;			IDL>  read_tsObj, dir, struct, start=13, nframes=100
;			,wstring=wstr
;
;          /add_seeing: Tag "seeing" is added to output struct if not already
;                       present. Seeing info is added from header.
;
;          /add_sky: same as add_seeing. Note: for photo 5.3 sky, skyerr are
;                    photo tags and it is evaluated at _each object_, better
;                    than the mean in the header, so it is better to just add
;                    sky and skyerr to your taglist.
;          front=:  The front string in the filename.  Default is "tsObj"
;
;
;          /corrected: read corrected files instead of tsObj files. Sets front
;                      equal to 'adatc' and look in the shapecorr_dir if
;                      the user inputs run,rerun,camcol.
;
;          /addrowc:  Add up objc_rowc continuously in order to make define an
;                     absolute position within run (used to be default)
;
;          /nomodrow: don't remove overlap region.  Handy if you are 
;                     reading your own files where this has already been done
;                     or if you want everything.
;
;
;
;          verbose: Verbosity level. Default is verbose > 1, full.
;                       verbose = 0    print only error messages
;                       verbose = 1    print minimum of messages
;                       verbose > 1    print all messages.  Includes updates
;                                      every twenty fields
;
;
;          status: returns exit status. if not zero then something went wrong
;
;	   addspec: 	if set it will match the data to the spectroscopic data
;		 	and add this information ,default tags are
;			plate,mjd,fiber,z,zerr,zstat,spec_cln
;			furthermore one can choose to add only a subset of
;			these for example "z" and "zerr"
;			by seeing this keyword to a structure like this
;			addspec={z:0.0,zerr:0.0}. This requires
;			match_radec_spec procedure to be  in your path
;			it is not part of the CVS archive currently.
;			Keep in mind that adding the spectro info to the tsobj
;			struct is rather wasteful of space since most objects
;			are not spectroscopically observed.
;			If space is an issue one can simply use
;			match_radec_spec.pro outside of read_tsobj.
;		
;	   spec_cat	This is the catalog of spectroscopic information that
;	                is used to match the tsobj data. If it is supplied to
;	                read_tsobj and it is already defined it won't have to
;	                reread the file each time it is called.	
;
;          /zero:        Add zero points from the tsField files in tag
;                       "zeropoint". Requires procedure compute_zeropoint to
;                       be in your path; not in CVS archive.
;
; CALLED ROUTINES:  
;     read_tsobj_make_tags.pro
;     read_tsobj_make_struct.pro
;     fetch_file_list.pro
;     (make_default_tags.pro)
;
; EXAMPLES:
;
; -- Just read the first field of run 756, rerun 1, camcol 3
;
; IDL> read_tsObj, [756, 1, 3], struct
;
; -- Read 100 fields, starting at number 13, from
;       run 273, camcol 2 rerun 1. Input the directory.
;
; IDL> fetch_dir, 273, 2, 1
; IDL> read_tsObj, dir, struct, start=13, nframes=100
; 
; Author:  Erin Scott Sheldon
; Date: 10/7/98
; Modified: 11/20/98  Comment:  Rewrote addrow stuff.
;           11/23/98  Comment:  Added start keyword.
;	    01/19/99  Comment:  Added the struct_type argument, Tim McKay
;           03/21/99  Comment:  Added (i gt start) and (i lt n_ext) 
;                               to where. (line 159) Erin S. Sheldon
;	    6/25/99   Added two systems variables needed for 
;			when you don't give it n_frames
;				'!TEXTOUT'  '!TEXTUNIT'	  
;			Dave Johnston  (U Chicago) 
;	    6/26/99  Replaced mrdfits2 with mrdfits3. This has the 
;		     	keyword deja_vu which allows one to use common
;			blocks so that one can remember variables that
;			one has already made when the program was called
;			before.	This speeds it up considerably.
;					Dave Johnston
;
;	    6/26/99  Changed the way that read_photo_col concatonates
;			the individual structures from each field into
;			the output structure struct. Now it uses pointers
;			for each individual struct and keeps and array
;			of pointers. Then in the end it makes the big
;			output struct and copies everything in.
;			This also helps with speed, but uses factor of 2
;                       more memory.
;				Dave Johnston	
;	    6/26/99   Added default nframes=50 and prints time
;						
;	    Oct/9/99  Adapted from read_photo_col to make read_tsObj
;		Now it reads the new format. These new files are
;		individually wrapped by field rather than one column
;		being a file.	
;           11/13/99 Added "front" and "nchar" inputs.  E.S.S.
;           05/03/00 Added tsobjstr, everytag keywords.  Also, no longer
;               requires all caps in taglist, and the user can enter
;               run,rerun,camcol,field,id
;	    10/9/00  Fixed so that it handles "empty" tsobj files
;			by skipping over them.  Uses modified version 
;			of mrdfits3 which returns 0 for these files  
;		     Added wstring keyword so one can apply a cut inside of
;		        read_tsobj saves memory and time when one is just
;		        interested in a small subset of the data
;		     Added ex_struct keyword  ,one can add on new tags of any
;		        type by including this extra structure.  It will be
;		        included in tsobjstr so that it "knows" next time.
;		     If ex_struct or tsobjstr includes the "SEEING" tag
;			it will fill it in with the seeing from the header
;			which is one number per band per field	
;                                       Dave Johnston
;           04-Apr-2002: Seeing only filled in if /add_seeing is set, so won't
;                        copy over seeing tag is already there.  If using
;                        tsobjstr it must be in that structure.
;                                --Erin Sheldon
;           08-Sep-2002: -Made first input either the directory in string
;                         form or an integer or long array containing
;                         [run,rerun,camcol] in that order.
;                        -Added "last" optional input. The last field to read.
;                         All fields betwee start and last will be read.
;                        -Made /everytag the default.  Added /default_tags
;                           which
;                         causes read_tsobj to run make_defaults_tags.pro
;                        -Removed addrow keyword
;                        -Added addrowc keyword: adds up objc_rowc continuosly
;                         which used to be default
;                        -No longer defaults to first field if input range
;                         begins at field less than the first field.  Same with
;                         the end. Thus there may be no files found.  This way
;                         the user always returns data from the requested field
;                         range or none at all.
;                        -Rewrote read_tsobj_make_tags, and _make_struct
;                        -Rewrote parts of fetch_file_list
;                        -Removed unneeded struct_type argument.  Not needed
;                         since we no longer concatenate structures with
;                         braces [struct1,struct2].  Quietly ignored for now
;                                --Erin Sheldon
;           09-Sep-2002:  Added /pointers keyword. Allows user to return an 
;                         array of pointers rather than array of structures.
;                         Can save factor of ~2 in max memory usage but is
;                         less intuitive and most routines expect array of
;                         structures.  I know of no way to return an array
;                         of structures without using this factor of 2 in
;                         memory because we need to first read the data in
;                         order to remove the overlap rows.
;                                --Erin Sheldon
;	    09/25/02: -Added addspec and spec_cat keywords and spectro
;                     matching capability. 
;                     -Added zeropoint reading from tsField files.  
;                                -Dave J
;           29-Jun-2004: no required tags. E.S.S.
;-                      
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Subroutine to define tsobjstr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
FUNCTION define_tsobjstr, dir, fieldmin, fieldmax, run, rerun, camcol, $
                          files, fnums, goodtags, indices, substruct

      tsobjstr = create_struct('dir', dir, $
                               'fieldmin', fieldmin, $
                               'fieldmax', fieldmax, $
                               'run', run, $
                               'camcol', camcol, $
                               'rerun', rerun,  $
                               'files', files, $
                               'fnums', fnums, $
                               'goodtags',goodtags, $
                               'indices',indices, $
                               'substruct',substruct)
      return, tsobjstr
END 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Help message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro read_tsobj_get_fields, start=start, last=last, nframes=nframes, fields=fields

    if n_elements(start) ne 0 then st = start else st=0
    if n_elements(last) ne 0 then begin
        nf = last - st > 0
        if nf eq 0 then return
        fields = st + lindgen(nf)
    endif else if n_elements(nframes) ne 0 then begin
        if nframes lt 1 then return
        fields = start+lindgen(nframes) 
    endif

end


PRO read_tsobj, dir_in, struct, $
                start=start, last=last, nframes=nframes, all=all, $
                taglist=taglist, phototags=phototags, $
                everytag=everytag, default_tags=default_tags, $
                corrected=corrected, $
                pointers=pointers,$
                addrowc=addrowc, noadd=noadd, $
                nomodrow=nomodrow, $
                front=front, nchar=nchar, tsobjstr=tsobjstr, $
                struct_type=struct_type, verbose=verbose,$
                ex_struct=ex_struct,wstring=wstring,$
                add_seeing=add_seeing, add_sky=add_sky, status=status,$
                spec_cat=spec_cat,zero=zero,addspec=addspec

  ;; status is 1 until successfully completed
  status=1
  
  ;; struct begins undefined (should use my fast delvarx program, 
  ;; not the slow idlastron version
  delvarx, struct

  IF n_params() EQ 0 THEN BEGIN 
      print,'-syntax read_tsobj, directory or [run,rerun,camcol], struct, $'
      print,'            start=start, last=last, nframes=nframes, /all, $'
      print,'            taglist=taglist, phototags=phototags, $'
      print,'            /default_tags,$'
      print,'            /corrected, $'
      print,'            /pointers, $'
      print,'            /addrowc, /noadd, $'
      print,'            /nomodrow, $'
      print,'            front=front, nchar=nchar, $'
      print,'            tsobjstr=tsobjstr, $'
      print,'            struct_type=struct_type, verbose=verbose, $'
      print,'            ex_struct=ex_struct,wstring=wstring,$'
      print,'            add_seeing=add_seeing,add_sky=add_sky,status=status,$'
      print,'            spec_cat=spec_cat,zero=zero,/addspec'
      print,''
      print,'Use doc_library,"read_tsObj"  for more help'
      return
  ENDIF 

  ;; convert to new interface
;  read_tsobj_get_fields, start=start, last=last, nframes=nframes, fields=fields
;  struct = sdss_read('tsobj', rrc[0], rrc[2], rerun=rrc[1], $
;      fields=fields, all=all, taglist=taglist, ex_struct=ex_struct, wstring=wstring, nomodrow=nomodrow, pointers=pointers, verbose=verbose, status=status)  
;
;  return 

  time = systime(1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Should we add spec stuff?
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  addsptype=size(addspec,/type)
  if keyword_set(addspec) then begin
      if addsptype ne 8 then begin
          ;; add the whole spec structure since one wasn't specified
          spec_struct={plate:0,mjd:0l,fiber:0,z1d:0.0,$
                       z1derr:0.0,zstat:0,spec_cln:0}
      endif else begin
          ;; the user passed a particular structure so use it 
          spec_struct=addspec
      endelse
      if n_elements(ex_struct) eq 0 then begin
          ex_struct=spec_struct
      endif else begin
          ex_struct=create_struct(ex_struct,spec_struct)
      endelse
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; zero point stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(zero) THEN BEGIN
      
      ;; add zeropoints from tsField file
      zpstr={zeropoint:fltarr(5)}
      
      IF n_elements(ex_struct) EQ 0 THEN BEGIN 
          ex_struct=zpstr
      ENDIF ELSE BEGIN  
          IF NOT tag_exist(ex_struct,'zeropoint') THEN BEGIN 
              ex_struct=create_struct(ex_struct,zpstr)
          ENDIF 
      ENDELSE 
  ENDIF 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; some input parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; did user input the run/rerun/camcol or just the directory?
  dir_type = size(dir_in,/tname)
  IF (dir_type EQ 'INT') OR (dir_type EQ 'LON') THEN BEGIN 
      IF n_elements(dir_in) EQ 3 THEN BEGIN 
          ;; User input run/rerun/camcol
          run_in = dir_in[0]
          rerun_in = dir_in[1]
          camcol_in = dir_in[2]
          fetch_dir, run_in, camcol_in, rerun_in, dir, corrdir=corrdir
          IF keyword_set(corrected) THEN BEGIN
              front='adatc'
              dir=corrdir
          ENDIF 
      ENDIF ELSE BEGIN 
          ;; bad input
          message,'Format of input dir/[run,rerun,camcol] is incorrect'
      ENDELSE 
  ENDIF ELSE IF dir_type EQ 'STR' THEN BEGIN 
      ;; User input the directory
      dir = dir_in[0]
      IF keyword_set(corrected) THEN BEGIN 
          front='adatc'
      ENDIF 
  ENDIF ELSE BEGIN 
      message,'First input directory/[run,rerun,camcol] must be either '+$
        'a scalar string or a int/long array of 3 numbers'
  ENDELSE 

  IF n_elements(verbose) EQ 0 THEN verbose=2
  IF NOT keyword_set(nomodrow) THEN nomodrow=0
  IF n_elements(nframes) EQ 0 THEN nframes=1
  IF (NOT keyword_set(noadd) ) THEN noadd=0 ELSE noadd=1

  ;; optional input "last" will take precedence over nframes later
  ;; /all takes precedence over all else
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;See if the user has input the info structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; if makall then create tsobjstr
  ;; if findinfo, then run fetch_file_list 
  makeall = 1
  findinfo=1
  IF n_elements(tsobjstr) NE 0 THEN BEGIN 
      IF size(tsobjstr,/tname) EQ 'STRUCT' THEN BEGIN 

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; check if same directory. Want to avoid running
          ;; fetch_file_list if possible since its rather slow, 
          ;; but if directories are different, we will have to
          ;; update the info in tsobjstr.  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          IF (dir EQ tsobjstr.dir) THEN BEGIN
              makeall = 0
              findinfo = 0
          ENDIF 
      ENDIF 
  ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; See if we should use fetch_file_list or not. If dir is same 
; exact directory as in tsobjstr, then there is no point
; in using this slow program. This makes a difference when
; reading one frame at a time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF findinfo THEN BEGIN 

      IF keyword_set(all) THEN BEGIN
          fetch_file_list, dir, files, fnums, $
            fieldmin=fieldmin, fieldmax=fieldmax, nfields=nfields, $
            run=run, camcol=camcol, rerun=rerun, $
            front=front, allfiles=allfiles, allfnums=allfnums
          startf = fieldmin
      ENDIF ELSE BEGIN 
          IF n_elements(start) NE 0 THEN startf=start
          fetch_file_list, dir, files, fnums, $
            start=startf, nframes=nframes, last=last, $
            fieldmin=fieldmin, fieldmax=fieldmax, nfields=nfields, $
            run=run, camcol=camcol, rerun=rerun, $
            front=front, allfiles=allfiles, allfnums=allfnums
      ENDELSE 

      IF nfields EQ 0 THEN return

      startf = long(min(fnums))
      lastf = long(max(fnums))

  ENDIF ELSE BEGIN

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Get files from tsobjstr info
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF n_elements(start) NE 0 THEN startf=start $
      ELSE startf = tsobjstr.fieldmin

      IF n_elements(last) NE 0 THEN lastf=last $
      ELSE lastf = nframes + startf - 1 
      
      ;; /all overrides all other range specifications
      IF keyword_set(all) THEN BEGIN 
          nfields = n_elements(tsobjstr.fnums)
          wff = lindgen(nfields)
          startf = tsobjstr.fnums[0]
          lastf = tsobjstr.fnums[nfields-1]
      ENDIF ELSE BEGIN 
          wff=where(tsobjstr.fnums GE startf AND $
                    tsobjstr.fnums LE lastf,nfields)
      ENDELSE 
      IF nfields NE 0 THEN BEGIN 
          files = tsobjstr.files[wff]
          fnums = tsobjstr.fnums[wff]
          startf = min(fnums)
          lastf = long(max(fnums))
      ENDIF ELSE BEGIN
          message,'No fields found in range: ['+$
                  ntostr(startf)+', '+ntostr(lastf)+']',/inf
          return
      ENDELSE 

  ENDELSE 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If needed, find info to create new struct
; If not makeall, then get the info from tsobjstr (saves 
; much time when reading one field at a time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF makeall THEN BEGIN 
      
      openr, unit, files(0), /get_lun, /block, ERROR = error
      IF ERROR NE 0 THEN BEGIN 
          print,!ERR_STRING
          free_lun,unit
          return
      ENDIF 
      
      lnew = mrdfits3(unit,1,0,hhdr,/silent)
      ;;lnew = mrdfits(unit,1,hhdr,/silent, mrdfits_struct=mrdfs)
      free_lun,unit
      
      IF (size(lnew))(0) EQ 0 THEN BEGIN 
          ;; do ones best to find a non-empty file to make the structure
          message,files(0)+'  IS AN EMPTY FILE',/inf
          numbfiles=n_elements(files)
          findex=1
          WHILE ( (numbfiles GT 1) AND $
                  (findex LE numbfiles-1) AND $
                  ((size(lnew))(0) EQ 0) $
                ) DO BEGIN 		 
              message,'TRYING NEXT ONE',/inf
              openr, unit, files(findex), /get_lun, /block, ERROR = error
              IF ERROR NE 0 THEN BEGIN 
                  print,!ERR_STRING
                  free_lun,unit
                  return
              ENDIF
              lnew = mrdfits3(unit,1,0,hhdr,/silent)
              ;;lnew = mrdfits(unit,1,hhdr,/silent, mrdfits_struct=mrdfs)
              free_lun,unit
              findex=findex+1	
          ENDWHILE 
          IF (size(lnew))(0) EQ 0 THEN BEGIN 
              message,"NONE OF THESE FILES ARE NON-EMPTY FILES",/inf
              return 
          ENDIF 
      ENDIF 
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; figure out which tags to get from file
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      phototags = tag_names(lnew)
      IF n_elements(taglist) NE 0 THEN BEGIN 
          w1='Using input taglist.'
          tags = taglist
      ENDIF ELSE IF keyword_set(default_tags) THEN BEGIN 
          make_default_tags, tags
          w1='Using default taglist.'
      ENDIF ELSE BEGIN 
          w1='Using all phototags'
          tags = phototags
      ENDELSE 
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Print out a message
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF verbose GT 0 THEN BEGIN 
          runstr=strtrim(string(run), 2)
          colstr=strtrim(string(camcol),2)
          rerstr=strtrim(string(rerun),2)
          sstartf = ntostr(startf)
          slastf = ntostr(lastf)
          print
          print,'*****************************************'
          print,'* Run: ',runstr,'  Camcol: ',colstr,'  Rerun: ',rerstr
          print,'* Reading fields ',sstartf,'-',slastf
          print,'* ',w1
          print,'*****************************************'
          print
      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; build up good taglist and structure
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      tags = strupcase(tags)
      read_tsobj_make_tags, tags, phototags, goodtags, indices, ntags, $
                            verbose=verbose
      IF ntags EQ 0 THEN BEGIN 
          message,'No good tags found',/inf
          return
      ENDIF 
      read_tsobj_make_struct, indices, lnew, substruct
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Add extra tags to the structure if ex_struct exists
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF n_elements(ex_struct) GT 0 THEN BEGIN
          substruct=create_struct(substruct,ex_struct)
      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; should we add seeing from header?
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF keyword_set(add_seeing) THEN BEGIN 
          ;; add seeing if its not already there   
          IF verbose GT 0 THEN BEGIN
              print,'Adding Seeing'
              print
          ENDIF 
          IF NOT tag_exist(substruct, "SEEING") THEN BEGIN
              substruct=create_struct(substruct, "SEEING", fltarr(5))
          ENDIF 
      ENDIF

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; should we add the sky from the header?
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF keyword_set(add_sky) THEN BEGIN 
          ;; add seeing if its not already there   
          IF verbose GT 0 THEN BEGIN
              print,'Adding Sky'
              print
          ENDIF 
          IF NOT tag_exist(substruct, "SKY") THEN BEGIN
              substruct=create_struct(substruct, "SKY", fltarr(5))
          ENDIF 
      ENDIF

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; define info structure, tsobjstr
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      tsobjstr = $
        define_tsobjstr(dir, fieldmin, fieldmax, run, rerun, camcol, $
                        allfiles, allfnums, goodtags, indices, substruct)
      

  ENDIF ELSE BEGIN

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; get the info we need from tsobjstr
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; may need to update some info
      IF findinfo THEN BEGIN 
          tsnew = $
            define_tsobjstr(dir, fieldmin, fieldmax, run, rerun, camcol, $
                            allfiles, allfnums, $
                            tsobjstr.goodtags, tsobjstr.indices, $
                            tsobjstr.substruct)
          tsobjstr=0
          tsobjstr=tsnew
      ENDIF ELSE BEGIN 
          run = tsobjstr.run
          rerun = tsobjstr.rerun
          camcol = tsobjstr.camcol
          fieldmin = tsobjstr.fieldmin
          fieldmax = tsobjstr.fieldmax
          ;; now get files, fnums
      ENDELSE 

      goodtags = tsobjstr.goodtags
      indices  = tsobjstr.indices
      substruct = tsobjstr.substruct
      ntags = n_elements(goodtags)

  ENDELSE 
  	
  IF tag_exist(substruct, "SEEING") THEN anyseeing=1 ELSE anyseeing=0
  IF tag_exist(substruct, "SKY") THEN anysky=1 ELSE anysky=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Now loop through and read the desired fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  e=ntostr(lastf)               ; string for printing

  numlist=lonarr(nfields)
  ptrlist=ptrarr(nfields)       ; the array of pointers
  ntotal=0lL

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; read in fields
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; can use these to select non-overlap objects when it is
  ;; finally set by the pipeline
  ;; make_status_struct, ss
  ;; ss.ok_run = 'Y'

  FOR i=0L, nfields-1 DO BEGIN 
      
      ;; open the file
      openr, unit, files[i], /get_lun, /block, ERROR = error
      IF ERROR NE 0 THEN BEGIN 
          print,!ERR_STRING
          free_lun,unit
          return
      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; read from file
      ;; /deja_vu: it can use structure it made above in
      ;; first call to mrdfits3
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      lnew = mrdfits3(unit,1,0,hdr,/silent,/deja_vu)
      ;;lnew = mrdfits(unit,1,hhdr,/silent, mrdfits_struct=mrdfs)
      free_lun,unit
          
      IF (size(lnew))(0) GT 0 THEN BEGIN 
          field = fnums[i]

          ;; any wstring cuts to make?
          IF n_elements(wstring) NE 0 THEN BEGIN 
              cuts="cut="+wstring
              IF NOT execute(cuts) THEN message,'wstring is invalid'
              IF cut[0] NE -1 THEN ng = n_elements(cut) $
              ELSE ng=0 
          ENDIF ELSE BEGIN
              ng = n_elements(lnew)
              cut = lindgen(ng)
          ENDELSE 

          IF ng NE 0 THEN BEGIN 

              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; Due to overlap of consecutive images, throw out 
              ;; first 64 and last 64 rows unless first frame (i eq fieldmin)
              ;; or last frame (i eq fieldmax) in which case one only 
              ;; throws out last or first 64 respectively.
              ;; Can use nomodrow if reading files where this has already
              ;; been done!
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
              IF NOT nomodrow THEN BEGIN 
                  ;;status_select, lnew, ss, g, input_index=cut
                  ;;IF g[0] EQ -1 THEN ng=0 ELSE ng=n_elements(g)
                  g=where(lnew[cut].objc_rowc GT (field GT fieldmin)*64 AND $
                          lnew[cut].objc_rowc LT (1489 - (field LT fieldmax)*64),ng)
                  IF ng NE 0 THEN g = cut[g]
              ENDIF ELSE BEGIN 
                  g=cut
              ENDELSE 

              ;; Anything pass the row cuts?
              IF ng NE 0 THEN BEGIN 
                                    
                  IF keyword_set(addrowc) THEN BEGIN
                      lnew.objc_rowc = lnew.objc_rowc + 1361L*field
                  ENDIF 

                  ;; print out messages for this field?
                  IF verbose GT 1 AND (  ((i MOD 20)  EQ 0 ) OR $
                                         (i EQ nfields-1) OR (i EQ 0) ) THEN BEGIN 
                      f=ntostr(field)
                      o=ntostr(ng)
                      print,'Field: ',f,'/',e,'  Objects: ',o
                  ENDIF 
                  
                  ;; the new array of structures
                  ln=replicate(substruct,ng)
                  
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; copy the tags that matched
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  IF goodtags[0] NE '' THEN BEGIN 
                      FOR k=0L, ntags-1 DO ln.(k) = lnew[g].(indices[k])
                  ENDIF 
                  
                  ;; Copy in seeing from header?
                  IF keyword_set(add_seeing) AND anyseeing THEN BEGIN 
                      ln.seeing[0]=fxpar(hdr,"SEEING_U")
                      ln.seeing[1]=fxpar(hdr,"SEEING_G")
                      ln.seeing[2]=fxpar(hdr,"SEEING_R")
                      ln.seeing[3]=fxpar(hdr,"SEEING_I")
                      ln.seeing[4]=fxpar(hdr,"SEEING_Z")
                  ENDIF 
                  
                  ;; sky?
                  IF keyword_set(add_sky) AND anysky THEN BEGIN 
                      ln.sky[0]=fxpar(hdr,"SKY_U")
                      ln.sky[1]=fxpar(hdr,"SKY_G")
                      ln.sky[2]=fxpar(hdr,"SKY_R")
                      ln.sky[3]=fxpar(hdr,"SKY_I")
                      ln.sky[4]=fxpar(hdr,"SKY_Z")
                  ENDIF 

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; add zero points from tsField
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  if keyword_set(zero) then begin
                      ;; add zeropoints
                      fetch_dir,run,camcol,rerun,jdir,jatldir,tsobjfile,$
                                field=field  
                      tspos=strpos(tsobjfile,'tsObj')	   
                      tsfieldf=strmid(tsobjfile,0,tspos)+'tsField'+$
                        strmid(tsobjfile,tspos+5,1000)
                      
                      if fexist(tsfieldf) then begin
                          tsfield=mrdfits(tsfieldf,1,hdr,/silent)
                          za=tsfield.aa
                          zb=tsfield.bb
                          zc=tsfield.cc
                          zk=tsfield.kk
                          airmass=tsfield.airmass
                          UGcolor=ln.counts_model(0)-ln.counts_model(1)
                          IZcolor=ln.counts_model(4)-ln.counts_model(4)
                          compute_zeropoint,za,zb,zc,zk,airmass,$
                                            UGcolor,IZcolor,zeropoint
                          ln.zeropoint=zeropoint
                      endif else begin
                          print,'tsfield file ',tsfieldf,' does not exist'
                      endelse  
                  endif        
                  

                  ;; pointer array and counts
                  numlist[i]=ng
                  ntotal=ntotal+ng
                  ptrlist[i]=ptr_new(ln, /no_copy)
                  
              ENDIF ELSE message,'Field: '+ntostr(field)+$
                '  No objects in field in rows 65-1424',/inf
          ENDIF ELSE message,'Field: '+ntostr(field)+$
            '  No objects passed wstring cuts',/inf
      ENDIF ELSE message,files[i]+' is an empty file',/inf
      
  ENDFOR 

  ;; did we find anything?
  IF ntotal EQ 0 THEN BEGIN
      message,'No objects found',/inf
      return
  ENDIF 

  IF verbose GT 0 THEN BEGIN 
      o=strtrim(string(ntotal), 2)
      print,'Total Number of Objects: ',o
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; combine field pointers into either one big array of
  ;; structures or an array of object pointers
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(pointers) THEN BEGIN 
      
      struct = ptrarr(ntotal)
      beg=0L
      FOR i=0L,nfields-1 DO BEGIN 
          IF numlist[i] NE 0 THEN BEGIN 
              tmpstruct = *ptrlist[i]
              FOR j=0L, numlist[i]-1 DO BEGIN 
                  struct[beg+j] = ptr_new(tmpstruct[j])
              ENDFOR 
              tmpstruct = 0
              ptr_free, ptrlist[i]
              beg = beg+numlist[i]
          ENDIF
      ENDFOR 
      
  ENDIF ELSE BEGIN 
      
      ;; create output structure
      struct=replicate(substruct,ntotal)
      
      ;; now make big list from the array of pointers ptrlist
      beg=0L
      FOR i=0L,nfields-1 DO BEGIN 
          IF numlist[i] NE 0 THEN BEGIN            
              struct[beg:beg+numlist[i]-1]=*ptrlist[i]
              ptr_free,ptrlist[i] ;kill the heap variables associated 
                                ;with the pointer
              beg=beg+numlist[i]
          ENDIF       
      ENDFOR 

  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; do the spectro matching
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if keyword_set(addspec) then begin
      if verbose gt 0 then print,'doing spectro match'
      match_radec_spec,struct.ra,struct.dec,spec_cat,m1,m2
      if m1[0] NE -1 then begin
          if tag_exist(spec_struct,'PLATE') then $
            struct(m1).plate=spec_cat(m2).plate
          if tag_exist(spec_struct,'MJD') then $
            struct(m1).mjd=spec_cat(m2).mjd
          if tag_exist(spec_struct,'FIBER') then $
            struct(m1).fiber=spec_cat(m2).fiber
          if tag_exist(spec_struct,'Z1D') then $
            struct(m1).z1d=spec_cat(m2).z
          if tag_exist(spec_struct,'Z1DERR') then $
            struct(m1).z1derr=spec_cat(m2).zerr
          if tag_exist(spec_struct,'ZSTAT') then $
            struct(m1).zstat=spec_cat(m2).zstat
          if tag_exist(spec_struct,'SPEC_CLN') then $
            struct(m1).spec_cln=spec_cat(m2).spec_cln
      endif
  endif	
  
  IF verbose GT 1 THEN ptime,systime(1)-time
  status=0

  return
END 

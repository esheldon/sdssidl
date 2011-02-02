;+
; NAME:
;  sdss_file()
;
; PURPOSE:
;  Return an SDSS file name for the input filetype, and id information.
;
; CATEGORY:
;  SDSS specific
;
; CALLING SEQUENCE:
;   file=sdss_file(type, run, camcol, rerun=, bandpass=, fields=, 
;                  dir=, /nodir, /stuffdb, exists=)
;  
;
; INPUTS:
;   type: file type.  For a list of types do
;     print,!sdss->supported_filetypes()
;   run: The sdss run.
;   camcol: The camcol.  This is optional for some file types.
;
; OPTIONAL INPUTS:
;   rerun: SDSS rerun.  If not sent, the run_status structure is searched
;       for the run and the latest rerun is returned.
;   bandpass: The bandpass in numerical of string form where
;       u,g,r,i,z -> 0,1,2,3,4
;   fields: The fields to read. Defaults to a pattern with '*' for the 
;       field number.
;   /nodir: Do not prepend the directory.
;   /stuffdb:  Filenames for db stuffing.
;
; OUTPUTS:
;   The file name.
;
; OPTIONAL OUTPUTS:
;   dir: The directory.
;
; RESTRICTIONS:
;   If rerun is not sent, the run must be defined in the run status structure.
;
; MODIFICATION HISTORY:
;   Created Erin Sheldon, UChicago 
;
;-

function sdss_file, type, run, camcol, rerun=rerun, bandpass=bandpass, fields=fields, dir=dir, nodir=nodir, _extra=_extra

    sdssidl_setup
    file = !sdss->file(type, run, camcol, rerun=rerun, bandpass=bandpass, fields=fields, dir=dir, nodir=nodir, _extra=_extra)
    return, file

end

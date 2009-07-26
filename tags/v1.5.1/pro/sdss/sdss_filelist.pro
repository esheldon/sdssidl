;+
; NAME:
;   sdss_filelist()
;
; PURPOSE:
;   Return a list of files for the input filetype and SDSS id info.
;
; CATEGORY:
;   SDSS specifie.
;
; CALLING SEQUENCE:
;   sdss_filelist(filetype, run, camcol, rerun=, bandpass=, fields=, 
;                 fnums=, dir=, /silent, status=)
;
; INPUTS:
;   filetype: file type.  For a list of types do
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
;   /silent:
;
; OUTPUTS:
;   The file name.
;
; OPTIONAL OUTPUTS:
;   dir: The directory.
;   exists: 1 for yes, 0 for no
;
; RESTRICTIONS:
;   If rerun is not sent, the run must be defined in the run status structure.
;
; MODIFICATION HISTORY:
;   Created Erin Sheldon, UChicago 

function sdss_filelist, filetype, run, camcol, rerun=rerun, bandpass=bandpass, fields=fields, fnums=fnums, indir=indir, dir=dir, silent=silent, status=status

    sdssidl_setup
    return, !sdss->filelist(filetype, run, camcol, rerun=rerun, bandpass=bandpass, fields=fields, fnums=fnums, indir=indir, dir=dir, silent=silent, status=status)

end


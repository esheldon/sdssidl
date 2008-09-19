;+
; NAME:
;  READ_SPEC1D
;
;
; PURPOSE: 
;  Read spectra data for the input plateid and fiberid.  The mjd is optional; 
;  if not entered, the latest mjd is used.
;
; CATEGORY:
;  SDSS specific routine
;
;
; CALLING SEQUENCE:
;  read_spec1d, plateid, fiberid, sp_struct, $
;               mjd=mjd, $
;               spec_vers=spec_vers, $
;               file=file, $
;               $
;               /no_substructs, $
;               silent=silent, $
;               $
;               z=z, err_z=err_z,$
;               $
;               plot=plot, $   ;Plotting stuff
;               /nolegend, $'
;               /nolines, $
;               nsmooth=nsmooth, $
;               errclr=errclr, $
;               leg_charsize=leg_charsize, $
;               label_charsize=label_charsize
;               _extra=_extra
;
;
; INPUTS:
;  plateid: the plate number
;  fiberid: fiber number
;
; OPTIONAL INPUTS:
;  mjd: The mjd; if not input, the latest mjd is used.
;  spec_vers: a string containing the spectro rerun id.  e.g. '1d_23'.  The
;             default is to read this from the SPEC_VERS config variable.
;  file: the file to read. Otherwise, it is created from the inputs.
;
; KEYWORD PARAMETERS:
;  /no_substructs: Don't add the structures found in the other HDU's as nested
;                  structures to sp_struct. This is useful if the user wants to
;                  save the structure in a fits file with MWRFITS.PRO, which
;                  does not allow nested structures. These structures can be
;                  returned individually; see below.
;
;  /silent: don't print out informative messages
;  /plot: make a plot of the spectra. Calls plot_spec1d.pro
;  ** inputs if /plot is set: nsmooth=nsmooth: boxcar smooth over this many
;                               adjacent pixels
;                             errclr=errclr:  color for the error curve.
;                               Default is green in X,Z and blue in PS
;                             /nolegend: don't plot the legend, with the type,
;                                        redshift, and ra/dec 
;                             /nolines: don't plot and label the lines
;                             leg_charsize=leg_charsize: the charsize for the
;                                               legend
;                             label_charsize=label_charsize: the charsize for
;                                               the line labels.
;                             _extra=_extra:  Plotting keywords
;
; OPTIONAL OUTPUTS:
;  sp_struct: all elements of the header are placed in a structure using the
;             hdr2struct procedure.  This includes redshift, classification,
;             etc. 
;  z=z: the redshift.  This is also contained in the sp_struct.
;  err_z=err_z: The uncertainty in redshift.
;
;  These are the binary tables from higher HDU's.  They can be returned
;    invidually with these keywords.  They are also added to the returned
;    sp_struct unless /no_substructs is set.
;        lines_auto: all lines found automatically by the wavelet filter, with
;              no apriori knowledge of the redshift.
;        lines_redshift: all lines measured based on their expected positions
;                        from the highest-confidence emission line redshift
;        em_z: Redshift determinations based on sets of emission lines. There
;              may be multiple entries, with each entry corresponding to a
;              different set of lines. 
;        cc_z: Redshift determinations from cross-correlations. More than one
;              peak may be measured in each cross-correlation function (i.e.,
;              for each template).
;        line_indices: Standard line indices and flux ratios are stored
;              here.  Lick indices, Brodie & Hanes, and Diaz, Terlevich, &
;              Terlevich are measured.
;        mask_res: The masks as passed from spPlate*.fit (the mask bit values
;              are a subset of those used in the mask array above), as well as
;              the resolution. The length of this table should be the same as
;              the length of the spectrum
;        *** See: http://www-sdss.fnal.gov:8000/edoc/dm/flatFiles/spSpec.html
;                 for more info 
; RESTRICTIONS:
;  You need the Goddard idl astronomy procedures and the Umich SDSS idl
;  libraries.  You need the SPEC_DIR Config variable set.
;
;
; EXAMPLES:
;  read_spec1d, 550, 125, sp_struct
;  read_spec1d,550,125,sp_struct,/plot,nsmooth=10
;
;
; MODIFICATION HISTORY:
;   Creation: 15-Aug-2003: Erin Sheldon UofChicago  Based roughly on Dave
;      Johnston's read_spec.pro
;
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


PRO read_spec1d, plateid, fiberid, sp_struct, $
                 mjd=mjd, $
                 spec_vers=spec_vers, $
                 file=file, $
                 $
                 no_substructs=no_substructs, $
                 silent=silent, $
                 $
                 z=z, err_z=err_z,$
                 lines_auto=lines_auto, $
                 lines_redshift=lines_redshift, $
                 em_z=em_z,$
                 cc_z=cc_z,$
                 line_indices=line_indices,$
                 mask_res=mask_res,$
                 $
                 plot=plot, $   ;Plotting stuff
                 nolegend=nolegend, $
                 nolines=nolines, $
                 nsmooth=nsmooth, $
                 yrange=yrange, xrange=xrange, $
                 xstyle=xstyle, ystyle=ystyle, $
                 position=position, xminor=xminor, thick=thick,$
                 charsize=charsize, $
                 leg_charsize=leg_charsize, $
                 label_charsize=label_charsize, $
                 errclr=errclr, $
                 _extra=_extra

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: read_spec1d, plateid, fiberid [, sp_struct, $'
      print,'    mjd=mjd, $'
      print,'    spec_vers=spec_vers, $'
      print,'    file=file, $'
      print,'    $'
      print,'    /no_substructs, $'
      print,'    /silent, $'
      print,'    $'
      print,'    z=z, err_z=err_z,$'
      print,'    lines_auto=lines_auto, $'
      print,'    lines_redshift=lines_redshift, $'
      print,'    em_z=em_z,$'
      print,'    cc_z=cc_z,$'
      print,'    line_indices=line_indices,$'
      print,'    mask_res=mask_res,$'
      print,'    $'
      print,'    /plot, $              ;Plotting stuff'
      print,'    /nolegend, $'
      print,'    /nolines,  $'
      print,'    nsmooth=nsmooth, $'
      print,'    errclr=errclr, $'
      print,'    leg_charsize=leg_charsize, $'
      print,'    label_charsize=label_charsize, $'
      print,'    _extra=_extra]'
      delvarx,sp_struct
      return
  ENDIF 

  On_error,2

  pstr = strn(plateid,length=4,padch='0')
  fstr = strn(fiberid,length=3,padch='0')

  IF n_elements(spec_vers) EQ 0 THEN BEGIN 
      spec_vers = sdssidl_config('SPEC_VERS', exists=v_exists)
      IF NOT v_exists THEN BEGIN 
          message,'SDSSIDL Config variable SPEC_VERS must be defined'
      ENDIF 
  ENDIF 

  sdss_spec_dir = sdssidl_config('SPEC_DIR', exists=d_exists)
  IF NOT d_exists THEN BEGIN 
      message,'SDSSIDL Config variable SPEC_DIR must be defined'
  ENDIF 

  dir=SDSS_SPEC_DIR+spec_vers+'/'+pstr+'/1d/'

  ;; took this from Dave's code basically
  IF n_elements(file) EQ 0 THEN BEGIN 

      IF n_elements(mjd) EQ 0 THEN BEGIN  
          files = findfile(dir + 'spSpec-?????-'+pstr+'-'+fstr+'.fit',$
                           count=count)
          IF count EQ 0 THEN BEGIN 
              IF NOT keyword_set(silent) THEN BEGIN
                  print,'plate: '+pstr+' fiber: '+fstr+' does not exist'
              ENDIF 
              delvarx,sp_struct
              return
          ENDIF 
          
          ;; get the latest
          file = files[count-1]
          
          tmp = str_sep(file, '-')
          mjdstr = tmp[1]
          mjd = long(mjdstr)
          ;;IF NOT keyword_set(silent) THEN print,'Using mjd: ',mjdstr
      ENDIF ELSE BEGIN 
          mjdstr = strn(mjd, length=5, padch='0')
          file = dir + 'spSpec-'+mjdstr+'-'+pstr+'-'+fstr+'.fit'
      ENDELSE 
  ENDIF 

  IF NOT fexist(file) THEN BEGIN
      IF NOT keyword_set(silent) THEN print,'file ',file,' does not exist'
      delvarx,sp_struct
      return
  ENDIF 

  s0=mrdfits(file,0,hdr0,/silent)
  siz=size(s0)
  npix=siz(1)

  ;; fix bad names
  hdr0 = repstr(hdr0, 'DATE-OBS', 'DATE_OBS')
  hdr0 = repstr(hdr0, 'TAI-BEG',  'TAI_BEG')
  hdr0 = repstr(hdr0, 'TAI-END',  'TAI_END')
  hdr0 = repstr(hdr0, 'DC-FLAG',  'DC_FLAG')
  hdr0 = repstr(hdr0, '1D_PDATE', 'ONED_PDATE')

  hdr_struct = hdr2struct(hdr0)
  c0 = hdr_struct.coeff0
  c1 = hdr_struct.coeff1

  z = hdr_struct.z
  err_z = hdr_struct.z_err

  lambda = 10.0^(c0+c1*findgen(npix))

  spec      = s0(*,0)           ; spectra
  spec_cs   = s0(*,1)           ; continuum subtracted
  spec_err  = s0(*,2)           ; the noise
  spec_mask = s0(*,3)           ; the mask

  sp_struct = create_struct(hdr_struct, $
                            'hdr0', hdr0,$
                            'lambda', lambda, $
                            'spec', spec,$
                            'spec_cs', spec_cs,$
                            'spec_err', spec_err,$
                            'spec_mask', spec_mask)
  
  IF arg_present(lines_auto)     THEN lines_auto     = mrdfits(file,1,/silent)
  IF arg_present(lines_redshift) THEN lines_redshift = mrdfits(file,2,/silent)
  IF arg_present(em_z)           THEN em_z           = mrdfits(file,3,/silent)
  IF arg_present(cc_z)           THEN cc_z           = mrdfits(file,4,/silent)
  IF arg_present(line_indices)   THEN line_indices   = mrdfits(file,5,/silent)
  IF arg_present(mask_res)       THEN mask_res       = mrdfits(file,6,/silent)

  IF NOT keyword_set(no_substructs) THEN BEGIN 
      sp_struct = create_struct(temporary(sp_struct), $
                                'lines_auto',     mrdfits(file,1,/silent),$
                                'lines_redshift', mrdfits(file,2,/silent),$
                                'em_z',           mrdfits(file,3,/silent),$
                                'cc_z',           mrdfits(file,4,/silent),$
                                'line_indices',   mrdfits(file,5,/silent),$
                                'mask_res',       mrdfits(file,6,/silent) )
  ENDIF 

  IF keyword_set(plot) THEN BEGIN  
      plot_spec1d, sp_struct, $
                   nolegend=nolegend, $
                   nolines=nolines, $
                   nsmooth=nsmooth, $
                   yrange=yrange, xrange=xrange, $
                   xstyle=xstyle, ystyle=ystyle, $
                   position=position, xminor=xminor, thick=thick,$
                   charsize=charsize, $
                   leg_charsize=leg_charsize, $
                   label_charsize=label_charsize, $
                   errclr=errclr, $
                   _extra=_extra
  ENDIF 
  
  return

END 

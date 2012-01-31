;+
; NAME:
;  PLOT_SPEC1D
;
;
; PURPOSE:
;  Plot an SDSS spectra.  The redshift, spectral classification, spectro id,
;  and photo object id are also placed on the plot. Lines are labeled if that
;  information is available.
;
; CATEGORY:
;  SDSS specific routine
;
;
; CALLING SEQUENCE:
;    By id: '
;       -Syntax: plot_spec1d, plateid, fiberid, $'
;            /noprompt, $'
;            /nolegend, $'
;            /nolines, $
;            mjd=mjd, spec_vers=spec_vers, nsmooth=nsmooth, $'
;            errclr=errclr, $'
;            leg_charsize=leg_charsize, $
;            label_charsize=label_charsize
;            _extra=_extra'
;    From structure: '
;       -Syntax: plot_spec1d, struct, $'
;            /nolegend, $'
;            /nolines, $
;            mjd=mjd, nsmooth=nsmooth, $'
;            errclr=errclr, $'
;            leg_charsize=leg_charsize, $
;            label_charsize=label_charsize
;            _extra=_extra'
;
;
; INPUTS:
;   Either 
;      *plateid, fiberid: identifier for SDSS plate number and the fiber number
;                         of the object in the plate. These can be arrays.
;   Or
;      *struct: The spectra structure for this object.
;   PLOT_SPEC1D will figure out what the user wants based on the inputs.
;
; KEYWORD PARAMETERS:
;   /noprompt: if plateid and fiberid sent as arrays, then the user will be
;              prompted to move to the next/previous object in 'X' device. This
;              behaviour can be turned off with /noprompt.
;   /nolegend: don't plot the legend, with the type, redshift, and ra/dec 
;   /nolines: don't plot and label the lines
;
; OPTIONAL INPUTS:
;   mjd=mjd: if plateid/fiberid are entered, then PLOT_SPEC1D will find the
;            latest mjd unless this input is given.  Must be the same size as
;            the plateid and fiberid inputs.
;   spec_vers: a string containing the spectro rerun id.  e.g. '1d_23'.  The
;              default is to read this from the SPEC_VERS config variable.
;   nsmooth=nsmooth: boxcar smooth over this many adjacent pixels
;   errclr=errclr: color for the error curve. Default is green in X,Z and
;                  blue in PS
;   nsmooth=nsmooth: Number of neighbors to use in a boxcar average smoothing.
;   errclr=errclr: color for the noise curve.
;   leg_charsize=leg_charsize: the charsize for the legend
;   label_charsize=label_charsize: the charsize for the line labels.
;
;   _extra=_extra: plotting keywords
;
; OUTPUTS:
;   A plot is made on the screen.
;
; RESTRICTIONS:
;  You need the Goddard idl astronomy procedures and the Umich SDSS idl
;  libraries.  You need the SPEC_DIR variable set in the configuration file.
;
;
; EXAMPLE:
;  plot_spec1d,550,125,nsmooth=10
;  plates=[550,655]
;  fibers=[125,36]
;  plot_spec1d,plates,fibers,nsmooth=10
;
;
; MODIFICATION HISTORY:
;  Creation: 15-Aug-2003: Erin Sheldon UofChicago
;  Renamed to view_spec1d, put this in obsolete
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


PRO plot_spec1d, plateid, fiberid, $
                 noprompt=noprompt, $
                 nolegend=nolegend, $
                 nolines=nolines, $
                 mjd=mjd, spec_vers=spec_vers, nsmooth=nsmooth, $
                 yrange=yrange, xrange=xrange, $
                 xstyle=xstyle, ystyle=ystyle, $
                 position=position, xminor=xminor, thick=thick,$
                 charsize=charsize, $
                 leg_charsize=leg_charsize, $
                 label_charsize=label_charsize, $
                 errclr=errclr, $
                 _extra=_extra

    view_spec1d, plateid, fiberid, $
                 noprompt=noprompt, $
                 nolegend=nolegend, $
                 nolines=nolines, $
                 mjd=mjd, spec_vers=spec_vers, nsmooth=nsmooth, $
                 yrange=yrange, xrange=xrange, $
                 xstyle=xstyle, ystyle=ystyle, $
                 position=position, xminor=xminor, thick=thick,$
                 charsize=charsize, $
                 leg_charsize=leg_charsize, $
                 label_charsize=label_charsize, $
                 errclr=errclr, $
                 _extra=_extra
 
END 

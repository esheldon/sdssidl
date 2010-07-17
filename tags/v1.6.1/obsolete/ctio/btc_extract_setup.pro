pro btc_extract_setup, param_struct
;+
; NAME:
;       BTC_EXTRACT_SETUP
; PURPOSE:
;	Set up a parameter structure for sextractor operation on btc data
;
; CALLING SEQUENCE:
;       btc_extract_setup, param_struct
;
; INPUTS:
;       
; OUTPUTS:
;	param_struct: a structure containing all the parameters needed
;		      to run sextractor
;
; OPTIONAL OUTPUT ARRAYS:
;
; INPUT KEYWORD PARAMETERS:
; 
; PROCEDURE: Prepares a param_struct for BTC sextractor operation
;	
;
; REVISION HISTORY:
;	Tim McKay	UM	1/8/98
;	Tim McKay	UM	3/7/98  
;	Tim McKay	UM 	1/14/99
;-
 On_error,2              ;Return to caller

 if N_params() ne 1 then begin
        print,'Syntax - btc_extract_setup, param_struct
        return
 endif
 
 config_dir=getenv('EXTRACT_CONFIG')
 if (config_dir eq "") then begin
	config_dir='/home/products/sextractor2.0.15/config'
 endif
 par_dir=getenv('EXTRACT_PAR')
 if (par_dir eq "") then begin
	par_dir='/sdss3/products/idltools/sdss_idl/ctio'
 endif

 param_struct = { $
	CATALOG_NAME: 'test.fts', $
	CATALOG_TYPE: 'FITS_1.0', $
	DETECT_TYPE: "CCD", $
	FLAG_IMAGE: 'flag.fits', $
	DETECT_MINAREA: '5', $
	DETECT_THRESH: '1.0', $
	ANALYSIS_THRESH: '1.2', $
	FILTER: 'Y', $
	FILTER_NAME: config_dir+"/gauss_1.5_3x3.conv", $
	DEBLEND_NTHRESH: '32', $
	DEBLEND_MINCONT: '0.00001', $
	CLEAN: 'N', $
	CLEAN_PARAM: '1.0', $
	MASK_TYPE: 'NONE', $
	PHOT_APERTURES: '5', $
	PHOT_AUTOPARAMS: '2.5,3.5', $
	PARAMETERS_NAME: par_dir+"/btc.par", $
	SATUR_LEVEL: '15000.0', $
	MAG_ZEROPOINT: '20.0', $
	MAG_GAMMA: '4.0', $
	GAIN: '2.5', $
	PIXEL_SCALE: '0.4', $
	SEEING_FWHM: '1.0', $
	STARNNW_NAME: config_dir+'/default.nnw',$
	BACK_SIZE: '32', $
	BACK_FILTERSIZE: '3', $
	BACKPHOTO_TYPE: 'GLOBAL', $
	CHECKIMAGE_TYPE: 'MINIBACKGROUND', $
	CHECKIMAGE_NAME: 'check.fits', $
	MEMORY_OBJSTACK: '30000', $
	MEMORY_PIXSTACK: '2000000', $
	MEMORY_BUFSIZE: '512', $
	VERBOSE_TYPE: 'NORMAL'}

 return

 end




pro make_default_tags,taglist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    MAKE_DEFAULT_TAGS
;       
; PURPOSE:
;    Produces a truncated list of photo tags.  Can be sent to READ_TSOBJ
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if n_params() eq 0 then begin
	print,'-syntax make_default_tags, taglist'
	return
endif

taglist=[                    $
            'ID',              $
            'PARENT',          $
            'NCHILD',          $
            'OBJC_TYPE',       $
            'TYPE',            $
            'FLAGS',           $
            'FLAGS2',          $
            'OBJC_FLAGS',      $
            'OBJC_FLAGS2',     $
            'OBJC_ROWC',       $
            'OBJC_COLC',       $
            'ROWC',            $
            'COLC',            $
            'COUNTS_MODEL',    $
            'COUNTS_MODELERR', $
            'COUNTS_EXP',      $
            'COUNTS_EXPERR',   $
            'COUNTS_DEV',      $
            'COUNTS_DEVERR',   $
            'FRACPSF',         $
            'PETROCOUNTS',     $
            'PETROCOUNTSERR',  $
            'PETRORAD',        $
            'PETRORADERR',     $
            'PETROR50',        $
            'PETROR50ERR',     $
            'PETROR90',        $
            'PETROR90ERR',     $
            'PSFCOUNTS',       $
            'PSFCOUNTSERR',    $
            'STATUS',          $
            'RA',              $
            'DEC',             $
            'PRIMTARGET',      $
            'SECTARGET',       $
            'REDDENING',       $
            'M_E1',            $
            'M_E2',            $
            'M_E1E1ERR',       $
            'M_E1E2ERR',       $
            'M_E2E2ERR',       $
            'M_RR_CC',         $
            'M_RR_CCERR',      $
            'M_CR4',           $
            'M_E1_PSF',        $
            'M_E2_PSF',        $
            'M_RR_CC_PSF',     $
            'M_CR4_PSF',       $
            'OBJC_PROB_PSF',   $
            'PROB_PSF',        $
            'SKY',             $
            'SKYERR',          $
            'FIRSTMATCH',      $
            'ROSATMATCH'       $
          ]

return
end

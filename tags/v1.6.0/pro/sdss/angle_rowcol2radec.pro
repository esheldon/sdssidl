function angle_rowcol2radec, run, camcol, field, clr, $
    rerun=rerun, indir=indir 

    if n_params() lt 4 then begin
        print,'-Syntax: angle_rowcol2radec, run, camcol, field, band, rerun=, indir='
        print
        message,'Halting'
    endif

    ;; for 756, increasing row -> increasing ra
    ;;          increasing col -> increasing dec
    ;; ra increases to the  East
    ;; dec increases to the North

    ;; find rotation between row, col and ra/dec
    trans = sdss_read('astrans',run, camcol, bandpass=clr, rerun=rerun, $
                       indir=indir, node=node, inc=inc, /silent)
  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; astrometry structure
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    radeg = 180./!dpi
    val = .4d/3600.
    astr={cd: double(identity(2)),$
          cdelt: [val,val], $
          crpix: dblarr(2), $
          crval: [0., 0.]}
  

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; define set of pixels to rotate
    ;; vertical is increasing row here
    ;; horizontal is increasing column
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lrow = 1489-1
    lcol = 2048-1
    nnr=200.
    vert=dblarr(2,nnr)
    horz=dblarr(2,nnr)
    ccol = 1023.5
    crow = 744.0
    for i=0l, nnr-1 do begin
        vert[0,i] = ccol          ;columns
        vert[1,i] = lrow-i        ;rows
        horz[0,i] = lcol - i      ;columns
        horz[1,i] = crow          ;rows
    endfor 
  
    ;; convert center (row,col) to (mu,nu) great circle coords
    rowcol2munu, trans, field, crow, ccol, cmu, cnu

    gc2eq, cmu, cnu, node, inc, cra, cdec

    ;; same for the vertical/horizontal points
    rowcol2munu, trans, field, vert[1,*], vert[0,*], vmu, vnu
    rowcol2munu, trans, field, horz[1,*], horz[0,*], hmu, hnu
    gc2eq, vmu, vnu, node, inc, vra, vdec
    gc2eq, hmu, hnu, node, inc, hra, hdec

    ;; tangent project ra/dec
    astr.crval = [cra, cdec]
    rd2xy,cra,cdec,astr,cxx,cyy
    rd2xy, vra, vdec, astr, vxx, vyy
    rd2xy, hra, hdec, astr, hxx, hyy

    ;; angle of points in (lambda,eta)
    radecaang = atan(vyy, vxx)
    radecbang = atan(hyy, hxx)

    mvert = mean(!dpi/2. - radecaang)
    mhorz = mean(0. - radecbang)
    angle = mean( [!dpi/2. - radecaang, 0. - radecbang] )

    ;; That is from between ra/dec and row/col
    ;; minus sign for rotation to ra/dec

    angle = -angle

    return,angle
  
end 

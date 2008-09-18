pro classify_galaxies,l,classprob,plot=plot,view=view,silent=silent, $
                       con=con,corrparams=corrparams,cut=cut,cd=cd, $
                       ad=ad,urd=urd,wd=wd

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:   CLASSIFY_GALAXIES
;    
;       
; PURPOSE:  
;   Classifies galaxies based on morphology and color. 
;   Can classify into spiral, or elliptical.  Can also return single parameter 
;   scaling from 0 to 1 with elliptical < Spiral.
;    
;
; CALLING SEQUENCE:  
;     classify_galaxies,pstruct,classprob,plot=plot,view=view,
;                     silent=silent,con=con,corrparams=corrparams,cut=cut
;                     ,cd=cd,ad=ad,urd=urd,wd=wd
;    
;
; INPUTS: 
;     pstruct:  photo structure of galaxy with tag for classification
;   
; KEYWORD PARAMETERS:
;     plot:  set to plot Con vs. u-r, Asym vs. Con, Asym vs. u-r
;     view:  set to view random images of any class
;     silent:  set to view no output
;     con:  returns concentration
;     corrparams:  recalculates parameters that depend on k-correction
;     cut:  set if desired different cut for spiral/elliptical division
;           default = .35
;       
; OUTPUTS:  
;     prints number in each classification bin if keyword silent not set
;     returns classifications into tag .classification and .class
;    
; CALLED ROUTINES:  
;     tfasym
;     view_galaxies
;     tag_names
;     aplot
;     fetch_dir
;     get_atlas
;    
; 
; PROCEDURE: 
;     Measures these parameters from photo outputs and atlas images.
;     u' - r'
;     Concentration=petror50/petror90
;     two-fold asymmetry  - see tfasym.pro
;     Then they are scaled based on a clean sample that was scaled with 
;       STANDARIZE() and adjusted to have a mean of .5 and a variance of .25
;     Scaled parameters are combined with a weighted mean by their errors
;     Flags are set in .classification and continuous parameter set in .class
;       and error in .classerr or can be output by program
;
;
; NOTE:
;     To decipher classifications use:
;         make_clflag_struct
;         clflag_select
;         plot_clflags
;         print_clflags
;
;     Galaxy Structure needs these tags:
;         .classification
;         .petror50
;         .petror90
;         .petrocounts
;         .petrocountserr
;         .petrorad
;         .petroraderr
;         .reddening
;         .e1
;         .e2
;         .ixx
;         .iyy
;         .colc
;         .rowc
;         will be filled if tag is there:
;            .asymmetry
;            .asymmetryerr
;            .class
;            .classerr
;         if keyword set /corrparams and .z1d then tags filled:
;             .lum
;             .absmag
;             .kcorr
;
; REVISION HISTORY:  Judith Racusin   8/7/00
;                          Uses c asym and gr to make a class probability
;                          2/13/01 changed scaleing of params in version 3
;                                  changed from petro g-r to petro u-r
;                          5/9/01  added errors and changed class
;                          5/17/01 changed from tfasym3 to tfasym5, 
;                                  now uses variable centers
;                          6/13/02 changed cut from 0.45 to 0.35 to adjust for
;                                  new asyms from bug fixed in tfasym5
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF N_PARAMS() EQ 0 THEN BEGIN
      PRINT,'Syntax:  classify_galaxies, pstruct, prob, plot=plot, view=view,'
      print,          'silent=silent,con=con,corrparams=corrparams,cut=cut,cd=cd,ad=ad,urd=urd,wd=wd'
      return
  END

  time=systime(1)
  tags=tag_names(l)
  IF keyword_set(silent) THEN output=0 ELSE output=1
  ngs=n_elements(l)
  IF (n_elements(l[0].classification) EQ 5) THEN n=2 ELSE n=0
;GOTO,jump
;;;;;; Make sure all galaxies
  gprim=where(tags EQ 'PRIMTARGET',nprim)
  gobjc=where(tags EQ 'OBJC_TYPE',nobjc)
  notgal=0
  primtarget=replicate(0,ngs)
  IF nprim+nobjc GT 0 THEN BEGIN 
      IF nprim GT 0 THEN BEGIN
          primtarget=l.primtarget 
          t=where(l.primtarget GT 0)
          IF t[0] NE -1 THEN BEGIN 
              make_tsflag_struct,ts
              ts.galaxy='N'
              ts.galaxy_red='N'
              tsflag_select,l,ts,notg
              IF notg[0] EQ -1 THEN notgal=0 ELSE notgal=n_elements(notg) 
          ENDIF
      ENDIF ELSE IF nobjc GT 0 THEN notg=where(l.objc_type NE 3,notgal)
      IF notgal GT 0 THEN l[notg].classification[n]=2L^9
;      IF NOT keyword_set(silent) THEN print,'Not galaxies:  '+ntostr(notgal)
  ENDIF ELSE print,'Assuming all objects in list are galaxies.'
;jump:

;;;;;;Classification of galaxies based on asymmetry, concentration and g-r
;;;;;;Concentration measured in i, tfasym measured in g image with var r center

;;;;;;concentration
  con=fltarr(ngs)
  cerr=con
  w=where(l.petror90[3] NE 0 AND l.petror50[3] NE 0 AND l.petroraderr[3] NE 0,nw)
  IF (nw GT 0) THEN con[w]=l[w].petror50[3]/l[w].petror90[3]
  cerr[w]=sqrt(2)*l[w].petroraderr[3]/l[w].petrorad[3]*con[w]
  fc=where(l.petror90[3] EQ 0 OR l.petror50[3] EQ 0 OR con EQ 0 OR l.petroraderr[3] EQ 0,nfc)

;;;;;;color
  ur=l.petrocounts[0]-l.petrocounts[2]
  urerr=sqrt(l.petrocountserr[0]^2+l.petrocountserr[2]^2)
  fur=where(l.petrocounts[0] EQ 0 OR l.petrocounts[2] EQ 0 $
            OR l.petrocountserr[0] GT 3 OR $
            l.petrocountserr[2] GT 3,nfur)
;  ur=l.counts_model[0]-l.counts_model[2]
;  urerr=sqrt(l.counts_modelerr[0]^2+l.counts_modelerr[2]^2)
;  fur=where(l.counts_model[0] EQ 0 OR l.counts_model[2] EQ 0 $
;            OR l.counts_modelerr[0] GT 3 OR $
;            l.counts_modelerr[2] GT 3,nfur)


  ;;;k-correct colors
  z=0
  t=where(tags EQ 'Z1D' OR tags EQ 'Z')
  IF t[0] NE -1 THEN BEGIN
      z=getztag(l)
      t=where(tags EQ 'KCORR')
      IF t[0] NE -1 THEN BEGIN
          kcorr0=l.kcorr[0]
          kcorr2=l.kcorr[2]
      ENDIF ELSE BEGIN 
          kcorr_color,l.(z),0,l.petrocounts[0],l.petrocounts[1]-l.petrocounts[2],kcorr0
          kcorr_color,l.(z),2,l.petrocounts[2],l.petrocounts[1]-l.petrocounts[2],kcorr2
      ENDELSE
      ur=ur+kcorr0-kcorr2
      z1d=l.(z) 
      z=1
  ENDIF ELSE z1d=intarr(ngs)


;;;;;;asymmetry measured if .asymmetry=0 or no .asymmetry
  t=where(tags EQ 'ASYMMETRY')
  te=where(tags EQ 'ASYMMETRYERR')
  th=where((l.classification[n] AND 2L^4) EQ 0 AND $
           (l.classification[n] AND 2L^10) EQ 0,nth)
 
  asym=fltarr(ngs)
  asymerr=fltarr(ngs)
  IF (t[0] NE -1 AND te[0] NE -1) THEN BEGIN
      IF nth GT 0 THEN w=where(l[th].asymmetry EQ 0,nw)
      IF nw GT 0 THEN w=th[w]
      IF (w[0] NE -1) THEN BEGIN
          alist=l[w]
          tfasym,alist,asym,asymerr,/silent,output=output
          l[w].asymmetry=asym
          IF (te[0] NE -1) THEN l[w].asymmetryerr=asymerr
      ENDIF
      asym=l.asymmetry
      asymerr=l.asymmetryerr
  ENDIF ELSE BEGIN
      alist=l[th]
      tfasym,alist,asymm,asymmerr,/silent,output=output
      asym[th]=asymm
      IF te[0] NE -1 THEN asymerr[th]=asymmerr
  ENDELSE

  fa=where(asym LE 0 OR asym GT 2,nfa)

;;;;;;;;;keeps lens_galaxy tags if set, but redoes classification

  w5=where((l.classification[n] AND 2^5) NE 0,nw5)
  w6=where((l.classification[n] AND 2^6) NE 0,nw6)
  w7=where((l.classification[n] AND 2^7) NE 0,nw7)
  l.classification=0
  IF (nw5 NE 0) THEN $
    l[w5].classification[n]=l[w5].classification[n]+2^5
  IF (nw6 NE 0) THEN $
    l[w6].classification[n]=l[w6].classification[n]+2^6
  IF (nw7 NE 0) THEN $
    l[w7].classification[n]=l[w7].classification[n]+2^7  

;;;;;;;;;scale parameters (came from stand_params and then fit for good sample)
;  IF z THEN urscale=[1.03376,-0.277261] ELSE urscale=[1.05656,-0.256657]
;  cscale=[-0.541424,2.65774]
;  ascale=[0.262635,0.770104]
  ascale=[0.151363,1.35077]; from tfasym5 before 5/02

  urscale=[1.16253   ,  -1.11846]
  cscale=[-0.535999 ,     2.63763]
  ascale=[0.262635,0.770104];[0.238860  ,   0.769669]


  urd=urscale[0]+urscale[1]*ur
  cd=cscale[0]+cscale[1]*con
  ad=ascale[0]+ascale[1]*asym

  IF nfur GT 0 THEN BEGIN & urd[fur]=1 & urerr[fur]=0 & ENDIF 
  IF nfc GT 0 THEN BEGIN & cd[fc]=1 & cerr[fc]=0 & ENDIF 
  IF nfa GT 0 THEN BEGIN & ad[fa]=1 & asymerr[fa]=0 & ENDIF

  urderr=abs(urerr*urscale[1])
  cderr=cerr*cscale[1]
  aderr=asymerr*ascale[1]

;  classerr=sqrt(urderr^2+cderr^2+aderr^2)
  classerr=sqrt(1/(1/urderr^2+1/cderr^2+1/aderr^2))

  urderr=urderr*.05/.014
  cderr=cderr*.05/.037
  aderr=aderr*.05/.024

  w0=where(urderr EQ 0,nw0)
  IF nw0 GT 0 THEN BEGIN & urderr[w0]=1 & urd[w0]=0 & ENDIF 
  w0=where(cderr EQ 0,nw0)
  IF nw0 GT 0 THEN BEGIN & cderr[w0]=1 & cd[w0]=0 & ENDIF 
  w0=where(aderr EQ 0,nw0)
  IF nw0 GT 0 THEN BEGIN & aderr[w0]=1 & ad[w0]=0 & ENDIF

  w1=1/urderr
  w2=1/cderr
  w3=1/aderr
  w1=w1^2
  w2=w2^2
  w3=w3^2
  w4=w1+w2+w3
  classprob=(w1*urd+w2*cd+w3*ad)/w4

;colprint,w1/w4,w2/w4,w3/w4
;colprint,urderr,cderr,aderr,classerr
;print,sqrt(urd^2+cd^2+ad^2)

  f=where(classprob EQ 0,nf)

  max=.95
  min=0.15

  classprob=(classprob-min)/(max-min)
  classerr=classerr/(max-min)

  IF NOT keyword_set(cut) THEN cut=.35

  class=lonarr(ngs)

  nb=0
  IF z THEN b=where(((primtarget AND 2^5) NE 0) AND z1d GT .15,nb)
  IF nb GT 0 THEN class[b]=1

  IF nf GT 0 THEN BEGIN 
      classprob[f]=-1
      class[f]=2
      classerr[f]=0
  ENDIF 

  s=where(classprob GT cut AND class EQ 0,n_s)
  e=where((classprob LE cut OR class EQ 1) AND class NE 2,n_e)
  class[*]=0

  IF n_s GT 0 THEN class[s]=class[s]+2^3
  IF n_e GT 0 THEN class[e]=class[e]+2^0
  IF nf GT 0 THEN class[f]=class[f]+2^4
  IF nb GT 0 THEN class[b]=class[b]+2^8
  IF notgal GT 0 THEN class[notg]=class[notg]+2^9
  IF nfa GT 0 THEN class[fa]=class[fa]+2^10
  IF nfc GT 0 THEN class[fc]=class[fc]+2^11

  ff=where(class EQ 0,nff)
  IF nff GT 0 THEN BEGIN 
      f=[f,ff]
      nf=nf+nff
      class[ff]=class[ff]+2^4
  ENDIF

  l.classification[n]=l.classification[n]+class
  t=where(tags EQ 'CLASS')
  t1=where(tags EQ 'MORPH_CLASS')
  tc=where(tags EQ 'CLASSERR')
  IF (t[0] NE -1) THEN l.class=classprob
  IF (t1[0] NE -1) THEN l.morph_class=classprob 
  IF (tc[0] NE -1) THEN l.classerr=classerr

;;;;;;fix z dependent params;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  IF keyword_set(corrparams) THEN  z_stuff,l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  IF NOT keyword_set(silent) THEN BEGIN
      print,'     Elliptical               ',n_e,'('+ntostr(n_e-nb)+' + ' $
        +ntostr(nb)+')'
      print,'     Spiral                   ',n_s
      print,'     BRG                      ',nb
      print,'     Failed                   ',nf
      print,'     Total                    ',n_e+n_s+nf
 
      IF keyword_set(plot) THEN BEGIN
          ;gr=l.absmag[1]-l.absmag[2]
          nw=n_elements(wd)
          !p.multi=[0,2,2]
          !y.ticks=4
          !x.ticks=0
          al=.8
          r1=[0,4]
          r2=[.2,.6]
          r3=[0,al]
          aplot,1,r1,r2,ytitle='Concentration',xtitle="u' - r'", $
            ystyle=1,xstyle=1,/nodata
          IF n_s gt 0 THEN oplot,ur[s],con[s],psym=3,color=!blue
          IF n_e gt 0 THEN oplot,ur[e],con[e],psym=3,color=!red
          IF nw gt 0 THEN oplot,ur[wd],con[wd],psym=3,color=!green
          xyouts,2.8,.22,'Ellipticals',charsize=1
          xyouts,.1,.57,'Spirals',charsize=1

          !x.ticks=0
          aplot,1,r1,r3,ystyle=1,xstyle=1,xtitle="u' - r'", $
            ytitle='Asymmetry',/nodata
          IF n_e GT 0 THEN oplot,ur[e],asym[e],psym=3,color=!red
          IF n_s GT 0 THEN oplot,ur[s],asym[s],psym=3,color=!blue
          IF nw gt 0 THEN oplot,ur[wd],asym[wd],psym=3,color=!green
          xyouts,.05,.75,'Spirals',charsize=1
          xyouts,2.8,.01,'Ellipticals',charsize=1

          !y.ticks=4
          !x.ticks=2
          aplot,1,r2,r3,ytitle='Asymmetry',xtitle='Concentration', ystyle=1,xstyle=1,/nodata
          IF n_e GT 0 THEN oplot,con[e],asym[e],psym=3,color=!red
          IF n_s GT 0 THEN oplot,con[s],asym[s],psym=3,color=!blue
          IF nw gt 0 THEN oplot,con[wd],asym[wd],psym=3,color=!green
          xyouts,.48,.75,'Spirals',charsize=1
          xyouts,.21,.02,'Ellipticals',charsize=1

          wp=where(classprob GT 0 AND classprob LT 1)
          plot_classprob,l[wp],classprob=classprob[wp],/nobrg
          
          !p.multi=0
      
      ENDIF
      IF keyword_set(view) then BEGIN
          key1='n'
          window,1
          WHILE key1 NE 'q' DO BEGIN
              print,"    View e, s, b, f ?   (or q to quit)"
              print," "
              key1=get_kbrd(10)
              set=0
              CASE key1 OF 
                  'e': set=e
                  's': set=s
                  'b': set=b
                  'f': set=f
                  'q': set=0
                  ELSE: print,'   No Such set'
              ENDCASE
              IF n_elements(set) GT 1 THEN BEGIN 
                  cleanplot,/sil
                  setupplot
                  view_galaxies,l[set],/random,$
                    asym=ad[set],ur=urd[set],con=cd[set], $ 
                    aserr=aderr[set]*.024/.05,$
                    u_rerr=urderr[set]*.011/.05,coerr=cderr[set]*.037/.05, $
                    clerr=classerr[set]
              ENDIF ELSE IF set NE 0 THEN print,'   No Galaxies in Set'
          ENDWHILE
      ENDIF 
      cleanplot,/sil
      setupplot
      ptime,systime(1)-time
  ENDIF 

  return
END

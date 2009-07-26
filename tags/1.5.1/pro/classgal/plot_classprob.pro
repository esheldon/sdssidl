PRO plot_classprob,struct,cuts=cuts,classprob=classprob,nobrg=nobrg,s0=s0,$
                   s1=s1,s2=s2,s3=s3,s4=s4,view=view,silent=silent,$
                   dont_classify=dont_classify,names=names,simp=simp

  IF n_params() EQ 0 THEN BEGIN
      print,'Syntax - plot_classprob,struct,silent=silent,'
      print,'   cuts=cuts,classprob=classprob,nobrg=nobrg,view=view,'
      print,'   s0=s0,s1=s1,s2=s2,s3=s3,s4=s4,dont_classify=dont_classify,'
      print,'   names=names,simp=simp'
      return
  END

  !x.ticks=2
  IF !p.multi[1] EQ 0 THEN IF keyword_set(simp) THEN setprefs,/no ELSE setprefs
 
  cp=0
  class=struct.classification
  IF NOT keyword_set(classprob) THEN BEGIN 
      classify_galaxies2,struct,classprob,/silent
      xtitle='Elliptical <--> Spiral'
      cp=1
  ENDIF ELSE xtitle='Elliptical <--> Spiral'
  IF keyword_set(dont_classify) THEN struct.classification=class
  IF NOT keyword_set(nobrg) THEN nobrg=0
  get_gal_samples,struct,s,e,b
  s0=e
  s1=s
  nsamp=2
  IF NOT keyword_set(names) THEN names=['Elliptical','Spiral']
  IF (b[0] EQ -1 OR nobrg EQ 1) THEN nobrg=1 ELSE nobrg=0
  IF keyword_set(cuts) THEN BEGIN  
      ncuts=n_elements(cuts)
      class=fltarr(n_elements(struct))
      IF NOT nobrg THEN class[b]=1
      cuts=[cuts,1]
      s0=where(classprob GE 0 AND classprob LE cuts[0] AND class EQ 0)
      IF ncuts GE 1 THEN s1=where(classprob GT cuts[0] AND classprob LE cuts[1] AND class EQ 0)
      IF ncuts GE 2 THEN s2=where(classprob GT cuts[1] AND classprob LE cuts[2] AND class EQ 0)
      IF ncuts GE 3 THEN s3=where(classprob GT cuts[2] AND classprob LE cuts[3] AND class EQ 0)
       IF ncuts GE 4 THEN s4=where(classprob GT cuts[3] AND classprob LE cuts[4] AND class EQ 0)
      nsamp=ncuts+1
      IF NOT keyword_set(names) THEN names=['Sample '+ntostr(indgen(nsamp))]
  ENDIF
  IF NOT keyword_set(silent) THEN BEGIN
      IF nsamp EQ 2 THEN color=[!red,!blue]
      IF nsamp EQ 3 THEN color=[!red,!magenta,!blue]
      IF nsamp EQ 4 THEN color=[!red,!yellow,!magenta,!blue]
      IF nsamp EQ 5 THEN color=[!red,!yellow,!magenta,!lightblue,!blue]
      forientation=[45,90,135,0,45,90,13,0]
      
      plothist,classprob,xhist,yhist,bin=.01,/noplot
      goodmax=where(xhist GT 0 AND xhist LT 1)
      aplot,1,[0,1],[0,max(yhist[goodmax])],bin=.01,xtitle=xtitle,title='Classification Distribution',/nodata
      num=intarr(nsamp)
      FOR i=0,nsamp-1 DO BEGIN
          CASE i OF 
              0: samp=s0
              1: samp=s1
              2: samp=s2
              3: samp=s3
              4: samp=s4
          ENDCASE
          num[i]=n_elements(samp)

          IF num[i] GT 1 THEN plothist,classprob[samp],xrange=[0,1],bin=.01,/overplot,color=color[i],thick=2,fcolor=color[i],/fline,forientation=forientation[i],/fill,fthick=2
      ENDFOR
      IF NOT keyword_set(nobrg) THEN BEGIN
          plothist,classprob[b],xrange=[0,1],bin=.01,/overplot,color=!green,thick=2,/fline,forientation=forientation[i+1],/fill,fcolor=!green,fthick=2
          num=[num,n_elements(b)]
          names=[names,'BRG']
      ENDIF
      IF cp THEN legend,['Total = '+ntostr(fix(total(num))),names+' = '+ntostr(num)],/top,/right,textcolor=[!black,color,!green],charsize=1.5
      IF keyword_set(view) THEN BEGIN 
          print,'View Sample # ?'
          key=get_kbrd(10)
          CASE key OF 
              '0': samp=s0
              '1': samp=s1
              '2': samp=s2
              '3': samp=s3
              '4': samp=s4
              'b': samp=b
              ELSE: print,'No Such Sample'
          ENDCASE
          print,'......Viewing Sample:  '+key+'.......'
          view_galaxies,struct[samp],/random,classprob=classprob[samp]
      ENDIF
  ENDIF
  !x.ticks=0

  return
END



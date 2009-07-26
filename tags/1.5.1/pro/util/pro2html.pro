
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    PRO2HTML
;       
; PURPOSE:
;    Convert an IDL .pro file into a pretty HTML document, with EMACS-like
;    color coding.
;
; CALLING SEQUENCE:
;    pro2html, pro_file, html_file, signature=
;
; INPUTS: 
;    pro_file: the full path of the IDL .pro file
;    html_file: the full path name of the output html file
;
; OPTIONAL INPUTS:
;    signature: Some text to put at the end.
;
; PROCEDURE: 
;    Look for reserved IDL words, comments, and strings and color-code them,
;    converting html senstive characters into codes.  You can change the color
;    coding by changing the style definitions in the header.
;	
;
; REVISION HISTORY:
;    Created: 18-NOV-2000 Erin Scott Sheldon UofMich
;    Inspired by the emacs htmlize.el, moved over to using styles
;    for a more compact and stable result.  E.S. NYU  2006-July-15 
;       
;-
;
;  This program is part of sdssidl.
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin.sheldon at gmail.com
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
;                                      



function pro2html_reserved_words

  return,     ['AND', $
               'BEGIN', $
               'BREAK', $
               'CASE', $
               'COMMON', $
               'CONTINUE', $
               'DO',$
               'ELSE', $
               'ELSE:', $
               'END', $
               'ENDCASE',$
               'ENDELSE', $
               'ENDFOR', $
               'ENDIF', $
               'ENDREP', $
               'ENDSWITCH',$
               'ENDWHILE',$
               'EQ', $
               'FOR', $
               'FUNCTION', $
               'GE', $
               'GOTO',$
               'GT', $
               'IF', $
               'INHERITS', $
               'LE', $
               'LT', $
               'MOD', $
               'NE', $
               'NOT', $
               'OF', $
               'ON_ERROR', $
               'ON_IOERROR', $
               'OR', $
               'PRO', $
               'REPEAT', $
               'SWITCH', $
               'THEN',$
               'UNTIL', $
               'WHILE',$
               'XOR','RETURN']


end 

function pro2html_reserved_char

  return, [ ['<', '&lt;'], $
            ['>', '&gt;'], $
            ['&', '&amp;'] ]

end

function is_resword, word

  common pro2html_reser, reswords, html_reschar
  w=where(reswords EQ strupcase(word), nw)
  if nw eq 0 then return, 0 else return, 1

end 


function is_html_reschar, char

  common pro2html_reser, reswords, html_reschar
  w=where(html_reschar[0,*] eq char, nw)
  if nw ne 0 then return, html_reschar[1,w[0]] else return,char

end


pro pro2html, proname, htmlname, silent=silent, signature=signature

  if n_params() lt 2 then begin 
      print,'-syntax: pro2html, proname, htmlname, silent=silent'
      return
  endif 


  common pro2html_reser, reswords, html_reschar




  if not keyword_set(silent) then begin 
      print
      print,'proname = ',proname
      print,'htmlname = ',htmlname
      print
  endif 

  dirsep, proname, dir, proc

  openr, inlun, proname, /get_lun
  openw, outlun, htmlname, /get_lun


  ;; The header
  printf, outlun, '<html>'
  printf, outlun, '  <head>'
  printf, outlun, '    <title>'+proc+'</title>'
  printf, outlun, '    <style type="text/css">'
  printf, outlun, '    <!--'
  printf, outlun, '    body {'
  printf, outlun, '      color: #000000;'
  printf, outlun, '      background-color: #ffffff;'
  printf, outlun, '    }'
  printf, outlun, '    .comment {'
  printf, outlun, '      /* font-lock-comment-face */'
  printf, outlun, '      color: #b22222;'
  printf, outlun, '    }'
  printf, outlun, '    .constant {'
  printf, outlun, '      /* font-lock-constant-face */'
  printf, outlun, '      color: #5f9ea0;'
  printf, outlun, '    }'
  printf, outlun, '    .function-name {'
  printf, outlun, '      /* font-lock-function-name-face */'
  printf, outlun, '      color: #0000ff;'
  printf, outlun, '    }'
  printf, outlun, '    .keyword {'
  printf, outlun, '      /* font-lock-keyword-face */'
  printf, outlun, '      color: #a020f0;'
  printf, outlun, '    }'
  printf, outlun, '    .string {'
  printf, outlun, '      /* font-lock-string-face */'
  printf, outlun, '      color: #bc8f8f;'
  printf, outlun, '    }'
  printf, outlun, '    .variable-name {'
  printf, outlun, '      /* font-lock-variable-name-face */'
  printf, outlun, '      color: #b8860b;'
  printf, outlun, '    }'
  printf, outlun
  printf, outlun, '    a {'
  printf, outlun, '      color: inherit;'
  printf, outlun, '      background-color: inherit;'
  printf, outlun, '      font: inherit;'
  printf, outlun, '      text-decoration: inherit;'
  printf, outlun, '    }'
  printf, outlun, '    a:hover {'
  printf, outlun, '      text-decoration: underline;'
  printf, outlun, '    }'
  printf, outlun, '    -->'
  printf, outlun, '    </style>'
  printf, outlun, '  </head>'
  printf, outlun, '<pre>'
  printf, outlun


  reswords = pro2html_reserved_words()
  html_reschar = pro2html_reserved_char()
  ;; color reserved words
  resf = '<span class="keyword">'
  resb = '</span>'

  ;; color after PRO/FUNCTION statement 
  aprof = '<span class="function-name">'
  aprob = '</span>'

  ;; color comments
  comm = ';'
  comf = '<span class="comment">'
  comb = '</span>'


  ;; color strings dark salmon
  str1 = "'"
  str2 = '"'

  strf = '<span class="string">'
  strb = '</span>'




  nl=numlines(proname)
  string=''
  for i=0l, nl-1 do begin 

      string=''
      readf, inlun, string
      string=detabify(string)
      length = strlen(string)
      
      line = ''
      j=0L
      ;; on the line level
      afterpro = 0
      iscomment = 0
      didcomment = 0
      isstring = 0
      oldstr = ''
      while j le length-1 do begin 

          ;; on the word level
          word = ''
          tmp=''
          repeat begin 
              word = word + tmp
              wupcase = strupcase(word)
              if (wupcase eq 'GOTO,') then word=resf+'GOTO'+resb+','
              if (wupcase eq 'RETURN,') then word=resf+'return'+resb+','
              tmp = strmid(string,j,1)
              if afterpro and tmp eq ',' then begin 
                  tmp = aprob+tmp
                  afterpro=0
              endif else if (tmp eq comm) and (not isstring) then begin
                  iscomment = 1 
                  if not didcomment then begin 
                      tmp = comf+tmp
                      didcomment=1
                  endif 
              endif else if ( (tmp eq str1) or (tmp eq str2) ) and (not iscomment) then begin
                  
                  if isstring and (tmp eq oldstr) then begin 
                      isstring = 0 
                      oldstr=''
                      tmp = tmp+strb
                  endif else if not isstring then begin
                      isstring = 1
                      oldstr=tmp
                      tmp = strf+tmp
                  endif 
              endif else tmp = is_html_reschar(tmp)
                  
              j=j+1
          endrep until ( (tmp eq ' ') or (j ge length+1) )

          if iscomment then begin 
              ;; do nothing
          endif else if isstring then begin 
              ;; do nothing
          endif else if afterpro and (tmp ne ' ') then begin 
              word = word + aprob
              afterpro = 0
          endif else if is_resword(word) then begin
              if (strupcase(word) eq 'PRO') or (strupcase(word) eq 'FUNCTION') then afterpro = 1
              if afterpro then word = resf+word+resb+aprof $
              else word = resf+word+resb
          endif 
          line = line + word
          if tmp eq ' ' then line = line + ' '

      endwhile 
      if iscomment then line = line + comb


      printf,outlun,line

  endfor 

  printf, outlun
  printf, outlun, '</pre>'
  printf, outlun, '<hr size=1 noshade>'
  printf, outlun, 'This file was generated from IDL code by pro2html.pro in the SDSSIDL codebase<br>'
  if n_elements(signature) ne 0 then begin 
      printf, outlun, signature
  endif 
  printf, outlun, '</BODY>'
  printf, outlun, '</HTML>'

  free_lun, inlun
  free_lun, outlun

  return
end 

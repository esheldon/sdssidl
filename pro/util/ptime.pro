
;+
;
; NAME: 
;    ptime
;       
; PURPOSE: 
;    print a time interval in a nice format if days,hours,minuties,seconds.   
;	
; CALLING SEQUENCE: 
;    ptime, t [, num, tstr=, command=, /savetime]
;      
;                 
; INPUTS: 
;    t: a time in seconds.
;
; OPTIONAL INPUTS: 
;    num: number of places to keep in seconds. Default is 4, 
;         which includes any decimal places.
;
; KEYWORD PARAMETERS:
;    command=: An execution string to be timed.
;    /savetime: if this is set, then t is set to the current systime(1)
;               and returned without printing
;       
; OPTIONAL OUTPUTS: 
;    tstr: a string containing the time in hr min sec format.
;
; EXAMPLES:
;  IDL> tm=systime(1)
;  IDL> run_some_command
;  IDL> ptime, systime(1)-tm
;  Time: 5 min 35.2000 sec
;
;  IDL> ptime, command='some_command'
;  Time: 3.0079839 seconds
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon UofM  7/8/99
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

pro ptime, ttime, num, tstr=tstr, savetime=savetime, command=command

    if n_elements(command) ne 0 then begin 

        ttime = systime(1)
        if not execute(command[0]) then begin 
            print,'Could not execute command: ',command[0]
            return
        endif 
        ttime = systime(1)-ttime

    endif else begin  

        if n_params() eq 0 then begin 
            print,'-Syntax: ptime, t [, num, command=, tstr=, /savetime]'
            print,''
            print,'Use doc_library,"ptime"  for more help.'  
            return
        endif 

    endelse 


    if keyword_set(savetime) then begin
        ttime=systime(1)
        return
    endif 

    oneday=86400.0
    onehour=3600.0
    onemin=60.0
  
    if n_elements(num) eq 0 then num = 4
    tstr='Time: '

    ;; Number of integer days
    days = long(ttime/oneday)
    ;; Fraction of a day left over
    dayfrac = ttime/oneday-days
    ;; How many hours that is
    hr = long(dayfrac*24.0)
    ;; fraction of an hour left over
    hrfrac = dayfrac*24.0 - hr
    ;; how many minutes that is
    min = long(hrfrac*60.0)
    ;; fraction of a minute left over
    minfrac = hrfrac*60.0 - min
    ;; How many seconds that is
    sec = minfrac*60.0
    
    if days gt 0 then begin
        tstr = ntostr(days)+' days '+ntostr(hr)+' hours '+ntostr(min)+' min '+ntostr(sec)+' sec'
    endif else if hr gt 0 then begin 
        tstr = ntostr(hr)+' hours '+ntostr(min)+' min '+ntostr(sec)+' sec'
    endif else if min gt 0 then begin 
        tstr = ntostr(min)+' min '+ntostr(sec)+' sec'
    endif else begin 
        tstr = ntostr(sec)+' sec'
    endelse
    print,'Time: '+tstr
    return


end

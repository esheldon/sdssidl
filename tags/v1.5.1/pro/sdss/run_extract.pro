pro run_extract,run,command_name,fend=fend,addon=addon,groupn=groupn,taglist=taglist,rerun=rerun

if n_params() LT 2 then begin
  print,'run_extract,run,command_name,fend=fend,addon=addon,groupn=groupn,rerun=rerun'
  return
endif

if not keyword_set(groupn) then begin
	groupn=30
endif

if not keyword_set(fend) then begin
	fend='0011'
endif
if not keyword_set(addon) then begin
	fend='_zobj.fit'
endif
if not keyword_set(rerun) then begin
   	rerun='-0-'
endif

for i=1,6,1 do begin

	print,'Processing column:',i
	fname='tsObj-000'+strtrim(string(run),2)+'-'+$
		strtrim(string(i),2)+rerun+fend+'.fit'
	oname='tsObj-000'+strtrim(string(run),2)+'-'+$
		strtrim(string(i),2)+rerun+fend+addon

	sdss_extract,fname,oname,command_name,groupn=groupn,taglist=taglist

endfor



return
end

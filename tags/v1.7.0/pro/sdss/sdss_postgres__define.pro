;+
; NAME:
;  sdss_postgres__define   (IDL Class File)
;
; PURPOSE:
;  This class inherits the POSTGRES class and provides a simple way to access
;  postgres tables by SDSS id information (run, rerun, camcol, field, id) and
;  the composite 64-bit id "photoid".  The user can send various levels of id
;  information, from just runs to individual run,rerun,camcol,field,ids (or
;  photoids).
;
; CATEGORY:
;  SDSS routine.
;
; CALLING SEQUENCE:
;  sp=obj_new('sdss_postgres', connect_info=)
;
; OPTIONAL INPUTS:
;  connect_info: This is stored at initialization so can be used later
;    without sending. Useful if not using the database listed in the
;    PGDATABASE environment variable for example.
;
; METHODS:
;
;  You can list the methods using 
;    IDL> methods,'postgres' and see the
;  individual documentaion for each using 
;    IDL> doc_method,'postgres::methodname' 
;  This class also inherits all the methods of POSTGRES.
; 
;    sdss_postgres::readbyid()  
;       Read data given the any of the id information, from just runs to the 
;       full run,camcol,field,id (rerun optional).
;    sdss_postgres::read_photoids()
;       Read data for a list of photoids. Given an input list of photoids,
;       generates a set of queries to retrieve the data.  There is a limit to
;       lenght of a query, so this program breaks up the list into multiple
;       queries.  Assumes the query length has been increases so it can send
;       about 10,000 photoids in a single query.
;
;       Alternatively, a structure with run,rerun,camcol,field,id may be
;       sent and photoids will be generated.
;         
;       photoids are generated from run,rerun,camcol,field,id with the 
;       sdss_util::photoid() method.
;
;    sdss_postgres::send_queries
;       Takes a list of input queries and collates the results from each and
;       returns a single structure.  This takes care of the bookkeeping for
;       multiple queries.
;
;    The readbyid() function calls one of a number of functions depending on 
;    the arguments.
;       sdss_postgres::read_runs(): If only runs are sent.
;       sdss_postgres::read_camcol(): If runs,camcols sent.
;       sdss_postgres::read_fields(): If runs,camcols,fields sent.
;       sdss_postgres::read_ids(): If runs,camcols,fields,ids sent.
;          |-> sdss_postgres::read_photoids().
;
;
; RESTRICTIONS:
;
;
; EXAMPLE:
;  IDL> sp=obj_new('sdss_postgres')
;  IDL> res = sp->readbyid('datasweep', 756, 3, [125, 166, 172])
;
;  IDL> res = sp->readbyid('datasweep', struct, columns=['ra','dec'])
;
;  IDL> pid = sp->photoid(struct)
;  IDL> res = sp->read_photoids('datasweep', pid, clauses='modelfux > 200000')
;
;
; MODIFICATION HISTORY:
;  Created: Mid-2005 Erin Sheldon.
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



FUNCTION sdss_postgres::init, connect_info=connect_info
  
  IF NOT self->postgres::init(connect_info=connect_info) THEN BEGIN 
      message,'Failed to initialize postgres',/inf
      return,0
  ENDIF 
  IF NOT self->sdss::init() THEN BEGIN 
      message,'Failed to initialize sdss',/inf
      return,0
  ENDIF 
  return,1

END 

; Execute the input set of queries.

FUNCTION sdss_postgres::send_queries, table, where_statements, $
                      columns=columns, $
                      clauses=clauses, $
                      connect_info=connect_info, $
                      slow=slow, $
                      verbose=verbose, $
                      status=status
  

  nQuery = n_elements(where_statements)
  ntable = n_elements(table)

  IF ntable NE 1 OR nquery EQ 0 THEN BEGIN 
      print,'-Syntax: struct = sp->send_queries(table, where_statements, '
      print,'                                   columns=, clauses=, '
      print,'                                   connect_info=, '
      print,'                                   /slow, /verbose, status=)'
      print
      message,'Halting'
  ENDIF 

  IF n_elements(columns) NE 0 THEN BEGIN 
      cols = strjoin( columns , ',')
  ENDIF ELSE BEGIN 
      cols = '*'
  ENDELSE 

  IF n_elements(clauses) EQ 0 THEN BEGIN 
      addclauses = ''
  ENDIF ELSE BEGIN 
      addclauses = ' AND '+clauses[0]
  ENDELSE 

  printlen = 124

  ;; Simple for a single query
  IF nQuery EQ 1 THEN BEGIN 

      query = $
        "SELECT "+cols+" FROM "+table+" WHERE "+where_statements+" "+addclauses
      IF keyword_set(verbose) THEN print,strmid(query, 0, printlen)

      struct = self->query(query, connect_info=connect_info, status=status)
      return,struct
  ENDIF 

  ;; Multiple Queries sent
  IF NOT keyword_set(slow) THEN BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Fast: we gather all the objects in pointers and then
      ;; copy them into an output structure.  This can be faster than the 
      ;; standard method below, but can use more memory.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      queries = $
        "SELECT "+cols+" FROM "+table+" WHERE "+where_statements+" "+addclauses

      ptrlist = ptrarr(nQuery)
      nrows = 0ULL
      
      FOR i=0L, nQuery-1 DO BEGIN 

          IF keyword_set(verbose) THEN print,strmid(queries[i], 0, printlen)
          st = self->query(queries[i], nrows=tnrows, $
                           connect_info=connect_info, $
                           status=status)

          IF tnrows GT 0 THEN BEGIN 
              nrows = nrows + tnrows
              ptrlist[i] = ptr_new(st, /no_copy)
          ENDIF 

      ENDFOR 
      IF nrows GT 0 THEN BEGIN 
          struct = combine_ptrlist(ptrlist)
      ENDIF ELSE BEGIN 
          struct = -1
      ENDELSE 
      return, struct

  ENDIF ELSE BEGIN

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Standard: Count the queries results first by running a 
      ;; SELECT count(*) statement first, then create output and
      ;; copy in as we re-run queries.  For many large queries this 
      ;; could save lots of memory. 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      numlist = ulon64arr(nQuery)
      nrows = 0ULL

      num_queries = $
        "SELECT count(*) from "+table+" WHERE "+where_statements+" "+addclauses

      FOR i=0L, nQuery-1 DO BEGIN 


          IF keyword_set(verbose) THEN BEGIN 
              print, strmid(num_queries[i], 0, printlen)
          ENDIF 
          numstruct = self->query(num_queries[i], nrows=cnrows, $
                                  connect_info=connect_info, $
                                  status=status)

          IF cnrows NE 1 THEN BEGIN 
              tnrows = numstruct.count
              IF strnumber(tnrows) THEN BEGIN 
                  tnrows = ulong64(tnrows)
                  nrows = nrows + tnrows
                  numlist[i] = tnrows
              ENDIF 
          ENDIF ELSE BEGIN 
              return, -1
          ENDELSE 
      ENDFOR 

      ;; Any found?
      w=where(numlist GT 0, nw)
      IF nw NE 0 THEN BEGIN 

          ;; Now get a single structure
          struct_query = "SELECT "+cols+" FROM "+table+$
            " WHERE "+where_statements[w[0]]+" LIMIT 1"

          IF keyword_set(verbose) THEN print,'getting struct'
          IF keyword_set(verbose) THEN print,strmid(struct_query, 0, printlen)
          st = self->query(struct_query, connect_info=connect_info, $
                           status=status)
          
          ;; Now all
          queries = $
            "SELECT "+cols+" FROM "+table+$
            " WHERE "+where_statements+" "+addclauses

          beg = 0ULL
          struct = replicate(st, nrows)
          FOR i=0ULL, nQuery-1 DO BEGIN 

              IF numlist[i] NE 0 THEN BEGIN 

                  IF keyword_set(verbose) THEN BEGIN 
                      print,strmid(queries[i], 0, printlen)
                  ENDIF 
                  st = self->query(queries[i], connect_info=connect_info, $
                                   status=status)
                  
                  struct[beg:beg+numlist[i]-1] = temporary(st)
                  beg=beg+numlist[i]
                  
              ENDIF 
          ENDFOR 
          
      ENDIF ELSE BEGIN 
          ;; None found
          return, -1
      ENDELSE 
      
  ENDELSE ;; Slower, memory efficient way


END 



;docstart::sdss_postgres::read_photoids
;
; NAME:
;  postgres::read_photoids
;
; PURPOSE:
;  Read a set of SDSS photoids from a postgres table.  photoids can be 
;  created using the sdss_util::photoid() method.
;  
;
; CALLING SEQUENCE:
;
;  res = sp->read_photoids(table, photoids/struct, $
;                          columns=, $
;                          clauses=, $
;                          connect_info=, $
;                          /slow, $
;                          /verbose=, $
;                          status=)
;
; INPUTS:
;  table: The tablename in the current database or that specified through
;     connect_info
;  photoids: A list of photoids. These can be created from run,rerun,camcol,
;    field,id using the sdss_util::photoid() method.
;  struct: Instead of photoids, the user may input a struct containing
;    id info.
; 
; OPTIONAL KEYWORD INPUTS:
;  columns: Which columns to read from the table. Defaults to all ('*').
;  clauses: Added restrictions on query. e.g. "modelflux[2] > 200000.0'
;  connect_info:  List of connection options separated by semicolon.
;     e.g. "user=somename;dbname=somename"
;     http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;  /slow:  Use a slower, more memory efficient method of retrieving multiple
;     queries.  Can be useful for example if reading from 10 different runs,
;     but is somewhat slower.
;  /verbose: pgsql_query() will print some informationl messges.
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_postgres::read_photoids


FUNCTION sdss_postgres::read_photoids, table, photoids, $
                      columns=columns, $
                      clauses=clauses, $
                      connect_info=connect_info, $
                      slow=slow, $
                      verbose=verbose, $
                      status=status

  IF n_elements(photoids) EQ 0 OR n_elements(table) EQ 0 THEN BEGIN 
      print,'-Syntax: struct = sp->read_photoids(table, photoids/struct, '
      print,'                                    columns=, clauses=, '
      print,'                                    connect_info=, '
      print,'                                    /slow, /verbose, status=)'
      print
      message,'Halting'
  ENDIF 

  ;; increased max_stack_depth to 16384 to handle large queries.
  maxnum = 10000

  IF size(photoids,/tname) EQ 'STRUCT' THEN BEGIN 
      tphotoids = self->sdss_util::photoid(photoids)
      tphotoids = tphotoids[ rem_dup(tphotoids) ]
  ENDIF ELSE BEGIN 
      tphotoids = photoids[rem_dup(photoids)]
  ENDELSE 

  nid = n_elements(tphotoids)
  IF nid LT maxnum THEN BEGIN 

      photoidstr = ntostr(tphotoids)
      photoidstr = '('+strjoin(photoidstr, ',')+')'

      where_statement = 'photoid IN '+photoidstr
  ENDIF ELSE BEGIN 
      ndiv = nid/maxnum
      nmod = nid MOD maxnum
      nstatement = ndiv + (nmod NE 0)
      photoidstr = strarr(nstatement)

      beg = 0L
      FOR i=0L, nstatement-1 DO BEGIN 
          IF i EQ nstatement-1 THEN BEGIN 
              tid = tphotoids[beg:nid-1]
          ENDIF ELSE BEGIN 
              tid = tphotoids[beg:beg+maxnum-1]
          ENDELSE 
          tphotoidstr = ntostr(tid)
          photoidstr[i] = '('+strjoin(tphotoidstr, ',')+')'    

          tid = 0
          tphotoidstr = 0
          beg = beg + maxnum
      ENDFOR 

      ;; Now an array of statements
      where_statement = 'photoid IN '+photoidstr
  ENDELSE 
  struct = self->send_queries(table, where_statement, $
                              columns=columns, $
                              clauses=clauses, $
                              connect_info=connect_info, $
                              slow=slow, $
                              verbose=verbose, $
                              status=status)
  return,struct

END 

;docstart::sdss_postgres::read_ids
;
; NAME:
;  postgres::read_ids
;
; PURPOSE: 
;  Read a set of full SDSS ids from a postgres table. The more generic function
;  sdss_postgres::readbyid() should usually be used; it will call this function
;  for the right inputs.
;  
;
; CALLING SEQUENCE:
;
;  res = sp->read_ids(table, runs, camcols, fields, ids, $
;                     reruns=, $
;                     columns=, $
;                     clauses=, $
;                     connect_info=, $
;                     /slow, $
;                     /verbose=, $
;                     status=)
;
; INPUTS:
;  table: The tablename in the current database or that specified through
;     connect_info
;  runs,camcols,fields,ids: All arrays must be same length. This 
;     function converts the inputs to photoids and calls ::read_photoids()
; 
; OPTIONAL KEYWORD INPUTS:
;  reruns: Must match the above run inputs in dimensions. If not sent, 
;     the result of sdss_files::rerun() is used.
;  columns: Which columns to read from the table. Defaults to all ('*').
;  clauses: Added restrictions on query. e.g. "modelflux[2] > 200000.0'
;  connect_info:  List of connection options separated by semicolon.
;     e.g. "user=somename;dbname=somename"
;     http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;  /slow:  Use a slower, more memory efficient method of retrieving multiple
;     queries.  Can be useful for example if reading from 10 different runs,
;     but is somewhat slower.
;  /verbose: pgsql_query() will print some informationl messges.
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_postgres::read_ids


FUNCTION sdss_postgres::read_ids, table, runs, camcols, fields, ids, $
                      reruns=reruns_in, minrerun=minrerun, $
                      columns=columns, $
                      clauses=clauses, $
                      connect_info=connect_info, $
                      slow=slow, $
                      verbose=verbose, $
                      status=status

  
  IF n_params() LT 5 THEN BEGIN 
      print,'-Syntax: struct = sp->read_ids(table, runs, camcols, fields, ids,'
      print,'                               reruns=, '
      print,'                               columns=, clauses=, '
      print,'                               connect_info=, '
      print,'                               /slow, /verbose, status=)'
      print
      message,'Halting'
  ENDIF 



  ;; always returns same length as runs
  reruns = $
    self->_get_reruns(runs, reruns=reruns_in, minrerun=minrerun)

  photoids = self->sdss_util::photoid(runs,reruns,camcols,fields,ids)
  IF photoids[0] EQ -1 THEN message,'Halting'

  return,self->read_photoids(photoids, $
                             columns=columns, $
                             clauses=clauses, $
                             connect_info=connect_info, $
                             slow=slow, $
                             verbose=verbose, $
                             status=status)

END 


;docstart::sdss_postgres::read_fields
;
; NAME:
;  postgres::read_fields
;
; PURPOSE:
;  Read a set of runs,camcols,fields from a postgres table. The more generic
;  function sdss_postgres::readbyid() should usually be used; it will call this
;  function for the right inputs.
;  
;
; CALLING SEQUENCE:
;
;  res = sp->read_fields(table, runs, camcols, fields, $
;                        reruns=, $
;                        columns=, $
;                        clauses=, $
;                        connect_info=, $
;                        /slow, $
;                        /verbose=, $
;                        status=)
;
; INPUTS:
;  table: The tablename in the current database or that specified through
;     connect_info
;  runs, camcols, fields: 
;     There are three ways to specify these ids:
;        * A single run,camcol,field set.
;        * A single run,camcol but multiple fields.
;        * Mutiple runs,camcols as well as fields.  The arrays of equal length.
; 
; OPTIONAL KEYWORD INPUTS:
;  reruns: Must match the above run inputs in dimensions. If not sent, 
;     the result of sdss_files::rerun() is used.
;  columns: Which columns to read from the table. Defaults to all ('*').
;  clauses: Added restrictions on query. e.g. "modelflux[2] > 200000.0'
;  connect_info:  List of connection options separated by semicolon.
;     e.g. "user=somename;dbname=somename"
;     http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;  /slow:  Use a slower, more memory efficient method of retrieving multiple
;     queries.  Can be useful for example if reading from 10 different runs,
;     but is somewhat slower.
;  /verbose: pgsql_query() will print some informationl messges.
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_postgres::read_fields

FUNCTION sdss_postgres::read_fields, table, runs, camcols, fields, $
                      reruns=reruns_in, minrerun=minrerun, $
                      columns=columns, $
                      clauses=clauses, $
                      connect_info=connect_info, $
                      slow=slow, $
                      verbose=verbose, $
                      status=status

  IF n_params() LT 4 THEN BEGIN 
      print,'-Syntax: struct = sp->read_fields(table, runs, camcols, fields, '
      print,'                                  reruns=, '
      print,'                                  columns=, clauses=, '
      print,'                                  connect_info=, '
      print,'                                  /slow, /verbose, status=)'
      print
      message,'Halting'
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For non-consecutive fields, faster way gives a 30% improvement for 
  ;; 100 fields
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; always returns same length as runs
  reruns = $
    self->_get_reruns(runs, reruns=reruns_in, minrerun=minrerun)

  nruns = n_elements(runs)
  ncamcols = n_elements(camcols)
  nfields = n_elements(fields)

  Ufields = fields[rem_dup(fields)]
  nUfields = n_elements(Ufields)

  IF nruns EQ 1 AND ncamcols EQ 1 AND nUfields EQ 1 THEN BEGIN 

      where_statements = $
        "run = "+ntostr(runs)+" AND " + $
        "rerun = "+ntostr(reruns) + " AND " + $
        "camcol = "+ntostr(camcols)+" AND "+ $
        "field = "+ntostr(Ufields)

  ENDIF ELSE IF nruns EQ 1 AND ncamcols EQ 1 THEN BEGIN 
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; single run, camcol, but more than one field
      ;; Detect the consecutive fields, which can speed things
      ;; up a lot
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Extract the consecutive ones
      get_consecutive, uFields, first, last
      nConsec = n_elements(first)

      ;; Generate where statements
      where_statements = strarr(nConsec)
      where_statements = $
        "run = "+ntostr(runs)+" AND " + $
        "rerun = "+ntostr(reruns)+" AND "+$
        "camcol = "+ntostr(camcols)+" AND "+ $
        "field BETWEEN "+$
        ntostr(uFields[first]) + " AND " + ntostr(uFields[last])

  ENDIF ELSE BEGIN

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Multiple runs,camcols.  User must input exactly same number of 
      ;; camcols and fields as runs in this situation
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF (ncamcols NE nruns) OR (nfields NE nruns) THEN BEGIN 
          message,'Number of camcols and fields must equal number '+$
            'of runs for multi-run, multi-camcol statements',/inf

          return,-1
      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Faster way, uses speed up for consecutive fields
      ;; For random fields is a 20-30% speedup.  Truly consecutive fields,
      ;; its a huge speedup
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Make tree structure of run,rerun,camcol,field
      idstruct = sdss_histid(runs, reruns, camcols, fields)
      n_unique = idStruct.Nleaves
      ptrlist = ptrarr(n_unique)
      ptrIndex = 0L
      
      pruns = idStruct.runs
      FOR ri=0L, idStruct.nruns-1 DO BEGIN 
          runStr = ntostr( (*pruns)[ri].run )
          preruns = (*pruns)[ri].reruns
          FOR rri=0L, (*pruns)[ri].nreruns-1 DO BEGIN 
              rerunStr = ntostr( (*preruns)[rri].rerun )
              pcamcols = (*preruns)[rri].camcols
              FOR ci=0L, (*preruns)[rri].ncamcols-1 DO BEGIN 
                  camcolStr = ntostr( (*pcamcols)[ci].camcol )
                  fieldStructs = *(*pcamcols)[ci].fields
                  
                  rmd = rem_dup(fieldStructs.field)
                  Ufields = fieldStructs[rmd].field
                  nUfields = n_elements(Ufields)
                  
                  ;; Extract the consecutive ones
                  get_consecutive, uFields, first, last
                  nConsec = n_elements(first)
                  
                  ;; Generate where statements
                  twhere_statements = strarr(nConsec)
                  twhere_statements = $
                    "run = "+runStr+" AND " + $
                    "rerun = "+rerunStr+" AND "+$
                    "camcol = "+camcolStr+" AND "+ $
                    "field BETWEEN "+$
                    ntostr(uFields[first])+" AND "+ntostr(uFields[last])

                  ;; Now copy these into our pointerlist
                  ptrlist[ptrIndex] = ptr_new(twhere_statements, /no_copy)
                  ptrIndex = ptrIndex + 1
              ENDFOR ;; camcols
          ENDFOR ;; reruns
      ENDFOR                    ;runs
      
      ;; Now combine the lists
      where_statements = combine_ptrlist(ptrlist)
      sdss_histid_destroy, idstruct

  ENDELSE ;; multiple runs....
  
  struct = self->send_queries(table, where_statements, $
                              columns=columns, $
                              clauses=clauses, $
                              connect_info=connect_info, $
                              slow=slow, $
                              verbose=verbose, $
                              status=status)
  return,struct


END 

;docstart::sdss_postgres::read_camcols
;
; NAME:
;  postgres::read_camcols
;
; PURPOSE:
;  Read a set of runs,camcols from a postgres table. The more generic function
;  sdss_postgres::readbyid() should usually be used; it will call this function
;  for the right inputs.
;  
;
; CALLING SEQUENCE:
;
;  res = sp->read_camcols(table, runs, camcols, $
;                         reruns=, $
;                         columns=, $
;                         clauses=, $
;                         connect_info=, $
;                         /slow, $
;                         /verbose=, $
;                         status=)
;
; INPUTS:
;  table: The tablename in the current database or that specified through
;     connect_info
;  runs,camcols: 
;     There are two ways to specify these ids: 
;        * A single run with multiple camcols 
;        * Multiple runs and camcols.  For multiple runs, the user must 
;          specify *pairs* of runs and camcols to avoid confusion.
; 
; OPTIONAL KEYWORD INPUTS:
;  reruns: Must match the above run inputs in dimensions. If not sent, 
;     the result of sdss_files::rerun() is used.
;  columns: Which columns to read from the table. Defaults to all ('*').
;  clauses: Added restrictions on query. e.g. "modelflux[2] > 200000.0'
;  connect_info:  List of connection options separated by semicolon.
;     e.g. "user=somename;dbname=somename"
;     http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;  /slow:  Use a slower, more memory efficient method of retrieving multiple
;     queries.  Can be useful for example if reading from 10 different runs,
;     but is somewhat slower.
;  /verbose: pgsql_query() will print some informationl messges.
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_postgres::read_camcols


FUNCTION sdss_postgres::read_camcols, table, runs, camcols, $
                      reruns=reruns_in, minrerun=minrerun, $
                      columns=columns, $
                      clauses=clauses, $
                      connect_info=connect_info, $
                      slow=slow, $
                      verbose=verbose, $
                      status=status

  IF n_params() LT 3 THEN BEGIN 
      print,'-Syntax: struct = sp->read_camcols(table, runs, camcols, '
      print,'                                   reruns=, '
      print,'                                   columns=, clauses=, '
      print,'                                   connect_info=, '
      print,'                                   /slow, /verbose, status=)'
      print
      message,'Halting'
  ENDIF 



  ;; always returns same length as runs
  reruns = $
    self->_get_reruns(runs, reruns=reruns_in, minrerun=minrerun)


  nruns = n_elements(runs)

  IF nruns EQ 1 THEN BEGIN 
      ;; Single run, perhaps multiple camcols
      where_statements = $
        "run = "+ntostr(runs)+" AND "+$
        "rerun = "+ntostr(reruns)+" AND "+$
        "camcol = "+ntostr(camcols)

  ENDIF ELSE BEGIN 

      ;; Multiple runs.  User must input exactly same number of camcols as
      ;; runs in this situation

      ncamcols = n_elements(camcols)
      IF ncamcols NE nruns THEN BEGIN 
          message,'Number of camcols must equal number of runs for '+$
            'multi-run statements',/inf

          return,-1
      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Not much speed advantage to optimizing this, so just split into
      ;; multiple unique camcol reads. Also, will use less memory to read
      ;; the individual camcols, since a copy is required. Note: doing a 
      ;; camcol IN (...) is actually SLOWER than doing the individual queries,
      ;; but still way faster than an OR on the camcols.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      where_statements = $
        "run = "+ntostr(runs)+" AND "+$
        "rerun = "+ntostr(reruns)+" AND "+$
        "camcol = "+ntostr(camcols)
      where_statements = where_statements[ rem_dup(where_statements) ]

  ENDELSE 
  struct = self->send_queries(table, where_statements, $
                              columns=columns, $
                              clauses=clauses, $
                              connect_info=connect_info, $
                              slow=slow, $
                              verbose=verbose, $
                              status=status)
  return,struct


END 



;docstart::sdss_postgres::read_runs
;
; NAME:
;  postgres::read_runs
;
; PURPOSE:
;
;  Read data for an entire SDSS run or set of runs from an postgres table.  The
;  more generic function sdss_postgres::readbyid() should usually be used; it
;  will call this function for the right inputs.
;  
;
; CALLING SEQUENCE:
;
;  res = sp->read_runs(table, runs, $
;                      reruns=, $
;                      columns=, $
;                      clauses=, $
;                      connect_info=, $
;                      /slow, $
;                      /verbose=, $
;                      status=)
;
; INPUTS:
;  table: The tablename in the current database or that specified through
;     connect_info
;  runs: Restrict the results to a specific set of runs.
; 
; OPTIONAL KEYWORD INPUTS:
;  reruns: Must match the above run inputs in dimensions. If not sent, 
;     the result of sdss_files::rerun() is used.
;  columns: Which columns to read from the table. Defaults to all ('*').
;  clauses: Added restrictions on query. e.g. "modelflux[2] > 200000.0'
;  connect_info:  List of connection options separated by semicolon.
;     e.g. "user=somename;dbname=somename"
;     http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;  /slow:  Use a slower, more memory efficient method of retrieving multiple
;     queries.  Can be useful for example if reading from 10 different runs,
;     but is somewhat slower.
;  /verbose: pgsql_query() will print some informationl messges.
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_postgres::read_runs


FUNCTION sdss_postgres::read_runs, table, runs, $
                      reruns=reruns_in, minrerun=minrerun, $
                      columns=columns, $
                      clauses=clauses, $
                      connect_info=connect_info, $
                      slow=slow, $
                      verbose=verbose, $
                      status=status

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: struct = sp->read_runs(table, runs, camcols, '
      print,'                                reruns=, '
      print,'                                columns=, clauses=, '
      print,'                                connect_info=, '
      print,'                                /slow, /verbose, status=)'
      print
      message,'Halting'
  ENDIF 


  status=1

  ;; always returns same length as runs
  reruns = $
    self->_get_reruns(runs, reruns=reruns_in, minrerun=minrerun)

  nruns = n_elements(runs)


  ;; Get unique runs/reruns
  ten = ulong64(10)
  tid = ulong64(reruns)*ten^12 + ulong64(runs)*ten^15
  rmd = rem_dup(tid)
  Uruns = runs[rmd]
  Ureruns = reruns[rmd]

  ;; Will test whether between is faster than
  ;; specifying run and rerun

  where_statements = $
    "run = "+ntostr(Uruns)+" AND "+$
    "rerun = "+ntostr(Ureruns)


  struct = self->send_queries(table, where_statements, $
                              columns=columns, $
                              clauses=clauses, $
                              connect_info=connect_info, $
                              slow=slow, $
                              verbose=verbose, $
                              status=status)

  return,struct

END 


FUNCTION sdss_postgres::_get_reruns, runs, reruns=reruns, minrerun=minrerun

  nruns = n_elements(runs)
  nreruns = n_elements(reruns)
  IF nreruns EQ 0 THEN BEGIN 
      use_reruns = self->sdss_files::rerun(runs, min=minrerun) 
      IF use_reruns[0] EQ -1 THEN BEGIN 
          message,'Halting'
      ENDIF 
      return, use_reruns
  ENDIF ELSE BEGIN 
      IF nreruns NE nruns THEN BEGIN 
          IF nreruns EQ 1 THEN BEGIN 
              return, replicate(reruns[0], nruns)
          ENDIF ELSE BEGIN 
              message,'reruns must be same size as runs or scalar'
          ENDELSE 
      ENDIF ELSE BEGIN 
          return, reruns
      ENDELSE 
  ENDELSE 

END 

;docstart::sdss_postgres::readbyid
;
; NAME:
;  postgres::readbyid
;
; PURPOSE:
;  Provides a simple way to read froma postgres table by SDSS id. The
;  user can specify runs or runs,camcols .... runs,camcols,fields.ids.
;  
;
; CALLING SEQUENCE:
;
;  res = sp->readbyid(table, runs, camcols, fields, ids, $
;                     reruns=, $
;                     columns=, $
;                     clauses=, $
;                     connect_info=, $
;                     /slow, $
;                     /verbose=, $
;                     status=)
;
;  -- OR --
; res = sp->readbyid(table, struct, ....)
;
; INPUTS:
;  table: The tablename in the current database or that specified through
;     connect_info
;  runs: Restrict the results to a specific set of runs.
;  camcols: Optional.  Restrict the query to a given set of runs,camcols.
;     There are two ways to specify these ids: 
;        * A single run with multiple camcols 
;        * Multiple runs and camcols.  For multiple runs, the user must 
;          specify *pairs* of runs and camcols to avoid confusion.
;  fields:  Optional. Restrict the query to a given set of runs,camcols,fields.
;     There are three ways to specify these ids:
;        * A single run,camcol,field set.
;        * A single run,camcol but multiple fields.
;        * Mutiple runs,camcols as well as fields.  The arrays of equal length.
;  ids: Optional. Restrict to a given set of runs,reruns,camcols,fields.ids.
;     In this case all entries must be arrays of the same length. This 
;     function converts the inputs to photoids and calls ::read_photoids()
;
;  struct: If the second input is a structure, then id information si
;     looked for and used to retrieve rows.  It is assumed a that info
;     run,camcol,field,id are to be used. I may add the ability to use
;     just certain info in the future.
; 
; OPTIONAL KEYWORD INPUTS:
;  reruns: Must match the above run inputs in dimensions. If not sent, 
;     the result of sdss_files::rerun() is used.
;  columns: Which columns to read from the table. Defaults to all ('*').
;  clauses: Added restrictions on query. e.g. "modelflux[2] > 200000.0'
;  connect_info:  List of connection options separated by semicolon.
;     e.g. "user=somename;dbname=somename"
;     http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;  /slow:  Use a slower, more memory efficient method of retrieving multiple
;     queries.  Can be useful for example if reading from 10 different runs,
;     but is somewhat slower.
;  /verbose: pgsql_query() will print some informationl messges.
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_postgres::readbyid

FUNCTION sdss_postgres::readbyid, $
                      table, $
                      runs, camcols, fields, ids, $
                      reruns=reruns, minrerun=minrerun, $
                      columns=columns, $
                      clauses=clauses, $
                      connect_info=connect_info, $
                      slow=slow, $
                      verbose=verbose, $
                      status=status
                      

  np = n_params()
  IF np LT 2 THEN BEGIN 
      print,'st = sp->readbyid(table, runs, camcols, fields, ids, '
      print,'                  reruns=, '
      print,'                  columns=, clauses=, '
      print,'                  connect_info='
      print,'                  /slow, /verbose, status=)'
      print,'The second argument may also be a structure containing '
      print,'SDSS id info.'
      return,-1
  ENDIF 


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Make calls depending on the entered id information
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF np EQ 2 AND size(runs,/tname) EQ 'STRUCT' THEN BEGIN 
      struct = self->read_photoids(table, runs, $
                                   columns=columns, $
                                   clauses=clauses, $
                                   connect_info=connect_info, $
                                   slow=slow, $
                                   verbose=verbose, $
                                   status=status)
      return, struct
  ENDIF 

  IF np EQ 2 THEN BEGIN 
      ;; The user wants to get entire runs
      struct = self->read_runs(table, runs, $
                               reruns=reruns, minrerun=minrerun, $
                               columns=columns, $
                               clauses=clauses, $
                               connect_info=connect_info, $
                               slow=slow, $
                               verbose=verbose, $
                               status=status)
      return,struct
  ENDIF 

  IF np EQ 3 THEN BEGIN 
      ;; The user wants to get entire runs,camcols
      struct = self->read_camcols(table, runs, camcols, $
                                  reruns=reruns, minrerun=minrerun, $
                                  columns=columns, $
                                  clauses=clauses, $
                                  connect_info=connect_info, $
                                  slow=slow, $
                                  verbose=verbose, $
                                  status=status)
      return,struct
  ENDIF 

  IF np EQ 4 THEN BEGIN 
      ;; The user wants individual runs,camcols,fields
      struct = self->read_fields(table, runs, camcols, fields, $
                                 reruns=reruns, minrerun=minrerun, $
                                 columns=columns, $
                                 clauses=clauses, $
                                 connect_info=connect_info, $
                                 slow=slow, $
                                 verbose=verbose, $
                                 status=status)
      return,struct
  ENDIF 

  IF np EQ 5 THEN BEGIN 
      ;; The user wants individual objects
      struct = self->read_ids(table, runs, camcols, fields, ids, $
                              reruns=reruns, minrerun=minrerun, $
                              columns=columns, $
                              clauses=clauses, $
                              connect_info=connect_info, $
                              slow=slow, $
                              verbose=verbose, $
                              status=status)
      return,struct
  ENDIF 

END 

PRO sdss_postgres__define

  struct = { sdss_postgres, $
             pgdummy: 0, $
             INHERITS postgres, $
             INHERITS sdss $
           }

END 

;+
; NAME:
;  POSTGRES   (IDL Class file)
;
;
; PURPOSE:
;  An IDL class file wrapping the pgsql_query() function, which provides
;  an interface to the postgres database.
;
; CALLING SEQUENCE:
;  pg = obj_new('postgres', connect_info=)
;
; OPTIONAL INPUTS:
;  connect_info: This is stored at initialization so can be used later
;    without sending. Useful if not using the database listed in the
;    PGDATABASE environment variable for example.
;
; METHODS:
;  All the functionality comes from the pgsql_query() function.  This
;  class provides some wrapper methods for complex but often-used queries.
;  Use:
;          methods,'postgres' 
;
;  to list the methods and 
;          doc_method,'postgres::methodname' 
;
;  to see the full documentation for each. Use 
;          doc_method,'postgres',/class  (or doc_library,'postgres__define')
;  to see this doc.
; 
;
;  The most useful methods:
;    ::query()
;       Send a query and return the results.  There is also a procedural
;           interface ::query useful when no results are returned.
;
;    ::struct2table: Stuff a structure into a postgres table, creating a
;       new table if necessary.  The input structure is written to disk using
;       the postgres binary copy format unless /ascii is sent.
;
;    ::tables()
;       Return a list of tables in the database.
;    ::table_exists() 
;       Return 1 if the table exists, 0 if not.
;    ::describe
;       Print a description of a table or, if no arguments, short descriptions 
;       of all tables.
;    ::table_indexes 
;       Print index information for a table.
;
;    ::status_val()
;       Return the query status value given the name
;    ::status_name()
;       Return status name given the value
;
;    ::tablerows()
;       Postgres does not store the number of rows for a table, it counts them
;       each time you run select count(*) from table....  If this info has been
;       stored in a table called tablename_meta it is retrieved, otherwise an
;       error is given and execution is stopped.
;
; RESTRICTIONS:
;
;
;
; MODIFICATION HISTORY:
;   Created: Mid-2005, Erin Sheldon, Uchicago
;   See also individual methods for changes.
;
;   Now uses binary file for the COPY command, used to load data into the
;   database. Support for multi-dimensional arrays via updates to pgsql_query().
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

function postgres::init, connect_info=connect_info

  funcnames  = routine_info(/system,/functions)

  w = where(funcNames eq 'PGSQL_QUERY',nw)
  if nw eq 0 then begin
      message,'The postgres library PGSQL_QUERY() is not available',/inf
      message,'See the SDSSIDL README file for help on compilation',/inf
      return,0
  endif


  self->set_parameters, $
    connect_info=connect_info, query_status=-1, nrows=0
  return,1
end 

pro postgres::set_parameters, connect_info=connect_info, query_status=query_status, nrows=nrows

  if n_elements(connect_info) ne 0 then begin 
      self.connect_info = connect_info 
  endif 
  if n_elements(query_status) ne 0 then begin 
      self.query_status = query_status
  endif 
  if n_elements(nrows) ne 0 then begin 
      self.nrows = nrows
  endif 

end 

;docstart::postgres::query
;
; NAME:
;  postgres::query()
;
; PURPOSE:
;  Simple wrapper for pgsql_query().  Useful if the user has inherited the
;  class and wants to store certain things internally, such as status,
;  nrows. Also, can store connect_info at initialization of this class so you
;  don't have to send it each time.  This is useful if not connecting to
;  alternative databases from PGDATABASE, or as another user.  
;
;  Finally, better to use this than the pgsql_query() function directly in 
;  your code because of a bug in IDL that it gives a compile error if that
;  C function has not been linked, which can be confusing.
;
; CALLING SEQUENCE:
;
;  res = pg->query(query, nrows=, connect_info=, file=, /append, /nointerrupt,
;                  /verbose, status=)
;
; INPUTS:
;  The query in string form.
; 
; OPTIONAL INPUTS:
;  connect_info:  List of connection options separated by semicolon.
;        e.g. "user=somename;dbname=somename"
;       http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;  file=: File into which the result will be written.
;  /append: Append the file.
;  /nointerrupt: Normally pgsql_query() runs a busy loop waiting for results.
;      This loop checks if control-c has been sent and if so, stops the
;      query and returns no data.  If /nointerrupt is sent then no such busy
;      loop is run.  This saves CPU if, for example, little results are being
;      returned from a long query.  Also good for batch mode when an interrupt
;      cannot be sent anyway.  The query cannot be killed however without
;      help from the administrator.
;  /verbose: pgsql_query() will print some informationl messges.
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::postgres::query

function postgres::query, query, nrows=nrows, connect_info=connect_info, file=file, append=append, nointerrupt=nointerrupt, verbose=verbose, status=status
  
  self->reset

  nq = n_elements(query)
  if nq eq 0 then begin 
      print,'-Syntax: res=pg->query(query_string, nrows=, connect_info=, file=, /append, /nointerrupt, /verbose, status=)'
      print
      message,'halting'
  endif 

  if n_elements(connect_info) eq 0 then begin 
      connect_info = self.connect_info
  endif 

  ;; Now send the query. IDL has a bug that it gives a compile
  ;; error if the pgsql_query() function was not linked (that
  ;; doesn't happen for procedures).  So we use the CALL_FUNCTION
  ;; procedure to avoid this bug.

  struct = CALL_FUNCTION('pgsql_query', $
                         query, $
                         nrows=nrows, $
                         connect_info=connect_info, $
                         file=file, append=append, $
                         nointerrupt=nointerrupt, $
                         verbose=verbose, status=status)
  
  self->set_parameters, query_status=status, nrows=nrows

  return,struct
end 

pro postgres::query, query, result=result, nrows=nrows, connect_info=connect_info, file=file, append=append, nointerrupt=nointerrupt, verbose=verbose, status=status

  on_error, 2
  nq = n_elements(query)
  if nq eq 0 then begin 
      print,'-Syntax: pg->query, query_string, result=, nrows=, connect_info=, file=, /append, /nointerrupt, /verbose, status='
      print
      print,'This is the procedural version of the query() method'
      print,'  useful if no results are returned.  If you expect results,'
      print,'  you should use the functional form res=pg->query() for '
      print,'  readability'
      print
      message,'Halting'
  endif 

  result = self->query(query, $
                       nrows=nrows, $
                       connect_info=connect_info, $
                       file=file, $
                       append=append, $
                       nointerrupt=nointerrupt, $
                       verbose=verbose, status=status)
end 

;docstart::postgres::status_name
;
; NAME:
;  postgres::status_name()
;
; PURPOSE:
;  Return the status name associated with the value.
;
; CALLING SEQUENCE:
;  print, pg->status_name(status)
;
; INPUTS:
;  The status returned from pgsql_query()
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::postgres::status_name

function postgres::status_name, status_val
  case status_val of
      0: return,'success'
      1: return,'connect_failure'
      2: return,'no_result'
      3: return,'write_failure'
      4: return,'fatal_error'
      else: message,'unknown status value: '+ntostr(status_val)
  endcase 
end 

;docstart::postgres::status_val
;
; NAME:
;  postgres::status_val()
;
; PURPOSE:
;  Return the status val associated with the name.
;
; CALLING SEQUENCE:
;  if status ne pg->status_val(status_name) then .....
;
; INPUTS:
;  The name of the status. One of:
;    success, connect_failure, no_result, write_failure, fatal_error
;  
; OUTPUTS:
;      'success': return,0
;      'connect_failure': return,1
;      'no_result': return,2
;      'write_failure':return,3
;      'fatal_error': return,4
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::postgres::status_val

function postgres::status_val, status_name

  case strlowcase(status_name) of
      'success': return,0
      'connect_failure': return,1
      'no_result': return,2
      'write_failure':return,3
      'fatal_error': return,4
      else: message,'unknown status name: ',status_name
  endcase 

end 

function postgres::connect_info
  return,self.connect_info
end 

function postgres::query_status
  return,self.query_status
end 

function postgres::nrows
  return,self.nrows
end 

;docstart::postgres::tables
;
; NAME:
;  postgres::tables(/all)
;
; PURPOSE:
;  Return a listing of the tables in the current database, or that sent
;  by connect_info.  By default returns the "public" tables.  If /all is 
;  sent then all are returned.
;
; CALLING SEQUENCE:
;  tablenames = pg->tables(connect_info=, /all, /struct, status=)
;
; INPUTS:
;  tablename:  The name of the table.
;
; OPTIONAL INPUTS:
;  /all: Return a list of all tables, not just public.
;  /struct: Instead of returning only the names, return a structure with
;    lots of info.
;  connect_info:  List of connection options separated by semicolon.
;        e.g. "user=somename;dbname=somename"
;   http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;
; OUTPUTS:
;  The listing of tables in string array.
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: 2006-05-19  Erin Sheldon NYU
;
;docend::postgres::tables

function postgres::tables, struct=struct, connect_info=connect_info, all=all, status=status

  query = "select * from pg_tables"
  if not keyword_set(all) then begin 
      query = query + " where schemaname = 'public'"
  endif 

  res = self->query(query, connect_info=connect_info, status=status)
  if status eq self->status_val('success') then begin 
      if keyword_set(struct) then begin 
          return, res
      endif else begin 
          return, res.tablename
      endelse 
  endif else if status eq self->status_val('no_result') then begin 
      if not keyword_set(all) then begin 
          message,'No public tables',/inf
      endif else begin 
          message,'No tables returned',/inf
      endelse 
      return, -1
  endif else begin 
      message,'Failed to query database for table names',/inf
      return, -1
  endelse 
end 




;docstart::postgres::table_exists
;
; NAME:
;  postgres::table_exists()
;
; PURPOSE:
;  Check if the table exists in the current database, or
;    the one specified by connect_info
;
; CALLING SEQUENCE:
;  if pg->table_exists(tablename, connect_info=, status=st) then .....
;
; INPUTS:
;  tablename:  The name of the table.
;
; OPTIONAL INPUTS:
;  connect_info:  List of connection options separated by semicolon.
;        e.g. "user=somename;dbname=somename"
;   http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: 2006-05-19  Erin Sheldon NYU
;
;docend::postgres::table_exists


function postgres::table_exists, tablename, connect_info=connect_info, status=status

  status = 1
  ntab = n_elements(tablename)
  if ntab eq 0 then begin 
      print,'-Syntax: if pg->table_exists(tablename,connect_info=, status=) then ..'
      print
      message,'Halting'
  endif 

  tablenames = self->tables(connect_info=connect_info, status=status)
  if status ne self->status_val('success') then begin 
      return, 0
  endif  

  match, strlowcase(tablenames), strlowcase(tablename), mall, min
  if mall[0] eq -1 then return,0 else return,1

end 



;docstart::postgres::table_indexes
;
; NAME:
;  postgres::table_indexes
;
; PURPOSE:
;  Print out index info for a table.
;
; CALLING SEQUENCE:
;  if pg->table_indexes, tablename, struct=, connect_info=, status=
;
; INPUTS:
;  tablename:  The name of the table.
;
; OPTIONAL INPUTS:
;  connect_info:  List of connection options separated by semicolon.
;        e.g. "user=somename;dbname=somename"
;   http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;
; OPTIONAL OUTPUTS:
;  struct=: The structure for the index descriptions.
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: 2006-05-19  Erin Sheldon NYU
;
;docend::postgres::table_indexes

pro postgres::table_indexes, tablename, struct=struct, connect_info=connect_info, status=status

  tname = strlowcase( strtrim(string( tablename[0] ), 2) )
  if not self->table_exists(tablename) then begin 
      message,'No table called '+tname,/inf
      return
  endif 

  ;; Get the oid for this table
  query = $
    "SELECT " +$
    "  c.oid, "+$
    "  n.nspname, "+$
    "  c.relname "+$
    "FROM "+$
    "  pg_catalog.pg_class c "+$
    "LEFT JOIN "+$
    "  pg_catalog.pg_namespace n ON n.oid = c.relnamespace "+$
    "WHERE "+$
    "  pg_catalog.pg_table_is_visible(c.oid) AND c.relname ~ '^"+tname+"$' "+$
    "ORDER BY 2, 3"

  
  oid_struct = self->query(query, connect_info=connect_info, status=status)
  if status ne self->status_val('success') then begin 
;      message,"could not retrieve oid for table '"+tname+"'",/inf
      return
  endif 
  if self->nrows() ne 1 then begin 
      message,'More than one row returned.  That should not happen'
  endif 
  oidstr = ntostr( oid_struct.oid )


  ;; Now that we have the oid, we can get the index list
  query = $
    "SELECT "                                 +$
    "  c2.relname as index, "                 +$
    "  i.indisprimary as primary, "           +$
    "  i.indisunique as unique, "             +$
    "  i.indisclustered as clustered, "       +$
    "  pg_catalog.pg_get_indexdef(i.indexrelid, 0, true) as indexdef "+$
    "FROM "                                   +$
    "  pg_catalog.pg_class c, "               +$
    "  pg_catalog.pg_class c2, "              +$
    "  pg_catalog.pg_index i "                +$
    "WHERE "                                  +$
    "  c.oid = '"+oidstr+"' AND "             +$
    "  c.oid = i.indrelid AND "               +$
    "  i.indexrelid = c2.oid "                +$
    "ORDER BY "+$
    "  i.indisprimary DESC, i.indisunique DESC, c2.relname"

  struct = self->query(query, connect_info=connect_info, status=status)
  if status ne self->status_val('success') then begin 
;      message,"could not retrieve indexes for table '"+tname+"'",/inf
      return
  endif 

  ;; print the output

  nindex = n_elements(struct)
  idesc = strarr(nindex)


  for i=0l, nindex-1 do begin 
      def = struct[i].indexdef

      if struct[i].primary then begin 
          idesc[i] = 'PRIMARY KEY, '
      endif else if stregex(def, 'UNIQUE', /bool) then begin 
          idesc[i] = 'UNIQUE KEY, '
      endif 

      iuse = stregex(def, 'USING')
      if iuse ne -1 then begin 
          idesc[i] = idesc[i] + strmid(def, iuse+6)
      endif 

  endfor 

  maxrel = max( strlen( struct.index ) ) 
  maxdef = max( strlen( idesc ) ) 

  
  padding = 3
  width = maxrel+maxdef+2*padding

  divider = mkstr(width+2, val='-')

  relformat = 'A'+ntostr(maxrel+padding)
  defformat = 'A'+ntostr(maxdef+padding)
  format = '('+relformat+','+defformat+')'
  
  print,"Indexes for '"+tname+"'"
  print
  print, 'index', 'description', format=format
  print,divider

  for i=0l, nindex-1 do begin 
      print,$
        struct[i].index, idesc[i], $
        format=format
  endfor 

end 


;docstart::postgres::describe
;
; NAME:
;  postgres::describe
;
; PURPOSE:
;  Print a description either the tables in the database or, if a tablename
;  is sent, a description ofthe table layout. In that case it runs
;       select * from tablename limit 1
;  and then help, result, /struct on the result.  The structure can be
;  returned via the keyword.
;
; CALLING SEQUENCE:
;  if pg->describe, [tablename, struct=, /all, connect_info=, status=]
;
; INPUTS:
;  If no inputs, a short description of all public tables is displayed.
;  If /all, the other tables are also described.
;
;  tablename:  The name of the table.
;
; OPTIONAL INPUTS:
;  /all: Return a list of all tables, not just public.
;  connect_info:  List of connection options separated by semicolon.
;        e.g. "user=somename;dbname=somename"
;   http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;
; OPTIONAL OUTPUTS:
;  struct=: The structure containing the description.
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: 2006-05-19  Erin Sheldon NYU
;
;docend::postgres::describe_table

pro postgres::describe, tablename, struct=struct, all=all, connect_info=connect_info, status=status


  if n_elements(tablename) ne 0 then begin 
      tname = strtrim(string(tablename[0]), 2)
      query = "select * from "+tname+" limit 1"

      struct = self->query(query, connect_info=connect_info, status=status)
      if status eq self->status_val('success') then begin 
          help,struct,/str
          self->table_indexes, tname
      endif  
  endif else begin 
      struct = self->tables(all=all, /struct, status=status)
      if status eq self->status_val('success') then begin 
          print_struct, struct
      endif  
  endelse 

end 




;docstart::postgres::tablerows
;
; NAME:
;  postgres::tablerows()
;
; PURPOSE:
;  If the metatable exists for the input tablename, return the number
;  of listed rows.
;
; CALLING SEQUENCE:
;  nrows = pg->tablerows(tablename, connect_info=, status=st) 
;
; INPUTS:
;  tablename:  The name of the table.
;
; OPTIONAL INPUTS:
;  connect_info:  List of connection options separated by semicolon.
;        e.g. "user=somename;dbname=somename"
;   http://www.postgresql.org/docs/8.0/interactive/libpq.html#LIBPQ-CONNECT
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; MODIFICATION HISTORY:
;  Created: 2006-05-19  Erin Sheldon NYU
;
;docend::postgres::tablerows

function postgres::tablerows, tablename, connect_info=connect_info, status=status

  metatable = tablename + '_meta'
  query = 'select * from '+metatable
  res = self->query(query, connect_info=connect_info, status=status)
  if status ne self->status_val('success') then begin 
      return, -1
  endif else begin 
      if tag_exist(res, 'nrows') then begin 
          tablerows = res.nrows
          ntr = n_elements(tablerows) 
          return, tablerows[ntr-1]
      endif else begin 
          message,'NROWS does not exist in table '+metatable,/inf
          return, -1
      endelse 
  endelse 

end 




;docstart::postgres::postgres_type
;
; NAME:
;  postgres::postgres_type()
;
;
; PURPOSE:
;  Method of the postgres class to convert an idl type description to a
;  postgres type description.  The idl type description is that returned
;  from the size(var, /tname) call.
;
; CALLING SEQUENCE:
;  pgtype = pg->postgres_type(idltype)
;
; INPUTS:
;  struct: An IDL type description.
;
; OUTPUTS:
;  A string containing the postgres type name.
;
; RESTRICTIONS:
;  Currently COMPLEX, STRUCTURE, and POINTER are not supported.
;
; EXAMPLE:
;  IDL> tn = size(var, /tname)
;  IDL> pg = obj_new('posgres')
;  IDL> pgtype = pg->postgres_type(tn)
;
; MODIFICATION HISTORY:
;  Created: Some time mid 2005, Erin Sheldon, UChicago
;  All strings are type text now.  No performance difference in postgresql
;   so just use text.  E.S.
;
;docend::postgres::postgres_type


function postgres::postgres_type, tname, length=length

  if n_params() lt 1 then begin 
      print,'-Syntax: pgtyep = obj->postgres_type(idl_type_name)'
      return,''
  endif 

  ;; Because there are no unsigned types in postgres, we must
  ;; convert uint to long, ulong to long64, etc
  ;; There may be issues with the ULONG64 since it is not supported
  ;; by postgres
  umess = 'Postgres does not support unsigned integer types.  '+$
      'Will abort rather than attempt conversion.'
  case strupcase(tname) of 
      ;'BYTE':    return,'CHAR(1)'
      'BYTE':    message,'Postgres does not support type BYTE'
      'INT':     return,'SMALLINT'
      'UINT':    message,umess
      'LONG':    return,'INT'
      'ULONG':   message,umess
      'LONG64':  return,'BIGINT'
      'ULONG64': message,umess
      'FLOAT':   return,'REAL'
      'DOUBLE':  return,'DOUBLE PRECISION'
      'STRING':  return,'TEXT'
  endcase 

end 

;docstart::postgres::struct2coldefs
;
; NAME:
;  postgres::struct2coldefs()
;
;
; PURPOSE:
;  Method of the postgres class to convert a structure into a list SQL type
;  definitions for each tag.
;
; CALLING SEQUENCE:
;  tdef=pg->struct2coldefs(struct, tags=, /verbose)
;
;
; INPUTS:
;  struct: A structure.
;
; KEYWORD PARAMETERS:
;   tags=: A list of tags from the structure. Default is to use all.
;   /verbose: Print out the tag definitions.
;
; OUTPUTS:
;   A string array containing the column definitions.
;
; EXAMPLE:
;   IDL> struct = mrdfits('some_fits_file.fits', 1)
;   IDL> pg = obj_new('posgres')
;   IDL> cdef = pg->struct2coldefs(struct)
;
; MODIFICATION HISTORY:
;   Created: Some time mid 2005, Erin Sheldon, UChicago
;
;docend::postgres::struct2coldefs

function postgres::struct2coldefs, struct, varchar=varchar, tags=tags, verbose=verbose, ascii=ascii

  if n_params() lt 1 then begin 
      print,'-Syntax: coldefs = obj->struct2coldefs(struct, tags=, /verbose)'
      return,''
  endif 

  ;; generate entries in a pgsql create table statement from the input
  ;; structure. 

  tags = strlowcase( tag_names(struct) )
  ntags = n_elements(tags)

  for i=0l, ntags-1 do begin 

      tmpvar = struct[0].(i)
      nelem = n_elements(tmpvar)

      tn = size(tmpvar,/tname)

      pgsql_type = self->postgres_type(tn)

      ndim=size(tmpvar,/n_dim)
      if ndim gt 1 and keyword_set(ascii) then message,'ndim > 1 not supported for ascii input files.  Use binary (the default)'
      if ndim gt 0 then begin
          ; this will support multi-dimen when we can support it in
          ; ascii_write.  Is supported in binary mode already
          dims = size(tmpvar,/dim)
          dimstr = strjoin(ntostr(dims), '][')
          pgsql_type = pgsql_type + '['+dimstr+']'
      endif
          
      coldef = tags[i] + ' '+pgsql_type
      if tn ne 'STRING' then coldef = coldef + ' NOT NULL'

      if keyword_set(verbose) then begin 
          print,tags[i],' '+tn
          print,' |--> '+coldef
      endif 

      add_arrval, coldef, coldefs

  endfor 

  return,coldefs

end 


;docstart::postgres::struct2tabledef
;
; NAME:
;  postgres::struct2tabledef()
;
;
; PURPOSE:
;  Method of the postgres class to convert a structure into an SQL
;  CREATE TABLE statement.  Allows defining of the primary key.
;
; CALLING SEQUENCE:
;  tdef=pg->struct2tabledef(struct, tablename, primary_key=, file=)
;
;
; INPUTS:
;  struct: A structure.
;  tablename:  The name of the table into which the data will be stuffed.
;
;
; KEYWORD PARAMETERS:
;  primary_key: The tag to be used as the primary key.
;  file: A file to write the create table statement to.
;
; OUTPUTS:
;  A string containing the CREATE TABLE statement.
;
; EXAMPLE:
;  IDL> struct = mrdfits('some_fits_file.fits', 1)
;  IDL> pg = obj_new('posgres')
;  IDL> tdef = pg->struct2tabledef(struct, 'newtable', primary_key='index')
;
; MODIFICATION HISTORY:
;  Created: Some time mid 2005, Erin Sheldon, UChicago
;
;docend::postgres::struct2tabledef

function postgres::struct2tabledef, struct, tablename, varchar=varchar, file=file, primary_key=primary_key, ascii=ascii

  on_error, 2
  if n_elements(struct) eq 0 or n_elements(tablename) eq 0 then begin 
      print,'-Syntax: pg->struct2tabledef, struct, tablename, file=, primary_key='
      print
      message,'Halting'
  endif 

  if n_elements(file) eq 0 then begin 
      lun = -1
  endif else begin 
      openw,lun,file,/get_lun
  endelse 

  coldefs = self->struct2coldefs(struct, ascii=ascii)

  tags = tag_names(struct)
  

  ;; add the primary key
  if n_elements(primary_key) eq 1 and $
    size(primary_key, /tname) EQ 'STRING' then begin 
      
      if tag_exist(struct, primary_key) then begin 
          coldefs = [coldefs, 'PRIMARY KEY ('+primary_key+')']
      endif else begin    
          message,'tag '+primary_key+' does not exist',/inf
          print,tag_names(struct)
      endelse 
  endif 

  tabledef = $
    'CREATE TABLE '+tablename + ' '+$
    '('+strjoin(coldefs,', ')+')'
  

  ncoldefs = n_elements(coldefs)
  printf,lun,'CREATE TABLE '+tablename
  printf,lun,'('
  for i=0l, ncoldefs-2 do begin 
      printf,lun,coldefs[i]+', '
  endfor 
  printf,lun,coldefs[i]
  printf,lun,');'

  if n_elements(file) ne 0 then free_lun,lun

  return, tabledef


end 


;docstart::postgres::struct2table
;
; NAME:
;  postgres::struct2table  
;
;
; PURPOSE:
;  Method of the postgres class to write a structure into a postgres
;  database.
;
; CALLING SEQUENCE:
;  pg->struct2table, struct, tablename, primary_key=, connect_info=,
;                    tmpdir=, status=, /ascii
;
;
; INPUTS:
;  struct: A structure.  May be an array.
;  tablename:  The name of the table into which the data will be stuffed.
;
;
; KEYWORD PARAMETERS:
;   primary_key: The tag to be used as the primary key.  Only used if the
;       table is created.
;   connect_info: Infor used for connecting. This is the standard string
;       sent to postgres clients: e.g. 
;                  "user=username;password=pass;host=hostname;"
;       If not sent, this info is gotten from the environment variables.
;       and ~/.pgpass file.
;   /ascii:  Use an ascii file to instead of the postgres binary file.  Unlike
;       the binary format, does not support multi-dimensional arrays in the 
;       structure.  Also requires the DLM ascii_write to be linked.
;   tmpdir: Directory to write the temporary postgres input file.
;
;
; OPTIONAL OUTPUTS:
;  status: The status of the query.  See the ::status_val() method for
;    the meaning of this output.
;
; SIDE EFFECTS:
;  Data is stuffed into the table. The table if non-existent will be
;  created if the permissions are sufficient.
;
;
; RESTRICTIONS:
;  The user must have the postgres password info in their ~/.pgpass file.
;  This is becuase the COPY command may only be run as the postgres
;  user.
;
; EXAMPLE:
;  IDL> struct = mrdfits('some_fits_file.fits', 1)
;  IDL> pg = obj_new('posgres')
;  IDL> pg->struct2table, struct, 'newtable'
;
; MODIFICATION HISTORY:
;   Created: Some time mid 2005, Erin Sheldon, UChicago
;   Converted to using binary file for copy.
;       Multi-dimensional arrays now supported.  2008-03-31, E.S.
;
;docend::postgres::struct2table

pro postgres::struct2table, struct, tablename, primary_key=primary_key, varchar=varchar, status=status, tmpdir=tmpdir, connect_info=connect_info, createonly=createonly, ascii=ascii, noremove=noremove

  status = 1
  on_error, 2
  if n_elements(struct) eq 0 or n_elements(tablename) eq 0 then begin 
      print,'-Syntax: pg->struct2table, struct, tablename, primary_key=, '+$
        'tmpdir=, connect_info=, createonly=creatonly, status='
      print
      message,'Halting'
  endif 

  ;; if the table already exists, no need to create a table 
  ;; definition.

  texist = self->table_exists(tablename)
  if texist and keyword_set(createonly) then begin 
      message,'/createonly set but table already exists',/inf
      message,'Nothing done',/inf
      status = 0
      return
  endif 

  if not texist then begin 

      message,'Creating table definition',/inf
      ;; WE will generate the table
      tabledef = self->struct2tabledef(struct, tablename, $
                                       primary_key=primary_key, $
                                       ascii=ascii)

      ;; Create the table
      message,'Creating table',/inf
      self->query, tabledef, status=qstatus, connect_info=connect_info
      
      if qstatus ne self->status_val('no_result') then begin 
          message,'Could not create table',/inf
          return
      endif 

      if keyword_set(createonly) then begin 
          status=0
          return
      endif 

  endif 

  ;; now write the input file.
  if n_elements(tmpdir) eq 0 then tmpdir = '~'
  tmpdir = expand_tilde(tmpdir)

  file = tmpfile(prefix=tablename+'-stuff-')+'.pgsql'
  file = concat_dir(tmpdir, file)
  
  message,'Writing input file: '+file,/inf
  tm=systime(1)
  self->input_write, struct, file, ascii=ascii, status=wstatus
  print,'Write time: ',systime(1)-tm
  if wstatus ne 0 then begin 
      message,'Could not write postgres input file'
      return
  endif 

  if not fexist(file) then begin 
      message,'Cannot find the file in /tmp'
  endif 

  ;; Stuff the table.  Must do this as postgres; this is
  ;; a security hole if no password protection is set up
  ;; for the postgres account.

  if not keyword_set(ascii) then begin
      query = "COPY BINARY "+tablename+" FROM '"+file+"'"
  endif else begin
      query = "COPY "+tablename+" FROM '"+file+"'"
  endelse
  message,query,/inf
  tm=systime(1)
  self->query, query, connect_info='user=postgres', status = stuff_status
  print,'Copy time: ',systime(1)-tm
  if stuff_status ne self->status_val('no_result') then begin 
      message,'Failed to stuff file',/inf
  endif else begin 
      if not keyword_set(noremove) then file_delete, file, /quiet
      status = 0
  endelse 



  return

end 





; does not support multi-column indexes
pro postgres::create_index, table, columns, connect_info=connect_info

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: pg->create_index, table, columns, connect_info='
        print
        message,'Halting'
    endif 

    ;; only trick here is dealing with arrays
    ncols = n_elements(columns)
    cstr = strarr(ncols)
    inames = strarr(ncols)
    for i=0l, ncols-1 do begin 

        if strmatch(columns[i], '*\[*') then begin 

            ;; remove the braces for index name
            tiname = repstr(columns[i], '[', '_')
            tiname = repstr(tiname, ']', '')

            inames[i] = tiname
            cstr[i] = '(('+columns[i]+'))'
        endif else begin 
            inames[i] = repstr(columns[i], ',', '_')
            cstr[i] = '('+columns[i]+')'
        endelse 

    endfor 

    create_index_arr = $
        'CREATE INDEX '+table+'_'+inames+'_index ON '+table+' '+cstr

    for i=0l, ncols-1 do begin 
        query = create_index_arr[i]
        print,query
        self->query, query, connect_info=connect_info, status=status
        if status ne self->postgres::status_val('no_result') then begin 
            message,'Error creating index'
        endif 
    endfor 

    self->query, 'analyze '+table, connect_info=connect_info

end 


pro postgres::create_metatable, table, connect_info=connect_info
    if n_elements(table) eq 0 then begin
        print,'-Syntax: p->create_metatable, tablename, connect_info='
        on_error, 2
        message,'Halting'
    endif

    metatable = table+'_meta'
    print
    print,'Creating metatable '+metatable+' with nrows'
 
    query = "CREATE TABLE "+metatable+" (nrows BIGINT, modified TIMESTAMP NOT NULL DEFAULT('now'::text)::timestamp(6))"
    self->query, query, conn=connect_info, status=status
    if status ne self->status_val('no_result') then message,'creation of metatable failed'

    query = 'INSERT INTO '+metatable+' (nrows) SELECT count(*) AS nrows FROM '+table
    print
    print,query
    self->query, query, conn=connect_info, status=status
    if status ne self->status_val('no_result') then message,'insert of nrows into metatable failed'
end



;; write a structure as a postgres input file
pro postgres::input_write, struct, file, ascii=ascii, status=status

    status = 1
    if n_params() lt 2 then begin 
        print,'-Syntax: pg->input_write, struct, file, status='
        return
    endif 
    if not keyword_set(ascii) then begin
        self->input_write_binary, struct, file, status=status
    endif else begin
        ascii_write, struct, file, /bracket_arrays, status=status
    endelse

end 





;
; Write a structure to a binary input file format
; This is kind of useless unless your data are purely signed since postgres
; only has signed data and we have to write it out as is.  Ascii is better
; in this sense as long as we don't go out of bounds
;

; return the field lenghts for each field(tag) in the structure
function postgres::wb_calc_array_totlen, ndim, nel, nbytes
    totlen = long( 2L*4L + 4L + 2L*ndim*4L + nel*4L + nel*nbytes )
    return, totlen
end
function postgres::wb_arrsig, type
    ; I don't know what these mean
    case type of
        1: sig=[0b,0b,0b,25b] ; byte is just a char(1)
        2: sig=[0b,0b,0b,21b] ;int (smallint)
        3: sig=[0b,0b,0b,23b] ; long (int)
        4: sig=[0b,0b,2b,188b] ; float (real)
        5: sig=[0b,0b,2b,189b] ; double (double precision)
        7: sig=[0b,0b,0b,25b]  ; string
        14: sig=[0b,0b,0b,20b] ; long64 (bigint)
        else: message,"Don't support type "+ntostr(type)+" yet"
    endcase
    return,sig
end



function postgres::wb_get_struct_field_info, struct

    ncolumns = n_tags(struct)
    maxdim=6
    field_info = {                  $
        maxdim:maxdim,              $
        ncolumns:fix(ncolumns),     $
        ntuples:n_elements(struct), $
        nbytes:lonarr(ncolumns),    $
        ndim:lonarr(ncolumns),      $
        dims:lonarr(ncolumns,maxdim), $
        nel:lonarr(ncolumns),       $
        totlen: lonarr(ncolumns),   $
        type: intarr(ncolumns),     $ 
        arrsig: bytarr(ncolumns,4), $

        bytesperrow: 0LL            $
    }

    ; first 2b for field count
    field_info.bytesperrow = 2
    for i=0L, field_info.ncolumns-1 do begin
        sstr = size(struct[0].(i), /struct)

        field_info.nbytes[i] = self->n_bytes(struct[0].(i)[0])
        field_info.ndim[i] = sstr.n_dimensions
        field_info.dims[i,0:maxdim-1] = sstr.dimensions[0:maxdim-1]
        field_info.nel[i] = sstr.n_elements
        field_info.totlen[i] = $
            self->wb_calc_array_totlen($
                field_info.ndim[i], field_info.nel[i], field_info.nbytes[i])

        field_info.type[i] = sstr.type
        field_info.arrsig[i,*] = self->wb_arrsig(sstr.type)

        ; add bytes to row count
        if field_info.ndim[i] eq 0 then begin
            field_info.bytesperrow = field_info.bytesperrow + $
                4L + field_info.nbytes[i]*field_info.nel[i]
        endif else begin
            field_info.bytesperrow = field_info.bytesperrow + $
                field_info.totlen[i] 
        endelse
    endfor
    return, field_info
end



pro postgres::wb_write_sig, lun
    ; header: the first 11 bytes are always the same
    printf, lun, format='(%"PGCOPY\n\377\r\n\0",$)'
end
function postgres::wb_read_sig, lun
    ; header: the first 11 bytes are always the same
    sig=bytarr(11)
    readu, lun, sig
    return, sig
end


pro postgres::wb_write_flags, lun
    ; just zero since no oids are in the file
    flags = long(0)
    writeu, lun, flags
end
function postgres::wb_read_flags, lun
    flags = long(0)
    readu, lun, flags
    return, flags
end



pro postgres::wb_write_hdext_len, lun
    ; just zero since this is not an extended header
    len= long(0)
    writeu, lun, len
end
function postgres::wb_read_hdext_len, lun
    len = long(0)
    readu, lun, len
    return, len
end

pro postgres::wb_write_nfields, lun, nfields
    ; 16-bit
    nfields= fix(nfields)
    writeu, lun, nfields
end
function postgres::wb_read_nfields, lun
    ; 16-bit
    nfields= fix(0)
    readu, lun, nfields
    return, nfields
end




pro postgres::wb_write_trailer, lun
    ; just -1 16-bit
    trailer = fix(-1)
    writeu, lun, trailer
end
function postgres::wb_read_trailer, lun
    trailer = fix(0)
    readu, lun, trailer
    return,trailer
end


function postgres::wb_open, file, mode, error=error
    isbig_endian = is_ieee_big()
    if not isbig_endian then begin
        swap_endian=1
    endif

    case strlowcase(mode) of
        'w': openw, lun, file, /get_lun, swap_endian=swap_endian, error=error
        'r': openr, lun, file, /get_lun, swap_endian=swap_endian, error=error
        else: message,'Unsupported mode: '+ntostr(mode)
    endcase

    print,'Opening file: ',file,'  as mode: ',mode
    if error ne 0 then begin 
        print,'Error opening file '+file+': '+!error_state.sys_msg
        return, -9999
    endif

    return, lun

end


; 
; Only works for fixed byte counts for the entire array
;
function postgres::wb_array_totlen, field, ndim, nel
    totlen=long(0)

    ; number of dimensions plus the "zero"
    totlen = totlen + (1L+1L)*4L
    ; four bytes for the unknown array signature
    totlen = totlen + 4L
    ; four bytes for each of the dimension lengths plus "one"
    totlen = totlen + (1L+1L)*4L*ndim
    ; four bytes for the length specifier of each element
    totlen = totlen + 4L*nel
    ; the total length of the data areas over all dimensions
    ; struct string members have extra bytes, so we do this individually
    totlen = totlen + long( self->n_bytes(field) )
    return,totlen
end
pro postgres::wb_write_array, lun, field, field_info, field_index

    one = long(1)
    zero=long(0)

    ndim = field_info.ndim[field_index]
    nel = field_info.nel[field_index]
    totlen=self->wb_array_totlen(field,ndim,nel)
    writeu, lun, totlen
    writeu, lun, ndim
    writeu, lun, zero

    writeu, lun, reform(field_info.arrsig[field_index,*])

    for i=field_info.maxdim-1,0,-1 do begin
        dim = field_info.dims[field_index,i]
        if dim ne 0 then begin
            writeu, lun, dim
            writeu, lun, one
        endif
    endfor

    ; Now write the data
    for i=0L, field_info.nel[field_index]-1 do begin
        nbytes = long(self->n_bytes(field[i]))
        writeu, lun, nbytes
        writeu, lun, field[i]
    endfor
    
end
pro postgres::wb_write_tuples_old, lun, struct

    ; Get the field lengths
    field_info = self->wb_get_struct_field_info(struct)

    ; Number of fields in each tuple: 16-bit
    nfields = fix(n_tags(struct))

    ; Now loop over the rows and write the tuple info
    ntuples = n_elements(struct)
    for i=0L, ntuples-1 do begin

        ; First write number of fields.  Wasteful, always the same
        writeu, lun, nfields
        for j=0L, nfields-1 do begin

            ; Now the size of the field, then the data.  again wasteful.
            ; Why not have a header specifying if each field is fixed
            ; width or not?
            if field_info.ndim[j] eq 0 then begin
                writeu, lun, field_info.nbytes[j]
                writeu, lun, struct[i].(j)
            endif else begin
                self->wb_write_array, lun, struct[i].(j), $
                    field_info, j
            endelse

        endfor

    endfor


end

function postgres::wb_make_output_structdef, struct, field_info

    ; build up the structure
    nfields = n_tags(struct)
    fnames = tag_names(struct)

    zero=long(0)
    one=long(1)

    ; same across all
    st = {nfields:fix(nfields)}
    for i=0L, nfields-1 do begin

        ; Will go ahead and fill all the lengths and stuff.  They will 
        ; only be overwritten later for strings

        field = struct[0].(i)

        ndim = field_info.ndim[i]
        nel = field_info.nel[i]
        nbytes = field_info.nbytes[i]

        if ndim eq 0 then begin
            st = create_struct(st, fnames[i]+'_len', nbytes)
            st = create_struct(st, fnames[i], field)
        endif else begin
            ; first add total length of what's to come
            totlen = self->wb_array_totlen(field,ndim,nel)
            st = create_struct(st, fnames[i]+'_totlen', totlen) 
            ; number of dimensions
            st = create_struct(st, fnames[i]+'_ndim', ndim) 
            ; zero
            st = create_struct(st, fnames[i]+'_zero', zero) 

            ; The enigmatic array type signature 
            arrsig = reform(field_info.arrsig[i,*])
            st = create_struct(st, fnames[i]+'_asig', arrsig)

            ; Now dimensions info
            for j=0L, ndim-1 do begin
                tname = fnames[i]+'_dim'+ntostr(j)+'_size'
                dim = field_info.dims[i,j]
                st = create_struct(st, tname, dim)
                ; one
                tname = fnames[i]+'_dim'+ntostr(j)+'_one'
                st = create_struct(st, tname, one) 
            endfor

            ; now the data areas
            for j=0L, nel-1 do begin
                ; lenght of this field
                tname = fnames[i]+'_'+ntostr(j)+'_len'
                st = create_struct(st, tname, nbytes) 
                tname = fnames[i]+'_'+ntostr(j)+'_data'
                st = create_struct(st, tname, field[j]) 
            endfor
        endelse
        
    endfor

    return, st
end

function postgres::wb_make_output_struct, struct, ostdef, field_info, ind

    n=n_elements(ind)
    ost = replicate(ostdef, n)

    nf=n_tags(struct)
    tnames=strlowcase(tag_names(struct))
    for i=0L, nf-1 do begin

        ndim = field_info.ndim[i]
        nel = field_info.nel[i]
        tname = tnames[i]
        type = field_info.type[i]

        ; scalars
        if ndim eq 0 then begin
            comm = $
                'ost.'+tname+' = struct[ind].'+tname
            ;print,comm
            if not execute(comm) then message,'Failed to copy tag: '+tname
            ; For strings we have to give the length also
            if type eq 7 then begin
                comm = $
                    'ost.'+tname+'_len = strlen(struct[ind].'+tname+')'
                ;print,comm
                if not execute(comm) then $
                    message,'Failed to set lengths for: '+tname
            endif
        endif else begin

            ; We need to keep track of the lengths
            if type eq 7 then begin
                ; This is an [nel, n] matrix
                lens = strlen(struct[ind].(i))
                ; an [n] matrix
                if float(!version.release) ge 6.1 then begin
                    totlens = $
                        2L*4L + 4L + 2L*ndim*4L + nel*4L + $
                        total(lens, 1, /preserve_type)
                endif else begin
                    totlens = $
                        2L*4L + 4L + 2L*ndim*4L + nel*4L + $
                        long(total(lens, 1,/double))
                endelse
                comm = $
                    'ost.'+tname+'_totlen = totlens'
                ;print,comm
                if not execute(comm) then $
                    message,'Could not copy totlens for tag: '+tname
            endif

            ; copying the data only here
            for j=0L, nel-1 do begin
                jstr=ntostr(j)
                elstr = '['+jstr+']'
                comm = $
                    'ost.'+tname+'_'+jstr+'_data = struct[ind].'+tname+elstr
                ;print,comm
                if not execute(comm) then $
                    message,'Failed to copy tag: '+tname+elstr


                ; Now strings have to be completely worked since they are
                ; variable length
                if type eq 7 then begin
                    ; length of this element of string array for each row
                    ; of the structure
                    comm = $
                        'ost.'+tname+'_'+jstr+'_len = reform(lens[j,*])'
                    ;print,comm
                    if not execute(comm) then $
                        message,'Could not copy lens for '+tname+'_'+jstr
                endif
            endfor
        endelse

    endfor

    return, ost

end

pro postgres::wb_write_tuples, lun, struct, mbperwrite=mbperwrite

    if n_elements(mbperwrite) eq 0 then mbperwrite=100LL

    ;
    ; Make a new structure with the field lengths and such embedded
    ;

    ; Get the field lengths
    field_info = self->wb_get_struct_field_info(struct)

    ; Number of fields in each tuple: 16-bit
    nfields = fix(n_tags(struct))

    ostdef = self->wb_make_output_structdef(struct, field_info)

    ; Make the new output struct with data


    ; Split up, don't use over mbperwrite at a time
    chunksize = long64( double(mbperwrite)/field_info.bytesperrow*1.e6 )
    
    ntot = n_elements(struct)
    nchunk = ntot/chunksize
    nleft = ntot mod chunksize

    if nchunk gt 0 then begin
        print,'    nrows:                    '+ntostr(ntot)
        print,'    bytes per row:            '+ntostr(field_info.bytesperrow)
        ;;print,'    chunksize for 100 Mb/row: '+ntostr(chunksize)
        print,'    chunksize for '+ntostr(mbperwrite)+' Mb/row: '+$
            ntostr(chunksize)
    endif
    for i=0LL, nchunk-1 do begin
        print,"        Writing chunk: "+ntostr(i+1)+'/'+ntostr(nchunk)
        ind=l64indgen(n_elements(struct))
        ibegin = i*chunksize
        ind = ibegin + l64indgen(chunksize)
        ost = self->wb_make_output_struct(struct, ostdef, field_info, ind)
        writeu, lun, ost
        ost=0
    endfor

    if nleft ne 0 then begin
        if nchunk gt 0 then print,"    Writing what's left: "+ntostr(nleft)
        ibegin = i*chunksize
        ind = ibegin + l64indgen(nleft)
        ost = self->wb_make_output_struct(struct, ostdef, field_info, ind)
        writeu, lun, ost
        ost=0
    endif

end

pro postgres::wb_test, tmpdir=tmpdir
    n=20000
    st=replicate({a:0, b:[0.0,0.0],s:'',sarr:['','']},n)
    st.a = lindgen(n)
    st.b[0] = lindgen(n)
    st.b[1] = 2*lindgen(n)
    st.s = 'test'+ntostr(long( 10000*randomu(seed,n) ))

    st.sarr[0] = 'test'+ntostr(long( 10000*randomu(seed,n) ))
    st.sarr[1] = 'test'+ntostr(long( 10000*randomu(seed,n) ))

    self->query,'drop table testnewbin'
    ;self->query,'drop table testnewasc'
    self->struct2table, st, 'testnewbin', tmpdir=tmpdir
    ;self->struct2table, st, 'testnewasc', tmpdir=tmpdir, /ascii
end


pro postgres::input_write_binary, struct, file, status=status

    status=1
    if n_params() lt 2 then begin 
        print,'-Syntax: pg->input_write_binary, struct, file, status='
        return
    endif 

    lun = self->wb_open(file, 'w', error=error)
    if error ne 0 then return

    ; print the header 
    self->wb_write_sig, lun
    self->wb_write_flags, lun
    self->wb_write_hdext_len, lun

    self->wb_write_tuples, lun, struct

    self->wb_write_trailer, lun

    status=0
    free_lun, lun


end



function postgres::n_bytes, a
    dtype = size(a,/type)                      ;data type
    if dtype EQ 0 then return,0            ;undefined
    nel = long64(N_elements(a))
    case dtype of
        1: nb = 1*nel                            ;Byte
        2: nb = 2*nel                            ;Integer*2
        3: nb = 4*nel                            ;Integer*4
        4: nb = 4*nel                            ;Real*4
        5: nb = 8*nel                            ;Real*8
        6: nb = 8*nel                            ;Complex
        7: begin
            nb=0LL
            for i=0L, nel-1 do begin
                nb=nb+strlen(a[i])
            endfor
        end
        9: nb = 16*nel                           ;Double Complex
        12: nb = 2*nel                            ;Unsigned Integer*2
        13: nb = 4*nel                            ;Unsigned Integer*4
        14: nb = 8*nel                            ;64 bit integer
        15: nb = 8*nel                            ;Unsigned 64 bit integer
        else: message,'ERROR - Object or Pointer data types not valid'
    endcase

    return,nb
end


; These are just a test program
pro postgres::_test_binary, file, n=n

    if n_elements(n) eq 0 then n=0
    lun = self->wb_open(file, 'r', error=error)
    if error ne 0 then return

    sig=self->wb_read_sig(lun)
    print,'Sig = ',string(sig)
    flags = self->wb_read_flags(lun)
    print,'Flags = ',flags
    hextlen = self->wb_read_hdext_len(lun)
    print,'Extra header len = ',hextlen

    ; first row
    nfields = self->wb_read_nfields(lun)
    print,'First row, nfields = ',nfields

    ; try to read next as long
    t=long(0)
    readu, lun, t
    print,'length of next = ',t

    for i=0L, n-1 do begin
        if i eq 2 then begin
            tb=bytarr(4)
            readu, lun, tb
            print,'  tb = ',tb
        endif else begin
            readu, lun, t
            print,i,' 32-bit = ',t,f='(i2,a,i2)'
        endelse
    endfor

    trailer=self->wb_read_trailer(lun)
    print,'Trailer = ',trailer,f='(a,i2)'

    free_lun, lun
end

pro postgres::_test_binary_char, file, nrows, array=array

    if n_elements(n) eq 0 then n=0
    lun = self->wb_open(file, 'r', error=error)
    if error ne 0 then return

    sig=self->wb_read_sig(lun)
    print,'Sig = ',string(sig)
    flags = self->wb_read_flags(lun)
    print,'Flags = ',flags
    hextlen = self->wb_read_hdext_len(lun)
    print,'Extra header len = ',hextlen

    if not keyword_set(array) then begin

        for i=0L, nrows-1 do begin
            nfields = self->wb_read_nfields(lun)

            print,'nfields = ',nfields

            t=long(0)
            readu, lun, t
            print,'length of next = ',t

            for i=0L, t-1 do begin
                t=0b
                readu, lun, t
                print,'  c='+ntostr(t)
            endfor
        endfor
    endif else begin

        for i=0L, nrows-1 do begin
            nfields = self->wb_read_nfields(lun)

            print,'nfields = ',nfields

            t=long(0)
            readu, lun, t
            print,'length of next = ',t



            ndim=long(0)
            readu, lun, ndim
            print,'ndim = ',ndim
            zero=long(0)
            readu,lun,zero
            print,'zero = ',zero

            arrsig=bytarr(4)
            readu, lun, arrsig
            print,'arrsig = ',arrsig

            dims = lonarr(ndim)
            for idim=0L, ndim-1 do begin
                tdim=0L
                readu, lun, tdim
                dims[idim] = tdim
                one=0L
                readu, lun, one
                print,'dim = ',tdim,' one = ',one
            endfor

            len=long(0)
            t=0b
            for idim=0L, ndim-1 do begin
                for idlen=0L, dims[idim]-1 do begin
                    readu, lun, len
                    for ival=0L, len-1 do begin
                        readu, lun, t
                        print,string(t),f='(a,$)'
                    endfor
                    print
                endfor
            endfor

        endfor
    endelse

    trailer=self->wb_read_trailer(lun)
    print,'Trailer = ',trailer,f='(a,i2)'

    free_lun, lun
end





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set status to unknown, nrows to zero
;; the connect information is left untouched
;  i never use this.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro postgres::reset

  ;; don't change the connect info: 
  self.query_status = -1
  self.nrows = 0

end 

function postgres::cleanup
  return,1
end 



pro postgres__define

  struct = {$
             postgres, $
             connect_info: '', $
             query_status: -1, $
             nrows: 0ull $
            }

end 

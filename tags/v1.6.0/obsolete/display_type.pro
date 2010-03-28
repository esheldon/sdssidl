FUNCTION display_type

  ;; just use !d.name rather than this program

  IF (!d.flags AND 1) EQ 0 THEN return,'X' ELSE return,'PS'

END

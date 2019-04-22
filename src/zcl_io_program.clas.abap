class ZCL_IO_PROGRAM definition
  public
  create public .

public section.

  class-data:
    sys_uccheck TYPE c LENGTH 1 read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods IS_UNICODE_PROGRAM
    importing
      !PROGNAME type CSEQUENCE
    returning
      value(RESULT) type ABAP_BOOL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IO_PROGRAM IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.

    CALL 'C_SAPGPARAM'
    ID 'NAME'  FIELD 'abap/unicode_check'
    ID 'VALUE' FIELD sys_uccheck.

  endmethod.


  method IS_UNICODE_PROGRAM.

    DATA l_uccheck TYPE c LENGTH 1.
    IF sys_uccheck = 'on'.
      result = abap_true.
    ELSE.
      SELECT SINGLE uccheck FROM trdir INTO l_uccheck
            WHERE name = progname.
      IF l_uccheck = 'X'.
        result = abap_true.
      ENDIF.
    ENDIF.

  endmethod.
ENDCLASS.

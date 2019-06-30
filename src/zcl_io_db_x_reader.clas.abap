"! <p class="shorttext synchronized" lang="en">Database column byte string reader</p>
CLASS zcl_io_db_x_reader DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_x_reader
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
*"* public components of class CL_ABAP_DB_X_READER
*"* do not include other source files here!!!

    INTERFACES if_abap_db_reader .
    INTERFACES if_abap_db_blob_handle .
    INTERFACES if_abap_db_lob_handle .

    ALIASES get_statement_handle
      FOR if_abap_db_reader~get_statement_handle .

    METHODS destructor .
  PROTECTED SECTION.
*"* protected components of class CL_ABAP_DB_X_READER
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class CL_ABAP_DB_X_READER
*"* do not include other source files here!!!

    METHODS close_internal                                  "#EC WARNOK
      RAISING
        cx_lob_sql_error .
    METHODS data_available_internal                         "#EC WARNOK
      RETURNING
        VALUE(available) TYPE abap_bool.
    METHODS read_internal                                   "#EC WARNOK
      IMPORTING
        !length       TYPE abap_msize
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        cx_lob_sql_error .
    METHODS is_closed_internal                              "#EC WARNOK
      RETURNING
        VALUE(closed) TYPE abap_bool .
    DATA lob_hdl TYPE lob_handle_attr .
ENDCLASS.



CLASS zcl_io_db_x_reader IMPLEMENTATION.


  METHOD close_internal.
  ENDMETHOD.


  METHOD data_available_internal.
  ENDMETHOD.


  METHOD destructor.
    SYSTEM-CALL C-DESTRUCTOR 'ab_CDestr_db_close720' USING lob_hdl-id lob_hdl-con_name.
  ENDMETHOD.


  METHOD if_abap_db_reader~get_statement_handle.
  ENDMETHOD.


  METHOD is_closed_internal.
  ENDMETHOD.


  METHOD read_internal.
  ENDMETHOD.

ENDCLASS.

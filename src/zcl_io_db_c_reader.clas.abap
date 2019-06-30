"! <p class="shorttext synchronized" lang="en">Database column character string reader</p>
CLASS zcl_io_db_c_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_c_reader
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_reader.

  PUBLIC SECTION.

*"* public components of class CL_ABAP_DB_C_READER
*"* do not include other source files here!!!
    INTERFACES if_abap_db_reader .
    INTERFACES if_abap_db_lob_handle .
    INTERFACES if_abap_db_clob_handle .

    METHODS constructor
      IMPORTING
        std_reader TYPE REF TO cl_abap_db_c_reader.

  PROTECTED SECTION.
*"* protected components of class CL_ABAP_DB_C_READER
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class CL_ABAP_DB_C_READER
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
        VALUE(result) TYPE string
      RAISING
        cx_lob_sql_error .
    METHODS is_closed_internal                              "#EC WARNOK
      RETURNING
        VALUE(closed) TYPE abap_bool .

    DATA: std_reader TYPE REF TO cl_abap_db_c_reader.
ENDCLASS.



CLASS zcl_io_db_c_reader IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->std_reader = std_reader.
  ENDMETHOD.

  METHOD close_internal.
    std_reader->close( ).
*      CATCH cx_close_resource_error.    " .
  ENDMETHOD.


  METHOD data_available_internal.
    available = std_reader->data_available( ).
  ENDMETHOD.


  METHOD is_closed_internal.
    closed = std_reader->is_closed( ).
  ENDMETHOD.


  METHOD read_internal.

*    TRY.
    result = std_reader->read( length ).
*    CATCH cx_root.
*    raise exception type cx_lob_sql_error.
*    endtry.
*    closed = std_reader->is_closed( ).
  ENDMETHOD.


  METHOD if_abap_db_reader~get_statement_handle.
    hdl = std_reader->get_statement_handle( ).
  ENDMETHOD.

  METHOD if_abap_close_resource~close.
    std_reader->close( ).
  ENDMETHOD.

  METHOD if_abap_reader~data_available.
    available = std_reader->data_available( ).
  ENDMETHOD.

  METHOD if_abap_reader~delete_mark.
    std_reader->delete_mark( ).
  ENDMETHOD.

  METHOD if_abap_close_resource~is_closed.
    closed = std_reader->is_closed( ).
  ENDMETHOD.

  METHOD if_abap_reader~is_mark_supported.
    res = std_reader->is_mark_supported( ).
  ENDMETHOD.

  METHOD if_abap_reader~is_reset_supported.
    result = std_reader->is_reset_supported( ).
  ENDMETHOD.

  METHOD if_abap_reader~is_x_reader.
    result = std_reader->is_x_reader( ).
  ENDMETHOD.

  METHOD if_abap_reader~read.
    read_data = std_reader->read( length ).
  ENDMETHOD.

  METHOD if_abap_reader~reset.
    std_reader->reset( ).
  ENDMETHOD.

  METHOD if_abap_reader~reset_to_mark.
    std_reader->reset_to_mark( ).
  ENDMETHOD.

  METHOD if_abap_reader~set_mark.
    std_reader->set_mark( ).
  ENDMETHOD.

  METHOD if_abap_reader~skip.
    std_reader->skip( length ).
  ENDMETHOD.

ENDCLASS.

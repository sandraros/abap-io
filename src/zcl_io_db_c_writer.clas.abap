"! <p class="shorttext synchronized" lang="en">Database column character string writer</p>
CLASS zcl_io_db_c_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_io_c_writer
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_io_c_writer.

  PUBLIC SECTION.

*"* public components of class CL_ABAP_DB_C_WRITER
*"* do not include other source files here!!!
    INTERFACES if_abap_db_writer .

    METHODS constructor
      IMPORTING
        std_writer TYPE REF TO cl_abap_db_c_writer.

    ALIASES get_statement_handle
      FOR if_abap_db_writer~get_statement_handle .

    METHODS destructor .
  PROTECTED SECTION.
*"* protected components of class CL_ABAP_DB_C_WRITER
*"* do not include other source files here!!!

  PRIVATE SECTION.
*"* private components of class CL_ABAP_DB_C_WRITER
*"* do not include other source files here!!!

    DATA lob_hdl TYPE lob_handle_attr .
    TYPE-POOLS abap .
    DATA maxlength TYPE abap_msize VALUE 0.               "#EC NOTEXT .
    DATA written_length TYPE abap_msize VALUE 0.          "#EC NOTEXT .

    METHODS write_internal
      IMPORTING
        !data TYPE string
      RAISING
        cx_lob_sql_error
        cx_sy_open_sql_db .
    METHODS close_internal
      RAISING
        cx_lob_sql_error
        cx_sy_open_sql_db .
    METHODS is_closed_internal
      RETURNING
        VALUE(closed) TYPE abap_bool .

    DATA: std_writer        TYPE REF TO cl_abap_db_c_writer,
          closed_internally TYPE abap_bool.

ENDCLASS.



CLASS zcl_io_db_c_writer IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->std_writer = std_writer.
  ENDMETHOD.


  METHOD close_internal.
    std_writer->close( ).
  ENDMETHOD.


  METHOD destructor.
    SYSTEM-CALL C-DESTRUCTOR 'ab_CDestr_db_close720' USING lob_hdl-id lob_hdl-con_name.
  ENDMETHOD.


  METHOD if_abap_db_writer~get_statement_handle.
    hdl = std_writer->get_statement_handle( ).
  ENDMETHOD.


  METHOD is_closed_internal.
    closed = std_writer->is_closed( ).
  ENDMETHOD.


  METHOD write_internal.
    std_writer->write( data ).
  ENDMETHOD.

  METHOD if_abap_close_resource~close.
    std_writer->close( ).
  ENDMETHOD.

  METHOD if_abap_writer~flush.
    std_writer->flush( ).
  ENDMETHOD.

  METHOD if_abap_close_resource~is_closed.
    closed = std_writer->is_closed( ).
  ENDMETHOD.

  METHOD if_abap_writer~is_x_writer.
    result = std_writer->is_x_writer( ).
  ENDMETHOD.

  METHOD if_abap_writer~write.
    std_writer->write( data ).
  ENDMETHOD.

ENDCLASS.

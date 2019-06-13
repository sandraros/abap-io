"! <p class="shorttext synchronized" lang="en">Abstract byte writer</p>
CLASS zcl_io_x_writer DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_close_resource
      ALL METHODS FINAL .
    INTERFACES zif_io_writer
      ALL METHODS FINAL .
    INTERFACES zif_io_x_writer
      ALL METHODS FINAL .

    ALIASES close
      FOR zif_io_close_resource~close .
    ALIASES flush
      FOR zif_io_writer~flush .
    ALIASES is_closed
      FOR zif_io_close_resource~is_closed .
    ALIASES is_x_writer
      FOR zif_io_writer~is_x_writer .
    ALIASES write
      FOR zif_io_x_writer~write .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_io_x_writer IMPLEMENTATION.


  METHOD constructor.


  ENDMETHOD.


  METHOD zif_io_close_resource~close.
  ENDMETHOD.


  METHOD zif_io_close_resource~is_closed.
  ENDMETHOD.


  METHOD zif_io_writer~flush.
  ENDMETHOD.


  METHOD zif_io_writer~is_x_writer.
  ENDMETHOD.


  METHOD zif_io_writer~write.
    CALL METHOD me->('WRITE_INTERNAL') EXPORTING data = data.
*    DATA type TYPE REF TO cl_abap_typedescr.
*    DATA l_name TYPE string.
*    type = cl_abap_typedescr=>describe_by_data( data ).
*    IF type = cl_abap_elemdescr=>get_xstring( ).
*      write( data ).
*    ELSE.
*      l_name = type->get_relative_name( ).
*      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
*         EXPORTING
*           textid = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
*           parameter = `DATA`
*           type = l_name.
*    ENDIF.
  ENDMETHOD.


  METHOD zif_io_x_writer~write.
    CALL METHOD me->('WRITE_INTERNAL') EXPORTING data = data.
  ENDMETHOD.
ENDCLASS.

"! <p class="shorttext synchronized" lang="en">Abstract character writer</p>
CLASS zcl_io_c_writer DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_c_writer
      ALL METHODS FINAL .
    INTERFACES zif_io_close_resource
      ALL METHODS FINAL .
    INTERFACES zif_io_writer
      ALL METHODS FINAL .

    ALIASES close
      FOR zif_io_close_resource~close .
    ALIASES flush
      FOR zif_io_c_writer~flush .
    ALIASES is_closed
      FOR zif_io_close_resource~is_closed .
    ALIASES is_x_writer
      FOR zif_io_c_writer~is_x_writer .
    ALIASES write
      FOR zif_io_c_writer~write .

    METHODS constructor .

  PROTECTED SECTION.

    DATA close_managed_internally TYPE abap_bool VALUE abap_false.

  PRIVATE SECTION.

    DATA closed TYPE abap_bool .

ENDCLASS.



CLASS zcl_io_c_writer IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD zif_io_close_resource~close.
    IF close_managed_internally = abap_true.
      CALL METHOD me->('CLOSE_INTERNAL').
    ELSE.
      " not implemented by the resource
      closed = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_io_close_resource~is_closed.
    closed = me->closed.
  ENDMETHOD.


  METHOD zif_io_c_writer~write.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    CALL METHOD me->('WRITE_INTERNAL')
      EXPORTING
        data = data.
  ENDMETHOD.


  METHOD zif_io_writer~flush.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.


  METHOD zif_io_writer~is_x_writer.
    result = abap_false.
  ENDMETHOD.


  METHOD zif_io_writer~write.
    DATA type TYPE REF TO cl_abap_typedescr.
    type = cl_abap_typedescr=>describe_by_data( data ).
    IF type = cl_abap_elemdescr=>get_string( ).
      zif_io_c_writer~write( data ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_type
        EXPORTING
          textid    = zcx_io_parameter_invalid_type=>zcx_io_parameter_invalid_type
          parameter = `DATA`
          type      = type->get_relative_name( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

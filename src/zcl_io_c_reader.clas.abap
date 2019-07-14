"! <p class="shorttext synchronized" lang="en">Abstract character reader</p>
CLASS zcl_io_c_reader DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_close_resource
      ALL METHODS FINAL .
    INTERFACES zif_io_reader
      FINAL METHODS
      data_available
      is_x_reader
      read
      skip.
    INTERFACES zif_io_c_reader
      FINAL METHODS read .

    ALIASES close
      FOR zif_io_close_resource~close .
    ALIASES data_available
      FOR zif_io_c_reader~data_available .
    ALIASES delete_mark
      FOR zif_io_c_reader~delete_mark .
    ALIASES is_closed
      FOR zif_io_close_resource~is_closed .
    ALIASES is_mark_supported
      FOR zif_io_c_reader~is_mark_supported .
    ALIASES is_reset_supported
      FOR zif_io_c_reader~is_reset_supported .
    ALIASES is_x_reader
      FOR zif_io_c_reader~is_x_reader .
    ALIASES read
      FOR zif_io_c_reader~read .
    ALIASES reset
      FOR zif_io_c_reader~reset .
    ALIASES reset_to_mark
      FOR zif_io_c_reader~reset_to_mark .
    ALIASES set_mark
      FOR zif_io_c_reader~set_mark .
    ALIASES skip
      FOR zif_io_c_reader~skip .
    ALIASES is_auto_close_performed
      FOR zif_io_reader~is_auto_close_performed .

    METHODS constructor .

  PROTECTED SECTION.

    DATA close_managed_internally TYPE abap_bool VALUE abap_false.

  PRIVATE SECTION.

    TYPE-POOLS abap.
    DATA closed TYPE abap_bool VALUE abap_false.          "#EC NOTEXT .

ENDCLASS.



CLASS ZCL_IO_C_READER IMPLEMENTATION.


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

    IF close_managed_internally = abap_true.
      CALL METHOD me->('IS_CLOSED_INTERNAL')
        RECEIVING
          closed = closed.
    ELSE.
      closed = me->closed.
    ENDIF.

  ENDMETHOD.


  METHOD zif_io_c_reader~read.

    IF length <= 0.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_range EXPORTING parameter = 'LENGTH' value = CONV #( length ).
    ENDIF.
    IF is_closed( ) = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    CALL METHOD me->('READ_INTERNAL')
      EXPORTING
        length = length
      RECEIVING
        result = result.

  ENDMETHOD.


  METHOD zif_io_reader~data_available.

    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    CALL METHOD me->('DATA_AVAILABLE_INTERNAL')
      RECEIVING
        available = available.

  ENDMETHOD.


  METHOD zif_io_reader~is_auto_close_performed.

    result = abap_false.

  ENDMETHOD.


  METHOD zif_io_reader~delete_mark.

    IF closed IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    " Improvement of standard
    ASSIGN ('ME->M_MARK') TO FIELD-SYMBOL(<mark>).
    CHECK sy-subrc = 0.
    <mark> = -1.

  ENDMETHOD.


  METHOD zif_io_reader~is_mark_supported.

    res = abap_false.

  ENDMETHOD.


  METHOD zif_io_reader~is_reset_supported.

    result = abap_false.

  ENDMETHOD.


  METHOD zif_io_reader~is_x_reader.

    result = abap_false.

  ENDMETHOD.


  METHOD zif_io_reader~read.
    DATA type TYPE REF TO cl_abap_typedescr.

    type = cl_abap_typedescr=>describe_by_data( read_data ).
    IF type = cl_abap_elemdescr=>get_string( ).
      read_data = read( length ).
    ELSE.
      RAISE EXCEPTION TYPE cx_parameter_invalid_type
        EXPORTING
          textid    = cx_parameter_invalid_type=>cx_parameter_invalid_type
          parameter = `READ_DATA`
          type      = type->get_relative_name( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_io_reader~reset.

    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_io_stream_position_error
      EXPORTING
        textid = zcx_io_stream_position_error=>zcx_io_reset_not_supported.

  ENDMETHOD.


  METHOD zif_io_reader~reset_to_mark.

    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_io_stream_position_error
      EXPORTING
        textid = zcx_io_stream_position_error=>zcx_io_mark_not_supported.

  ENDMETHOD.


  METHOD zif_io_reader~set_mark.

    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_io_stream_position_error
      EXPORTING
        textid = zcx_io_stream_position_error=>zcx_io_mark_not_supported.

  ENDMETHOD.


  METHOD zif_io_reader~skip.

    read( length ).

  ENDMETHOD.
ENDCLASS.

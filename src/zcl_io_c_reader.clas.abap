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
      FOR zif_io_reader~data_available .
    ALIASES delete_mark
      FOR zif_io_reader~delete_mark .
    ALIASES is_closed
      FOR zif_io_close_resource~is_closed .
    ALIASES is_mark_supported
      FOR zif_io_reader~is_mark_supported .
    ALIASES is_reset_supported
      FOR zif_io_reader~is_reset_supported .
    ALIASES is_x_reader
      FOR zif_io_reader~is_x_reader .
    ALIASES read
      FOR zif_io_c_reader~read .
    ALIASES reset
      FOR zif_io_reader~reset .
    ALIASES reset_to_mark
      FOR zif_io_reader~reset_to_mark .
    ALIASES set_mark
      FOR zif_io_reader~set_mark .
    ALIASES skip
      FOR zif_io_reader~skip .
    ALIASES is_auto_close_performed
      FOR zif_io_reader~is_auto_close_performed .

    METHODS constructor .

  PROTECTED SECTION.

    DATA:
      close_managed_internally TYPE abap_bool VALUE abap_false.

  PRIVATE SECTION.

    DATA:
      closed TYPE abap_bool VALUE abap_false. "#EC NOTEXT .

ENDCLASS.



CLASS zcl_io_c_reader IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD close.
    IF close_managed_internally = abap_true.
      CALL METHOD me->('CLOSE_INTERNAL').
    ELSE.
      " not implemented by the resource
      closed = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_closed.
    IF close_managed_internally = abap_true.
      CALL METHOD me->('IS_CLOSED_INTERNAL')
        RECEIVING
          closed = closed.
    ELSE.
      closed = me->closed.
    ENDIF.
  ENDMETHOD.


  METHOD read.

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


  METHOD data_available.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    CALL METHOD me->('DATA_AVAILABLE_INTERNAL')
      RECEIVING
        available = available.
  ENDMETHOD.


  METHOD delete_mark.
    IF closed IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.


  METHOD is_mark_supported.
    res = abap_false.
  ENDMETHOD.


  METHOD is_reset_supported.
    result = abap_false.
  ENDMETHOD.


  METHOD is_x_reader.
    result = abap_false.
  ENDMETHOD.


  METHOD zif_io_reader~read.
    read_data = read( length ).
  ENDMETHOD.


  METHOD reset.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_io_stream_position_error
      EXPORTING
        textid = zcx_io_stream_position_error=>zcx_io_reset_not_supported.
  ENDMETHOD.


  METHOD reset_to_mark.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_io_stream_position_error
      EXPORTING
        textid = zcx_io_stream_position_error=>zcx_io_mark_not_supported.
  ENDMETHOD.


  METHOD set_mark.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    RAISE EXCEPTION TYPE zcx_io_stream_position_error
      EXPORTING
        textid = zcx_io_stream_position_error=>zcx_io_mark_not_supported.
  ENDMETHOD.


  METHOD skip.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    read( length ).
  ENDMETHOD.


  METHOD zif_io_reader~is_auto_close_performed.
    result = abap_false.
  ENDMETHOD.


ENDCLASS.

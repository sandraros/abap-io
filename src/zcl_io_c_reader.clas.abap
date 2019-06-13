"! <p class="shorttext synchronized" lang="en">Abstract character reader</p>
CLASS zcl_io_c_reader DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    INTERFACES zif_io_close_resource
      ALL METHODS FINAL .
    INTERFACES zif_io_reader .
    INTERFACES zif_io_c_reader .

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
      FOR zif_io_reader~is_reset_supported .
    ALIASES is_x_reader
      FOR zif_io_c_reader~is_x_reader .
    ALIASES read
      FOR zif_io_c_reader~read .
    ALIASES reset
      FOR zif_io_reader~reset .
    ALIASES reset_to_mark
      FOR zif_io_c_reader~reset_to_mark .
    ALIASES set_mark
      FOR zif_io_c_reader~set_mark .
    ALIASES skip
      FOR zif_io_c_reader~skip .

    METHODS constructor .
  PROTECTED SECTION.

    DATA ao_data_available TYPE abap_bool .
  PRIVATE SECTION.

    DATA closed TYPE abap_bool .
ENDCLASS.



CLASS zcl_io_c_reader IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD zif_io_close_resource~close.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_close_resource_error.
    ENDIF.
    closed = abap_true.
  ENDMETHOD.


  METHOD zif_io_close_resource~is_closed.
    closed = me->closed.
  ENDMETHOD.


  METHOD zif_io_c_reader~read.

    IF data_available( ) = abap_false.
      RAISE EXCEPTION TYPE zcx_io_stream_error." exporting textid = zcx_io_stream_error=>.
    ENDIF.
    CALL METHOD me->('READ_INTERNAL')
      EXPORTING
        length = length
      RECEIVING
        result = result.

  ENDMETHOD.


  METHOD zif_io_reader~data_available.
    CALL METHOD me->('DATA_AVAILABLE_INTERNAL')
      RECEIVING
        available = available.
  ENDMETHOD.


  METHOD zif_io_reader~delete_mark.
  ENDMETHOD.


  METHOD zif_io_reader~is_mark_supported.
  ENDMETHOD.


  METHOD zif_io_reader~is_reset_supported.

    result = abap_false.

  ENDMETHOD.


  METHOD zif_io_reader~is_x_reader.
  ENDMETHOD.


  METHOD zif_io_reader~read.
    CALL METHOD me->('READ_INTERNAL')
      EXPORTING
        length    = length
      RECEIVING
        read_data = read_data.
*    zcl_io_stream_utilities=>check_data_type_is_string( read_data ).
*    read_data = read( length ).
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
  ENDMETHOD.


  METHOD zif_io_reader~set_mark.
  ENDMETHOD.


  METHOD zif_io_reader~skip.
    read( length ).
  ENDMETHOD.
ENDCLASS.

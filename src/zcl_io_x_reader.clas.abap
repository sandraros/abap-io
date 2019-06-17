"! <p class="shorttext synchronized" lang="en">Abstract byte reader</p>
CLASS zcl_io_x_reader DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_io_close_resource
      ALL METHODS FINAL .
    INTERFACES zif_io_x_reader .

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
      FOR zif_io_x_reader~read .
    ALIASES reset
      FOR zif_io_reader~reset .
    ALIASES reset_to_mark
      FOR zif_io_reader~reset_to_mark .
    ALIASES set_mark
      FOR zif_io_reader~set_mark .
    ALIASES skip
      FOR zif_io_reader~skip .

    METHODS constructor .
  PROTECTED SECTION.

*    DATA aov_closed TYPE abap_bool .
*    DATA:
*      aov_next_byte TYPE x LENGTH 1 .
  PRIVATE SECTION.

    DATA closed TYPE abap_bool.
ENDCLASS.



CLASS zcl_io_x_reader IMPLEMENTATION.


  METHOD constructor.
    closed = abap_false.
  ENDMETHOD.


  METHOD zif_io_close_resource~close.
    closed = abap_true.
  ENDMETHOD.


  METHOD zif_io_close_resource~is_closed.
    closed = me->closed.
  ENDMETHOD.


  METHOD zif_io_reader~data_available.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    CALL METHOD me->('DATA_AVAILABLE_INTERNAL')
      RECEIVING
        available = available.
  ENDMETHOD.


  METHOD zif_io_reader~delete_mark.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.


  METHOD zif_io_reader~is_mark_supported.
    res = abap_true.
  ENDMETHOD.


  METHOD zif_io_reader~is_reset_supported.
    result = abap_true.
  ENDMETHOD.


  METHOD zif_io_reader~is_x_reader.
    result = abap_true.
  ENDMETHOD.


  METHOD zif_io_reader~read.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    CALL METHOD me->('READ_INTERNAL')
      EXPORTING
        length = length
      RECEIVING
        read_data = read_data.
  ENDMETHOD.


  METHOD zif_io_reader~reset.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.


  METHOD zif_io_reader~reset_to_mark.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.


  METHOD zif_io_reader~set_mark.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
  ENDMETHOD.


  METHOD zif_io_reader~skip.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    read( length ).
  ENDMETHOD.


  METHOD zif_io_x_reader~read.
    IF length <= 0.
      RAISE EXCEPTION TYPE zcx_io_parameter_invalid_range EXPORTING parameter = 'LENGTH' value = CONV #( length ).
    ENDIF.
    IF closed = abap_true.
      RAISE EXCEPTION TYPE zcx_io_resource_already_closed.
    ENDIF.
    CALL METHOD me->('READ_INTERNAL')
      EXPORTING
        length = length
      RECEIVING
        result = result.
  ENDMETHOD.
ENDCLASS.

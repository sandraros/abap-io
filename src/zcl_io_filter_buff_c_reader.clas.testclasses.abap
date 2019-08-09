*"* use this source file for your ABAP unit test classes

CLASS lcl_c_reader DEFINITION
      FOR TESTING
      FRIENDS zcl_io_c_reader.
  PUBLIC SECTION.
    INTERFACES zif_io_c_reader PARTIALLY IMPLEMENTED.
  PRIVATE SECTION.
    DATA stream TYPE string VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    DATA offset TYPE string.
    METHODS data_available_internal
      RETURNING
        VALUE(available) TYPE abap_bool .
    METHODS read_internal
      IMPORTING
        !length       TYPE abap_msize
      RETURNING
        VALUE(result) TYPE string .
ENDCLASS.

CLASS lcl_c_reader IMPLEMENTATION.
  METHOD zif_io_close_resource~is_closed.
    closed = abap_false.
  ENDMETHOD.
  METHOD zif_io_c_reader~read.
    result = stream+offset(length).
    ADD length TO offset.
  ENDMETHOD.
  METHOD zif_io_c_reader~data_available.
    available = abap_true.
  ENDMETHOD.
  METHOD read_internal.
    result = stream+offset(length).
    ADD length TO offset.
  ENDMETHOD.
  METHOD data_available_internal.
    available = abap_true.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_io_filter_buff_c_reader DEFINITION LOCAL FRIENDS ltc_main.

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.
  METHOD test.
    DATA: b TYPE string.
    DATA(a) = NEW zcl_io_filter_buff_c_reader( reader = NEW lcl_c_reader( ) i_buffer_size = 10 ).
    b = a->read_internal( 2 ).
    cl_abap_unit_assert=>assert_equals( exp = '01' act = b ).
    b = a->read_internal( 20 ).
    cl_abap_unit_assert=>assert_equals( exp = '23456789ABCDEFGHIJKL' act = b ).
  ENDMETHOD.
ENDCLASS.

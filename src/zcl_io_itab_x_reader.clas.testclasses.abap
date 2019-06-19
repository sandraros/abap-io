*"* use this source file for your ABAP unit test classes
CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test.

  PRIVATE SECTION.
    METHODS itab_xstring FOR TESTING RAISING cx_static_check.
    METHODS itab_xstring_length FOR TESTING RAISING cx_static_check.
    METHODS itab_xstring_empty FOR TESTING RAISING cx_static_check.
    METHODS itab_x FOR TESTING RAISING cx_static_check.
    METHODS itab_x_length FOR TESTING RAISING cx_static_check.
    METHODS itab_x_empty FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.
  METHOD itab_xstring.
    test_x_reader( NEW zcl_io_itab_x_reader( itab = CONV xstring_table( itab_01_to_1a ) ) ).
  ENDMETHOD.
  METHOD itab_xstring_length.
    test_x_reader( NEW zcl_io_itab_x_reader( itab = CONV xstring_table( itab_01_to_1c ) length = 26 ) ).
  ENDMETHOD.
  METHOD itab_xstring_empty.
    test_x_reader_empty( NEW zcl_io_itab_x_reader( itab = VALUE xstring_table( ) ) ).
    test_x_reader_empty( NEW zcl_io_itab_x_reader( itab = CONV xstring_table( itab_01_to_1a ) length = 0 ) ).
  ENDMETHOD.
  METHOD itab_x.
    test_x_reader( NEW zcl_io_itab_x_reader( itab = itab_01_to_1a ) ).
  ENDMETHOD.
  METHOD itab_x_length.
    test_x_reader( NEW zcl_io_itab_x_reader( itab = itab_01_to_1c length = 26 ) ).
  ENDMETHOD.
  METHOD itab_x_empty.
    test_x_reader_empty( NEW zcl_io_itab_x_reader( itab = VALUE zcl_io_test=>ty_x3_s( ) ) ).
    test_x_reader_empty( NEW zcl_io_itab_x_reader( itab = itab_01_to_1a length = 0 ) ).
  ENDMETHOD.
ENDCLASS.

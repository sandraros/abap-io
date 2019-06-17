*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test.
  PRIVATE SECTION.
    METHODS itab_string FOR TESTING RAISING cx_static_check.
    METHODS itab_string_length FOR TESTING RAISING cx_static_check.
    METHODS itab_string_empty FOR TESTING RAISING cx_static_check.
    METHODS itab_c FOR TESTING RAISING cx_static_check.
    METHODS itab_c_length FOR TESTING RAISING cx_static_check.
    METHODS itab_c_empty FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.
  METHOD itab_string.
    test_c_reader( NEW zcl_io_itab_c_reader( itab = CONV string_table( itab_a_to_z ) ) ).
  ENDMETHOD.
  METHOD itab_string_length.
    test_c_reader( NEW zcl_io_itab_c_reader( itab = CONV string_table( itab_a_to_z01 ) length = 26 ) ).
  ENDMETHOD.
  METHOD itab_string_empty.
    test_c_reader_empty( NEW zcl_io_itab_c_reader( itab = VALUE string_table( ) ) ).
    test_c_reader_empty( NEW zcl_io_itab_c_reader( itab = CONV string_table( itab_a_to_z ) length = 0 ) ).
  ENDMETHOD.
  METHOD itab_c.
    test_c_reader( NEW zcl_io_itab_c_reader( itab = itab_a_to_z ) ).
  ENDMETHOD.
  METHOD itab_c_length.
    test_c_reader( NEW zcl_io_itab_c_reader( itab = itab_a_to_z01 length = 26 ) ).
  ENDMETHOD.
  METHOD itab_c_empty.
    test_c_reader_empty( NEW zcl_io_itab_c_reader( itab = VALUE zcl_io_test=>ty_c3_s( ) ) ).
    test_c_reader_empty( NEW zcl_io_itab_c_reader( itab = itab_a_to_z length = 0 ) ).
  ENDMETHOD.
ENDCLASS.

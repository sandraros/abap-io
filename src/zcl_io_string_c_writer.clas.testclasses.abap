*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test.
  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.
  METHOD test.
    DATA(writer) = NEW zcl_io_string_c_writer( ).
    WRITER->bind_result_area( IMPORTING str = data(str) ).
    test_c_writer( writer ).
    cl_abap_unit_assert=>assert_equals( exp = CONV string( sy-abcde ) act = str ).
  ENDMETHOD.
ENDCLASS.

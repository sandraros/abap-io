*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test.
  PRIVATE SECTION.
    METHODS bind_result_area FOR TESTING RAISING cx_static_check.
    METHODS get_result_string FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.
  METHOD bind_result_area.
    DATA(writer) = NEW zcl_io_string_x_writer( ).
    writer->bind_result_area( IMPORTING str = DATA(str) ).
    test_x_writer( writer ).
    cl_abap_unit_assert=>assert_equals( exp = CONV xstring( zcl_io_test=>_01_to_1a ) act = str ).
  ENDMETHOD.
  METHOD get_result_string.
    DATA(writer) = NEW zcl_io_string_x_writer( ).
    test_x_writer( writer ).
    cl_abap_unit_assert=>assert_equals( exp = CONV xstring( zcl_io_test=>_01_to_1a ) act = writer->get_result_string( ) ).
  ENDMETHOD.
ENDCLASS.

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
    test_x_reader( NEW zcl_io_string_x_reader( xstr = conv #( _01_to_1a ) ) ).
  ENDMETHOD.
ENDCLASS.

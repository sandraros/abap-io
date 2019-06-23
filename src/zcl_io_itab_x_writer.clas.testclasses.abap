*"* use this source file for your ABAP unit test classes

*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test.
  PRIVATE SECTION.
    METHODS itab_x_bind_result_area FOR TESTING RAISING cx_static_check.
    METHODS itab_xstring_bind_result_area FOR TESTING RAISING cx_static_check.
    METHODS itab_x_get_result_table FOR TESTING RAISING cx_static_check.
    METHODS itab_xstring_get_result_table FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD itab_x_bind_result_area.
    DATA table TYPE zcl_io_test=>ty_x2_s.
    DATA(writer) = NEW zcl_io_itab_x_writer( ).
    writer->bind_result_area(
        IMPORTING
        table               = table
        length_of_last_line = DATA(length_of_last_line)
        length              = DATA(length) ).
    test_x_writer( writer ).
    cl_abap_unit_assert=>assert_equals( act = table  exp = zcl_io_test=>itab_01_to_1a ).
    cl_abap_unit_assert=>assert_equals( act = length_of_last_line exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = length exp = 26 ).
  ENDMETHOD.

  METHOD itab_x_get_result_table.
    DATA table TYPE zcl_io_test=>ty_x2_s.
    DATA(writer) = NEW zcl_io_itab_x_writer(
            line_type   = cl_abap_typedescr=>typekind_hex
            line_length = 2 ).
    test_x_writer( writer ).
    writer->get_result_table(
      IMPORTING
        table               = table
        length_of_last_line = DATA(length_of_last_line)
        length              = DATA(length) ).
    cl_abap_unit_assert=>assert_equals( act = table                 exp = zcl_io_test=>itab_01_to_1a ).
    cl_abap_unit_assert=>assert_equals( act = length_of_last_line   exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = length                exp = 26 ).
  ENDMETHOD.

  METHOD itab_xstring_bind_result_area.
    DATA table TYPE TABLE OF xstring.
    DATA(writer) = NEW zcl_io_itab_x_writer( ).
    writer->bind_result_area( EXPORTING string_line_length = 2
        IMPORTING
        table               = table
        length_of_last_line = DATA(length_of_last_line)
        length              = DATA(length) ).
    test_x_writer( writer ).
    cl_abap_unit_assert=>assert_equals( act = table  exp = CONV Xstring_table( zcl_io_test=>itab_01_to_1a ) ).
    cl_abap_unit_assert=>assert_equals( act = length_of_last_line exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = length exp = 26 ).
  ENDMETHOD.

  METHOD itab_xstring_get_result_table.
    DATA table TYPE TABLE OF xstring.
    DATA(writer) = NEW zcl_io_itab_x_writer(
            line_type   = cl_abap_typedescr=>typekind_xstring
            line_length = 2 ).
    test_x_writer( writer ).
    writer->get_result_table(
      IMPORTING
        table               = table
        length_of_last_line = DATA(length_of_last_line)
        length              = DATA(length) ).
    cl_abap_unit_assert=>assert_equals( act = table                 exp = conv xstring_table( zcl_io_test=>itab_01_to_1a ) ).
    cl_abap_unit_assert=>assert_equals( act = length_of_last_line   exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = length                exp = 26 ).
  ENDMETHOD.

ENDCLASS.

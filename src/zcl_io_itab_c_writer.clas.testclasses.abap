*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test.
  PRIVATE SECTION.
    METHODS itab_c_bind_result_area FOR TESTING RAISING cx_static_check.
    METHODS itab_string_bind_result_area FOR TESTING RAISING cx_static_check.
    METHODS itab_c_get_result_table FOR TESTING RAISING cx_static_check.
    METHODS itab_string_get_result_table FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD itab_c_bind_result_area.
    DATA table TYPE zcl_io_test=>ty_c2_s.
    DATA(writer) = NEW zcl_io_itab_c_writer( ).
    writer->bind_result_area(
        IMPORTING
        table               = table
        length_of_last_line = DATA(length_of_last_line)
        length              = DATA(length) ).
    test_c_writer( writer ).
    cl_abap_unit_assert=>assert_equals( act = table  exp = zcl_io_test=>itab_a_to_z ).
    cl_abap_unit_assert=>assert_equals( act = length_of_last_line exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = length exp = 26 ).
  ENDMETHOD.

  METHOD itab_c_get_result_table.
    DATA table TYPE zcl_io_test=>ty_c2_s.
    DATA(writer) = NEW zcl_io_itab_c_writer(
            line_type   = cl_abap_typedescr=>typekind_char
            line_length = 2 ).
    test_c_writer( writer ).
    writer->get_result_table(
      IMPORTING
        table               = table
        length_of_last_line = DATA(length_of_last_line)
        length              = DATA(length) ).
    cl_abap_unit_assert=>assert_equals( act = table                 exp = zcl_io_test=>itab_a_to_z ).
    cl_abap_unit_assert=>assert_equals( act = length_of_last_line   exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = length                exp = 26 ).
  ENDMETHOD.

  METHOD itab_string_bind_result_area.
    DATA table TYPE TABLE OF string.
    DATA(writer) = NEW zcl_io_itab_c_writer( ).
    writer->bind_result_area( EXPORTING string_line_length = 2
        IMPORTING
        table               = table
        length_of_last_line = DATA(length_of_last_line)
        length              = DATA(length) ).
    test_c_writer( writer ).
    cl_abap_unit_assert=>assert_equals( act = table  exp = CONV string_table( zcl_io_test=>itab_a_to_z ) ).
    cl_abap_unit_assert=>assert_equals( act = length_of_last_line exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = length exp = 26 ).
  ENDMETHOD.

  METHOD itab_string_get_result_table.
    DATA table TYPE TABLE OF string.
    DATA(writer) = NEW zcl_io_itab_c_writer(
            line_type   = cl_abap_typedescr=>typekind_string
            line_length = 2 ).
    test_c_writer( writer ).
    writer->get_result_table(
      IMPORTING
        table               = table
        length_of_last_line = DATA(length_of_last_line)
        length              = DATA(length) ).
    cl_abap_unit_assert=>assert_equals( act = table                 exp = conv string_table( zcl_io_test=>itab_a_to_z ) ).
    cl_abap_unit_assert=>assert_equals( act = length_of_last_line   exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = length                exp = 26 ).
  ENDMETHOD.

ENDCLASS.

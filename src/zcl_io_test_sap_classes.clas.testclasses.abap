*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test_sap_classes.
  PRIVATE SECTION.

    METHODS string_reader FOR TESTING RAISING cx_static_check.
    METHODS xstring_reader FOR TESTING RAISING cx_static_check.
    METHODS itab_c_reader FOR TESTING RAISING cx_static_check.
    METHODS itab_string_reader FOR TESTING RAISING cx_static_check.
    METHODS itab_x_reader FOR TESTING RAISING cx_static_check.
    METHODS itab_xstring_reader FOR TESTING RAISING cx_static_check.
    METHODS db_c_reader FOR TESTING RAISING cx_static_check.
    METHODS db_x_reader FOR TESTING RAISING cx_static_check.

    METHODS string_writer FOR TESTING RAISING cx_static_check.
    METHODS xstring_writer FOR TESTING RAISING cx_static_check.
    METHODS itab_x_writer FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD string_reader.

    DATA(reader) = NEW cl_abap_string_c_reader( str = CONV #( sy-abcde ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).

    test_c_reader( reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    reader = NEW cl_abap_string_c_reader( str = `` ).
    cl_abap_unit_assert=>assert_equals( msg = 'data_available should be false for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD itab_c_reader.
    DATA: itab TYPE zcl_io_test=>ty_c2_s.

    zcl_io_test=>split_c( EXPORTING i_field = sy-abcde IMPORTING et_chunk = itab ).

    DATA(reader) = NEW cl_abap_itab_c_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).

    test_c_reader( reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_false
                   expect_avail_last_char      = abap_true
                   expect_exc_read_beyond_end  = abap_false ).

    CLEAR itab.
    reader = NEW cl_abap_itab_c_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'data_available should be false for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD itab_string_reader.

    DATA(reader) = NEW cl_abap_string_c_reader( str = CONV #( sy-abcde ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).

    test_c_reader( reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    reader = NEW cl_abap_string_c_reader( str = `` ).
    cl_abap_unit_assert=>assert_equals( msg = 'data_available should be false for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD db_c_reader.
    DATA reader TYPE REF TO cl_abap_db_c_reader.

    DATA(demo_lob_table) = VALUE demo_lob_table( idx = 1 clob1 = sy-abcde clob2 = `` ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).

    SELECT SINGLE clob1 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should not be supported' exp = abap_false act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should not be supported' exp = abap_false act = reader->is_mark_supported( ) ).

    test_c_reader( reader                    = reader
                   expect_read_auto_close      = abap_true
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    SELECT SINGLE clob2 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_equals( msg = 'data_available should be false for empty stream' exp = abap_false act = reader->data_available( ) ).

    ROLLBACK WORK.
  ENDMETHOD.

  METHOD xstring_reader.
    DATA(reader) = NEW cl_abap_string_x_reader( str = CONV #( zcl_io_test=>_01_to_1a ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).

    test_x_reader( reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    reader = NEW cl_abap_string_x_reader( VALUE #( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'data_available should be false for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD itab_x_reader.
    DATA: itab TYPE zcl_io_test=>ty_x2_s.

    zcl_io_test=>split_x( EXPORTING i_field = zcl_io_test=>_01_to_1a IMPORTING et_chunk = itab ).

    DATA(reader) = NEW cl_abap_itab_x_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).

    test_x_reader( reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_false
                   expect_avail_last_char      = abap_true
                   expect_exc_read_beyond_end  = abap_false ).

    CLEAR itab.
    reader = NEW cl_abap_itab_x_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'data_available should be false for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD itab_xstring_reader.

    DATA(reader) = NEW cl_abap_string_x_reader( str = CONV #( zcl_io_test=>_01_to_1a ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).

    test_x_reader( reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    reader = NEW cl_abap_string_x_reader( str = VALUE #( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'data_available should be false for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD db_x_reader.
    DATA reader TYPE REF TO cl_abap_db_x_reader.

    DATA(demo_lob_table) = VALUE demo_lob_table( idx = 1 blob1 = zcl_io_test=>_01_to_1a blob2 = VALUE #( ) ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).

    SELECT SINGLE blob1 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should not be supported' exp = abap_false act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should not be supported' exp = abap_false act = reader->is_mark_supported( ) ).

    test_x_reader( reader                    = reader
                   expect_read_auto_close      = abap_true
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    SELECT SINGLE blob2 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_equals( msg = 'data_available should be false for empty stream' exp = abap_false act = reader->data_available( ) ).

    ROLLBACK WORK.

  ENDMETHOD.

  METHOD string_writer.

    DATA(writer) = NEW cl_abap_string_c_writer( ).
    test_c_writer( writer ).
    cl_abap_unit_assert=>assert_equals( msg = 'string should be A...Z' exp = CONV string( sy-abcde ) act = writer->get_result_string( ) ).

  ENDMETHOD.

  METHOD xstring_writer.

    DATA(writer) = NEW cl_abap_string_x_writer( ).
    test_x_writer( writer ).
    cl_abap_unit_assert=>assert_equals( msg = 'xstring should be 01...1A' exp = CONV xstring( zcl_io_test=>_01_to_1a ) act = writer->get_result_string( ) ).

  ENDMETHOD.

  METHOD itab_x_writer.

    DATA: itab2  TYPE zcl_io_test=>ty_x2_s,
          itab3  TYPE zcl_io_test=>ty_x3_s,
          writer TYPE REF TO cl_abap_itab_x_writer,
          length TYPE i.

    writer = NEW cl_abap_itab_x_writer(
        line_type   = cl_abap_typedescr=>typekind_hex
        line_length = 2 ).
    test_x_writer( writer ).
    writer->get_result_table(
      IMPORTING
        table               = itab2
        length_of_last_line = length ).
    cl_abap_unit_assert=>assert_equals( msg = 'xstring should be 01...1A' exp = zcl_io_test=>itab_01_to_1a act = itab2 ).
    cl_abap_unit_assert=>assert_equals( msg = 'length of last line should be 2' exp = 2 act = length ).

    writer = NEW cl_abap_itab_x_writer(
        line_type   = cl_abap_typedescr=>typekind_hex
        line_length = 3 ).
    test_x_writer( writer ).
    writer->get_result_table(
      IMPORTING
        table               = itab3
        length_of_last_line = length ).
    cl_abap_unit_assert=>assert_equals( msg = 'xstring should be 01...1C' exp = zcl_io_test=>itab_01_to_1c act = itab3 ).
    cl_abap_unit_assert=>assert_equals( msg = 'length of last line should be 1' exp = 1 act = length ).

  ENDMETHOD.

ENDCLASS.

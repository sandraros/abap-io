*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test_sap_classes.
  PRIVATE SECTION.

    METHODS:
      string_reader FOR TESTING RAISING cx_static_check.
    METHODS:
      xstring_reader FOR TESTING RAISING cx_static_check.
    METHODS:
      itab_c_reader FOR TESTING RAISING cx_static_check,
      itab_string_reader FOR TESTING RAISING cx_static_check.
    METHODS:
      itab_x_reader FOR TESTING RAISING cx_static_check,
      itab_xstring_reader FOR TESTING RAISING cx_static_check.
    METHODS:
      db_string_reader FOR TESTING RAISING cx_static_check.
    METHODS:
      db_xstring_reader FOR TESTING RAISING cx_static_check.

    METHODS:
      string_writer FOR TESTING RAISING cx_static_check.
    METHODS:
      xstring_writer FOR TESTING RAISING cx_static_check.
    METHODS:
      itab_c_writer FOR TESTING RAISING cx_static_check.
    METHODS:
      itab_x_writer FOR TESTING RAISING cx_static_check.
    METHODS:
      db_string_writer FOR TESTING RAISING cx_static_check.
    METHODS:
      db_xstring_writer FOR TESTING RAISING cx_static_check.

    CONSTANTS not_applicable TYPE C LENGTH 1 VALUE '!'.

ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD string_reader.

    DATA(reader) = NEW cl_abap_string_c_reader( str = CONV #( sy-abcde ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_RESET_SUPPORTED' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_MARK_SUPPORTED' exp = abap_true act = reader->is_mark_supported( ) ).

    test_c_reader( reader                      = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    reader = NEW cl_abap_string_c_reader( str = `` ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of DATA_AVAILABLE for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD itab_c_reader.
    DATA: itab TYPE zcl_io_test=>ty_c2_s.

    zcl_io_test=>split_c( EXPORTING i_field = sy-abcde IMPORTING et_chunk = itab ).

    DATA(reader) = NEW cl_abap_itab_c_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_RESET_SUPPORTED' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_MARK_SUPPORTED' exp = abap_true act = reader->is_mark_supported( ) ).

    test_c_reader( reader                      = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_false
                   expect_avail_last_char      = abap_true
                   expect_exc_read_beyond_end  = abap_false ).

    CLEAR itab.
    reader = NEW cl_abap_itab_c_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of DATA_AVAILABLE for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD itab_string_reader.

    DATA(reader) = NEW cl_abap_string_c_reader( str = CONV #( sy-abcde ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_RESET_SUPPORTED' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_MARK_SUPPORTED' exp = abap_true act = reader->is_mark_supported( ) ).

    test_c_reader( reader                      = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    reader = NEW cl_abap_string_c_reader( str = `` ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of DATA_AVAILABLE for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD db_string_reader.
    DATA reader TYPE REF TO cl_abap_db_c_reader.

    DATA(demo_lob_table) = VALUE demo_lob_table( idx = 1 clob1 = sy-abcde clob2 = `` ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).

    SELECT SINGLE clob1 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_RESET_SUPPORTED' exp = abap_false act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_MARK_SUPPORTED' exp = abap_false act = reader->is_mark_supported( ) ).

    test_c_reader( reader                      = reader
                   expect_read_auto_close      = abap_true
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = not_applicable ).

    SELECT SINGLE clob2 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of DATA_AVAILABLE for empty stream' exp = abap_false act = reader->data_available( ) ).

    ROLLBACK WORK.
  ENDMETHOD.

  METHOD xstring_reader.
    DATA(reader) = NEW cl_abap_string_x_reader( str = CONV #( zcl_io_test=>_01_to_1a ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_RESET_SUPPORTED' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_MARK_SUPPORTED' exp = abap_true act = reader->is_mark_supported( ) ).

    test_x_reader( reader                      = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    reader = NEW cl_abap_string_x_reader( VALUE #( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of DATA_AVAILABLE for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD itab_x_reader.
    DATA: itab TYPE zcl_io_test=>ty_x2_s.

    zcl_io_test=>split_x( EXPORTING i_field = zcl_io_test=>_01_to_1a IMPORTING et_chunk = itab ).

    DATA(reader) = NEW cl_abap_itab_x_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_RESET_SUPPORTED' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_MARK_SUPPORTED' exp = abap_true act = reader->is_mark_supported( ) ).

    test_x_reader( reader                      = reader
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
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_RESET_SUPPORTED' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_MARK_SUPPORTED' exp = abap_true act = reader->is_mark_supported( ) ).

    test_x_reader( reader                      = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_true ).

    reader = NEW cl_abap_string_x_reader( str = VALUE #( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of DATA_AVAILABLE for empty stream' exp = abap_false act = reader->data_available( ) ).

  ENDMETHOD.

  METHOD db_xstring_reader.
    DATA reader TYPE REF TO cl_abap_db_x_reader.

    DATA(demo_lob_table) = VALUE demo_lob_table( idx = 1 blob1 = zcl_io_test=>_01_to_1a blob2 = VALUE #( ) ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).

    SELECT SINGLE blob1 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_RESET_SUPPORTED' exp = abap_false act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_MARK_SUPPORTED' exp = abap_false act = reader->is_mark_supported( ) ).

    test_x_reader( reader                      = reader
                   expect_read_auto_close      = abap_true
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = not_applicable ).

    SELECT SINGLE blob2 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of DATA_AVAILABLE for empty stream' exp = abap_false act = reader->data_available( ) ).

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

  METHOD itab_c_writer.

    DATA: itab2       TYPE zcl_io_test=>ty_c2_s,
          itab3       TYPE zcl_io_test=>ty_c3_s,
          itab_string TYPE TABLE OF string,
          writer      TYPE REF TO cl_abap_itab_c_writer,
          length      TYPE i.

    writer = NEW cl_abap_itab_c_writer(
        line_type   = cl_abap_typedescr=>typekind_char
        line_length = 2 ).
    test_c_writer( writer ).
    writer->get_result_table(
      IMPORTING
        table               = itab2
        length_of_last_line = length ).
    cl_abap_unit_assert=>assert_equals( msg = 'C table should be A...Z' exp = zcl_io_test=>itab_a_to_z act = itab2 ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid length of last line' exp = 2 act = length ).

    " test "LENGTH_OF_LAST_LINE"
    writer = NEW cl_abap_itab_c_writer(
        line_type   = cl_abap_typedescr=>typekind_char
        line_length = 3 ).
    writer->write( sy-abcde && '01' ).
    writer->close( ).
    writer->get_result_table(
      IMPORTING
        table               = itab3
        length_of_last_line = length ).
    cl_abap_unit_assert=>assert_equals( msg = 'C table should be A...Z01' act = itab3 exp = zcl_io_test=>itab_a_to_z01 ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid length of last line' exp = 1 act = length ).

    " table of STRING
    TYPES: ty_exp_itab_string_line TYPE c LENGTH 100.
    DATA: c_big           TYPE c LENGTH 2558,
          exp_itab_string TYPE STANDARD TABLE OF ty_exp_itab_string_line WITH EMPTY KEY.

    writer = NEW cl_abap_itab_c_writer(
        line_type   = cl_abap_typedescr=>typekind_string
        line_length = 100 ). " <== to fill maximum 100 characters per line
    cl_abap_string_utilities=>c2str_preserving_blanks( EXPORTING source = c_big IMPORTING dest = DATA(string_big) ).
    writer->write( string_big ).
    writer->close( ).
    writer->get_result_table(
      IMPORTING
        table               = itab_string
        length_of_last_line = length ).
    cl_abap_unit_assert=>assert_equals( msg = 'itab of xstring should be 26 lines (25 * 100 + 1 * 58)' act = itab_string
            exp = zcl_io_test=>split_c2( i_field = c_big it_model = exp_itab_string ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid length of last line' exp = 58 act = length ).

  ENDMETHOD.

  METHOD itab_x_writer.

    DATA: itab2        TYPE zcl_io_test=>ty_x2_s,
          itab3        TYPE zcl_io_test=>ty_x3_s,
          itab_xstring TYPE TABLE OF xstring,
          writer       TYPE REF TO cl_abap_itab_x_writer,
          length       TYPE i.

    writer = NEW cl_abap_itab_x_writer(
        line_type   = cl_abap_typedescr=>typekind_hex
        line_length = 2 ).
    test_x_writer( writer ).
    writer->get_result_table(
      IMPORTING
        table               = itab2
        length_of_last_line = length ).
    cl_abap_unit_assert=>assert_equals( msg = 'xstring should be 01...1A' exp = zcl_io_test=>itab_01_to_1a act = itab2 ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid length of last line' exp = 2 act = length ).

    " test "LENGTH_OF_LAST_LINE"
    writer = NEW cl_abap_itab_x_writer(
        line_type   = cl_abap_typedescr=>typekind_hex
        line_length = 3 ).
    writer->write( CONV #( zcl_io_test=>_01_to_1c ) ).
    writer->close( ).
    writer->get_result_table(
      IMPORTING
        table               = itab3
        length_of_last_line = length ).
    cl_abap_unit_assert=>assert_equals( msg = 'xstring should be 01...1C' act = itab3 exp = zcl_io_test=>itab_01_to_1c ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid length of last line' exp = 1 act = length ).

    " table of XSTRING
    TYPES: ty_exp_itab_xstring_line TYPE x LENGTH 100.
    DATA: x_big            TYPE x LENGTH 2558,
          exp_itab_xstring TYPE STANDARD TABLE OF ty_exp_itab_xstring_line WITH EMPTY KEY.

    writer = NEW cl_abap_itab_x_writer(
        line_type   = cl_abap_typedescr=>typekind_xstring
        line_length = 100 ). " <== to fill maximum 100 bytes per line
    writer->write( CONV #( x_big ) ).
    writer->close( ).
    writer->get_result_table(
      IMPORTING
        table               = itab_xstring
        length_of_last_line = length ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid value written to stream (should be 26 lines: 25 * 100 + 1 * 58)' act = itab_xstring
            exp = zcl_io_test=>split_x2( i_field = x_big it_model = exp_itab_xstring ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'invalid length of last line' exp = 58 act = length ).

  ENDMETHOD.

  METHOD db_string_writer.
    DATA: demo_lob_table type demo_lob_table WRITER FOR COLUMNS clob1.

    demo_lob_table = VALUE #( idx = 1 ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'invalid value of SY-SUBRC after INSERT' exp = 2 act = sy-subrc ).

    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_X_WRITER' exp = abap_false act = demo_lob_table-clob1->is_x_writer( ) ).

    test_c_writer( demo_lob_table-clob1 ).

    SELECT SINGLE clob1 FROM demo_lob_table INTO @data(clob1) WHERE idx = 1.
    cl_abap_unit_assert=>assert_equals( msg = 'invalid INSERTed data' exp = sy-abcde act = clob1 ).

    ROLLBACK WORK.
  ENDMETHOD.

  METHOD db_xstring_writer.
    DATA: demo_lob_table type demo_lob_table WRITER FOR COLUMNS blob1.

    demo_lob_table = VALUE #( idx = 1 ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'invalid value of SY-SUBRC after INSERT' exp = 2 act = sy-subrc ).

    cl_abap_unit_assert=>assert_equals( msg = 'invalid result of IS_X_WRITER' exp = abap_true act = demo_lob_table-blob1->is_x_writer( ) ).

    test_x_writer( demo_lob_table-blob1 ).

    SELECT SINGLE blob1 FROM demo_lob_table INTO @data(blob1) WHERE idx = 1.
    cl_abap_unit_assert=>assert_equals( msg = 'invalid INSERTed data' exp = CONV xstring( zcl_io_test=>_01_to_1a ) act = blob1 ).

    ROLLBACK WORK.
  ENDMETHOD.

ENDCLASS.

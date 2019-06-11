*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS
      INHERITING FROM zcl_io_test_sap_classes.
  PRIVATE SECTION.

    METHODS string_c_reader FOR TESTING RAISING cx_static_check.
    METHODS itab_c_reader FOR TESTING RAISING cx_static_check.
    METHODS itab_string_c_reader FOR TESTING RAISING cx_static_check.
    METHODS db_c_reader FOR TESTING RAISING cx_static_check.

    METHODS string_x_reader FOR TESTING RAISING cx_static_check.
    METHODS itab_x_reader FOR TESTING RAISING cx_static_check.
    METHODS itab_string_x_reader FOR TESTING RAISING cx_static_check.
    METHODS db_x_reader FOR TESTING RAISING cx_static_check.

    METHODS split_c
      IMPORTING
        i_field  TYPE clike
      EXPORTING
        et_chunk TYPE STANDARD TABLE.

    METHODS split_x
      IMPORTING
        i_field  TYPE xsequence
      EXPORTING
        et_chunk TYPE STANDARD TABLE.

ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD string_c_reader.
    DATA(reader) = NEW cl_abap_string_c_reader( str = CONV #( sy-abcde ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).
    test_c_reader( c_reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_True ).
  ENDMETHOD.

  METHOD itab_c_reader.
    TYPES: c2      TYPE c LENGTH 2,
           ty_itab TYPE STANDARD TABLE OF c2 WITH EMPTY KEY.
    DATA: itab TYPE ty_itab.

    split_c( EXPORTING i_field = sy-abcde IMPORTING et_chunk = itab ).

    DATA(reader) = NEW cl_abap_itab_c_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).
    test_c_reader( c_reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_false
                   expect_avail_last_char      = abap_true
                   expect_exc_read_beyond_end  = abap_false ).
  ENDMETHOD.

  METHOD split_c.
    DATA(i) = CAST cl_abap_elemdescr( CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( et_chunk ) )->get_table_line_type( ) )->length / cl_abap_char_utilities=>charsize.
    FIND ALL OCCURRENCES OF REGEX replace( val = '.{5}|.{1,5}$' sub = '5' with = |{ i }| occ = 0 ) IN i_field RESULTS DATA(matches).
    et_chunk = VALUE string_table( FOR <match> IN matches ( CONV #( sy-abcde+<match>-offset(<match>-length) ) ) ).
  ENDMETHOD.

  METHOD itab_string_c_reader.
    DATA(reader) = NEW cl_abap_string_c_reader( str = CONV #( sy-abcde ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).
    test_c_reader( c_reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_True ).
  ENDMETHOD.

  METHOD db_c_reader.
    DATA reader TYPE REF TO cl_abap_db_c_reader.
    DATA(demo_lob_table) = VALUE demo_lob_table( idx = 1 clob1 = sy-abcde ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).

    SELECT SINGLE clob1 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should not be supported' exp = abap_false act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should not be supported' exp = abap_false act = reader->is_mark_supported( ) ).
    test_c_reader( c_reader                    = reader
                   expect_read_auto_close      = abap_true
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_True ).
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD string_x_reader.
    DATA(reader) = NEW cl_abap_string_x_reader( str = CONV #( zcl_io_test_sap_classes=>_01_to_1a ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).
    test_x_reader( x_reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_True ).
  ENDMETHOD.

  METHOD itab_x_reader.
    TYPES: x2      TYPE x LENGTH 2,
           ty_itab TYPE STANDARD TABLE OF x2 WITH EMPTY KEY.
    DATA: itab TYPE ty_itab.

    split_x( EXPORTING i_field = zcl_io_test_sap_classes=>_01_to_1a IMPORTING et_chunk = itab ).

    DATA(reader) = NEW cl_abap_itab_x_reader( itab ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).
    test_x_reader( x_reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_false
                   expect_avail_last_char      = abap_true
                   expect_exc_read_beyond_end  = abap_false ).
  ENDMETHOD.

  METHOD split_x.
    DATA(i) = CAST cl_abap_elemdescr( CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( et_chunk ) )->get_table_line_type( ) )->length.
    et_chunk = VALUE xstring_table( FOR j = 0 THEN j + i WHILE j < xstrlen( i_field ) ( conv xstring( i_field+j(i) ) ) ).
  ENDMETHOD.

  METHOD itab_string_x_reader.
    DATA(reader) = NEW cl_abap_string_x_reader( str = CONV #( zcl_io_test_sap_classes=>_01_to_1a ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should be supported' exp = abap_true act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should be supported' exp = abap_true act = reader->is_mark_supported( ) ).
    test_x_reader( x_reader                    = reader
                   expect_read_auto_close      = abap_false
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_True ).
  ENDMETHOD.

  METHOD db_x_reader.
    DATA reader TYPE REF TO cl_abap_db_x_reader.
    DATA(demo_lob_table) = VALUE demo_lob_table( idx = 1 blob1 = zcl_io_test_sap_classes=>_01_to_1a ).
    INSERT INTO demo_lob_table VALUES demo_lob_table.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).

    SELECT SINGLE blob1 FROM demo_lob_table INTO reader WHERE idx = 1.
    cl_abap_unit_assert=>assert_subrc( msg = 'test cannot be executed' exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals( msg = 'reset should not be supported' exp = abap_false act = reader->is_reset_supported( ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'mark should not be supported' exp = abap_false act = reader->is_mark_supported( ) ).
    test_x_reader( x_reader                    = reader
                   expect_read_auto_close      = abap_true
                   expect_exc_reset_if_no_mark = abap_true
                   expect_avail_last_char      = abap_false
                   expect_exc_read_beyond_end  = abap_True ).
    ROLLBACK WORK.
  ENDMETHOD.

ENDCLASS.
